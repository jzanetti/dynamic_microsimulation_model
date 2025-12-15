library(shiny)
library(bslib)
library(plotly)
library(digest)

source("../../process/R/deps.R")
source("../../process/R/data/sample.R", local = data_sample_env)
source("../../process/R/data/utils.R", local = data_utils_env)
source("../../process/R/data/input.R", local = data_input_env)
source("../../process/R/data/output.R", local = data_output_env)
source("../../process/R/data/filename.R", local = data_filename_env)
source("../../process/R/data/tawa.R", local = data_tawa_env)
source("../../process/R/model/wrapper.R", local = model_wrapper_env)
source("../../process/R/model/linear.R", local = model_linear_env)
source("../../process/R/model/heckman_wage.R", local = model_heckman_wage_env)
source("../../process/R/vis.R", local = vis_env)
source("../../process/R/dmm.R", local = dmm_env)
source("../../process/R/person.R", local = person_env)
source("../../process/R/mortality.R", local = mortality_env)
source("../../process/R/employment.R", local = employment_env)
source("../../process/R/model/utils.R", local = model_utils_env)
source("../../process/R/model/random_utility_function.R", local = model_ruf_env)
source("../../process/R/model/validation.R", local = model_validation_env)

TAWA_DB <- "Synthetic-HES23-single-period.csv"
OUTPUT_DIR <- "../../etc/app"
FORCE_RERUN <- TRUE

# ==============================================================================
# SECTION 1: BACKEND MOCKUPS (Replica of process.Python modules)
# ==============================================================================

# --- Constants ---
HH_SIZE_PRESETS <- list(
  "No Filter" = NULL,
  "Single Adult (No Kids)" = list(
    H_Counts_Adults = c(1, 1),
    H_Counts_DependentKids = c(0, 0)
  ),
  "Single Parent (1+ Kids)" = list(
    H_Counts_Adults = c(1, 1),
    H_Counts_DependentKids = c(1, 10)
  ),
  "Couple (No Kids)" = list(
    H_Counts_Adults = c(2, 2),
    H_Counts_DependentKids = c(0, 0)
  ),
  "Couple (1+ Kids)" = list(
    H_Counts_Adults = c(2, 2),
    H_Counts_DependentKids = c(1, 3)
  )
)

# --- Main Wrapper: load_and_process_model ---
load_and_process_model <- function(input_params, hes_path=TAWA_DB, output_dir=OUTPUT_DIR) {
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  filename_hash <- data_filename_env$create_hash_filename(input_params)
  sens_path <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  acc_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  
  # 1. Check if results exist (Caching logic)
  if (FORCE_RERUN | !file.exists(sens_path)) {
    
    # Simulate reading HES data (Mock)
    hes_data <- read.csv(hes_path)
    
    # 2. Preprocess
    data <- data_tawa_env$tawa_data_preprocess(
      hes_data,
      min_hourly_wage = input_params$min_hourly_wage,
      hours_options = input_params$hours_options,
      exclude_seniors = input_params$exclude_seniors,
      apply_household_income_filter = input_params$apply_household_income_filter,
      apply_earner_type_filter = input_params$apply_earner_type_filter,
      apply_household_size_filter = input_params$apply_household_size_filter
    )
    
    # 3. Run Utility Function
    model_ruf_env$utility_func(
      data,
      input_params,
      income_name = list("market" = "market_income_per_hour"),
      working_hours_name = "working_hours",
      output_dir = output_dir,
      recreate_data = TRUE
    )
    
    # 4. Validation & Sensitivity
    model_validation_env$run_ruf_validation(input_params, output_dir = output_dir)
    model_validation_env$run_ruf_sensitivity(input_params, output_dir = output_dir)
  }
  
  # 5. Retrieve Results
  tryCatch({
    sensitivity_df <- read.csv(sens_path)
    accuracy_df <- read.csv(acc_path)
    
    # Extract scalar scores
    highest_utility_accuracy <- accuracy_df$value[accuracy_df$scores == "highest_utility_accuracy"]
    total_hrs_accuracy <- accuracy_df$value[accuracy_df$scores == "total_hrs_accuracy"]
    
    score_summary <- sprintf("Util Acc: %.2f%% | Hrs Acc: %.2f%%", highest_utility_accuracy, total_hrs_accuracy)
    
    return(list(
      sensitivity = sensitivity_df,
      metrics = list(
        utility_acc = highest_utility_accuracy,
        hours_acc = total_hrs_accuracy
      ),
      score_str = score_summary,
      params = input_params,
      success = TRUE
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, error = as.character(e)))
  })
}


# ==============================================================================
# SECTION 2: SHINY APP (UI & SERVER)
# ==============================================================================

# --- UI ---
ui <- page_sidebar(
  title = "Random Utility Function: Interactive Model",
  theme = bs_theme(version = 5, bootswatch = "cosmo"), 
  
  # CSS for spinner customization if needed
  tags$head(tags$style(".text-info { font-weight: bold; }")),
  
  sidebar = sidebar(
    width = 400,
    title = "Model Parameters",
    
    card(
      class = "shadow-sm", # cosmetic touch
      card_body(
        # Wage
        numericInput("input_wage", "Min Hourly Wage ($)", value = 23.0, step = 0.5),
        
        # Leisure
        numericInput("input_leisure", "Leisure Value", value = 23.0, step = 0.5),
        
        # Hours Options
        textInput("input_hours_opt", "Hours Options (comma separated)", value = "0, 10, 20, 30"),
        
        # Total Hours
        numericInput("input_total_hours", "Total Hours Constraint", value = 80.0),
        
        # Household Income Filter
        tags$label("Household Income Filter (Min - Max)", class = "form-label"),
        splitLayout(
          numericInput("input_inc_min", NULL, value = 0.1, step = 0.05),
          numericInput("input_inc_max", NULL, value = 0.7, step = 0.05),
          cellWidths = c("50%", "50%")
        ),
        
        # Switch
        checkboxInput("input_exclude_seniors", "Exclude Seniors", value = TRUE),
        
        # Earner Type
        selectInput("input_earner_type", "Earner type:", 
                    choices = c("All", "Primary", "Secondary"), selected = "All"),
        
        # Household Filter
        selectInput("input_hh_size", "Household Size/Type Filter",
                    choices = names(HH_SIZE_PRESETS), selected = "No Filter"),
        
        hr(),
        
        # Buttons
        layout_columns(
          col_widths = c(8, 4),
          actionButton("btn_run", "Run Model", class = "btn-primary w-100"),
          actionButton("btn_clear", "Refresh", class = "btn-secondary w-100")
        ),
        
        div(id = "status_msg", class = "mt-2 text-info", style = "font-size: 0.9em;")
      )
    )
  ),
  
  # --- Main Content ---
  div(
    class = "container-fluid p-0",
    
    # Description Header
    div(
      class = "text-center text-muted mb-4",
      h4("This portal is used to test the Employment Behaviour Model"),
      p("Cached runs are instant, but new scenarios will require calculation time", class="lead")
    ),
    
    # Right Panel Card
    card(
      card_header(
        div(class="d-flex justify-content-between align-items-center",
            span("Visualization Results", class="h5 m-0"),
            span(textOutput("header_scores", inline=TRUE), class="text-success fw-bold")
        )
      ),
      card_body(
        plotlyOutput("graph_employment_rate", height = "400px"),
        hr(),
        plotlyOutput("graph_total_hours", height = "400px"),
        h5("Run History & Accuracy Scores", class = "mt-4"),
        tableOutput("score_table")
      )
    )
  )
)


# --- Server ---
server <- function(input, output, session) {
  
  # Reactive storage (mimicking dcc.Store)
  store <- reactiveValues(history = list())
  
  # Initial status update
  output$status_msg <- renderText("Ready.")
  
  # --- Callback: Run Model ---
  observeEvent(input$btn_run, {
    
    # Notify user of processing
    output$status_msg <- renderText("Running simulation...")
    
    # Delay to allow UI to update text before heavy calculation (mimicked)
    shiny::invalidateLater(100, session)
    
    tryCatch({
      # 1. Parse Inputs
      hours_list <- as.numeric(trimws(unlist(strsplit(input$input_hours_opt, ","))))
      is_exclude_seniors <- isTRUE(input$input_exclude_seniors)
      selected_hh_filter <- HH_SIZE_PRESETS[[input$input_hh_size]]
      
      earner_type <- if(input$input_earner_type == "All") NULL else input$input_earner_type
      
      input_params <- list(
        total_hours = as.numeric(input$input_total_hours),
        min_hourly_wage = as.numeric(input$input_wage),
        leisure_value = as.numeric(input$input_leisure),
        exclude_seniors = is_exclude_seniors,
        hours_options = hours_list,
        apply_household_income_filter = list(min = input$input_inc_min, max = input$input_inc_max),
        apply_earner_type_filter = earner_type,
        apply_household_size_filter = selected_hh_filter
      )
      
      # 2. Run Backend Logic
      # Using withProgress to show visual feedback like Dash spinner
      withProgress(message = 'Calculating...', detail = 'Running micro-simulation', value = 0.5, {
        result <- load_and_process_model(input_params)
      })
      
      if (result$success) {
        # Append new result to history
        current_history <- store$history
        new_history <- c(current_history, list(result))
        store$history <- new_history
        
        output$status_msg <- renderText(paste("Run success. Total runs:", length(new_history)))
      } else {
        output$status_msg <- renderText(paste("Error:", result$error))
      }
      
    }, error = function(e) {
      output$status_msg <- renderText(paste("Input Error:", e$message))
    })
  })
  
  # --- Callback: Clear ---
  observeEvent(input$btn_clear, {
    store$history <- list()
    output$status_msg <- renderText("Plots cleared.")
  })
  
  # --- Callback: Update Graphs ---
  # Helper to get data efficiently
  history_data <- reactive({
    store$history
  })
  
  output$header_scores <- renderText({
    data <- history_data()
    if (length(data) == 0) return("")
    latest_run <- data[[length(data)]]
    paste("Latest Run:", latest_run$score_str)
  })
  
  output$graph_employment_rate <- renderPlotly({

    data <- history_data()
    fig <- plot_ly()
    
    if (length(data) > 0) {
      
      for (i in seq_along(data)) {
        run_data <- data[[i]]
        df <- run_data$sensitivity
        label_prefix <- paste("Run", i)
        
        fig <- fig %>%
          add_trace(
            x = df$scaler, 
            y = df$full_time, 
            name = paste(label_prefix, "- Full Time"),
            mode = 'lines', 
            type = 'scatter',
            line = list(dash = 'solid')
          ) %>%
          add_trace(
            x = df$scaler, 
            y = df$part_time, 
            name = paste(label_prefix, "- Part Time"),
            mode = 'lines', 
            type = 'scatter',
            line = list(dash = 'dot')
          )
      }
    }
    
    fig %>% layout(
      title = "Employment Rate Sensitivity",
      xaxis = list(title = "Income Scaler"),
      yaxis = list(title = "Employment rate (%)", tickformat = ".0%"),
      hovermode = "x unified",
      template = "plotly_white",
      legend = list(orientation = "h", y = -0.2),
      shapes = list(
        list(type="line", x0=1, x1=1, y0=0, y1=1, yref="paper", line=list(color="red", dash="dash", opacity=0.5)),
        list(type="line", x0=0, x1=1, xref="paper", y0=1, y1=1, line=list(color="blue", dash="dash", opacity=0.5))
      )
    )
  })
  
  output$graph_total_hours <- renderPlotly({
    data <- history_data()
    fig <- plot_ly()
    
    if (length(data) > 0) {
      for (i in seq_along(data)) {
        run_data <- data[[i]]
        df <- run_data$sensitivity
        label_prefix <- paste("Run", i)
        
        fig <- fig %>%
          add_trace(
            x = df$scaler, 
            y = df$total_employment_hrs, 
            name = paste(label_prefix, "- Total Hrs"),
            mode = 'lines',
            type = 'scatter'
          )
      }
    }
    
    fig %>% layout(
      title = "Total Employment Hours Sensitivity",
      xaxis = list(title = "Income Scaler"),
      yaxis = list(title = "Employment hours (%)", tickformat = ".0%"),
      hovermode = "x unified",
      template = "plotly_white",
      legend = list(orientation = "h", y = -0.2),
      shapes = list(
        list(type="line", x0=1, x1=1, y0=0, y1=1, yref="paper", line=list(color="red", dash="dash", opacity=0.5)),
        list(type="line", x0=0, x1=1, xref="paper", y0=1, y1=1, line=list(color="blue", dash="dash", opacity=0.5))
      )
    )
  })
  
  # --- Callback: Update Table ---
  output$score_table <- renderTable({
    data <- history_data()
    if (length(data) == 0) return(NULL)
    
    # Construct the table dataframe
    rows <- lapply(seq_along(data), function(i) {
      run <- data[[i]]
      params <- run$params
      metrics <- run$metrics
      
      # Handle complex objects for display
      hh_filter_display <- if (is.null(params$apply_household_size_filter)) {
        "No Filter"
      } else {
        # Find the key name that matches the value, or just print simplified
        # For simplicity in table:
        "Custom Filter" 
      }
      
      # Find matching preset key for display if possible
      for (k in names(HH_SIZE_PRESETS)) {
        if (identical(HH_SIZE_PRESETS[[k]], params$apply_household_size_filter)) {
          hh_filter_display <- k
          break
        }
      }
      
      data.frame(
        `Run #` = i,
        `Total Hrs ($)` = params$total_hours,
        `Min wage ($)` = params$min_hourly_wage,
        `Leisure ($)` = params$leisure_value,
        `Exclude seniors` = params$exclude_seniors,
        `Hours options` = paste(params$hours_options, collapse=", "),
        `Household filter` = hh_filter_display,
        `Earner` = ifelse(is.null(params$apply_earner_type_filter), "All", params$apply_earner_type_filter),
        `Utility Acc (%)` = sprintf("%.2f%%", metrics$utility_acc),
        `Hours Acc (%)` = sprintf("%.2f%%", metrics$hours_acc),
        check.names = FALSE
      )
    })
    
    bind_rows(rows)
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}

# --- Run App ---
shinyApp(ui, server)
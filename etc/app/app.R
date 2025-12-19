TAWA_DB <- "Synthetic-HES23-single-period.csv"
OUTPUT_DIR <- "runs"
SCRIPT_DIR <- "../.."


library(shiny)
library(bslib)
library(plotly)
library(digest)

# --- SOURCE DEPENDENCIES ---
source(paste0(SCRIPT_DIR, "/process/R/deps.R"))
source(paste0(SCRIPT_DIR, "/process/R/data/sample.R"), local = data_sample_env)
source(paste0(SCRIPT_DIR, "/process/R/data/utils.R"), local = data_utils_env)
source(paste0(SCRIPT_DIR, "/process/R/data/input.R"), local = data_input_env)
source(paste0(SCRIPT_DIR, "/process/R/data/output.R"), local = data_output_env)
source(paste0(SCRIPT_DIR, "/process/R/data/filename.R"), local = data_filename_env)
source(paste0(SCRIPT_DIR, "/process/R/data/tawa.R"), local = data_tawa_env)
source(paste0(SCRIPT_DIR, "/process/R/model/wrapper.R"), local = model_wrapper_env)
source(paste0(SCRIPT_DIR, "/process/R/model/linear.R"), local = model_linear_env)
source(paste0(SCRIPT_DIR, "/process/R/model/heckman_wage.R"), local = model_heckman_wage_env)
source(paste0(SCRIPT_DIR, "/process/R/vis.R"), local = vis_env)
source(paste0(SCRIPT_DIR, "/process/R/dmm.R"), local = dmm_env)
source(paste0(SCRIPT_DIR, "/process/R/person.R"), local = person_env)
source(paste0(SCRIPT_DIR, "/process/R/mortality.R"), local = mortality_env)
source(paste0(SCRIPT_DIR, "/process/R/employment.R"), local = employment_env)
source(paste0(SCRIPT_DIR, "/process/R/model/utils.R"), local = model_utils_env)
source(paste0(SCRIPT_DIR, "/process/R/model/random_utility_function.R"), local = model_ruf_env)
source(paste0(SCRIPT_DIR, "/process/R/model/validation.R"), local = model_validation_env)

# Create necessary directory if not exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# --- CONSTANTS ---

HH_SIZE_PRESETS <- list(
  "No Filter" = NULL,
  "Single Adult (No Kids)" = list(
    "H_Counts_Adults" = c(1, 1), 
    "H_Counts_DependentKids" = c(0, 0)
  ),
  "Single Parent (1+ Kids)" = list(
    "H_Counts_Adults" = c(1, 1), 
    "H_Counts_DependentKids" = c(1, 10)
  ),
  "Couple (No Kids)" = list(
    "H_Counts_Adults" = c(2, 2), 
    "H_Counts_DependentKids" = c(0, 0)
  ),
  "Couple (1+ Kids)" = list(
    "H_Counts_Adults" = c(2, 2), 
    "H_Counts_DependentKids" = c(1, 3)
  )
)

# --- HELPER FUNCTIONS ---

load_and_process_model <- function(input_params, force_rerun=FALSE, hes_path=TAWA_DB, output_dir=OUTPUT_DIR) {
  # Wraps the user's original main execution block.
  
  filename_hash <- data_filename_env$create_hash_filename(input_params)
  sens_path_check <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  
  if (force_rerun || !file.exists(sens_path_check)) {
    # 1. Read Data
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
  
  sens_path <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  acc_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  
  tryCatch({
    sensitivity_df <- read.csv(sens_path)
    accuracy_df <- read.csv(acc_path)
    
    # Extract scalar scores
    highest_utility_accuracy <- accuracy_df[accuracy_df$scores == "highest_utility_accuracy", "value"][1]
    total_hrs_accuracy <- accuracy_df[accuracy_df$scores == "total_hrs_accuracy", "value"][1]
    r2_mcfadden <- accuracy_df[accuracy_df$scores == "r2_mcfadden", "value"][1]
    score_summary <- sprintf("Util Acc: %.2f%% | Hrs Acc: %.2f%% | R2: %.2f", round(highest_utility_accuracy, 2), round(total_hrs_accuracy, 2), round(r2_mcfadden, 2))
    
    # Return list (equivalent to Python Dict)
    return(list(
      "sensitivity" = sensitivity_df, 
      "metrics" = list(
        "utility_acc" = highest_utility_accuracy,
        "hours_acc" = total_hrs_accuracy,
        "r2" = r2_mcfadden
      ),
      "score_str" = score_summary,
      "params" = input_params,
      "success" = TRUE
    ))
    
  }, error = function(e) {
    return(list("success" = FALSE, "error" = as.character(e)))
  })
}

# 1. Define a helper function to create labels with help text
render_label_with_help <- function(label_text, help_text, id_suffix) {
  div(class = "d-flex align-items-center mb-1",
      tags$label(label_text, class = "me-2", `for` = paste0("input-", id_suffix)),
      
      # 2. The Tooltip (Wraps the badge)
      tooltip(
        placement = "right",
        # The first argument is the 'trigger' (the UI element you hover over)
        trigger = span("?", 
                       id = paste0("tooltip-target-", id_suffix),
                       class = "badge bg-light text-primary border ms-1",
                       style = "cursor: help; border-radius: 50%; padding: 4px 8px;"
        ),
        # The second argument is the text to display
        help_text
      )
  )
}

# --- SHINY APP SETUP ---

ui <- page_fluid(
  theme = bs_theme(version = 5),
  
  # --- ADDED: FULL PAGE FREEZE OVERLAY ---
  # When 'shiny-busy' class is active (calculation running), this div covers everything.
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    div(
      # CSS for full-screen overlay with transparency
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 9999; 
               background-color: rgba(255, 255, 255, 0.7); display: flex; justify-content: center; align-items: center;",
      
      # The inner box with the spinner and text
      div(
        style = "background-color: white; padding: 20px 40px; border-radius: 10px; 
                 box-shadow: 0 4px 15px rgba(0,0,0,0.2); border: 1px solid #ddd;
                 display: flex; align-items: center; flex-direction: column;",
        
        div(class = "spinner-border text-primary mb-3", style="width: 3rem; height: 3rem;", role="status"),
        h4("Model Running...", class = "text-primary")
      )
    )
  ),
  # -----------------------------
  
  div(class = "container-fluid",
      div(class = "text-center my-4",
          h2("Employment Behaviour Model TestBed"),
          p(class = "lead text-muted mb-4", 
            "This portal is used to test the Employment Behaviour Model ~ Cached runs are instant, but new scenarios will require calculation time")
      ),
      
      # 2. Academic Context
      div(class = "text-center text-muted small",
          markdown(
            "The model is implemented based on the discrete choice model described in [Aaberge and Colombino (2018)](https://www.microsimulation.pub/articles/00177) and [Bronka et al. (2025)](https://www.microsimulation.pub/articles/00318)."
          )
      ),
      
      div(class = "row",
          # --- LEFT PANEL: PARAMETERS ---
          div(class = "col-4",
              div(class = "card h-100",
                  div(class = "card-header", "Model Parameters"),
                  div(class = "card-body",
                      # Wage
                      render_label_with_help(
                        "Min Hourly Wage ($)", 
                        "The minimum wage is assigned to people who just start working, ideally this should come from Heckman Wage Model, but minimum wage is used here for simplicity", 
                        "wage"
                      ),
                      numericInput("input_wage", NULL, value = 23.0, step = 0.5, width = "100%"),
                      div(class="mb-2"),
                      
                      # Leisure
                      render_label_with_help(
                        "Leisure Value ($)", 
                        "How much leisure worth ($), by default it is set to minimum wage/hour", 
                        "leisure"
                      ),
                      numericInput("input_leisure", NULL, value = 23.0, step = 0.5, width = "100%"),
                      div(class="mb-2"),
                      
                      render_label_with_help(
                        "Working hours options (comma separated)", 
                        "How many hours people may choose to work per week. The result is very sensitive to this option.", 
                        "hours"
                      ),
                      textInput("input_hours_opt", NULL, value = "0, 10, 20, 30, 40", width = "100%"),
                      div(class="mb-2"),
                      
                      render_label_with_help(
                        "Total Hours Constraint", 
                        "How many hours people can spend in total per week. Based on most literatures, it is set to 80 hours.", 
                        "hours2"
                      ),
                      numericInput("input_total_hours", NULL, value = 80.0, width = "100%"),
                      div(class="mb-2"),
                      
                      render_label_with_help(
                        "Household Income Filter (Min - Max)", 
                        "These parameters define thresholds to exclude values that are too large or too small, thereby preserving numerical stability.", 
                        "value_filter"
                      ),
                      div(class = "row mb-2",
                          div(class = "col", numericInput("input_inc_min", NULL, value = 0.1, step = 0.01)),
                          div(class = "col", numericInput("input_inc_max", NULL, value = 0.7, step = 0.01))
                      ),
                      
                      hr(),
                      
                      tags$label("Earner type:"),
                      selectInput("input_earner_type", NULL, 
                                  choices = c("All", "Primary", "Others"), 
                                  selected = "All", selectize = FALSE, width = "100%"),
                      div(class="mb-3"),
                      
                      # Household filter
                      tags$label("Household Size/Type Filter"),
                      selectInput("input_hh_size", NULL, 
                                  choices = names(HH_SIZE_PRESETS), 
                                  selected = "No Filter", selectize = FALSE, width = "100%"),
                      div(class="mb-3"),
                      
                      # Checklist equivalent
                      checkboxInput("input_exclude_seniors", "Exclude Seniors (Seniors may have very different behaviours)", value = TRUE),
                      div(class="mb-3"),
                      
                      hr(),
                      
                      # Switch
                      div(class = "form-check form-switch mb-3 text-danger",
                          tags$input(class = "form-check-input", type = "checkbox", id = "switch_force_rerun"),
                          tags$label(class = "form-check-label", `for` = "switch_force_rerun", "Force Rerun (Ignore Cache)")
                      ),
                      
                      # Buttons
                      div(class = "row",
                          div(class = "col-8", actionButton("btn_run", "Run Model", class = "btn-primary w-100")),
                          div(class = "col-4", actionButton("btn_clear", "Refresh / Clear", class = "btn-outline-secondary w-100"))
                      ),
                      
                      # Spinner / Status
                      div(class = "mt-2",
                          uiOutput("status_msg") # Contains the text and spinner logic
                      )
                  )
              )
          ),
          
          # --- RIGHT PANEL: VISUALIZATION ---
          div(class = "col-8",
              div(class = "card",
                  div(class = "card-header",
                      span("Visualization Results"),
                      span(id = "header_scores", class = "float-end text-success", style = "font-size: 0.9rem; font-weight: bold;")
                  ),
                  
                  div(class = "card-body",
                      plotlyOutput("graph_total_hours", height = "400px"),
                      hr(),
                      h5("Run History & Accuracy Scores", class = "mt-4"),
                      uiOutput("score_table_container")
                  )
              )
          )
      )
  )
)

# --- SERVER ---

server <- function(input, output, session) {
  
  # Initialize Reactive Stores
  store <- reactiveValues(
    history = list(),
    status_text = NULL
  )
  
  # Combined Observer for Run and Clear
  observeEvent(list(input$btn_run, input$btn_clear), {
    # Trigger logic placeholder
  }, ignoreInit = TRUE)
  
  # 1. Handle Clear
  observeEvent(input$btn_clear, {
    store$history <- list()
    store$status_text <- "Plots cleared."
  })
  
  # 2. Handle Run
  observeEvent(input$btn_run, {
    
    # Spinner logic: We can render the spinner immediately here
    output$status_msg <- renderUI({
      div(class="spinner-border spinner-border-sm text-primary", role="status", style="margin-right: 5px;")
    })
    
    tryCatch({
      # Parse inputs
      hours_opt_str <- input$input_hours_opt
      hours_list <- as.numeric(trimws(unlist(strsplit(hours_opt_str, ","))))
      
      exclude_seniors <- input$input_exclude_seniors # Boolean in Shiny
      hh_size_selection <- input$input_hh_size
      selected_hh_filter <- HH_SIZE_PRESETS[[hh_size_selection]]
      
      earner_type <- input$input_earner_type
      if (earner_type == "All") {
        earner_type <- NULL
      }
      
      input_params <- list(
        "total_hours" = as.numeric(input$input_total_hours),
        "min_hourly_wage" = as.numeric(input$input_wage),
        "leisure_value" = as.numeric(input$input_leisure),
        "exclude_seniors" = exclude_seniors,
        "hours_options" = hours_list,
        "apply_household_income_filter" = list("min" = as.numeric(input$input_inc_min), "max" = as.numeric(input$input_inc_max)),
        "apply_earner_type_filter" = earner_type,
        "apply_household_size_filter" = selected_hh_filter
      )
      
      # Run Backend Logic
      force_rerun_val <- isTRUE(input$switch_force_rerun)
      
      result <- load_and_process_model(input_params, force_rerun = force_rerun_val)
      
      if (result$success) {
        # Append new result to history
        new_history <- c(store$history, list(result))
        store$history <- new_history
        store$status_text <- paste("Run success. Total runs:", length(new_history))
      } else {
        store$status_text <- paste("Error:", result$error)
      }
      
    }, error = function(e) {
      store$status_text <- paste("Input Error:", as.character(e))
    })
  })
  
  # Status Message Output
  output$status_msg <- renderUI({
    div(class = "mt-2 text-info", style = "font-size: 0.9em", store$status_text)
  })
  
  # Header Scores Update
  observe({
    history_data <- store$history
    header_text <- ""
    if (length(history_data) > 0) {
      latest_run <- history_data[[length(history_data)]]
      header_text <- paste("Latest Run:", latest_run$score_str)
    }
    
    removeUI(selector = "#header_scores", immediate = TRUE)
    insertUI(selector = ".card-header span:first-child", where = "afterEnd",
             ui = span(id = "header_scores", class = "float-end text-success", 
                       style = "font-size: 0.9rem; font-weight: bold;", header_text))
  })
  
  
  # Graphs Update
  output$graph_total_hours <- renderPlotly({
    history_data <- store$history
    
    fig_hours <- plot_ly()
    
    # Iterate through history
    for (i in seq_along(history_data)) {
      run_data <- history_data[[i]]
      df <- run_data$sensitivity
      
      label_prefix <- paste("Run", i)
      
      fig_hours <- add_trace(fig_hours, 
                             x = df$scaler, 
                             y = df$total_employment_hrs,
                             type = 'scatter', 
                             mode = 'lines',
                             name = paste(label_prefix, "- Total Hrs")
      )
    }
    
    # Formatting Graph
    fig_hours <- layout(fig_hours,
                        title = "Total Employment Hours Sensitivity",
                        xaxis = list(title = "Income Scaler"),
                        yaxis = list(title = "Employment hours (%)", tickformat = ".0%"),
                        hovermode = "x unified",
                        template = "plotly_white",
                        legend = list(orientation = "h", y = -0.2),
                        shapes = list(
                          list(type = "line", x0 = 1.0, x1 = 1.0, y0 = 0, y1 = 1, yref = "paper", line = list(color = "red", dash = "dash", opacity = 0.5)),
                          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 1.0, y1 = 1.0, line = list(color = "blue", dash = "dash", opacity = 0.5))
                        )
    )
    
    fig_hours
  })
  
  # Score Table Update
  output$score_table_container <- renderUI({
    history_data <- store$history
    
    if (length(history_data) == 0) {
      return(div(class = "alert alert-light", "No runs yet."))
    }
    
    # Define Table Header
    thead <- tags$thead(tags$tr(
      tags$th("Run #"),
      tags$th("Total Hrs ($)"),
      tags$th("Min wage ($)"),
      tags$th("Leisure ($)"),
      tags$th("Exclude seniors"),
      tags$th("Hours options"),
      tags$th("Household filter"),
      tags$th("Earner"),
      tags$th("Utility Acc (%)"),
      tags$th("Hours Acc (%)"),
      tags$th("R2")
    ))
    
    # Define Table Rows
    rows <- lapply(seq_along(history_data), function(i) {
      run <- history_data[[i]]
      params <- run$params
      metrics <- run$metrics
      
      hh_filter_display <- if(is.null(params$apply_household_size_filter)) "No Filter" else names(HH_SIZE_PRESETS)[match(list(params$apply_household_size_filter), HH_SIZE_PRESETS)]
      if (length(hh_filter_display) == 0) hh_filter_display <- "Filtered"
      
      earner_display <- if(is.null(params$apply_earner_type_filter)) "All" else params$apply_earner_type_filter
      
      row_style <- if (i == length(history_data)) "font-weight: bold; background-color: #f8f9fa;" else ""
      
      tags$tr(style = row_style,
              tags$td(i),
              tags$td(as.character(params$total_hours)),
              tags$td(as.character(params$min_hourly_wage)),
              tags$td(as.character(params$leisure_value)),
              tags$td(as.character(params$exclude_seniors)),
              tags$td(paste(params$hours_options, collapse=", ")),
              tags$td(as.character(hh_filter_display)),
              tags$td(as.character(earner_display)),
              tags$td(sprintf("%.2f%%", metrics$utility_acc)),
              tags$td(sprintf("%.2f%%", metrics$hours_acc)),
              tags$td(sprintf("%.2f", metrics$r2))
      )
    })
    
    tags$table(class = "table table-bordered table-hover table-striped table-responsive",
               thead,
               tags$tbody(rows)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
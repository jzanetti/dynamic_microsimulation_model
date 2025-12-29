
plot_intermediate <- function(input_params, data_name, tawa_data_name = "sq", output_dir = "/tmp") {
  
  # Assuming create_hash_filename exists in your R environment. 
  # If input_params is a list, this functions similarly to the Python version.
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  
  # --- Scenario 1: RUF Validation (Histogram/Bar) ---
  if (data_name == "ruf_validation") {
    
    # Read Data
    file_path <- file.path(output_dir, paste0("validation_err_", filename_hash, ".csv"))
    err_dist_results <- read_csv(file_path, show_col_types = FALSE)
    
    # Calculate bin width logic (needed for geom_rect to mimic align='edge')
    # In Python align='edge' means the bar starts at x and goes right.
    
    p <- ggplot(err_dist_results) +
      geom_rect(aes(
        xmin = bin_start,
        xmax = bin_end,
        ymin = 0,
        ymax = count
      ), 
      fill = "steelblue", 
      color = "black", 
      alpha = 0.7) +
      labs(title = "Validation Error Distribution") + # Added title for clarity
      theme_minimal()
    
    output_path <- file.path(output_dir, paste0("ruf_validation_err_dist_", filename_hash, ".png"))
    ggsave(output_path, plot = p, width = 10, height = 6, dpi = 300)
    
    print(paste0("The plots are written with hashname: ", filename_hash))
  }
  
  # --- Scenario 2: RUF Sensitivity (Line Charts) ---
  if (data_name == "ruf_sensitivity") {
    
    output_path1 <- file.path(output_dir, paste0("utility_employment_rate_", filename_hash, ".png"))
    output_path2 <- file.path(output_dir, paste0("ruf_total_employment_hrs_", filename_hash, ".png"))
    
    # Read Data
    sensitivity_results <- read_csv(file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv")), show_col_types = FALSE)
    accuracy_results <- read_csv(file.path(output_dir, paste0("validation_score_", filename_hash, ".csv")), show_col_types = FALSE)
    
    # Extract values using dplyr
    highest_utility_accuracy <- accuracy_results %>% 
      filter(scores == "highest_utility_accuracy") %>% 
      pull(value)
    
    total_hrs_accuracy <- accuracy_results %>% 
      filter(scores == "total_hrs_accuracy") %>% 
      pull(value)
    
    r2_mcfadden <- accuracy_results %>% 
      filter(scores == "r2_mcfadden") %>% 
      pull(value)
    
    # Construct String
    accuracy_score_str <- paste0(
      "Total Utility Accuracy: ", round(highest_utility_accuracy, 2), " %, ",
      "Total Hours Accuracy: ", round(total_hrs_accuracy, 2), " %; ",
      "McFadden's R2: ", round(r2_mcfadden, 2)
    )
    
    # --- Active Plot: Total Employment Hours ---
    p2 <- ggplot(sensitivity_results, aes(x = scaler, y = total_employment_hrs)) +
      geom_line() +
      geom_vline(xintercept = 1.0, color = 'red', linetype = "dashed") +
      geom_hline(yintercept = 1.0, color = 'blue', linetype = "dashed") +
      # Matplotlib used PercentFormatter(xmax=1.0), implying 1.0 = 100%
      scale_y_continuous(labels = scales::percent) + 
      labs(
        title = paste0("Employment behaviours with Random Utility Function \n ", accuracy_score_str),
        x = "Income Scaler",
        y = "Employment hours"
      ) +
      theme_minimal()
    
    ggsave(output_path2, plot = p2, width = 10, height = 6, dpi = 300)
    
    print(paste0("The plots are written with hashname: ", filename_hash))
  }
}


# --- 2. Plot Outputs ---
plot_outputs <- function(output_results, output_dir = "") {
  
  ensure_dir(output_dir)
  
  # -- Plot 1: Population Status (Alive/Dead) --
  pop_data <- output_results$population$pop
  
  p1 <- ggplot(pop_data, aes(x = year, y = count, color = life_stage)) +
    geom_line(linewidth = 1) +
    labs(title = "Number of people: Alive/Dead", x = "Year", y = "Count") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "grey80")) + # Grid
    scale_x_continuous(breaks = function(x) unique(floor(x))) # Integer ticks
  
  ggsave(file.path(output_dir, "results_pop_stats.png"), plot = p1, width = 10, height = 6)
  
  # -- Plot 2: Household Status --
  hhd_data <- output_results$population$hhd
  
  p2 <- ggplot(hhd_data, aes(x = year, y = count)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(title = "Number of household", x = "Year", y = "Count") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "grey80")) +
    scale_x_continuous(breaks = function(x) unique(floor(x)))
  
  ggsave(file.path(output_dir, "results_hhd_stats.png"), plot = p2, width = 10, height = 6)
  
  # -- Plot 3: Employment Income --
  # Combine the separate dataframes in the list into one for easier plotting
  income_list <- output_results$employment$income
  
  # Helper to standardize df before binding
  prep_df <- function(df_name) {
    d <- income_list[[df_name]]
    # The value column has the same name as the key in the python dict logic
    # We rename it to 'value' for ggplot
    col_val <- df_name 
    d %>% 
      select(year, value = all_of(col_val)) %>%
      mutate(metric = col_val)
  }
  
  keys <- c("mean_market_income_per_week", "mean_latent_market_income_per_week")
  plot_data <- bind_rows(lapply(keys, prep_df))
  
  p3 <- ggplot(plot_data, aes(x = year, y = value, color = metric)) +
    geom_line(linewidth = 1) +
    labs(title = "Income Statistics (Per Week)", x = "Year", y = "Value") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "grey80")) +
    scale_x_continuous(breaks = function(x) unique(floor(x)))
  
  ggsave(file.path(output_dir, "employment_income.png"), plot = p3, width = 10, height = 6)
}

# --- 3. Plot Inputs (HTML Report) ---
plot_inputs <- function(df, exclude_cols = c("id"), plots_per_row = 2, output_dir = "") {
  
  ensure_dir(output_dir)
  
  # Filter columns
  all_cols <- setdiff(names(df), exclude_cols)
  
  if (length(all_cols) == 0) {
    writeLines("<html><body><p>No columns found for distributions.</p></body></html>", 
               file.path(output_dir, "input_distributions.html"))
    return()
  }
  
  # Identify types
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  numeric_cols <- intersect(numeric_cols, all_cols)
  
  non_numeric_cols <- setdiff(all_cols, numeric_cols)
  
  # Combine list
  plot_cols <- c(numeric_cols, non_numeric_cols)
  num_plots <- length(plot_cols)
  
  if (num_plots == 0) return()
  
  num_rows <- ceiling(num_plots / plots_per_row)
  
  # Start HTML string construction
  html_parts <- c(
    "<html>",
    "<head>",
    "<style>",
    "table { width: 100%; border-collapse: collapse; }",
    "td { text-align: center; padding: 10px; }",
    "img { max-width: 100%; height: auto; }",
    "</style>",
    "</head>",
    "<body>",
    "<table>"
  )
  
  plot_index <- 1
  
  for (r in 1:num_rows) {
    html_parts <- c(html_parts, "<tr>")
    
    for (c in 1:plots_per_row) {
      if (plot_index <= num_plots) {
        col_name <- plot_cols[plot_index]
        is_num <- col_name %in% numeric_cols
        
        # create plot object
        p <- NULL
        if (is_num) {
          # Histogram
          p <- ggplot(df, aes_string(x = col_name)) +
            geom_histogram(bins = 20, fill = "skyblue", color = "black") +
            labs(title = paste("Distribution of", col_name), y = "Frequency") +
            theme_minimal()
        } else {
          # Pie Chart
          # Aggregate counts
          counts <- df %>% count(.data[[col_name]]) %>% mutate(prop = n/sum(n))
          
          # If too many categories, show legend on side, else standard
          show_legend <- nrow(counts) > 10
          
          p <- ggplot(counts, aes(x = "", y = prop, fill = .data[[col_name]])) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            labs(title = paste("Distribution of", col_name, "(%)"), x = NULL, y = NULL) +
            theme_void() + 
            geom_text(aes(label = scales::percent(prop, accuracy = 1.1)), 
                      position = position_stack(vjust = 0.5), 
                      check_overlap = TRUE, size = 3)
          
          # Remove legend if not strictly needed or handle large legends
          if (!show_legend) p <- p + theme(legend.position = "right")
        }
        
        # Save plot to temp file then encode to base64
        tmp_file <- tempfile(fileext = ".png")
        ggsave(tmp_file, plot = p, width = 6, height = 4, dpi = 100)
        
        # Read and encode
        b64_str <- base64enc::base64encode(tmp_file)
        unlink(tmp_file) # Clean up
        
        html_parts <- c(html_parts, sprintf('<td><img src="data:image/png;base64,%s" alt="%s"></td>', b64_str, col_name))
        
        plot_index <- plot_index + 1
      } else {
        html_parts <- c(html_parts, "<td></td>")
      }
    }
    html_parts <- c(html_parts, "</tr>")
  }
  
  html_parts <- c(html_parts, "</table></body></html>")
  
  # Write file
  writeLines(paste(html_parts, collapse = "\n"), file.path(output_dir, "input_distributions.html"))
}
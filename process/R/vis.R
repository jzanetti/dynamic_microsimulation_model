
plot_intermediate <- function(input_params, data_name, tawa_data_name = "sq", output_dir = "/tmp", data_scaler = 1000.0) {
  
  # Assuming create_hash_filename exists in your R environment. 
  # If input_params is a list, this functions similarly to the Python version.
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  
  # --- Scenario 1: RUF Validation (Histogram/Bar) ---
  if (data_name == "ruf_validation") {

    # 1. Error Distribution Bar Plot
    err_dist_results <- read_csv(file.path(output_dir, paste0("validation_err_", filename_hash, ".csv")))
    
    p1 <- ggplot(err_dist_results, aes(xmin = bin_start, xmax = bin_end, ymin = 0, ymax = count)) +
      geom_rect(fill = "steelblue", color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Error Distribution", x = "Bin Range", y = "Count")
    
    ggsave(file.path(output_dir, paste0("ruf_validation_err_dist_", filename_hash, ".png")), 
           plot = p1, width = 10, height = 6)

    # 2. Scatter Plots with shared Colorbar
    param_results <- read_csv(file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv")))
    
    # Convert parameters to a named vector/list
    params <- setNames(param_results$Value, param_results$parameter)
    params_list <- params[c("beta_income_hhld", "beta_income_hhld2", "beta_leisure", "beta_leisure2", "beta_interaction")]
    
    data_to_check <- read_parquet(file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet")))
    
    # Calculate Utility (Z)
    # Ensure your R quadratic_utility function is vectorized
    data_to_check$Z <- model_ruf_env$quadratic_utility(
      params_list,
      data_to_check$income,
      data_to_check$income_hhld,
      data_to_check$leisure
    )
    # Add leisure hours for coloring
    data_to_check$leisure_hrs <- data_to_check$leisure * data_scaler / 23
    
    # Individual Income Plot
    scatter1 <- ggplot(data_to_check, aes(x = income * data_scaler, y = Z, color = leisure_hrs)) +
      geom_point(alpha = 0.7, size = 1.5) +
      scale_color_viridis_c() +
      labs(x = "Disposable Income ($, individual) per week", y = "Utility", color = "Leisure (hours)") +
      theme_minimal()
    
    # Household Income Plot
    scatter2 <- ggplot(data_to_check, aes(x = income_hhld * 1000, y = Z, color = leisure_hrs)) +
      geom_point(alpha = 0.7, size = 1.5) +
      scale_color_viridis_c() +
      labs(x = "Disposable Income ($, household) per week", y = "Utility", color = "Leisure (hours)") +
      theme_minimal()

    # Combine plots with patchwork (shared legend)
    combined_scatter <- (scatter1 + scatter2) + 
      plot_layout(guides = "collect") & 
      theme(legend.position = "right")
    
    ggsave(file.path(output_dir, paste0("ruf_validation_scatter_", filename_hash, ".png")), 
           plot = combined_scatter, width = 14, height = 6)
    
    print(paste("Validation plots written with hash:", filename_hash))
  }
  
  if (data_name == "ruf_sensitivity") {
    
    sensitivity_results <- read_csv(file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv")))
    accuracy_results <- read_csv(file.path(output_dir, paste0("validation_score_", filename_hash, ".csv")))
    
    # Extract scores
    hi_util_acc <- accuracy_results$value[accuracy_results$scores == "highest_utility_accuracy"]
    tot_hrs_acc <- accuracy_results$value[accuracy_results$scores == "total_hrs_accuracy"]
    r2_mcfadden <- accuracy_results$value[accuracy_results$scores == "r2_mcfadden"]
    
    accuracy_score_str <- sprintf(
      "Total Utility Accuracy: %.2f%%, Total Hours Accuracy: %.2f%%; McFadden's R2: %.2f",
      hi_util_acc, tot_hrs_acc, r2_mcfadden
    )
    
    # Sensitivity Plot
    p_sens <- ggplot(sensitivity_results, aes(x = scaler, y = total_employment_hrs)) +
      geom_line(color = "black") +
      geom_vline(xintercept = 1.0, color = "red", linetype = "dashed") +
      geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste("Employment behaviours with Random Utility Function\n", accuracy_score_str),
           x = "Income Scaler",
           y = "Employment hours") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("ruf_total_employment_hrs_", filename_hash, ".png")), 
           plot = p_sens, width = 10, height = 6)
    
    print(paste("Sensitivity plots written with hash:", filename_hash))
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

run_ruf_sensitivity <- function(input_params, 
                                income_scaler = seq(0.5, 1.9, by = 0.1), 
                                output_dir = "", 
                                plot_using_ratio = TRUE) {


  filename_hash <- data_filename_env$create_hash_filename(input_params)
  data_output_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  model_output_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  accuracy_output_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  results_path <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  
  print(paste("Model estimated parameters are read from", model_output_path))
  model_params_df <- read_csv(model_output_path, show_col_types = FALSE)
  model_params <- setNames(model_params_df$Value, model_params_df$parameter)
  
  print(paste("Model training data are read from", data_output_path))
  data_to_check <- read_parquet(data_output_path)
  
  # Note: model_accuracy is read but not used in the logic, kept for consistency with Python
  print(paste("Model accuracy is read from", accuracy_output_path))
  model_accuracy_df <- read_csv(accuracy_output_path, show_col_types = FALSE)
  model_accuracy <- setNames(model_accuracy_df$value, model_accuracy_df$scores)
  
  # Initialize results lists
  res_full_time <- numeric()
  res_part_time <- numeric()
  res_total_employment_hrs <- numeric()
  
  # Extract hour options for easier access
  # Python [-1] is last element; Python [1] is second element (index 1)
  # R is 1-based, so [1] becomes [2], and [-1] becomes length()
  hours_options <- input_params[["hours_options"]]
  last_option <- hours_options[length(hours_options)]
  second_option <- hours_options[2]
  
  for (scaler in income_scaler) {
    
    print(paste("Processing sensitivity study for scaler:", scaler))
    
    # Assuming ruf_predict is loaded in the environment
    predicted_choices <- model_ruf_env$predict(data_to_check, model_params, method = "top30", scaler = scaler)
    
    n_preds <- nrow(predicted_choices)
    
    # Full Time Rate
    ft_count <- nrow(filter(predicted_choices, option_hours >= last_option))
    full_time_employment_rate <- round((ft_count / n_preds) * 100, 2)
    
    # Part Time Rate
    # Python: >= options[1] AND < options[-1]
    pt_count <- nrow(filter(predicted_choices, 
                            option_hours >= second_option & option_hours < last_option))
    part_time_employment_rate <- round((pt_count / n_preds) * 100, 2)
    
    # Total Employment Hours
    # Logic: Sum of the mean option_hours per person
    total_hrs <- predicted_choices %>%
      group_by(people_id) %>%
      summarise(mean_hrs = mean(option_hours, na.rm = TRUE)) %>%
      summarise(sum_mean = sum(mean_hrs, na.rm = TRUE)) %>%
      pull(sum_mean)
    
    # Append to results
    res_full_time <- c(res_full_time, full_time_employment_rate)
    res_part_time <- c(res_part_time, part_time_employment_rate)
    res_total_employment_hrs <- c(res_total_employment_hrs, total_hrs)
  }
  
  # Create Results DataFrame
  results <- data.frame(
    full_time = res_full_time,
    part_time = res_part_time,
    total_employment_hrs = res_total_employment_hrs,
    scaler = income_scaler
  )
  
  if (plot_using_ratio) {
    # Find index where scaler is approximately 1.0
    # Python uses isclose with atol=1e-5. In R:
    idx <- which(abs(results$scaler - 1.0) < 1e-5)[1]
    
    if (!is.na(idx)) {
      results$full_time <- results$full_time / results$full_time[idx]
      results$part_time <- results$part_time / results$part_time[idx]
      results$total_employment_hrs <- results$total_employment_hrs / results$total_employment_hrs[idx]
    }
  }
  
  print(paste("Writing sensitivity study results to", results_path))
  write_csv(results, results_path)
}


run_ruf_validation <- function(input_params, output_dir, method = "top30") {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params)
  
  parquet_path <- file.path(
    output_dir, 
    paste0("utility_func_data_", 
           filename_hash, 
           ".parquet"))
  data <- read_parquet(parquet_path)
  
  csv_path <- file.path(
    output_dir, 
    paste0("utility_func_parameters_", 
           filename_hash, 
           ".csv"))
  params_df <- read_csv(csv_path, show_col_types = FALSE)
  
  params <- setNames(params_df$Value, params_df$parameter)
  

  predicted_choices <- model_ruf_env$predict(data, params, method = method)
  
  pred_n <- nrow(filter(predicted_choices, is_chosen == 1))
  truth_n <- nrow(filter(data, is_chosen == 1))
  truth_hrs <- sum(data$option_hours[data$is_chosen == 1], na.rm = TRUE)
  
  # Logic for method == "top30"
  if (method == "top30") {
    pred_hrs <- data %>%
      group_by(people_id) %>%
      summarise(mean_hrs = mean(option_hours, na.rm = TRUE)) %>%
      summarise(total_pred_hrs = sum(mean_hrs)) %>%
      pull(total_pred_hrs)
  } else {
    pred_hrs <- sum(
      predicted_choices$option_hours[predicted_choices$is_chosen == 1], na.rm = TRUE)
  }
  
  # Create Scores Dataframe
  scores <- data.frame(
    scores = c("highest_utility_accuracy", "total_hrs_accuracy"),
    value = c(100.0 * pred_n / truth_n, 100.0 * pred_hrs / truth_hrs)
  )
  
  # Write to CSV
  output_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  write_csv(scores, output_path)
}

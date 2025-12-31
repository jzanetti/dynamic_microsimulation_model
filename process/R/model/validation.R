

run_ruf_sensitivity <- function(input_params,
                                income_scaler = seq(0.5, 2.0, by = 0.1),
                                output_dir = "",
                                tawa_data_name = "sq",
                                plot_using_ratio = TRUE,
                                method = RUF_METHOD) {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  data_output_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  model_output_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  accuracy_output_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  results_path <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  
  message(paste("Model estimated parameters are read from", model_output_path))
  model_params_df <- read_csv(model_output_path, show_col_types = FALSE)
  # Convert to named vector (equivalent to Python dict)
  model_params <- setNames(model_params_df$Value, model_params_df$parameter)
  
  message(paste("Model training data are read from", data_output_path))
  data_to_check <- read_parquet(data_output_path)
  
  message(paste("Model accuracy is read from", accuracy_output_path))
  model_accuracy_df <- read_csv(accuracy_output_path, show_col_types = FALSE)
  model_accuracy <- setNames(model_accuracy_df$value, model_accuracy_df$scores)
  
  total_employment_hrs <- c()
  
  for (scaler in income_scaler) {
    print(paste("Processing sensitivity study for scaler:", scaler))
    
    # Logic for ruf_predict
    predicted_choices <- model_ruf_env$predict(
      data_to_check, 
      model_params, 
      method = method, 
      scaler = scaler
    )
    
    # Calculate sum of mean hours per person
    person_mean_hrs <- predicted_choices %>%
      group_by(people_id) %>%
      summarise(mean_hrs = mean(hours, na.rm = TRUE), .groups = "drop")
    
    total_employment_hrs <- c(total_employment_hrs, sum(person_mean_hrs$mean_hrs))
  }
  
  results <- data.frame(
    scaler = income_scaler,
    total_employment_hrs = total_employment_hrs
  )
  
  if (plot_using_ratio) {
    # Find index where scaler is close to 1.0 (numpy.isclose logic)
    index <- which(abs(results$scaler - 1.0) < (1.0 / 1e5))[1]
    
    if (!is.na(index)) {
      results$total_employment_hrs <- results$total_employment_hrs / results$total_employment_hrs[index]
    }
  }
  
  print(paste("Writing sensitivity study results to", results_path))
  write_csv(results, results_path)
}


run_ruf_validation <- function(input_params, 
                               tawa_data_name, 
                               output_dir, 
                               method = RUF_METHOD) {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  
  data_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  data <- read_parquet(data_path)
  
  params_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  params_df <- read_csv(params_path, show_col_types = FALSE)
  params <- setNames(params_df$Value, params_df$parameter)
  
  # 1. Calculate r2_mcfadden
  options_n <- length(unique(data$option_hours))
  
  param_keys <- c("beta_income_hhld", "beta_income_hhld2", "beta_leisure", 
                  "beta_leisure2", "beta_interaction")
  
  # Ensure params are in the correct list format for the likelihood function
  params_list <- as.numeric(params[param_keys])
  
  # Calculate Log Likelihoods
  # negative_log_likelihood must be defined to return the scalar value
  ll_model <- -model_ruf_env$negative_log_likelihood(params_list, data, options_n)
  
  n_people <- nrow(data) / options_n
  ll_null <- n_people * log(1 / options_n)
  r2_mcfadden <- 1 - (ll_model / ll_null)
  
  # 2. Calculate utility accuracy
  df_clean <- data %>% select(-calibrated_err)
  predicted_choices <- model_ruf_env$predict(df_clean, params, method = method)
  
  pred_n <- sum(predicted_choices$is_chosen == 1)
  truth_n <- sum(data$is_chosen == 1)
  
  # 3. Calculate employment hours accuracy
  truth_hrs <- sum(data$option_hours[data$is_chosen == 1])
  
  if (! is.null(method)) {
    person_hrs <- data %>%
      group_by(people_id) %>%
      summarise(mean_hrs = mean(option_hours), .groups = "drop")
    pred_hrs <- sum(person_hrs$mean_hrs)
  } else {
    pred_hrs <- sum(predicted_choices$hours[predicted_choices$is_chosen == 1])
  }
  
  # 4. Show calibration distribution
  # Equivalent to numpy.histogram(bins=50)
  hist_obj <- hist(data$calibrated_err, breaks = 50, plot = FALSE)
  
  err_dist <- data.frame(
    bin_start = hist_obj$breaks[-length(hist_obj$breaks)],
    bin_end = hist_obj$breaks[-1],
    count = hist_obj$counts
  )
  
  dist_output_path <- file.path(output_dir, paste0("validation_err_", filename_hash, ".csv"))
  print(paste("Validation (distribution) is written to", dist_output_path))
  write_csv(err_dist, dist_output_path)
  
  # Results formatting
  scores <- data.frame(
    scores = c("highest_utility_accuracy", "total_hrs_accuracy", "r2_mcfadden"),
    value = c(
      100.0 * pred_n / truth_n,
      100.0 * pred_hrs / truth_hrs,
      r2_mcfadden
    )
  )
  
  score_output_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  print(paste("Validation (score) is written to", score_output_path))
  write_csv(scores, score_output_path)
}
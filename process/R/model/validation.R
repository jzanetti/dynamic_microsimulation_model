run_ruf_sensitivity <- function(
    input_params, 
    income_scaler = seq(0.5, 2.0, by = 0.1), 
    output_dir = "",
    tawa_data_name = "sq",
    plot_using_ratio = TRUE,
    method = RUF_METHOD
) {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  
  data_output_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  model_output_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  accuracy_output_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  results_path <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  
  print(paste("Model estimated paramaters are read from", model_output_path))
  model_params_df <- fread(model_output_path)
  # Convert to named list/vector for easy access
  model_params <- as.list(setNames(model_params_df$Value, model_params_df$parameter))
  
  print(paste("Model training data are read from", data_output_path))
  data_to_check <- read_parquet(data_output_path)
  setDT(data_to_check)
  
  print(paste("Model accuracy is read from", accuracy_output_path))
  model_accuracy_df <- fread(accuracy_output_path)
  # Create named list/vector
  model_accuracy <- as.list(setNames(model_accuracy_df$value, model_accuracy_df$scores))
  
  # Initialize results storage
  results_hrs <- numeric(length(income_scaler))
  
  for (i in seq_along(income_scaler)) {
    scaler <- income_scaler[i]
    print(paste("Processing sensitivity study for scaler:", scaler))
    
    # Call external ruf_predict
    predicted_choices <- model_ruf_env$predict(data_to_check, model_params, method = method, scaler = scaler)
    setDT(predicted_choices)
    
    # Logic: sum(predicted_choices.groupby("people_id")["hours"].mean())
    # In R data.table: Calculate mean hours by people_id, then sum those means
    mean_hours_by_person <- predicted_choices[, .(mean_hrs = mean(hours)), by = people_id]
    results_hrs[i] <- sum(mean_hours_by_person$mean_hrs)
  }
  
  results <- data.table(
    total_employment_hrs = results_hrs,
    scaler = income_scaler
  )
  
  if (plot_using_ratio) {
    # Find index where scaler is close to 1.0 (tolerance 1e-5)
    index_candidates <- which(abs(results$scaler - 1.0) < (1.0 / 1e5))
    
    if (length(index_candidates) > 0) {
      index <- index_candidates[1]
      baseline_val <- results$total_employment_hrs[index]
      
      # Python: results[proc_key] = results[proc_key] / results[proc_key][index]
      results[, total_employment_hrs := total_employment_hrs / baseline_val]
    }
  }
  
  print(paste("Writing sensitivity study results to", results_path))
  write.csv(results, results_path)
}


run_ruf_validation <- function(input_params, tawa_data_name, output_dir, method = RUF_METHOD) {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  
  data_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  params_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  
  data <- read_parquet(data_path)
  setDT(data)
  
  params_df <- fread(params_path)
  params <- as.list(setNames(params_df$Value, params_df$parameter))
  
  # ---------------------------------------------------------
  # 1. Calculate r2_mcfadden
  # ---------------------------------------------------------
  options_n <- uniqueN(data$option_hours)
  
  # Order must match the negative_log_likelihood expectation
  params_list <- c(
    params[["beta_income_hhld"]],
    params[["beta_income_hhld2"]],
    params[["beta_leisure"]],
    params[["beta_leisure2"]],
    params[["beta_interaction"]]
  )
  
  # Calculate Log Likelihood of the Model
  # Note: negative_log_likelihood returns positive value (minimized neg LL), 
  # so we negate it to get actual LL
  ll_model <- -model_ruf_env$negative_log_likelihood(params_list, data, options_n)
  
  n_people <- nrow(data) %/% options_n
  
  # Calculate Null Log Likelihood (Equal probability assumption)
  ll_null <- n_people * log(1 / options_n)
  
  r2_mcfadden <- 1 - (ll_model / ll_null)
  
  # ---------------------------------------------------------
  # 2. Calculate utility accuracy
  # ---------------------------------------------------------
  # Drop "calibrated_err" column if exists
  if ("calibrated_err" %in% names(data)) {
    df_clean <- data[, !c("calibrated_err"), with = FALSE]
  } else {
    df_clean <- copy(data)
  }

  predicted_choices <- model_ruf_env$predict(df_clean, params, method = method)
  setDT(predicted_choices)
  
  # Number of predictions (assuming 'is_chosen' logic from predict or implicit structural logic)
  # In Python code, predict() returns a DataFrame of predictions.
  # If predict() logic filters to only chosen rows, then nrow(predicted_choices) might be enough.
  # However, strictly following Python: pred_n = len(predicted_choices[predicted_choices["is_chosen"] == 1])
  
  # We check if 'is_chosen' exists in prediction output, otherwise assume all rows are "predictions"
  if ("is_chosen" %in% names(predicted_choices)) {
    pred_n <- nrow(predicted_choices[is_chosen == 1])
  } else {
    # If ruf_predict returns only the predicted selections
    pred_n <- nrow(predicted_choices) 
  }
  
  truth_n <- nrow(data[is_chosen == 1])
  
  # ---------------------------------------------------------
  # 3. Calculate employment hours accuracy
  # ---------------------------------------------------------
  truth_hrs <- sum(data[is_chosen == 1]$option_hours)
  
  if (! is.null(method)) {
    # Python: sum(data.groupby("people_id")["option_hours"].mean())
    # Note: Using 'data' (original with all options) here, as per Python code
    mean_hrs <- data[, .(mean_opt = mean(option_hours)), by = people_id]
    pred_hrs <- sum(mean_hrs$mean_opt)
  } else {
    if ("is_chosen" %in% names(predicted_choices)) {
      pred_hrs <- sum(predicted_choices[is_chosen == 1]$hours)
    } else {
      pred_hrs <- sum(predicted_choices$hours)
    }
  }
  
  # ---------------------------------------------------------
  # 4. Show calibration distribution
  # ---------------------------------------------------------
  # Python: counts, bin_edges = histogram(data["calibrated_err"], bins=50)
  
  # hist() in R returns counts and breaks
  h <- hist(data$calibrated_err, breaks = 50, plot = FALSE)
  
  err_dist <- data.table(
    bin_start = head(h$breaks, -1),
    bin_end = tail(h$breaks, -1),
    count = h$counts
  )
  
  output_path_dist <- file.path(output_dir, paste0("validation_err_", filename_hash, ".csv"))
  print(paste("Validation (distribution) is written to", output_path_dist))
  fwrite(err_dist, output_path_dist)
  
  # Scores DataFrame
  scores <- data.table(
    scores = c("highest_utility_accuracy", "total_hrs_accuracy", "r2_mcfadden"),
    value = c(100.0 * pred_n / truth_n, 100.0 * pred_hrs / truth_hrs, r2_mcfadden)
  )
  output_path_score <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  print(paste("Validation (score) is written to", output_path_score))
  write.csv(scores, output_path_score)
}
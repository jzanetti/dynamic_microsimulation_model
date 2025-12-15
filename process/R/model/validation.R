

run_ruf_sensitivity <- function(input_params, 
                                income_scaler = seq(0.5, 2.0, 0.01), 
                                output_dir = "", 
                                plot_using_ratio = TRUE) {
  
  # Generate hash (mimicking your create_hash_filename logic)
  # You might need to adjust this depending on how your Python hash was generated
  # heavily dependent on the exact structure of input_params
  filename_hash <- data_filename_env$create_hash_filename(input_params)
  
  data_output_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  model_output_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  accuracy_output_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  results_path <- file.path(output_dir, paste0("sensitivity_tests_", filename_hash, ".csv"))
  
  print(sprintf("Model estimated parameters are read from %s", model_output_path))
  model_params_df <- read_csv(model_output_path, show_col_types = FALSE)
  # Convert to named vector for fast lookup
  model_params <- setNames(model_params_df$Value, model_params_df$parameter)
  
  print(sprintf("Model training data are read from %s", data_output_path))
  # Load as data.table immediately for speed
  data_to_check <- as.data.table(read_parquet(data_output_path))
  
  print(sprintf("Model accuracy is read from %s", accuracy_output_path))
  model_accuracy_df <- read_csv(accuracy_output_path, show_col_types = FALSE)
  
  # Initialize results vectors
  res_full_time <- numeric(length(income_scaler))
  res_part_time <- numeric(length(income_scaler))
  res_total_hrs <- numeric(length(income_scaler))
  
  # Pre-calculate invariant terms to speed up loop
  # (Though in your specific formula, interaction terms depend on scaler, so we calc inside)
  
  for (i in seq_along(income_scaler)) {
    scaler <- income_scaler[i]
    
    # Scale variables
    # We use temporary vectors to avoid modifying the main data.table in place repeatedly
    # or we can modify by reference if memory is tight, but here we just compute vectors.
    scaled_income_hhld <- data_to_check$income_hhld * scaler
    scaled_income <- data_to_check$income * scaler
    scaled_leisure <- data_to_check$leisure
    
    # Calculate Utility
    # Using 'with' is cleaner than repeated data_to_check$...
    utility <- (
      model_params["beta_income_hhld"] * scaled_income_hhld +
        model_params["beta_income_hhld2"] * (scaled_income_hhld^2) +
        model_params["beta_leisure"] * scaled_leisure +
        model_params["beta_leisure2"] * (scaled_leisure^2) +
        model_params["beta_interaction"] * (scaled_income * scaled_leisure)
    )
    
    # Assign utility back to DT temporarily
    data_to_check[, current_utility := utility]
    
    # Find choice with max utility per person
    # data.table syntax: .SD[which.max(current_utility)] by people_id
    predicted_choices <- data_to_check[, .SD[which.max(current_utility)], by = people_id]
    
    n_preds <- nrow(predicted_choices)
    
    # Calculate rates
    # Hours options from params (assuming standard R vector indexing)
    ft_threshold <- tail(input_params$hours_options, 1) # Last element
    pt_min <- input_params$hours_options[2]             # Second element (index 2 in R)
    
    ft_count <- sum(predicted_choices$option_hours >= ft_threshold)
    pt_count <- sum(predicted_choices$option_hours >= pt_min & 
                      predicted_choices$option_hours < ft_threshold)
    
    res_full_time[i] <- round(ft_count / n_preds * 100, 2)
    res_part_time[i] <- round(pt_count / n_preds * 100, 2)
    
    # Total employment hours (sum of option_hours for the chosen rows)
    # Note: In your python script you multiplied by 'is_chosen', but 'predicted_choices'
    # IS the chosen row in this simulation context.
    res_total_hrs[i] <- sum(predicted_choices$option_hours)
  }
  
  # Construct results data frame
  results <- data.frame(
    full_time = res_full_time,
    part_time = res_part_time,
    total_employment_hrs = res_total_hrs,
    scaler = income_scaler
  )
  
  # Normalize if requested
  if (plot_using_ratio) {
    # find index where scaler is approx 1.0
    idx <- which(abs(results$scaler - 1.0) < 1e-5)[1]
    
    if (!is.na(idx)) {
      results$full_time <- results$full_time / results$full_time[idx]
      results$part_time <- results$part_time / results$part_time[idx]
      results$total_employment_hrs <- results$total_employment_hrs / results$total_employment_hrs[idx]
    }
  }
  
  print(paste0("Writing sensitivity study results to ", results_path))
  write_csv(results, results_path)
}


run_ruf_validation <- function(input_params, output_dir) {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params)
  
  data_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  params_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  score_path <- file.path(output_dir, paste0("validation_score_", filename_hash, ".csv"))
  
  data <- read_parquet(data_path)
  # data is already a data.frame (tibble) from arrow
  
  params_df <- read_csv(params_path, show_col_types = FALSE)
  params <- setNames(params_df$Value, params_df$parameter)
  
  # Assuming ruf_predict is an R function you have defined elsewhere
  # source("process/R/model/random_utility_function.R")
  predicted_choices <- model_ruf_env$predict(data, params)
  
  pred_n <- sum(predicted_choices$is_chosen == 1, na.rm = TRUE)
  truth_n <- sum(data$is_chosen == 1, na.rm = TRUE)
  
  pred_hrs <- sum(predicted_choices$option_hours[predicted_choices$is_chosen == 1], na.rm = TRUE)
  truth_hrs <- sum(data$option_hours[data$is_chosen == 1], na.rm = TRUE)
  
  scores <- data.frame(
    scores = c("highest_utility_accuracy", "total_hrs_accuracy"),
    value = c(100.0 * pred_n / truth_n, 100.0 * pred_hrs / truth_hrs)
  )
  
  write_csv(scores, score_path)
}
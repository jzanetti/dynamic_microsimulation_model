

run_rate_model <- function(
    pop_data, target_data_name, cfg, output_dir) {
  
  # 1. Extract Data
  pop <- pop_data[["pop"]]
  target_data <- pop_data[[target_data_name]]
  
  # 2. Create Data Mapping & Validation
  data_mapping <- list()
  predictors_raw <- cfg[["predictors"]]
  predictors_new <- c() # To store the "_group" names
  
  for (proc_key in predictors_raw) {
    proc_group_key <- paste0(proc_key, "_group")
    predictors_new <- c(predictors_new, proc_group_key)
    
    if (!proc_group_key %in% names(target_data)) {
      stop(sprintf("%s is required for mortality model", proc_group_key))
    } else {
      # Store unique values (Python: list(unique()))
      data_mapping[[proc_key]] <- unique(target_data[[proc_group_key]])
    }
  }

  # 3. Aggregate Population: We select the columns needed + "id"
  cols_to_select <- c(predictors_raw, "id") # Assuming predictors_raw exists in pop
  pop_subset <- pop %>% select(all_of(cols_to_select))
  pop_to_use <- data_utils_env$aggregate_population(
    pop_subset, 
    "id", 
    data_mapping
  )
  
  target_data <- target_data %>%
    mutate(across(where(~ !is.numeric(.)), as.character))
  pop_to_use <- pop_to_use %>%
    mutate(across(where(~ !is.numeric(.)), as.character))
  
  # 4. Merge Data
  data_to_use <- left_join(
    target_data, 
    pop_to_use, 
    by = predictors_new
  )
  
  # 5. Obtain Probabilities (Fit Model)
  model_fit <- model_linear_env$fit_aggregated_rate_model(
    df = data_to_use,
    target_col = cfg[["target"]],
    predictor_cols = predictors_new,
    population_col = "count"
  )
  
  # 6. Structure the Output Object
  model_output <- list(
    model = model_fit,
    predictors = predictors_new,
    trained_data = data_to_use
  )
  
  # 7. Save Model
  model_path <- file.path(output_dir, paste0("model_", target_data_name, ".rds"))
  print(sprintf("Saving %s model to %s", target_data_name, model_path))
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  saveRDS(model_output, model_path)
}
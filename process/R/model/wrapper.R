
run_ruf_model <- function(df_input, cfg, recreate_data) {
  
  # Construct input_params list from the nested cfg object
  # Python's None becomes NULL in R
  # Python's True becomes TRUE in R
  input_params <- list(
    total_hours = cfg$behaviour_model$employment$ruf$total_hrs,
    min_hourly_wage = cfg$behaviour_model$employment$ruf$min_hourly_wage,
    leisure_value = cfg$behaviour_model$employment$ruf$leisure_value,
    exclude_seniors = TRUE,
    hours_options = cfg$behaviour_model$employment$ruf$possible_working_hrs,
    apply_household_income_filter = list(min = 0.1, max = 0.7),
    apply_earner_type_filter = NULL,
    apply_household_size_filter = NULL
  )
  
  output_dir <- cfg$output_dirs$models

  # model_validation_env$run_ruf_validation(input_params, output_dir = output_dir)
  # Call the utility function
  # Note: 'income_name' was a dict in Python, so it becomes a named list in R
  model_ruf_env$utility_func(
    df_input = df_input,
    params = input_params,
    income_name = list(market = "market_income_per_hour"),
    working_hours_name = "working_hours",
    output_dir = output_dir,
    recreate_data = recreate_data
  )
  
  # Run validation (calls the function we defined in the previous step)
  model_validation_env$run_ruf_validation(input_params, output_dir = output_dir)
}

run_heckman_wage_model <- function(pop_data, cfg) {
  
  # R is "copy-on-modify", so a simple assignment acts as a deep copy
  # for standard data frames.
  pop_data_input <- pop_data
  
  heckman_cfg <- cfg$behaviour_model$employment$heckman
  
  # Identify categorical columns needed for this specific model
  # Intersect finds common elements between two vectors
  required_vars <- unique(c(heckman_cfg$selection, heckman_cfg$outcome))
  cal_cols <- intersect(cfg$cat_cols, required_vars)
  
  print(
    paste0("Identified categorical features are: ",
    paste(cal_cols, collapse = ", "))
  )
  
  # --- Step 1: Preprocessing ---
  # Unlike Python, we do NOT manually expand dummies here.
  # We just convert them to factors. R's formula system handles the expansion.
  for (col in cal_cols) {
    pop_data_input[[col]] <- as.factor(pop_data_input[[col]])
    
    # NOTE: If you defined specific reference groups in Python, 
    # use relevel() here. E.g.:
    # pop_data_input[[col]] <- relevel(pop_data_input[[col]], ref = "SomeGroup")
  }
  
  # --- Step 2: Define Predictors ---
  # In R, we pass the raw column names (e.g., "education"), NOT the 
  # expanded dummy names (e.g., "education_High", "education_Low").
  selection_predictors <- heckman_cfg$selection
  outcome_predictors <- heckman_cfg$outcome
  
  # Construct output path using file.path (handles slashes automatically)
  model_outputpath <- file.path(cfg$output_dirs$models, "model_heckman_wage.rds")
  
  # --- Step 3: Run Model ---
  # Uses the helper function defined in the previous step
  results <- model_heckman_wage_env$heckman_wage_model(
    df = pop_data_input,
    selection_col = "employed",
    outcome_col = "market_income_per_week",
    select_exog = selection_predictors,
    outcome_exog = outcome_predictors
  )
  
  # --- Step 4: Save Results ---
  # saveRDS is the R equivalent of pickle for single objects
  saveRDS(results, file = model_outputpath)
  
  return(results)
}

run_model <- function(
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
  model_fit <- model_linear_env$linear_model(
    df = data_to_use,
    target_col = cfg[["target"]],
    predictor_cols = predictors_new,
    population_col = "count",
    use_rate = TRUE
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
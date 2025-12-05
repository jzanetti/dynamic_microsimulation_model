

run_heckman_wage_model <- function(pop_data,
                                   cfg,
                                   heckman_age_groups = c("0-18", "18-65", "65-999")) {
  
  employment_cfg <- cfg$behaviour_model$employment
  
  # R: intersect() performs set intersection
  predictors_union <- c(employment_cfg$predictors$selection,
                        employment_cfg$predictors$outcome)
  
  cal_cols <- intersect(cfg$cat_cols, unique(predictors_union))
  print(paste0("Identified categoriec features as", 
               paste(cal_cols, collapse = ", "), 
               ", and start one-shot preprocessing ..."))
  
  
  # Call preprocess_data (returns a list with $df_encoded and $new_cols_map based on previous conversion)
  pre_res <- model_utils_env$preprocess_data(pop_data, cal_cols, reference_groups = NULL)
  pop_data_input <- pre_res$df_encoded
  cal_cols_map <- pre_res$new_cols_map
  
  # Python: heckman_model_predictors = {"selection": [], "outcome": []}
  heckman_model_predictors <- list(selection = character(), outcome = character())
  
  for (proc_pred_type in names(heckman_model_predictors)) {
    # Get original list from config
    raw_preds <- employment_cfg$predictors[[proc_pred_type]]
    
    for (proc_var in raw_preds) {
      if (proc_var %in% cal_cols) {
        # Python: extend(cal_cols_map[proc_var])
        heckman_model_predictors[[proc_pred_type]] <- c(
          heckman_model_predictors[[proc_pred_type]], 
          cal_cols_map[[proc_var]]
        )
      } else {
        # Python: append(proc_var)
        heckman_model_predictors[[proc_pred_type]] <- c(
          heckman_model_predictors[[proc_pred_type]], 
          proc_var
        )
      }
    }
  }
  
  print(
    sprintf("Due to different wage behaviours, the heckman model run over different age groups %s", 
            paste(heckman_age_groups, collapse = ", "))
  )
  
  all_data <- list()
  
  for (proc_ages_str in heckman_age_groups) {
    print(paste("Heckman model for", proc_ages_str))
    
    # Construct path
    # Changing extension to .rds for R standard
    model_outputpath <- file.path(
      cfg$output_dirs$models,
      paste0("model_heckman_wage_", proc_ages_str, ".rds")
    )
    
    # Split "18-65" -> c("18", "65")
    proc_ages_vec <- unlist(str_split(proc_ages_str, "-"))
    min_age <- as.integer(proc_ages_vec[1])
    max_age <- as.integer(proc_ages_vec[2])
    
    # Filter data
    proc_data_input_subset <- pop_data_input %>%
      filter(age >= min_age & age < max_age)
    
    # Run Model
    # Note: heckman_wage_model returns the DF with predictions added
    model_results <- model_heckman_wage_env$heckman_wage_model(
      df = proc_data_input_subset,
      selection_col = "employed",
      outcome_col = "market_income",
      select_exog = heckman_model_predictors$selection,
      outcome_exog = heckman_model_predictors$outcome
    )
    
    saveRDS(
      list(selection = model_results$selection, 
           outcome = model_results$outcome, 
           data = model_results$data),
      file = model_outputpath
    )
  }
  
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
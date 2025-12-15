forward <- function(pop_data, cfg) {
  pop_data <- run_heckman_model(pop_data, cfg)
  pop_data <- run_ruf(pop_data, cfg)
  return(pop_data)
}

run_ruf <- function(pop_data, cfg) {
  return(pop_data)
}

run_heckman_model <- function(pop_data, cfg) {
  
  updated_results <- run_heckman_wage_model_prediction(
    pop_data, cfg$output_dirs$models
  )
  
  income_keys <- c("market_income_per_week", "latent_market_income_per_week")
  
  # Remove columns if they exist (equivalent to pandas drop errors='ignore')
  pop_data <- pop_data %>%
    select(-any_of(income_keys))
  
  # Merge results (equivalent to pandas merge how='left')
  # select() inside the join ensures we only bring in the necessary columns
  pop_data <- pop_data %>%
    left_join(
      updated_results %>% select(id, all_of(income_keys)), 
      by = "id"
    )
  
  return(pop_data)
}

run_heckman_wage_model_prediction <- function(pop_data, data_dir) {
  
  # NOTE: Changed extension to .rds. R cannot read .pkl files directly.
  model_outputpath <- file.path(data_dir, "model_heckman_wage.rds")
  
  # Load the model object (Equivalent to pickle_load)
  models <- readRDS(model_outputpath)
  
  # NOTE: In your original Python code, you load 'all_data' FROM the model object 
  # and ignore the 'pop_data' passed into the function arguments.
  # I have preserved this logic below.
  all_data <- models$data
  
  # Prepare predictors
  cols_to_select <- c(models$outcome_exog, "imr")
  X_outcome_all <- all_data %>%
    select(all_of(cols_to_select))
  
  # Predict
  all_data$latent_market_income_per_week <- predict(models$outcome, newdata = X_outcome_all)
  
  return(all_data)
}
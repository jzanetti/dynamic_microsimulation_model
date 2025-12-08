library(dplyr)

# Define Epsilon globally or import it if defined elsewhere
EPSILON <- 1e-9

# --- 1. Forward Orchestration Function ---
forward <- function(pop_data, 
                    cfg, 
                    weeks_per_year = 50, 
                    utility_age_groups = c("18-65"), 
                    run_utility_func = FALSE) {
  
  # Calculate market income per week
  # Note: logic assumes 'working_hours' handles 0 properly or is non-zero
  pop_data$market_income_per_week <- pop_data$market_income / (pop_data$working_hours * weeks_per_year)
  
  # Handle possible working hours (Vectorized replacement)
  possible_working_hrs <- cfg$behaviour_model$employment$possible_working_hrs
  possible_working_hrs[possible_working_hrs == 0.0] <- EPSILON
  
  # ------------------------
  # Run Heckman model and Utility function
  # ------------------------
  for (proc_age_group in utility_age_groups) {
    
    # Split age string "18-65" -> c(18, 65)
    range_parts <- as.numeric(unlist(strsplit(proc_age_group, "-")))
    min_age <- range_parts[1]
    max_age <- range_parts[2]
    
    # Filter data for current age group
    proc_data <- pop_data %>% 
      filter(age >= min_age, age < max_age)
    
    # Run Heckman Model (Updates pop_data with predictions)
    pop_data <- run_heckman_model(proc_data, cfg, proc_age_group)
    
    if (run_utility_func) {
      run_utility(
        pop_data = pop_data, # Using updated pop_data
        utility_age_group = proc_age_group,
        output_dir = cfg$output_dirs$models,
        hours_options = possible_working_hrs,
        total_hours = cfg$behaviour_model$employment$total_hrs
      )
    }
  }
  
  return(pop_data)
}

# --- 2. Utility Wrapper ---
run_utility <- function(pop_data, 
                        utility_age_group, 
                        output_dir, 
                        hours_options, 
                        total_hours = 80.0) {
  
  # R expects the age string "18-65" here based on previous context,
  # but if the internal function needs a vector, we split it here.
  # Assuming run_utility_func_model_fit expects the string to handle naming itself:
  model_wrapper_env$run_utility_func_model_fit(
    df_input = pop_data,
    output_dir = output_dir,
    proc_ages = utility_age_group, 
    hours_options = hours_options,
    total_hours = total_hours
  )
}

# --- 3. Heckman Wrapper ---
run_heckman_model <- function(pop_data, cfg, proc_age_group) {
  
  # Call the external training function (assumed to be sourced/loaded)
  model_wrapper_env$run_heckman_wage_model(pop_data, cfg, heckman_age_group = proc_age_group)
  
  # Get predictions
  updated_results <- run_heckman_wage_model_prediction(
    data_dir = cfg$output_dirs$models, 
    proc_age_group = proc_age_group
  )
  
  # Clean up existing column before merge to avoid duplication
  if ("latent_market_income_per_week" %in% names(pop_data)) {
    pop_data$latent_market_income_per_week <- NULL
  }
  
  # Left Join results back to pop_data
  # We select only the ID and the target column to keep the merge clean
  to_merge <- updated_results %>% 
    select(id, latent_market_income_per_week)
  
  pop_data <- left_join(pop_data, to_merge, by = "id")
  
  return(pop_data)
}

# --- 4. Heckman Prediction ---
run_heckman_wage_model_prediction <- function(data_dir, proc_age_group) {
  
  # Construct path (Using .rds instead of .pkl for R)
  filename <- paste0("model_heckman_wage_", proc_age_group, ".rds")
  model_outputpath <- file.path(data_dir, filename)
  
  # Load the model object
  # Assumes the object was saved using saveRDS() in R
  models <- readRDS(model_outputpath)
  
  all_data <- models$data
  
  # PREDICTION LOGIC:
  # Python uses sm.add_constant() explicitly. 
  # In R, if 'models$outcome' is a standard lm/glm object, predict() 
  # automatically handles the intercept/constant.
  
  # We assume 'models$outcome' is the model object and 'all_data' contains the features.
  # If the model was trained on specific columns, R's predict will look for them by name.
  
  preds <- predict(models$outcome, newdata = all_data)
  
  all_data$latent_market_income_per_week <- preds
  
  return(all_data)
}
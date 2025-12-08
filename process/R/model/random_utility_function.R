
# --- 1. Utility Function ---
quadratic_utility <- function(params, income, leisure) {
  # Unpack parameters (R vectors are 1-indexed)
  # params order: beta_income, beta_income2, beta_leisure, beta_leisure2, beta_interaction
  b_i <- params[1]
  b_i2 <- params[2]
  b_l <- params[3]
  b_l2 <- params[4]
  b_cl <- params[5]
  
  util <- (b_i * income) +
    (b_i2 * (income^2)) +
    (b_l * leisure) +
    (b_l2 * (leisure^2)) +
    (b_cl * (income * leisure))
  return(util)
}

# --- 2. Negative Log Likelihood ---
negative_log_likelihood <- function(params, df) {
  # 1. Calculate Utility for all rows
  V <- quadratic_utility(params, df$income, df$leisure)
  
  # 2. Reshape to (Num_People, 3_Options)
  # Assumes df is sorted by people_id and option order implies blocks of 3
  n_people <- nrow(df) / 3
  V_matrix <- matrix(V, nrow = n_people, ncol = 3, byrow = TRUE)
  
  # 3. Calculate Probabilities (Softmax)
  # Subtract max for numerical stability
  max_V <- apply(V_matrix, 1, max)
  exp_V <- exp(V_matrix - max_V)
  sum_exp_V <- rowSums(exp_V)
  probs <- exp_V / sum_exp_V
  
  # 4. Get probability of the ACTUAL choice
  # Reshape choice mask to match matrix
  choice_mask <- matrix(df$is_chosen, nrow = n_people, ncol = 3, byrow = TRUE)
  
  # Select probabilities where choice_mask == 1
  chosen_probs <- probs[choice_mask == 1]
  
  # 5. Sum Log Likelihoods
  return(-sum(log(chosen_probs + 1e-10)))
}

# --- 3. Results Validation ---
results_validation <- function(data_to_check, params_list) {
  
  results <- list(
    full_time = numeric(),
    part_time = numeric(),
    total_employment_hrs = numeric(),
    scaler = seq(0.3, 3.0, by = 0.1)
  )
  
  # Convert list params to named vector for easy access if needed, 
  # or access via keys if params_list is a named list
  p <- params_list
  
  for (scaler in results$scaler) {
    # Copy data to avoid mutating original in loop
    sim_data <- data_to_check
    
    scaled_income <- sim_data$income * scaler
    scaled_leisure <- sim_data$leisure
    
    # Calculate utility
    sim_data$utility <- (p$beta_income * scaled_income) +
      (p$beta_income2 * (scaled_income^2)) +
      (p$beta_leisure * scaled_leisure) +
      (p$beta_leisure2 * (scaled_leisure^2)) +
      (p$beta_interaction * (scaled_income * scaled_leisure))
    
    # Predict choices: Group by person, find row with max utility
    predicted_choices <- sim_data %>%
      group_by(people_id) %>%
      slice_max(utility, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # Calculate rates
    n_preds <- nrow(predicted_choices)
    
    # Note: Using python logic for leisure cutoffs (0.6, 0.9)
    ft_count <- sum(predicted_choices$leisure < 0.6)
    pt_count <- sum(predicted_choices$leisure > 0.6 & predicted_choices$leisure < 0.9)
    
    full_time_rate <- round((ft_count / n_preds) * 100, 2)
    part_time_rate <- round((pt_count / n_preds) * 100, 2)
    
    print(paste("Predicted employment rate (full-time)", scaler, ":", full_time_rate, "%"))
    print(paste("Predicted employment rate (part-time)", scaler, ":", part_time_rate, "%"))
    
    results$full_time <- c(results$full_time, full_time_rate)
    results$part_time <- c(results$part_time, part_time_rate)
    
    # Total hours (sum of option_hours for chosen rows)
    # Since predicted_choices only contains the chosen rows:
    results$total_employment_hrs <- c(results$total_employment_hrs, sum(predicted_choices$option_hours))
  }
  
  vis_env$plot_intermediate(results, "utility_func")
  # Plotting (Mocking the python 'plot_intermediate')
  # plot(results$scaler, results$full_time, type="l", col="blue", 
  #      ylim=c(0, 100), xlab="Scaler", ylab="Rate %", main="Validation Check")
  # lines(results$scaler, results$part_time, col="red")
  # legend("topright", legend=c("Full Time", "Part Time"), col=c("blue", "red"), lty=1)
  
  return(results)
}

# --- 4. Prepare Inputs ---
prepare_inputs <- function(df_input, hours_options, total_hours) {
  
  # Helper to find closest option
  map_to_closest <- function(actual, options) {
    options[which.min(abs(options - actual))]
  }
  
  # Initialize list to collect rows
  long_data_list <- list()
  
  # Iterate rows (Note: Apply/Map is more R-like, but loops are clearer for complex logic)
  for (i in 1:nrow(df_input)) {
    row <- df_input[i, ]
    person_wage <- row$latent_market_income_per_week
    actual_hours <- row$working_hours
    
    observed_choice <- map_to_closest(actual_hours, hours_options)
    
    for (option_hours in hours_options) {
      # Scenario 1: Leisure
      simulated_leisure <- total_hours - option_hours
      
      # Scenario 2: Income
      if (option_hours > 0) {
        gross_income <- person_wage * option_hours
        net_income <- gross_income 
        # (Add tax logic here if needed to match commented python code)
      } else {
        net_income <- 0
      }
      
      simulated_income <- max(net_income, 1.0)
      
      long_data_list[[length(long_data_list) + 1]] <- data.frame(
        people_id = row$id,
        option_hours = option_hours,
        is_chosen = ifelse(option_hours == observed_choice, 1, 0),
        income = simulated_income,
        leisure = simulated_leisure
      )
    }
  }
  
  # Combine into one dataframe
  return(bind_rows(long_data_list))
}

# --- 5. Main Utility Func Wrapper ---
utility_func <- function(df_input, 
                         hours_options, 
                         total_hours, 
                         income_name = "latent_market_income_per_week",
                         working_hours_name = "working_hours",
                         params_dict = NULL) {
  
  # Default parameters if not provided
  if (is.null(params_dict)) {
    params_dict <- list(
      beta_income = list(initial = 1.0, bound = c(1e-6, Inf)),
      beta_income2 = list(initial = -0.1, bound = c(-Inf, -1e-6)),
      beta_leisure = list(initial = 1.0, bound = c(1e-6, Inf)),
      beta_leisure2 = list(initial = -1.0, bound = c(-Inf, -1e-6)),
      beta_interaction = list(initial = 0.01, bound = c(-Inf, Inf))
    )
  }
  
  # Select relevant features
  df_input <- df_input %>%
    select(id, household_id, all_of(income_name), all_of(working_hours_name), employed)
  
  df_input$leisure_hours <- total_hours - df_input[[working_hours_name]]
  
  # Process data
  proc_data <- prepare_inputs(df_input, hours_options, total_hours)
  
  # Scale variables
  proc_data$income <- proc_data$income / 100.0
  proc_data$leisure <- proc_data$leisure / 80.0
  
  # Extract initial guesses and bounds
  param_names <- names(params_dict)
  initial_guess <- sapply(params_dict, function(x) x$initial)
  
  # R optim L-BFGS-B takes separate lower and upper vectors
  # Python's None maps to Inf in R
  lower_bounds <- sapply(params_dict, function(x) {
    val <- x$bound[1]
    if (is.null(val)) -Inf else val
  })
  
  upper_bounds <- sapply(params_dict, function(x) {
    val <- x$bound[2]
    if (is.null(val)) Inf else val
  })
  
  # Optimization
  result <- optim(
    par = initial_guess,
    fn = negative_log_likelihood,
    df = proc_data,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(
      factr = 10,       # Tighter convergence (default is 1e7). 10 is very strict.
      ndeps = rep(1e-8, length(initial_guess)), # Step size for gradient (matches Python's 1e-8)
      maxit = 1000      # Ensure it has enough iterations to converge
    )
  )
  
  # Format Results
  result_params <- list()
  for (i in seq_along(param_names)) {
    name <- param_names[i]
    val <- result$par[i]
    result_params[[name]] <- val
    print(paste(name, ":", round(val, 3)))
  }
  
  print("--- Model Check ---")
  print(paste("Convergence:", result$convergence)) # 0 means success in R
  print(paste("Message:", result$message))
  
  # Validation
  validation <- results_validation(proc_data, result_params)
  
  return(list(params = result_params, validation = validation))
}
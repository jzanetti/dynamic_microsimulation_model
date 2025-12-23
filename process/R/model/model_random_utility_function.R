


cal_utility <- function(data, params, income_scaler = 1.0) {
  # 1. Ensure data is a data.table for in-place modification
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # 2. Extract parameters into a local list or vector for readability
  params <- c(params[["beta_income_hhld"]],
              params[["beta_income_hhld2"]],
              params[["beta_leisure"]],
              params[["beta_leisure2"]],
              params[["beta_interaction"]])
  
  # 3. Calculate Utility
  data[, utility := quadratic_utility(
    params,
    income * income_scaler,
    income_hhld * income_scaler,
    leisure
  )]
  
  # 4. Handle Calibration
  if ("calibrated_err" %in% names(data)) {
    data[, utility_calibrated := utility + calibrated_err]
  }
  
  return(data)
}

run_ruf_calibrate <- function(input_params, output_dir) {
  
  # 1. Helper function for drawing Truncated Gumbel errors
  # This replicates the _obtain_err logic but is vectorized for speed
  obtain_err <- function(utility, is_chosen) {
    # Identify the observed choice
    k_idx <- which(is_chosen == 1)
    V_k <- utility[k_idx]
    
    # Draw error for chosen alternative (Standard Gumbel)
    # Gumbel(0,1) quantile function: -log(-log(u))
    u_k <- runif(1, 0, 1)
    eps_k <- -log(-log(u_k))
    
    # Initialize error vector
    epsilons <- numeric(length(utility))
    epsilons[k_idx] <- eps_k
    
    # Indices for unchosen alternatives
    j_indices <- which(is_chosen == 0)
    
    for (j in j_indices) {
      V_j <- utility[j]
      u_j <- runif(1, 0, 1)
      
      # Upper bound for the unchosen error to ensure V_k + eps_k > V_j + eps_j
      threshold <- eps_k + V_k - V_j
      
      # Truncated Gumbel draw
      # Replicating Python: -log(exp(-threshold) - log(u_j))
      epsilons[j] <- -log(exp(-threshold) - log(u_j))
    }
    
    return(epsilons)
  }
  
  print("Calibrating the RUF function ...")
  # 2. Setup Filenames (using md5 for consistency)
  filename_hash <- data_filename_env$create_hash_filename(input_params)
  data_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  param_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  
  # 3. Load Data
  data <- as.data.table(read_parquet(data_path))
  params_df <- fread(param_path)
  
  # Convert params DF to a named list
  params <- setNames(as.list(params_df$Value), params_df$parameter)

  # 4. Initial Utility Calculation
  data <- cal_utility(data, params)
  
  # 5. Apply Calibration (Group by person)
  data[, calibrated_err := obtain_err(utility, is_chosen), by = people_id]
  
  # 6. Final cleanup and export
  data[, utility := NULL]
  
  write_parquet(data, data_path)
  print(sprintf("Calibration complete. Results saved with hash: %s", filename_hash))
}

predict_ruf <- function(data, 
                        params, 
                        method = "top30", 
                        scaler = 1.0) {
  
  # 1. Ensure data is a data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # 2. Calculate utility
  data <- cal_utility(data, params, income_scaler = scaler)
  
  # 3. Determine which utility column to use
  utility_col <- "utility"
  if ("utility_calibrated" %in% names(data)) {
    utility_col <- "utility_calibrated"
  }
  
  # 4. Apply prediction logic
  if (method == "top30") {
    data[, threshold := quantile(get(utility_col), probs = 0.7), by = people_id]
    predictions <- data[get(utility_col) >= threshold]
    data[, threshold := NULL]
  } else {
    predictions <- data[, .SD[which.max(get(utility_col))], by = id]
  }
  
  return(predictions)
}


negative_log_likelihood <- function(params, df, options_n) {
  # 1. Calculate Utility for all rows
  V <- quadratic_utility(
    params, 
    df[["income"]], 
    df[["income_hhld"]], 
    df[["leisure"]]
  )

  # 2. Reshape to (Num_People, Options_N)
  # R matrices are filled by column by default, Python by row.
  # Assuming data is sorted by person, we fill by row to match Python behavior.
  n_people <- nrow(df) %/% options_n
  V_matrix <- matrix(V, nrow = n_people, ncol = options_n, byrow = TRUE)
  
  # 3. Calculate Probabilities (Softmax)
  # Subtract max for stability
  # apply(..., 1, max) gets max per row
  max_V <- apply(V_matrix, 1, max)
  
  # Broadcast subtraction (R does this column-wise by default, so we need care)
  # V_matrix - max_V works if max_V is a vector matching rows
  exp_V <- exp(V_matrix - max_V)
  
  # Sum exponentials per row
  sum_exp_V <- rowSums(exp_V)
  
  # Calculate probabilities
  probs <- exp_V / sum_exp_V
  
  # 4. Get probability of the ACTUAL choice
  # We use the 'is_chosen' mask
  choice_mask <- matrix(df[["is_chosen"]], nrow = n_people, ncol = options_n, byrow = TRUE)
  
  # Select probabilities where choice_mask == 1
  # In R, subsetting a matrix with a logical matrix returns a vector
  chosen_probs <- probs[choice_mask == 1]
  
  # 5. Sum Log Likelihoods
  return(-sum(log(chosen_probs + 1e-10)))
}

quadratic_utility <- function(
    params, 
    income, 
    income_hhld, 
    leisure,          
    show_debug = FALSE, 
    apply_log = RUN_LOG) {
  
  b_hhld_i  <- params[1]
  b_hhld_i2 <- params[2]
  b_l       <- params[3]
  b_l2      <- params[4]
  b_cl      <- params[5]
  
  # Apply Log transformation if requested
  if (apply_log) {
    income_hhld_to_use <- log(income_hhld)
    income_to_use      <- log(income)
    leisure_to_use     <- log(leisure)
  } else {
    income_hhld_to_use <- income_hhld
    income_to_use      <- income
    leisure_to_use     <- leisure
  }
  
  # Calculate Utility
  util <- (
    b_hhld_i * income_hhld_to_use +
      b_hhld_i2 * (income_hhld_to_use^2) +
      b_l * leisure_to_use +
      b_l2 * (leisure_to_use^2) +
      b_cl * (income_to_use * leisure_to_use)
  )
  
  if (show_debug) {
    print("Total util %f", sum(util, na.rm = TRUE))
  }
  
  return(util)
}


utility_func <- function(
    df_input,
    params,
    output_dir = "",
    income_name = "income_per_hour",
    working_hours_name = "working_hours",
    params_dict = list(
      "beta_income_hhld"  = list("initial" = 0.1,  "bound" = c(1e-6, 15.0)),
      "beta_income_hhld2" = list("initial" = -0.01, "bound" = c(-15.0, -1e-6)),
      "beta_leisure"      = list("initial" = 0.1,  "bound" = c(1e-6, 15.0)),
      "beta_leisure2"     = list("initial" = -0.01, "bound" = c(-15.0, -1e-6)),
      "beta_interaction"  = list("initial" = 0.1,  "bound" = c(-15.0, 15.0))
    ),
    recreate_data = TRUE
) {
  
  hours_options <- params[["hours_options"]]
  total_hours <- params[["total_hours"]]
  leisure_value <- params[["leisure_value"]]
  
  filename_hash <- data_filename_env$create_hash_filename(params)
  
  data_output_path <- sprintf("%s/utility_func_data_%s.parquet", output_dir, filename_hash)
  model_output_path <- sprintf("%s/utility_func_parameters_%s.csv", output_dir, filename_hash)
  
  if (recreate_data | !file.exists(data_output_path)) {
    data_input_env$prepare_ruf_inputs(
      df_input,
      hours_options,
      total_hours,
      leisure_value,
      income_name,
      working_hours_name,
      data_output_path=data_output_path
    )
  }
  proc_data <- read_parquet(data_output_path)

  # Extract initials and bounds
  initial_guess <- sapply(params_dict, function(x) x[["initial"]])
  
  # Optim L-BFGS-B takes lower and upper vectors
  lower_bounds <- sapply(params_dict, function(x) if(is.null(x[["bound"]][1])) -Inf else x[["bound"]][1])
  upper_bounds <- sapply(params_dict, function(x) if(is.null(x[["bound"]][2])) Inf else x[["bound"]][2])
  
  # Optimize
  result <- optim(
    par = initial_guess,
    fn = negative_log_likelihood,
    df = proc_data,
    options_n = length(hours_options),
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(
      trace = 1, 
      REPORT = 1
    ) 
  )
  
  result_params <- list()
  param_names <- names(params_dict)
  
  for (i in seq_along(param_names)) {
    result_params[[param_names[i]]] <- result$par[i]
  }
  
  # Write outputs
  results_params_df <- data.frame(
    parameter = names(result_params),
    Value = unlist(result_params),
    row.names = NULL
  )
  
  print(sprintf("The model estimated paramaters are written to %s", model_output_path))
  write_csv(results_params_df, model_output_path)
  
  print(sprintf("The model training data are written to %s", data_output_path))
  write_parquet(proc_data, data_output_path)
}
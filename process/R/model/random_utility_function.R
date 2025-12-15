

predict <- function(data, params) {
  scaled_income_hhld <- data[["income_hhld"]]
  scaled_income <- data[["income"]]
  scaled_leisure <- data[["leisure"]]
  
  # Note: Accessing params by name assumes params is a named list or vector
  data[["utlity"]] <- (
    params[["beta_income_hhld"]] * scaled_income_hhld
    + params[["beta_income_hhld2"]] * (scaled_income_hhld^2)
    + params[["beta_leisure"]] * scaled_leisure
    + params[["beta_leisure2"]] * (scaled_leisure^2)
    + params[["beta_interaction"]] * (scaled_income * scaled_leisure)
  )
  
  # Group by people_id and find the row with max utility
  predictions <- data %>%
    group_by(people_id) %>%
    slice(which.max(utlity)) %>%
    ungroup()
  
  return(predictions)
}

quadratic_utility <- function(params, income, income_hhld, leisure, income_to_income_hhld) {
  # Utility Function: U = b_c*C + b_c2*C^2 + b_l*L + b_l2*L^2 + b_cl*(C*L)
  
  # Unpack parameters (R vectors are indexed from 1)
  b_hhld_i  <- params[1]
  b_hhld_i2 <- params[2]
  b_l       <- params[3]
  b_l2      <- params[4]
  b_cl      <- params[5]

  util <- (
    #b_hhld_i * income
    #+ b_hhld_i2 * (income ^ 2)
    b_hhld_i * income_hhld
    + b_hhld_i2 * (income_hhld^2)
    + b_l * leisure
    + b_l2 * (leisure^2)
    + b_cl * (income * leisure)
  )
  
  return(util)
}

negative_log_likelihood <- function(params, df, options_n) {
  # 1. Calculate Utility for all rows
  V <- quadratic_utility(
    params, 
    df[["income"]], 
    df[["income_hhld"]], 
    df[["leisure"]], 
    df[["income_to_income_hhld"]]
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

utility_func <- function(
    df_input,
    params,
    output_dir = "",
    income_name = list("market_income" = "test"),
    working_hours_name = "working_hours",
    params_dict = list(
      "beta_income_hhld"  = list("initial" = 1.0,  "bound" = c(1e-6, Inf)),
      "beta_income_hhld2" = list("initial" = -0.01, "bound" = c(-Inf, -1e-6)),
      "beta_leisure"      = list("initial" = 1.0,  "bound" = c(1e-6, Inf)),
      "beta_leisure2"     = list("initial" = -0.01, "bound" = c(-Inf, -1e-6)),
      "beta_interaction"  = list("initial" = 1.0,  "bound" = c(-Inf, Inf))
    ),
    recreate_data = TRUE
) {
  
  hours_options <- params[["hours_options"]]
  total_hours <- params[["total_hours"]]
  leisure_value <- params[["leisure_value"]]
  
  filename_hash <- data_filename_env$create_hash_filename(params)
  
  data_output_path <- sprintf("%s/utility_func_data_%s.parquet", output_dir, filename_hash)
  model_output_path <- sprintf("%s/utility_func_parameters_%s.csv", output_dir, filename_hash)
  
  if (recreate_data) {
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
  proc_data <- arrow::read_parquet(data_output_path)

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
      # ndeps = rep(1e-8, length(initial_guess))
    ) 
  )
  
  result_params <- list()
  param_names <- names(params_dict)
  
  for (i in seq_along(param_names)) {
    result_params[[param_names[i]]] <- result$par[i]
  }
  
  # Write outputs
  # Creating a dataframe from the list
  results_params_df <- data.frame(
    parameter = names(result_params),
    Value = unlist(result_params),
    row.names = NULL
  )
  
  print(sprintf("The model estimated paramaters are written to %s", model_output_path))
  write_csv(results_params_df, model_output_path)
  
  print(sprintf("The model training data are written to %s", data_output_path))
  arrow::write_parquet(proc_data, data_output_path)
}
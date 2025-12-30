


quadratic_utility <- function(params, income, income_hhld, leisure, show_debug = FALSE, apply_log = RUN_LOG) {
  # params: Vector expected c(b_hhld_i, b_hhld_i2, b_l, b_l2, b_cl)
  b_hhld_i <- params[1]
  b_hhld_i2 <- params[2]
  b_l <- params[3]
  b_l2 <- params[4]
  b_cl <- params[5]
  
  if (apply_log) {
    income_hhld_to_use <- log(income_hhld)
    income_to_use <- log(income)
    leisure_to_use <- log(leisure)
  } else {
    income_hhld_to_use <- income_hhld
    income_to_use <- income
    leisure_to_use <- leisure
  }
  
  util <- (
    b_hhld_i * income_hhld_to_use +
      b_hhld_i2 * (income_hhld_to_use^2) +
      b_l * leisure_to_use +
      b_l2 * (leisure_to_use^2) +
      b_cl * (income_to_use * leisure_to_use)
  )
  
  if (show_debug) {
    debug_info <- paste("Total util", sum(util))
    print(debug_info)
  }
  
  return(util)
}


cal_utility <- function(data, params, income_scaler = 1.0) {
  setDT(data)
  
  # Ensure params are in the correct vector order for quadratic_utility
  # Python dict keys: beta_income_hhld, beta_income_hhld2, beta_leisure, beta_leisure2, beta_interaction
  param_vec <- c(
    params[["beta_income_hhld"]],
    params[["beta_income_hhld2"]],
    params[["beta_leisure"]],
    params[["beta_leisure2"]],
    params[["beta_interaction"]]
  )
  
  data[, utility := quadratic_utility(
    param_vec,
    income * income_scaler,
    income_hhld * income_scaler,
    leisure,
    show_debug = FALSE
  )]
  
  if ("calibrated_err" %in% names(data)) {
    data[, utility_calibrated := utility + calibrated_err]
  }
  
  return(data)
}

run_ruf_calibrate <- function(input_params, tawa_data_name, output_dir) {
  
  # Inner function to generate Gumbel errors (Chosen vs Unchosen)
  # Operates on a specific group (one person's options)
  obtain_err <- function(group_dt) {
    # Identify the observed choice (where is_chosen == 1)
    # In R, we find the index within the group vector
    k_idx <- which(group_dt$is_chosen == 1)[1]
    
    # Python: V_k = group.loc[k_idx, 'utility']
    V_k <- group_dt$utility[k_idx]
    
    # Draw error for the chosen alternative (Standard Gumbel)
    u_k <- runif(1, 0, 1)
    # Python: eps_k = -np_log(-np_log(u_k))
    eps_k <- -log(-log(u_k))
    
    # Initialize epsilons vector for this group
    epsilons <- numeric(nrow(group_dt))
    epsilons[k_idx] <- eps_k
    
    # Draw errors for unchosen alternatives (Truncated Gumbel)
    # Ensures V_k + eps_k > V_j + eps_j
    for (j_idx in seq_len(nrow(group_dt))) {
      if (j_idx == k_idx) {
        next
      }
      V_j <- group_dt$utility[j_idx]
      u_j <- runif(1, 0, 1)
      
      # Upper bound for the unchosen error
      threshold <- eps_k + V_k - V_j
      
      # Python: eps_j = -np_log(np_exp(-threshold) - np_log(u_j))
      term <- exp(-threshold) - log(u_j)
      eps_j <- -log(term)
      
      # for numerical stability, when the threshold is too big, the RUF model gives too much error
      if (! (V_k + eps_k > V_j + eps_j)) {
        eps_j = -99999.0
      }
      
      epsilons[j_idx] <- eps_j
    }
    
    return(epsilons)
  }
  
  print("Calibrating the RUF function ...")
  
  # Generate filenames
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  ruf_data_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  ruf_params_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  
  # Read Data and Params
  data <- read_parquet(ruf_data_path)
  setDT(data)
  
  params_df <- fread(ruf_params_path)
  # Convert params DataFrame to a named list/vector
  params <- as.list(setNames(params_df$Value, params_df$parameter))
  
  # Calculate Utility
  data <- cal_utility(data, params)
  
  # Calculate calibrated errors by group
  # Python: data.groupby("people_id").apply(_obtain_err)
  data[, calibrated_err := obtain_err(.SD), by = people_id]
  
  # Drop utility column
  data[, utility := NULL]
  
  # Write output
  write_parquet(data, ruf_data_path)
}

predict <- function(data, params, method = RUF_METHOD, scaler = 1.0, use_hhld = FALSE) {
  
  setDT(data)
  data <- cal_utility(data, params, income_scaler = scaler)
  
  utility_name <- "utility"
  if ("utility_calibrated" %in% names(data)) {
    utility_name <- "utility_calibrated"
  }
  
  if (! is.null(method)) {
    # Threshold is top 30% (quantile 0.7)
    data[, utility_threshold := quantile(get(utility_name), probs = 0.7), by = people_id]
    predictions <- data[get(utility_name) >= utility_threshold]
    # Calculate mean hours for valid options
    predictions[, hours := mean(option_hours), by = .(household_id, people_id)]
    # Clean up temporary column
    predictions[, utility_threshold := NULL]
    
  } else {
    if (use_hhld) {
      hhld_total_utility <- data[, .(total_util = sum(get(utility_name))), by = .(household_id, option_hours_id)]
      hhld_best_option <- hhld_total_utility[hhld_total_utility[, .I[which.max(total_util)], by = household_id]$V1]
      
      predictions <- merge(
        data,
        hhld_best_option[, .(household_id, option_hours_id)],
        by = c("household_id", "option_hours_id"),
        all = FALSE # Inner join
      )
      
    } else {
      predictions <- data[data[, .I[which.max(get(utility_name))], by = people_id]$V1]
    }
    
    setnames(predictions, "option_hours", "hours")
  }
  
  return(predictions)
}

negative_log_likelihood <- function(params, df, options_n) {

  income <- df$income
  inc_hh <- df$income_hhld 
  leisure <- df$leisure
  is_chosen <- df$is_chosen
  
  V <- quadratic_utility(
    params, 
    income, 
    inc_hh, 
    leisure
  )
  
  # 2. Reshape to (Num_People, Options_n)
  n_people <- nrow(df) %/% options_n
  V_matrix <- matrix(V, nrow = n_people, ncol = options_n, byrow = TRUE)
  
  # 3. Calculate Probabilities (Softmax)
  max_V <- apply(V_matrix, 1, max)
  exp_V <- exp(V_matrix - max_V)
  sum_exp_V <- rowSums(exp_V)
  
  # Element-wise division with recycling: Each column divided by sum_exp_V
  probs <- exp_V / sum_exp_V
  
  # 4. Get probability of the ACTUAL choice
  choice_mask <- matrix(is_chosen, nrow = n_people, ncol = options_n, byrow = TRUE)
  
  # Extract probabilities where choice_mask == 1
  chosen_probs <- probs[choice_mask == 1]
  
  return(-sum(log(chosen_probs + 1e-10)))
}

utility_func <- function(
    params,
    tawa_data_name = "sq",
    output_dir = "",
    hours_options = c(0, 20, 40),
    params_dict = list(
      "beta_income_hhld" = list("initial" = 0.1, "bound" = c(1e-6, Inf)),
      "beta_income_hhld2" = list("initial" = -0.01, "bound" = c(-Inf, -1e-6)),
      "beta_leisure" = list("initial" = 0.1, "bound" = c(1e-6, Inf)),
      "beta_leisure2" = list("initial" = -0.01, "bound" = c(-Inf, -1e-6)),
      "beta_interaction" = list("initial" = 0.1, "bound" = c(-Inf, Inf))
    ),
    optima_method = "nloptr", # optim, nloptr, lbfgsb3c
    maxit = 1000
) {
  
  # ----------------------------
  # Reading input data
  # ----------------------------
  filename_hash <- data_filename_env$create_hash_filename(params, filename_suffix = tawa_data_name)
  data_output_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  model_output_path <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash, ".csv"))
  proc_data <- read_parquet(data_output_path)
  
  # ----------------------------
  # Extract initial guesses and bounds from the list structure
  # ----------------------------
  initial_guess <- sapply(params_dict, function(x) x$initial)
  
  # ----------------------------
  # Obtain lower and upper bounds
  # ----------------------------
  lower_bounds <- sapply(params_dict, function(x) {
    val <- x$bound[1]
    if (is.null(val) || is.na(val) || val == -Inf) return(-Inf) # Explicit check
    return(val)
  })
  upper_bounds <- sapply(params_dict, function(x) {
    val <- x$bound[2]
    if (is.null(val) || is.na(val) || val == Inf) return(Inf)
    return(val)
  })
  
  # ----------------------------
  # Optimization
  # ----------------------------
  print(paste0("Start running optimization ..., using ", optima_method))
  
  proc_data <- proc_data %>%
    arrange(household_id, people_id, option_hours)
  
  if (optima_method == "optim") {
    result <- optim(
      par = initial_guess,
      fn = negative_log_likelihood,
      df = proc_data[, c("income", "income_hhld", "leisure", "is_chosen")],
      options_n = length(hours_options),
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(
        trace = 1,
        REPORT = 1,
        factr = 1e7,
        ndeps = rep(1e-8, length(initial_guess)),
        maxit = maxit
      )
    )
    
    result_params <- result$par
    
  } else if (optima_method %in% c("nloptr", "lbfgsb3c")) {
    obj_func_wrapper <- function(x) {
      negative_log_likelihood(
        x,
        df = proc_data[, c("income", "income_hhld", "leisure", "is_chosen")],
        options_n = length(hours_options)
      )
    }
    
    if (optima_method == "nloptr") {
      result <- nloptr(
        x0 = initial_guess,          # equivalent to 'par'
        eval_f = obj_func_wrapper,   # your objective function
        lb = lower_bounds,           # lower bounds
        ub = upper_bounds,           # upper bounds
        opts = list(
          "algorithm" = "NLOPT_LN_BOBYQA", # Derivative-free, supports bounds.
          "xtol_rel" = 1.0e-8,             # Convergence tolerance (similar to factr)
          "maxeval" = maxit,                # Max iterations
          "print_level" = 2                # Trace output (0=none, 3=verbose)
        )
      )
      result_params <- result$solution
    }
    
    if (optima_method == "lbfgsb3c") {
      result <- lbfgsb3c(
        par = initial_guess,
        fn = obj_func_wrapper,
        lower = lower_bounds,
        upper = upper_bounds,
        control = list(
          trace = 1,
          factr = 1e7,
          maxit = maxit
        )
      )
      result_params <- result$par
    }
    
  }
  
  # ----------------------------
  # Collect Results
  # ----------------------------
  param_names <- names(params_dict)
  results_params_df <- data.table(
    parameter = param_names,
    Value = result_params
  )
  
  print(paste("The model estimated paramaters are written to", model_output_path))
  fwrite(results_params_df, model_output_path)
}



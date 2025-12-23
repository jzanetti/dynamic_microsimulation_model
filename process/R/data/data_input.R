

add_initial_pop_status <- function(pop) {
  pop[, life_stage := "alive"]
  return(pop)
}

create_inputs <- function(data_dir, required_data_types = c("pop"), data_type = "parquet", base_year = NULL) {
  
  inputs <- list()
  for (proc_data_type in required_data_types) {
    proc_data_path <- paste0(
      data_dir, "/", paste0(proc_data_type, "_data.", data_type)
    )
    proc_data <- read_parquet(proc_data_path)
    
    proc_data <- as.data.table(proc_data)
    
    if (proc_data_type == "pop") {
      proc_data = add_initial_pop_status(proc_data)
    }
    
    if (! is.null(base_year)) {
      proc_data$base_year <- base_year
    }
    
    inputs[[proc_data_type]] <- as.data.frame(proc_data)
  }
  
  return (inputs)
}

prepare_ruf_inputs <- function(df_input, 
                               hours_options, 
                               total_hours, 
                               leisure_value, 
                               income_name, 
                               working_hours_name, 
                               data_scaler = 1000.0,
                               data_output_path = NULL) {
  

  # 1. Convert to data.table and select relevant columns
  dt <- as.data.table(df_input)[, .(
    id, 
    household_id, 
    selected, 
    income = get(income_name), 
    working_hours = get(working_hours_name)
  )]
  
  all_household_ids <- unique(dt$household_id)
  long_data_list <- list()
  
  # 2. Iterate through households
  for (i in seq_along(all_household_ids)) {
    proc_hhld_id <- all_household_ids[i]
    
    # Progress logging
    if (i %% 10 == 0) {
      message(sprintf("Processing input: %.3f%%", 100 * i / length(all_household_ids)))
    }
    
    proc_hhld <- dt[household_id == proc_hhld_id]
    num_people <- nrow(proc_hhld)
    
    # 3. Generate all possible hours combinations
    # Equivalent to Python's iter_product(hours_options, repeat=num_people)
    combos_list <- replicate(num_people, hours_options, simplify = FALSE)
    all_possible_combinations <- as.matrix(do.call(expand.grid, combos_list))
    
    # 4. Find the 'chosen' combination (minimum squared distance)
    # This replicates the min(key=lambda...) logic
    actual_hours <- proc_hhld$working_hours
    distances <- apply(all_possible_combinations, 1, function(row) {
      sum((row - actual_hours)^2)
    })
    chosen_idx <- which.min(distances)
    chosen_combination <- all_possible_combinations[chosen_idx, ]
    
    # 5. Process each combination
    for (j in 1:nrow(all_possible_combinations)) {
      proc_combination <- all_possible_combinations[j, ]
      is_chosen <- if (all(proc_combination == chosen_combination)) 1 else 0
      
      # Calculate incomes and leisure
      # Using 1e-9 for zero hours to match Python logic
      market_incomes <- ifelse(proc_combination > 0, 
                               proc_hhld$income * proc_combination, 
                               1e-9)
      
      hhld_income <- sum(market_incomes)
      if (hhld_income == 0) hhld_income <- 1e-9
      
      leisure_hours <- (total_hours - proc_combination) * leisure_value
      
      # Only keep data for 'selected' individuals
      selected_idx <- which(proc_hhld$selected)
      
      if (length(selected_idx) > 0) {
        row_data <- data.table(
          household_id = proc_hhld_id,
          people_id = proc_hhld$id[selected_idx],
          option_hours = proc_combination[selected_idx],
          is_chosen = is_chosen,
          income = market_incomes[selected_idx],
          leisure = leisure_hours[selected_idx],
          income_hhld = hhld_income
        )
        long_data_list[[length(long_data_list) + 1]] <- row_data
      }
    }
  }
  
  # 6. Combine and Post-Process
  results <- rbindlist(long_data_list)
  
  # Scaling
  cols_to_scale <- c("income", "income_hhld", "leisure")
  results[, (cols_to_scale) := lapply(.SD, function(x) x / data_scaler), .SDcols = cols_to_scale]
  
  # Sorting
  setorder(results, household_id, people_id, option_hours)
  
  # 7. Write to Parquet
  if (!is.null(data_output_path)) {
    write_parquet(results, data_output_path)
  }
  
  return(results)
}


prepare_ruf_inputs2 <- function(df_input, 
                               hours_options, 
                               total_hours, 
                               leisure_value, 
                               income_name, 
                               working_hours_name, 
                               data_scaler = 1000.0,
                               data_output_path = NULL) {

  dt <- as.data.table(df_input)
  dt <- dt[, c("id", "household_id", "selected", income_name$market, working_hours_name), with = FALSE]
  setnames(dt, old = c(income_name$market, working_hours_name), new = c("wage", "actual_hours"))
  
  opts <- data.table(option_hours = hours_options)
  
  # Join every person to every hour option (Cartesian Join within person)
  dt_expanded <- dt[rep(1:.N, each = nrow(opts))]
  dt_expanded[, option_hours := rep(opts$option_hours, length.out = .N)]
  
  # Find the nearest hours
  dt_expanded[, error_sq := (option_hours - actual_hours)^2]
  dt_expanded[, is_chosen := 0L]
  dt_expanded[dt_expanded[, .I[which.min(error_sq)], by = id]$V1, is_chosen := 1L]
  

  # Calculate individual income and leisure for every theoretical option
  dt_expanded[, market_income_person := fifelse(option_hours > 0, wage * option_hours, 0)]
  dt_expanded[, leisure := (total_hours - option_hours) * leisure_value]

  
  print("Preparing RUF data ...")
  results_list <- lapply(split(dt, by = "household_id"), function(hh_data) {

    # Get people IDs
    p_ids <- hh_data$id
    n_people <- length(p_ids)
    
    # Generate all combinations of hours (Grid Expansion)
    combos <- do.call(expand.grid, rep(list(hours_options), n_people))
    
    # Convert combos to a long format suitable for binding with person attributes
    combos$scenario_id <- 1:nrow(combos)
    
    # Melt to long: [scenario_id, person_idx, option_hours]
    combos_long <- as.data.table(combos)
    combos_long <- melt(combos_long, id.vars = "scenario_id", value.name = "option_hours")
    
    # Variable 'variable' is "Var1", "Var2" etc. Map to p_ids
    combos_long[, p_idx := as.integer(gsub("Var", "", variable))]
    combos_long[, id := p_ids[p_idx]]
    
    # Join back static person info (wage, actual_hours, selected)
    hh_merged <- merge(combos_long, hh_data[, .(id, wage, actual_hours, selected)], by = "id")
    
    # --- Logic 1: Determine Chosen Scenario ---
    # Sum squared error by scenario
    hh_merged[, error_sq := (option_hours - actual_hours)^2]
    scenario_errors <- hh_merged[, .(total_error = sum(error_sq)), by = scenario_id]
    
    # Find best scenario
    best_scenario <- scenario_errors[which.min(total_error), scenario_id]
    
    hh_merged[, is_chosen := fifelse(scenario_id == best_scenario, 1L, 0L)]
    
    # --- Logic 2: Calculate Finance ---
    hh_merged[, income := fifelse(option_hours > 0, wage * option_hours, 1e-9)]
    hh_merged[, leisure_val := (total_hours - option_hours) * leisure_value]
    
    # Household Income (sum of income per scenario)
    hh_merged[, income_hhld := sum(income), by = scenario_id]
    
    # Handle zero income safety
    hh_merged[income_hhld == 0, income_hhld := 1e-9]
    
    # Filter only "selected" people for final output
    res <- hh_merged[selected == TRUE, .(
      household_id = hh_data$household_id[1],
      people_id = id,
      option_hours,
      is_chosen,
      income,
      leisure = leisure_val,
      income_hhld
    )]
    
    return(res)
  })
  
  # Combine all results
  results <- rbindlist(results_list)
  
  cols_to_scale <- c("income", "income_hhld", "leisure")
  results[, (cols_to_scale) := lapply(.SD, function(x) x / data_scaler), .SDcols = cols_to_scale]
  
  results <- results %>% 
    arrange(household_id, people_id, option_hours)
  
  write_parquet(as.data.frame(results), data_output_path)
  
}
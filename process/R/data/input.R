

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
  
  # Convert to data.table
  dt <- as.data.table(df_input)
  
  # Select relevant features
  dt <- dt[, c("id", "household_id", "selected", income_name$market, working_hours_name), with = FALSE]
  
  # Rename columns for easier internal handling
  setnames(dt, old = c(income_name$market, working_hours_name), new = c("wage", "actual_hours"))
  
  # --- Step 1: Generate Combinations ---
  # Instead of a Python loop, we use a join approach.
  
  # 1. Create a table of all hour options
  opts <- data.table(option_hours = hours_options)
  
  # 2. Join every person to every hour option (Cartesian Join within person)
  # This creates: Person A -> [0, 10, 20...], Person B -> [0, 10, 20...]
  dt_expanded <- dt[rep(1:.N, each = nrow(opts))]
  dt_expanded[, option_hours := rep(opts$option_hours, length.out = .N)]
  
  # --- Step 2: Determine "Chosen" Combination ---
  # The Python logic minimizes the sum of squared errors between actual hours and option hours
  # for the *entire household*. 
  # This implies we must look at combinations of (Person A, Person B).
  
  # For computational efficiency in R (avoiding massive grid expansion of HHLD_SIZE^N),
  # we can solve this per person because the "cost function" (a - b)^2 is additive.
  # minimizing Sum((actual_i - option_i)^2) is mathematically identical to 
  # minimizing (actual_i - option_i)^2 for each person individually.
  # (Unless there are constraints linking them, which this specific script doesn't show).
  
  # Calculate squared error for each option row
  dt_expanded[, error_sq := (option_hours - actual_hours)^2]
  
  # Flag the row with the minimum error per person
  dt_expanded[, is_chosen := 0L]
  dt_expanded[dt_expanded[, .I[which.min(error_sq)], by = id]$V1, is_chosen := 1L]
  
  # --- Step 3: Calculate Incomes ---
  
  # Calculate individual income and leisure for every theoretical option
  dt_expanded[, market_income_person := fifelse(option_hours > 0, wage * option_hours, 0)]
  dt_expanded[, leisure := (total_hours - option_hours) * leisure_value]
  
  # To get "household income" for every combination, we need to sum up the incomes.
  # Note: The Python script generates ALL combinations (N^M).
  # If we strictly follow the Python logic, we must expand grid by household.
  # IF households are small (e.g., < 5 people), we can do this.
  
  # Aggregating strictly by household to match Python "all_possible_hours_combination" logic:
  
  # Split data by household
  results_list <- lapply(split(dt, by = "household_id"), function(hh_data) {
    
    # Get people IDs
    p_ids <- hh_data$id
    n_people <- length(p_ids)
    
    # Generate all combinations of hours (Grid Expansion)
    # This creates a matrix where each column is a person, each row is a scenario
    combos <- do.call(expand.grid, rep(list(hours_options), n_people))
    
    # Convert combos to a long format suitable for binding with person attributes
    # We assign a "scenario_id" to each combination
    combos$scenario_id <- 1:nrow(combos)
    
    # Melt to long: [scenario_id, person_idx, option_hours]
    # We need to map person_idx (1, 2, 3) back to actual people IDs
    combos_long <- as.data.table(combos)
    combos_long <- melt(combos_long, id.vars = "scenario_id", value.name = "option_hours")
    
    # Variable 'variable' is "Var1", "Var2" etc. Map to p_ids
    # Extract numeric index from "VarX"
    combos_long[, p_idx := as.integer(gsub("Var", "", variable))]
    combos_long[, id := p_ids[p_idx]]
    
    # Join back static person info (wage, actual_hours, selected)
    # We join on 'id'
    hh_merged <- merge(combos_long, hh_data[, .(id, wage, actual_hours, selected)], by = "id")
    
    # --- Logic 1: Determine Chosen Scenario ---
    # Sum squared error by scenario
    hh_merged[, error_sq := (option_hours - actual_hours)^2]
    scenario_errors <- hh_merged[, .(total_error = sum(error_sq)), by = scenario_id]
    
    # Find best scenario
    best_scenario <- scenario_errors[which.min(total_error), scenario_id]
    
    hh_merged[, is_chosen := fifelse(scenario_id == best_scenario, 1L, 0L)]
    
    # --- Logic 2: Calculate Finance ---
    hh_merged[, income := fifelse(option_hours > 0, wage * option_hours, 0)]
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
  
  # --- Step 4: Final Scaling ---
  results[, income_to_income_hhld := income / income_hhld]
  
  cols_to_scale <- c("income", "income_hhld", "leisure")
  results[, (cols_to_scale) := lapply(.SD, function(x) x / data_scaler), .SDcols = cols_to_scale]
  
  write_parquet(as.data.frame(results), data_output_path)
  
}
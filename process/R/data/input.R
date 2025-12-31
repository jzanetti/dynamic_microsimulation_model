

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
                               market_income_name_per_hour, 
                               benefit_income_name_per_week, 
                               working_hours_name, 
                               data_scaler = 1000.0, 
                               data_output_path = NULL) {
  
  # 1. Setup data.table and select relevant columns
  # Using copy to avoid side effects on the input dataframe
  dt <- as.data.table(df_input)[, .(
    id, 
    household_id, 
    selected, 
    market_hour = get(market_income_name_per_hour),
    benefit_week = get(benefit_income_name_per_week),
    working_hours = get(working_hours_name)
  )]
  
  # 2. Process by Household
  # We use a split-apply-combine approach which is highly optimized in data.table
  results <- dt[, {
    num_people <- .N
    # Generate all possible hours combinations for the household size
    # CJ (Cross Join) is the data.table equivalent of itertools.product
    hours_list <- replicate(num_people, hours_options, simplify = FALSE)
    combs <- do.call(CJ, hours_list)
    setnames(combs, paste0("V", 1:num_people))
    
    # Add an option ID
    combs[, option_hours_id := .I - 1]
    
    # 3. Find 'chosen_combination' (Nearest Neighbor via Euclidean Distance)
    # Vectorized distance calculation across rows of combinations
    obs_hours <- .SD$working_hours
    # Calculate sum of squared differences for each row
    dists <- rowSums(mapply(function(col, obs) (col - obs)^2, combs[, 1:num_people, with=FALSE], obs_hours))
    chosen_idx <- which.min(dists)
    combs[, is_chosen := as.integer(.I == chosen_idx)]
    
    # 4. Calculate Household Totals for each combination
    # Calculate market income for each person (matrix multiplication for speed)
    wages <- .SD$market_hour
    person_benefits <- .SD$benefit_week
    
    # Calculate hhld totals for each combination (rowSums)
    market_incomes_matrix <- mapply(`*`, combs[, 1:num_people, with=FALSE], wages)
    combs[, market_income_hhld := rowSums(as.matrix(market_incomes_matrix))]
    combs[, benefit_income_hhld := sum(person_benefits)] # Based on Python logic
    
    # 5. Transform to Long Format (People within Options)
    # We only keep rows where 'selected' is TRUE (matching proc_person["selected"])
    selected_indices <- which(.SD$selected)
    
    # Melt the combinations to person-level
    long_combs <- melt(combs, 
                       id.vars = c("option_hours_id", "is_chosen", "market_income_hhld", "benefit_income_hhld"),
                       measure.vars = paste0("V", 1:num_people),
                       variable.name = "person_idx", 
                       value.name = "option_hours")
    
    # Map person_idx back to actual data
    long_combs[, person_idx := as.numeric(gsub("V", "", person_idx))]
    
    # Filter for 'selected' people only
    long_combs <- long_combs[person_idx %in% selected_indices]
    
    # Join with original person attributes
    person_map <- .SD[, .(person_idx = .I, people_id = id)]
    long_combs <- merge(long_combs, person_map, by = "person_idx")
    
    # 6. Final calculations for the group
    long_combs[, `:=`(
      market_income = wages[person_idx] * option_hours,
      benefit_income = person_benefits[person_idx],
      leisure = (total_hours - option_hours) * leisure_value
    )]
    
    long_combs[, .(option_hours_id, people_id, 
                   option_hours, is_chosen, market_income, benefit_income, 
                   leisure, market_income_hhld, benefit_income_hhld)]
    
  }, by = household_id]
  
  # 7. Post-processing (Clipping and Scaling)
  income_cols <- c("market_income", "benefit_income", "market_income_hhld", "benefit_income_hhld")
  for (col in income_cols) {
    set(results, i = which(results[[col]] < 1e-9), j = col, value = 1e-9)
  }
  # Call external benefit function (ensure this is defined in your environment)
  results <- obtain_benefit_income(results)
  setDT(results)
  
  # Scaling
  # Note: Python code defines 'income' and 'income_hhld' which weren't in the list above.
  # Assuming they are created by obtain_benefit_income.
  scale_cols <- intersect(c("income", "income_hhld", "leisure"), names(results))
  for (col in scale_cols) {
    results[, (col) := get(col) / data_scaler]
  }
  
  # Apply the specific leisure multiplier from your Python code
  results[, leisure := leisure * 23.0]
  
  # 8. Sort and Save
  setorder(results, household_id, people_id, option_hours)
  
  if (!is.null(data_output_path)) {
    write_parquet(results, data_output_path)
  }
  
  return(results)
}


obtain_benefit_income <- function(df) {
  print("Predict benefit income using XGBoost model")
  
  df_for_train <- df %>% filter(is_chosen == 1)
  
  # Prepare data for XGBoost
  train_features <- as.matrix(df_for_train[, c("market_income", "market_income_hhld")])
  train_label <- df_for_train$benefit_income
  
  model <- model_xgboost_env$xgboost_model(df_for_train,
                                  "benefit_income",
                                  c("market_income", "market_income_hhld"))
  # Prediction
  full_features <- as.matrix(df[, c("market_income", "market_income_hhld")])
  df$benefit_income_pred <- predict(model, full_features)
  
  # Calculate after-tax income
  df <- df %>%
    mutate(
      income = market_income + benefit_income_pred,
      tax = model_tax_calculator_env$cal_tax(income), # Assuming cal_tax is defined globally
      income = income - tax
    ) %>%
    group_by(option_hours_id, household_id) %>%
    mutate(income_hhld = sum(income)) %>%
    ungroup() %>%
    select(
      option_hours_id,
      household_id,
      people_id,
      option_hours,
      is_chosen,
      leisure,
      market_income,
      benefit_income_pred,
      income,
      income_hhld
    )
  
  return(df)
}

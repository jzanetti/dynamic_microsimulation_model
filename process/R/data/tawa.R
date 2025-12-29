run_tawa_predict <- function(tawa_data, output_dir, input_params, updated_tawa_data_path) {
  
  filename_hash_sq <- data_filename_env$create_hash_filename(input_params, filename_suffix = "sq")
  
  # Read Parquet
  sq_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash_sq, ".parquet"))
  data_sq <- read_parquet(sq_path)
  setDT(data_sq)
  
  # Run Preprocess
  ref_ids <- unique(data_sq$people_id)
  
  #data_tawa_env$tawa_data_preprocess(
  #  tawa_data = tawa_data,
  #  input_params = input_params,
  #  tawa_data_name = "test2",
  #  output_dir = output_dir,
  #  ref_ids = ref_ids
  #)
  
  run_ruf_for_tawa(
    tawa_data = tawa_data,
    input_params = input_params,
    updated_tawa_data_path = updated_tawa_data_path,
    tawa_data_name = "test2",
    output_dir = output_dir
  )
}


add_utility_err_to_tawa <- function(
    input_params, 
    tawa_data_name, 
    output_dir,
    join_keys = c("household_id", "people_id", "option_hours", "option_hours_id")
) {
  # """
  # Merges calibrated error terms from the status quo (sq) dataset into 
  # the reform dataset based on household and person identifiers.
  # """
  
  # 1. Load Reform Data
  reform_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  reform_file <- file.path(output_dir, paste0("utility_func_data_", reform_hash, ".parquet"))
  df_reform <- read_parquet(reform_file)
  setDT(df_reform)
  
  # 2. Load SQ Data (Optimized)
  sq_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = "sq")
  sq_file <- file.path(output_dir, paste0("utility_func_data_", sq_hash, ".parquet"))
  
  # Reading specific columns only
  cols_to_read <- c(join_keys, 'calibrated_err')
  df_sq <- read_parquet(sq_file, col_select = all_of(cols_to_read))
  setDT(df_sq)
  
  # 3. Merge
  # Drop duplicates in SQ before merge
  df_sq_unique <- unique(df_sq, by = join_keys)
  
  # Left merge
  df_merged <- merge(
    df_reform,
    df_sq_unique,
    by = join_keys,
    all.x = TRUE
  )
  
  # 4. Write Output
  output_file <- file.path(output_dir, paste0("utility_func_data_", reform_hash, "_updated.parquet"))
  write_parquet(df_merged, output_file)
  
  return(output_file)
}


run_ruf_for_tawa <- function(
    tawa_data, 
    input_params, 
    updated_tawa_data_path = "test.csv",
    tawa_data_name = "reform1", 
    output_dir = ".",
    method = RUF_METHOD
) {
  
  print("Adding utility error from SQ to Reform ...")
  add_utility_err_to_tawa(input_params, tawa_data_name, output_dir)
  
  print("Obtain RUF paramaters ...")
  filename_hash_sq <- data_filename_env$create_hash_filename(input_params, filename_suffix = "sq")
  ruf_params_file <- file.path(output_dir, paste0("utility_func_parameters_", filename_hash_sq, ".csv"))
  
  ruf_params_df <- fread(ruf_params_file)
  # Create a named vector (dictionary equivalent)
  ruf_params <- setNames(ruf_params_df$Value, ruf_params_df$parameter)
  # Convert to list if your RUF predict function expects a list
  ruf_params <- as.list(ruf_params) 
  
  print("Get TAWA RUF data (Reform)")
  filename_hash_reform <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  reform_updated_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash_reform, "_updated.parquet"))
  
  data_reform <- read_parquet(reform_updated_path)
  setDT(data_reform)
  
  # Call external RUF predict function (Must be defined elsewhere)
  data_reform <- model_ruf_env$predict(data_reform, ruf_params, method = method, use_hhld = FALSE)
  
  print("Get TAWA RUF data (SQ)")
  sq_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash_sq, ".parquet"))
  data_sq <- read_parquet(sq_path)
  setDT(data_sq)
  
  # Filter for chosen options
  data_sq <- data_sq[is_chosen == 1]
  data_sq <- data_sq[, .(household_id, people_id, option_hours)]
  setnames(data_sq, "option_hours", "hours")
  
  print("Merge SQ and Reform data together ...")
  employment_scaler_data <- merge(
    data_reform,
    data_sq,
    by = c("household_id", "people_id"),
    suffixes = c("_reform", "_sq")
  )
  
  employment_scaler_data[, employment_scaler := hours_reform / hours_sq]
  
  employment_num1 <- nrow(employment_scaler_data[employment_scaler > 1])
  employment_num2 <- nrow(employment_scaler_data[employment_scaler < 1])
  
  print(paste(" - Number of people with inreased working hours", employment_num1))
  print(paste(" - Number of people with decreased working hours", employment_num2))
  
  print("Update TAWA data ...")
  employment_scaler_data <- employment_scaler_data[, .(household_id, people_id, employment_scaler)]
  
  setnames(employment_scaler_data, 
           old = c("household_id", "people_id"), 
           new = c("snz_hes_hhld_uid", "snz_hes_uid"))
  
  # Accessing tawa_data input (assuming tawa_data is a list of data.tables or data.frames)
  tawa_input <- as.data.table(tawa_data[["input"]])
  
  tawa_data_input <- merge(
    tawa_input,
    employment_scaler_data,
    by = c("snz_hes_hhld_uid", "snz_hes_uid"),
    all.x = TRUE
  )
  
  # Fill NA scalers with 1.0
  tawa_data_input[is.na(employment_scaler), employment_scaler := 1.0]
  
  # Update incomes
  for (key in c("P_Income_SelfEmployed", "P_Income_WageSalary")) {
    if (key %in% names(tawa_data_input)) {
      tawa_data_input[[key]] <- tawa_data_input[[key]] * tawa_data_input$employment_scaler
    }
  }
  
  tawa_data_input[, employment_scaler := NULL]
  
  write.csv(tawa_data_input, updated_tawa_data_path)
}


tawa_data_preprocess <- function(
    tawa_data,
    input_params,
    tawa_data_name = "sq",
    income_types = c("P_Income_Disposable"),
    income_name = "income_per_hour",
    working_hours_name = "working_hours",
    ref_ids = NULL,
    output_dir = "."
) {
  
  # ---------------------------------------------
  # Step 0: Obtain parameters
  # ---------------------------------------------
  total_hours <- input_params[["total_hours"]]
  leisure_value <- input_params[["leisure_value"]]
  min_hourly_wage <- input_params[["min_hourly_wage"]]
  hours_options <- input_params[["hours_options"]]
  exclude_seniors <- input_params[["exclude_seniors"]]
  apply_earner_type_filter <- input_params[["apply_earner_type_filter"]]
  apply_household_size_filter <- input_params[["apply_household_size_filter"]]
  apply_people_income_filter <- input_params[["apply_people_income_filter"]]
  apply_household_income_filter <- input_params[["apply_household_income_filter"]]
  
  # ---------------------------------------------
  # Step 1: Merge tawa input and output
  # ---------------------------------------------
  df_input <- as.data.table(tawa_data[["input"]])[, .(
    snz_hes_hhld_uid,
    snz_hes_uid,
    H_Counts_Adults,
    H_Counts_DependentKids,
    P_Attributes_Age,
    P_Attributes_Sex,
    P_Job_TotalHoursPerWeek
  )]
  
  df_output <- as.data.table(tawa_data[[tawa_data_name]])[, .(
    snz_hes_hhld_uid,
    snz_hes_uid,
    P_Income_Disposable
  )]
  
  df <- merge(df_input, df_output, by = c("snz_hes_uid", "snz_hes_hhld_uid"))
  
  # ---------------------------------------------
  # Step 2: Apply household filter
  # ---------------------------------------------
  if (!is.null(apply_household_size_filter)) {
    mask <- rep(TRUE, nrow(df))
    # Loop through list items (assuming Python dict structure maps to named list in R)
    for (col in names(apply_household_size_filter)) {
      bounds <- apply_household_size_filter[[col]]
      min_val <- bounds[1]
      max_val <- bounds[2]
      mask <- mask & (df[[col]] >= min_val & df[[col]] <= max_val)
    }
    df <- df[mask]
  }
  
  # ---------------------------------------------
  # Step 3: Apply Person filter (only people over 18)
  # ---------------------------------------------
  if (isTRUE(exclude_seniors)) {
    df <- df[P_Attributes_Age < 65]
  }
  df <- df[P_Attributes_Age >= 18]
  
  # ---------------------------------------------
  # Step 3: Rename the data and convert year income to weekly
  # ---------------------------------------------
  # Summing income columns row-wise
  valid_income_cols <- intersect(names(df), income_types)
  if (length(valid_income_cols) > 0) {
    df[, P_Total_income := rowSums(.SD, na.rm = TRUE) / 52.0, .SDcols = valid_income_cols]
  } else {
    df[, P_Total_income := 0]
  }
  
  setnames(df, 
           old = c("snz_hes_hhld_uid", "snz_hes_uid", "P_Attributes_Age", "P_Attributes_Sex", "P_Total_income", "P_Job_TotalHoursPerWeek"),
           new = c("household_id", "id", "age", "gender", "income_per_week", "working_hours_tawa"),
           skip_absent = TRUE)
  
  # Filter columns to keep
  cols_to_keep <- c("household_id", "id", "age", "gender", "income_per_week", "working_hours_tawa")
  df <- df[, ..cols_to_keep]
  
  # Map gender (1 -> Male, else Female)
  df[, gender := ifelse(gender == 1, "Male", "Female")]
  
  # ---------------------------------------------
  # Step 4: Quality control for the market income
  # ---------------------------------------------
  if (!is.null(apply_people_income_filter)) {
    thres_min <- quantile(df$income_per_week, probs = apply_people_income_filter[["min"]], na.rm = TRUE)
    thres_max <- quantile(df$income_per_week, probs = apply_people_income_filter[["max"]], na.rm = TRUE)
    
    df <- df[income_per_week >= thres_min & income_per_week <= thres_max]
  }
  
  # ---------------------------------------------
  # Step 5: Obtain working hours
  # ---------------------------------------------
  # Assuming obtain_working_hours is an external function you have
  df <- data_sample_env$obtain_working_hours(df) 
  
  # Calculate max working hours
  df[, working_hours := pmax(working_hours, working_hours_tawa, na.rm = TRUE)]
  
  # Apply threshold check
  # Python: df["working_hours"] < hours_options[1]
  # In R, hours_options[[2]] corresponds to Python's [1] index
  threshold <- hours_options[[2]] 
  
  df[working_hours < threshold, working_hours := 1e-9]
  
  # ---------------------------------------------
  # Step 6: Data quality control
  # ---------------------------------------------
  # 6.1: Remove outlier households 
  if (!is.null(apply_household_income_filter)) {
    # Group by household_id and sum income
    df[, household_income_per_week := sum(income_per_week, na.rm = TRUE), by = household_id]
    
    thres_min_hh <- quantile(df$household_income_per_week, probs = apply_household_income_filter[["min"]], na.rm = TRUE)
    thres_max_hh <- quantile(df$household_income_per_week, probs = apply_household_income_filter[["max"]], na.rm = TRUE)
    
    df <- df[household_income_per_week >= thres_min_hh & household_income_per_week <= thres_max_hh]
  }
  
  # 5.2: Correct wage for people who are not working (use minimum wage)
  df[, income_per_hour := income_per_week / working_hours]
  # clip lower bound
  df[income_per_hour < min_hourly_wage, income_per_hour := min_hourly_wage]
  
  # Recalculate working hours based on clipped wage
  df[, working_hours := income_per_week / income_per_hour]
  
  # ---------------------------------------------
  # Step 6: Obtain primary/secondary earner
  # ---------------------------------------------
  if (!is.null(apply_earner_type_filter)) {
    # Sort: household_id ASC, income_per_week DESC
    setorder(df, household_id, -income_per_week)
    
    selected_id <- c()
    
    filter_type <- tolower(apply_earner_type_filter)
    
    if (filter_type == "primary") {
      # Select first row per group
      df_primary <- df[, .SD[1], by = household_id]
      selected_id <- unique(df_primary$id)
      
    } else if (filter_type == "others") {
      # Select all rows except first
      # .SD[-1] selects from 2nd row onwards
      df_rest <- df[, .SD[-1], by = household_id]
      selected_id <- unique(df_rest$id)
    }
    
    df[, selected := ifelse(id %in% selected_id, TRUE, FALSE)]
  } else {
    df[, selected := TRUE]
  }
  
  if (!is.null(ref_ids)) {
    df <- df[id %in% ref_ids]
  }
  
  # ---------------------------------------------
  # Step 7: Prepare RUF inputs
  # ---------------------------------------------
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  
  output_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  
  # Call external prepare_ruf_inputs function
  data_input_env$prepare_ruf_inputs(
    df,
    hours_options,
    total_hours,
    leisure_value,
    income_name,
    working_hours_name,
    data_output_path = output_path
  )
}

run_tawa_predict <- function(tawa_data,
                             output_dir,
                             input_params,
                             reform_name,
                             updated_tawa_data_path) {
  
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = "sq")
  sq_path <- file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  data_sq <- read_parquet(sq_path)
  
  tawa_data_preprocess(
    tawa_data = tawa_data,
    input_params = input_params,
    tawa_data_name = reform_name,
    output_dir = output_dir,
    ref_ids = unique(data_sq$people_id)
  )
  
  run_ruf_for_tawa(
    tawa_data = tawa_data,
    input_params = input_params,
    updated_tawa_data_path = updated_tawa_data_path,
    tawa_data_name = reform_name,
    output_dir = output_dir
  )
}

add_utility_err_to_tawa <- function(input_params,
                                    tawa_data_name,
                                    output_dir,
                                    join_keys = c("household_id", "people_id", "option_hours", "option_hours_id")) {
  
  # 1. Load Reform Data
  reform_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  reform_file <- file.path(output_dir, paste0("utility_func_data_", reform_hash, ".parquet"))
  df_reform <- read_parquet(reform_file)
  
  # 2. Load SQ Data (Optimized column selection)
  sq_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = "sq")
  sq_file <- file.path(output_dir, paste0("utility_func_data_", sq_hash, ".parquet"))
  df_sq <- read_parquet(sq_file, col_select = all_of(c(join_keys, "calibrated_err")))
  
  # 3. Merge
  df_merged <- df_reform %>%
    left_join(distinct(df_sq, across(all_of(join_keys)), .keep_all = TRUE), by = join_keys)
  
  # 4. Write Output
  output_file <- file.path(output_dir, paste0("utility_func_data_", reform_hash, "_updated.parquet"))
  write_parquet(df_merged, output_file)
  
  return(output_file)
}

run_ruf_for_tawa <- function(tawa_data,
                             input_params,
                             updated_tawa_data_path = "test.csv",
                             tawa_data_name = "reform1",
                             output_dir = ".",
                             method = RUF_METHOD,
                             run_dist_plot = TRUE) {
  
  # ----------------------------------------------
  print("Adding utility error from SQ to Reform ...")
  # ----------------------------------------------
  add_utility_err_to_tawa(input_params, tawa_data_name, output_dir)
  
  # ----------------------------------------------
  print("Obtain RUF parameters ...")
  # ----------------------------------------------
  sq_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = "sq")
  params_file <- file.path(output_dir, paste0("utility_func_parameters_", sq_hash, ".csv"))
  ruf_params_df <- read_csv(params_file)
  
  # Convert to named vector/list to mimic Python dict index
  ruf_params <- setNames(ruf_params_df$Value, ruf_params_df$parameter)
  
  # ----------------------------------------------
  print("Get TAWA RUF data (Reform)")
  # ----------------------------------------------
  reform_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  reform_data_path <- file.path(output_dir, paste0("utility_func_data_", reform_hash, "_updated.parquet"))
  data_reform_raw <- read_parquet(reform_data_path)
  data_reform_raw <- data_reform_raw[data_reform_raw$household_id == 5,]
  data_reform <- model_ruf_env$predict(data_reform_raw, ruf_params, method = method, use_hhld = TRUE)
  
  # ----------------------------------------------
  print("Get TAWA RUF data (SQ)")
  # ----------------------------------------------
  data_sq_raw <- read_parquet(file.path(output_dir, paste0("utility_func_data_", sq_hash, ".parquet")))
  data_sq_raw <- data_sq_raw[data_sq_raw$household_id == 5,]
  data_sq <- model_ruf_env$predict(data_sq_raw, ruf_params, method = method, use_hhld = TRUE)
  
  # ----------------------------------------------
  print("Merge SQ and Reform data together ...")
  # ----------------------------------------------
  employment_scaler_data <- data_reform %>%
    inner_join(data_sq, by = c("household_id", "people_id"), suffix = c("_reform", "_sq")) %>%
    mutate(scaler = hours_reform / hours_sq)
  
  income_scaler_data <- data_reform_raw %>%
    filter(is_chosen == 1) %>%
    inner_join(data_sq_raw %>% filter(is_chosen == 1), by = c("household_id", "people_id"), suffix = c("_reform", "_sq")) %>%
    mutate(scaler = income_hhld_reform / income_hhld_sq)
  
  combined_data <- list(
    "employment" = employment_scaler_data,
    "income" = income_scaler_data
  )
  
  # ----------------- For testing ----------------------#
  # data_sq_raw2 <- data_sq_raw[, c("option_hours_id", "household_id", "income_hhld", "is_chosen")]
  # data_reform_raw2 <- data_reform_raw[, c("option_hours_id", "household_id", "income_hhld", "is_chosen")]
  # 
  # data_filter <- data_reform_raw2 %>%
  #   filter(is_chosen == 1) %>%
  #   inner_join(data_sq_raw2 %>% filter(is_chosen == 1), by = c("option_hours_id", "household_id"), suffix = c("_reform", "_sq")) %>%
  #   filter(income_hhld_reform <= income_hhld_sq)
  # 
  # combined_data$employment <- combined_data$employment %>%
  #   filter(household_id %in% unique(data_filter$household_id))
  # combined_data$income <- combined_data$income %>%
  #   filter(household_id %in% unique(data_filter$household_id))
  # 
  # browser()
  #ggplot(combined_data$employment, aes(x = income_hhld, y = scaler)) +
  #  geom_point()
  # ----------------- For testing ----------------------#
  
  # ----------------------------------------------
  print("Obtain/Plotting distribution ...")
  # ----------------------------------------------
  emp_inc <- sum(combined_data$employment$scaler > 1, na.rm = TRUE)
  emp_dec <- sum(combined_data$employment$scaler < 1, na.rm = TRUE)
  print(paste("Number of people with increased working hours", emp_inc))
  print(paste("Number of people with decreased working hours", emp_dec))
  
  dist_p <- ggplot(combined_data$employment, aes(x = income_hhld, fill = group))
  
  if (run_dist_plot == TRUE) {
    
    for (data_type in c("income", "employment")) {
      
      proc_data <- combined_data[[data_type]]
      hist_summary <- data_sq_raw %>%
        filter(is_chosen == 1) %>%
        inner_join(proc_data, by = c("household_id", "people_id")) %>%
        filter(scaler != 1) %>%
        mutate(group = ifelse(scaler > 1, "Reform > SQ", "Reform < SQ")) %>%
        select(household_id, people_id, income_hhld, income, group) %>%
        mutate(income_hhld = income_hhld * INCOME_DATA_SCALER,
               income = income * INCOME_DATA_SCALER)
      
      dist_p <- ggplot(hist_summary, aes(x = income_hhld, fill = group)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
        theme_minimal() +
        labs(title = paste0("Distribution of income (household) by", data_type," change"),
             x = "Income (household)",
             fill = "Group")
      
      filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
      ggsave(file.path(output_dir, paste0("tawa_", data_type, "_distribution_", filename_hash, ".png")), 
             plot = dist_p, width = 14, height = 6)
      
    }
  }

  # ----------------------------------------------
  print("Update TAWA data ...")
  # ----------------------------------------------
  scaler_subset <- employment_scaler_data %>%
    select(snz_hes_hhld_uid = household_id, snz_hes_uid = people_id, scaler)
  
  tawa_data_input <- tawa_data$input %>%
    left_join(scaler_subset, by = c("snz_hes_hhld_uid", "snz_hes_uid")) %>%
    mutate(scaler = replace_na(scaler, 1.0))
  
  # Apply scaling to specified income columns
  target_cols <- c("P_Income_SelfEmployed", "P_Income_WageSalary")
  tawa_data_input <- tawa_data_input %>%
    mutate(across(all_of(target_cols), ~ .x * scaler)) %>%
    select(-scaler)
  
  write_csv(tawa_data_input, updated_tawa_data_path)
}

tawa_data_preprocess <- function(tawa_data,
                                 input_params,
                                 tawa_data_name = "sq",
                                 income_name = "income_per_hour",
                                 working_hours_name = "working_hours",
                                 ref_ids = NULL,
                                 output_dir = ".") {
  
  # Step 0: Parameters
  total_hours <- input_params$total_hours
  leisure_value <- input_params$leisure_value
  min_hourly_wage <- input_params$min_hourly_wage
  hours_options <- input_params$hours_options
  exclude_seniors <- input_params$exclude_seniors
  apply_earner_type_filter <- input_params$apply_earner_type_filter
  apply_household_size_filter <- input_params$apply_household_size_filter
  apply_people_income_filter <- input_params$apply_people_income_filter
  apply_household_income_filter <- input_params$apply_household_income_filter
  
  # Step 1: Merge Input and Output
  df_input <- tawa_data$input %>%
    select(snz_hes_hhld_uid, snz_hes_uid, H_Counts_Adults, H_Counts_DependentKids, 
           P_Attributes_Age, P_Attributes_Sex, P_Job_TotalHoursPerWeek)
  
  df_output <- tawa_data[[tawa_data_name]] %>%
    select(snz_hes_hhld_uid, snz_hes_uid, P_Income_SelfEmployed, P_Income_WageSalary, 
           P_Benefits_All_Taxable, P_Benefits_All_NonTaxable, P_Income_TaxPayable)
  
  df <- inner_join(df_input, df_output, by = c("snz_hes_uid", "snz_hes_hhld_uid")) %>%
    mutate(
      market_income_person = P_Income_SelfEmployed + P_Income_WageSalary,
      benefit_income_person = P_Benefits_All_Taxable + P_Benefits_All_NonTaxable
    )
  
  # Step 2: Household size filter
  if (!is.null(apply_household_size_filter)) {
    for (col in names(apply_household_size_filter)) {
      range_vals <- apply_household_size_filter[[col]]
      df <- df %>% filter(between(!!sym(col), range_vals[1], range_vals[2]))
    }
  }
  
  # Step 3: Person Filter
  if (exclude_seniors) {
    df <- df %>% filter(P_Attributes_Age < 65)
  }
  df <- df %>% filter(P_Attributes_Age >= 18)
  
  # Step 4: Rename and Convert to Weekly
  df <- df %>%
    mutate(
      market_income_per_week = market_income_person / 52.0,
      benefit_income_per_week = benefit_income_person / 52.0,
      total_income_per_week = market_income_per_week + benefit_income_per_week,
      gender = if_else(P_Attributes_Sex == 1, "Male", "Female")
    ) %>%
    select(
      household_id = snz_hes_hhld_uid,
      id = snz_hes_uid,
      age = P_Attributes_Age,
      gender,
      market_income_per_week,
      benefit_income_per_week,
      total_income_per_week,
      working_hours_tawa = P_Job_TotalHoursPerWeek
    )
  
  # Step 5: Obtain working hours
  df <- data_sample_env$obtain_working_hours(df)
  df <- df %>%
    mutate(
      working_hours = pmax(working_hours, working_hours_tawa),
      working_hours = if_else(working_hours < hours_options[2], 1e-9, working_hours),
      market_income_per_hour = pmax(market_income_per_week / working_hours, min_hourly_wage),
      working_hours = market_income_per_week / market_income_per_hour
    )
  
  # Step 6: Data Quality Control (Income Filters)
  if (!is.null(apply_household_income_filter)) {
    df <- df %>%
      group_by(household_id) %>%
      mutate(total_income_per_week_hhld = sum(total_income_per_week)) %>%
      ungroup()
    
    thres <- quantile(df$total_income_per_week_hhld, 
                      probs = c(apply_household_income_filter$min, apply_household_income_filter$max))
    df <- df %>% filter(total_income_per_week_hhld >= thres[1], total_income_per_week_hhld <= thres[2])
  }
  
  if (!is.null(apply_people_income_filter)) {
    thres_p <- quantile(df$total_income_per_week, 
                        probs = c(apply_people_income_filter$min, apply_people_income_filter$max))
    df <- df %>% filter(total_income_per_week >= thres_p[1], total_income_per_week <= thres_p[2])
  }
  
  # Step 7: Earner Type Filter
  if (!is.null(apply_earner_type_filter)) {
    df <- df %>% arrange(household_id, desc(market_income_per_week))
    
    if (tolower(apply_earner_type_filter) == "primary") {
      selected_id <- df %>% group_by(household_id) %>% slice(1) %>% pull(id)
    } else if (tolower(apply_earner_type_filter) == "others") {
      selected_id <- df %>% group_by(household_id) %>% slice(-1) %>% pull(id)
    }
    df <- df %>% mutate(selected = id %in% selected_id)
  } else {
    df$selected <- TRUE
  }
  
  if (!is.null(ref_ids)) {
    df <- df %>% filter(id %in% ref_ids)
  }
  
  # Step 8: Prepare RUF inputs
  filename_hash <- data_filename_env$create_hash_filename(input_params, filename_suffix = tawa_data_name)
  data_input_env$prepare_ruf_inputs(
    df_input = df,
    hours_options = hours_options,
    total_hours = total_hours,
    leisure_value = leisure_value,
    market_income_name_per_hour = "market_income_per_hour",
    benefit_income_name_per_week = "benefit_income_per_week",
    working_hours_name = working_hours_name,
    data_output_path = file.path(output_dir, paste0("utility_func_data_", filename_hash, ".parquet"))
  )
}
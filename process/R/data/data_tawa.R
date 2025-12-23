

tawa_data_preprocess <- function(tawa_data, 
                                 hours_options = c(0, 20, 40),
                                 min_hourly_wage = 23.0, 
                                 exclude_seniors = FALSE,
                                 income_types = c("P_Income_Disposable"),
                                 apply_earner_type_filter = NULL,
                                 apply_household_income_filter = list(min = 0.3, max = 0.7),
                                 apply_people_income_filter = list(min = 0.1, max = 0.9),
                                 apply_household_size_filter = list(
                                   H_Counts_Adults = c(2, 2),
                                   H_Counts_DependentKids = c(1, 3)
                                 ),
                                 yearly_income = TRUE) {
  
  # --- Step 0: Merge Input and Output ---
  df_input <- as.data.table(tawa_data$input)[, .(
    snz_hes_hhld_uid, snz_hes_uid, H_Counts_Adults, 
    H_Counts_DependentKids, P_Attributes_Age, 
    P_Attributes_Sex, P_Job_TotalHoursPerWeek
  )]
  
  df_output <- as.data.table(tawa_data$output)[, .(
    snz_hes_hhld_uid, snz_hes_uid, P_Income_Disposable
  )]
  
  df <- merge(df_input, df_output, by = c("snz_hes_uid", "snz_hes_hhld_uid"))
  
  # --- Step 1: Apply Household Size & Senior Filter ---
  if (!is.null(apply_household_size_filter)) {
    for (col in names(apply_household_size_filter)) {
      limits <- apply_household_size_filter[[col]]
      df <- df[get(col) >= limits[1] & get(col) <= limits[2]]
    }
  }
  
  if (exclude_seniors) {
    df <- df[P_Attributes_Age < 65]
  }
  
  # --- Step 2: Age Filter ---
  df <- df[P_Attributes_Age > 18]
  
  # --- Step 3: Income Calculation & Renaming ---
  # Sum specified income types across rows
  df[, P_Total_income := rowSums(.SD), .SDcols = income_types]
  
  if (yearly_income) {
    df[, P_Total_income := P_Total_income / 52.0]
  }
  
  # Rename and select columns
  setnames(df, 
           old = c("snz_hes_hhld_uid", "snz_hes_uid", "P_Attributes_Age", 
                   "P_Attributes_Sex", "P_Total_income", "P_Job_TotalHoursPerWeek"),
           new = c("household_id", "id", "age", "gender", "income_per_week", "working_hours"))
  
  df <- df[, .(household_id, id, age, gender, income_per_week, working_hours)]
  
  # Map gender
  df[, gender := ifelse(gender == 1, "Male", "Female")]
  
  # --- Step 4: Person Level Income Outliers ---
  if (!is.null(apply_people_income_filter)) {
    quants <- quantile(df$income_per_week, c(apply_people_income_filter$min, apply_people_income_filter$max))
    df <- df[income_per_week >= quants[1] & income_per_week <= quants[2]]
  }
  
  # --- Step 5: Working Hours ---
  df <- data_sample_env$obtain_working_hours(df) 
  df[, working_hours := ifelse(working_hours < hours_options[2], 1e-9, working_hours)]
  
  # --- Step 6: Household Level Quality Control ---
  if (!is.null(apply_household_income_filter)) {
    # Transform sum equivalent:
    df[, household_income_per_week := sum(income_per_week), by = household_id]
    
    h_quants <- quantile(df$household_income_per_week, 
                         c(apply_household_income_filter$min, apply_household_income_filter$max))
    df <- df[household_income_per_week >= h_quants[1] & household_income_per_week <= h_quants[2]]
  }
  
  # Correct wage for non-workers
  df[, income_per_hour := ifelse(working_hours == 1e-9, 
                                 min_hourly_wage, 
                                 income_per_week / working_hours)]
  
  # --- Step 7: Earner Type Logic ---
  if (!is.null(apply_earner_type_filter)) {
    # Sort by household and then income (descending)
    setorder(df, household_id, -income_per_week)
    
    if (tolower(apply_earner_type_filter) == "primary") {
      # Get the first person in each group
      selected_ids <- df[, .SD[1], by = household_id]$id
    } else if (tolower(apply_earner_type_filter) == "others") {
      # Get everyone EXCEPT the first person
      selected_ids <- df[, .SD[-1], by = household_id]$id
    }
    
    df[, selected := id %in% selected_ids]
  } else {
    df[, selected := TRUE]
  }
  
  return(df)
}

tawa_data_preprocess2 <- function(
    df, 
    hours_options = c(0, 20, 40),
    min_hourly_wage = 23.0, 
    exclude_seniors = FALSE,
    income_types = c("P_Income_SelfEmployed", "P_Income_WageSalary"),
    apply_earner_type_filter = NULL,
    apply_household_income_filter = list("min" = 0.3, "max" = 0.7),
    apply_people_income_filter = list("min" = 0.1, "max" = 0.9),
    apply_household_size_filter = list(
      "H_Counts_Adults" = c(2, 2),
      "H_Counts_DependentKids" = c(1, 3)
    )
) {
  
  # ---------------------------------------------
  # Step 1: Apply household filter
  # ---------------------------------------------
  if (!is.null(apply_household_size_filter)) {
    for (col in names(apply_household_size_filter)) {
      min_val <- apply_household_size_filter[[col]][1]
      max_val <- apply_household_size_filter[[col]][2]
      
      # Using between equivalent
      df <- df[df[[col]] >= min_val & df[[col]] <= max_val, ]
    }
  }
  
  if (exclude_seniors) {
    # Group by household and check if min age in household < 65
    # df <- df %>%
    #  group_by(snz_hes_hhld_uid) %>%
    #  mutate(min_age_in_hh = min(P_Attributes_Age)) %>%
    #  ungroup() %>%
    #  filter(min_age_in_hh < 65) %>%
    #  select(-min_age_in_hh)
    df = df[df$P_Attributes_Age < 65,]
  }
  
  # ---------------------------------------------
  # Step 2: Apply Person filter (only people over 18 will be considered)
  # ---------------------------------------------
  df <- df[df$P_Attributes_Age > 18, ]
  
  # ---------------------------------------------
  # Step 3: Rename the data
  # ---------------------------------------------
  # Sum rows for specific columns
  # rowSums is faster but need to ensure columns exist and handle NAs if necessary (defaulting to 0 if python sum(axis=1) implies 0 for all NA)
  # Python sum(axis=1) treats NaN as 0 by default for numeric summation usually, or skips them.
  df$P_Total_income <- rowSums(df[, income_types], na.rm = TRUE)
  
  df <- df %>%
    rename(
      household_id = snz_hes_hhld_uid,
      id = snz_hes_uid,
      age = P_Attributes_Age,
      gender = P_Attributes_Sex,
      market_income_per_week = P_Total_income,
      working_hours = P_Job_TotalHoursPerWeek
    ) %>%
    select(
      household_id,
      id,
      age,
      gender,
      market_income_per_week,
      working_hours
    )
  
  df$gender <- ifelse(df$gender == 1, "Male", "Female")
  # ---------------------------------------------
  # Step 4: Quality control for the market income
  # ---------------------------------------------
  if (!is.null(apply_people_income_filter)) {

    thres_min <- quantile(df$market_income_per_week, probs = apply_people_income_filter[["min"]], na.rm = TRUE)
    thres_max <- quantile(df$market_income_per_week, probs = apply_people_income_filter[["max"]], na.rm = TRUE)
    
    df <- df %>%
      filter(
        market_income_per_week >= thres_min &
          market_income_per_week <= thres_max
      )
  }
  
  # ---------------------------------------------
  # Step 5: Obtain working hours
  # ---------------------------------------------
  df <- data_sample_env$obtain_working_hours(df)
  
  df$working_hours <- ifelse(
    df$working_hours < hours_options[2], # R is 1-indexed, so index 1 in python is index 2 here (20)
    1e-9,
    df$working_hours
  )
  
  # ---------------------------------------------
  # Step 6: Data quality control
  # ---------------------------------------------
  # 6.1: Remove outlier households 
  if (!is.null(apply_household_income_filter)) {
    df <- df %>%
      group_by(household_id) %>%
      mutate(household_market_income_per_week = sum(market_income_per_week, na.rm = TRUE)) %>%
      ungroup()
    
    thres_min <- quantile(df$household_market_income_per_week, probs = apply_household_income_filter[["min"]], na.rm = TRUE)
    thres_max <- quantile(df$household_market_income_per_week, probs = apply_household_income_filter[["max"]], na.rm = TRUE)
    
    df <- df %>%
      filter(
        household_market_income_per_week >= thres_min &
          household_market_income_per_week <= thres_max
      )
  }
  
  # 6.2: Correct wage for people who are not working (here we use minimum wage)
  df$market_income_per_hour <- ifelse(
    df$working_hours == 1e-9,
    min_hourly_wage,
    df$market_income_per_week / df$working_hours
  )
  
  # ---------------------------------------------
  # Step 6: Obtain primary/secondary earner
  # ---------------------------------------------
  if (!is.null(apply_earner_type_filter)) {
    # Sort values
    df <- df %>%
      arrange(household_id, desc(market_income_per_week))
    
    selected_id <- c()
    
    if (tolower(apply_earner_type_filter) == "primary") {
      # pandas nth(0) is the first row
      df_primary <- df %>%
        group_by(household_id) %>%
        slice(1) %>%
        ungroup()
      selected_id <- unique(df_primary$id)
      
    } else if (tolower(apply_earner_type_filter) == "others") {
      # pandas nth(1) is the second row
      df_secondary <- df %>%
        group_by(household_id) %>%
        # slice(2) %>%'
        slice(-1) %>%
        ungroup()
      selected_id <- unique(df_secondary$id)
    }
    
    df$selected <- ifelse(df$id %in% selected_id, TRUE, FALSE)
  } else {
    df$selected <- TRUE
  }
  
  return(df)
}
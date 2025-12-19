

# Assuming 'obtain_working_hours' is available in your R environment
# source("process/R/data/sample.R")

tawa_data_preprocess <- function(
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
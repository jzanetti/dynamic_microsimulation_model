
# --- Configuration & Constants ---

SAMPLE_DATA_DIR <- file.path("etc", "sample")
EPSILON <- 1e-6

SAMPLE_DATA_CFG <- list(
  # --- Single Person ---
  list(code = "1A", A = 1, S = 0, C = 0, prob = 0.14),
  list(code = "1S", A = 0, S = 1, C = 0, prob = 0.10),
  # --- Couples (No Kids) ---
  list(code = "2A", A = 2, S = 0, C = 0, prob = 0.15),
  list(code = "1A+1S", A = 1, S = 1, C = 0, prob = 0.03),
  list(code = "2S", A = 0, S = 2, C = 0, prob = 0.08),
  # --- Families with Children ---
  list(code = "2A+1C", A = 2, S = 0, C = 1, prob = 0.10),
  list(code = "2A+2C", A = 2, S = 0, C = 2, prob = 0.12),
  list(code = "2A+3C", A = 2, S = 0, C = 3, prob = 0.05),
  list(code = "1A+1C", A = 1, S = 0, C = 1, prob = 0.05),
  list(code = "1A+2C", A = 1, S = 0, C = 2, prob = 0.04),
  # --- Multi-Generational / Flatting ---
  list(code = "3A", A = 3, S = 0, C = 0, prob = 0.05),
  list(code = "2A+1S", A = 2, S = 1, C = 0, prob = 0.03),
  list(code = "2A+1S+1C", A = 2, S = 1, C = 1, prob = 0.02),
  list(code = "Others", A = 2, S = 0, C = 0, prob = 0.04)
)

EMP_PARAMS <- list(
  age_base_prob = list(
    "0-14" = 0.00,
    "15-19" = 0.40,
    "20-29" = 0.75,
    "30-54" = 0.88,
    "55-64" = 0.70,
    "65-74" = 0.02,
    "75-100" = 0.0
  ),
  # Helper to reconstruct bins for cut()
  age_breaks = c(0, 14, 19, 29, 54, 64, 74, 100),
  age_labels = c(0.00, 0.40, 0.75, 0.88, 0.70, 0.02, 0.0),
  
  gender_multiplier = c("Male" = 1.0, "Female" = 0.88),
  education_multiplier = c(
    "Not finished" = 0.30,
    "School" = 0.90,
    "Vocational" = 1.05,
    "University" = 1.2
  )
)

WORKING_HRS <- data.frame(
  age_group = c("15-24", "15-24", "25-34", "25-34", "35-44", "35-44", 
                "45-54", "45-54", "55-64", "55-64", "65+", "65+"),
  gender = c("Male", "Female", "Male", "Female", "Male", "Female",
             "Male", "Female", "Male", "Female", "Male", "Female"),
  hours_mean = c(27.5, 23.5, 41.0, 35.0, 42.0, 29.5, 
                 41.0, 34.0, 39.0, 33.0, 26.0, 22.0),
  stringsAsFactors = FALSE
)

# --- Helper: Clip function ---
clip_val <- function(x, lower, upper) {
  pmax(lower, pmin(x, upper))
}

# --- 1. Simulation Sample Mortality ---

simulation_sample_mortality <- function(pop_input,
                                        b0 = -4.5,
                                        b_age = 0.095,
                                        eth_offset_map = c(
                                          "Maori" = 0.4,
                                          "Pacific" = 0.35,
                                          "Asian" = -0.3,
                                          "European" = 0.0,
                                          "Other" = 0.0
                                        )) {
  
  pop <- pop_input # Copy on write behavior in R
  
  # Map ethnicity
  pop$b_eth <- eth_offset_map[pop$ethnicity]
  # Handle potential NAs if ethnicity not in map (optional safety)
  pop$b_eth[is.na(pop$b_eth)] <- 0.0
  
  # Calculate "True" rate
  z_score <- b0 + (b_age * pop$age) + pop$b_eth
  pop$true_rate <- pnorm(z_score)
  
  # Calculate deaths (Binomial)
  pop$deaths <- rbinom(n = nrow(pop), size = 1, prob = pop$true_rate)
  
  # Select columns
  mortality_data <- pop %>% select(age, ethnicity, deaths)
  
  # Binning
  bins <- seq(0, 110, by = 10)
  # labels equivalent to f"{i}-{i+9}"
  labels <- paste0(bins[-length(bins)], "-", bins[-length(bins)] + 9)
  
  mortality_data$age_group <- cut(
    mortality_data$age, 
    breaks = bins, 
    labels = labels, 
    right = FALSE, 
    include.lowest = TRUE
  )
  
  # Group and Sum
  mortality_summary <- mortality_data %>%
    group_by(age_group, ethnicity) %>%
    summarise(deaths = sum(deaths), .groups = "drop") %>%
    arrange(ethnicity, age_group) %>%
    rename(ethnicity_group = ethnicity)
  
  return(mortality_summary)
}

# --- 2. Generate Sample Supplements ---

generate_sample_supplements <- function(required_data_types = c("mortality")) {
  
  proc_data_path <- file.path(SAMPLE_DATA_DIR, "pop_data.parquet")
  
  if (!file.exists(proc_data_path)) {
    stop(paste("File not found:", proc_data_path))
  }
  
  pop_data <- read_parquet(proc_data_path)
  
  if ("mortality" %in% required_data_types) {
    mortality_data <- simulation_sample_mortality(pop_data)
    mortality_data_path <- file.path(SAMPLE_DATA_DIR, "mortality_data.parquet")
    write_parquet(mortality_data, mortality_data_path)
  }
  
  if ("ruf" %in% required_data_types) {
    ruf_data <- simulation_sample_ruf(pop_data)
    ruf_data_path <- file.path(SAMPLE_DATA_DIR, "ruf_data.parquet")
    write_parquet(ruf_data, ruf_data_path)
  }
  
  if ("heckman" %in% required_data_types) {
    heckman_data <- simulation_sample_heckman(pop_data)
    heckman_data_path <- file.path(SAMPLE_DATA_DIR, "heckman_data.parquet")
    write_parquet(heckman_data, heckman_data_path)
  }
  
}

# --- 3. Obtain Employment Status ---

obtain_employment_status <- function(df, params) {
  
  # A. Apply Age Probabilities
  # Using pre-calculated breaks and labels from EMP_PARAMS constant
  df$base_prob <- cut(
    df$age,
    breaks = params$age_breaks,
    labels = params$age_labels,
    include.lowest = TRUE,
    right = TRUE 
  )
  
  # Convert factor to numeric
  df$base_prob <- as.numeric(as.character(df$base_prob))
  df$base_prob[is.na(df$base_prob)] <- 0.0
  
  # B. Apply Multipliers
  gender_factor <- params$gender_multiplier[df$gender]
  gender_factor[is.na(gender_factor)] <- 1.0
  
  edu_factor <- params$education_multiplier[df$education_level]
  edu_factor[is.na(edu_factor)] <- 1.0
  
  # C. Joint Probability
  df$prob_employed <- df$base_prob * gender_factor * edu_factor
  
  # D. Add Noise
  noise <- rnorm(n = nrow(df), mean = 0.0, sd = 0.07)
  df$prob_employed <- df$prob_employed + noise
  
  # E. Clip and Draw
  df$prob_employed <- clip_val(df$prob_employed, 0, 1)
  
  random_draws <- runif(nrow(df))
  df$is_employed <- random_draws < df$prob_employed
  
  return(df)
}

# --- 4. Obtain Market Income ---

obtain_market_income <- function(df) {
  
  # 1. Base Wage (Log-normal)
  # R's rlnorm takes meanlog and sdlog
  base_wage_dist <- rlnorm(n = nrow(df), meanlog = 10.6, sdlog = 0.1)
  
  # 2. Education Multiplier
  educ_map <- c(
    "Not finished" = 0.15,
    "School" = 0.85,
    "Vocational" = 1.10,
    "University" = 1.55
  )
  edu_factor <- educ_map[df$education_level]
  
  # 3. Age Multiplier
  age_calc <- clip_val(df$age, 18, 80)
  age_factor <- 1 + 0.025 * (age_calc - 20) - 0.00035 * (age_calc - 20)^2
  age_factor <- pmax(age_factor, 0.5)
  
  # 4. Gender Multiplier
  gender_factor <- ifelse(df$gender == "Male", 1.0, 0.90)
  
  # 5. Final Calculation
  df$market_income <- base_wage_dist * edu_factor * age_factor * gender_factor
  
  # 6. Apply Unemployment
  df$market_income[!df$is_employed] <- 0
  
  # Rounding
  df$market_income <- round(df$market_income, 0)
  
  return(df)
}

# --- 5. Obtain Benefit Income ---

obtain_benefit_income <- function(df) {
  df$benefit_income <- 0.0
  df$benefit_income[df$age >= 65] <- 26000
  return(df)
}

# --- 6. Obtain Working Hours ---

obtain_working_hours <- function(df) {
  
  # Setup Bins matching Python logic
  bins <- c(14, 24, 34, 44, 54, 64, 999)
  age_order <- unique(WORKING_HRS$age_group) # e.g. "15-24", "25-34"...
  
  # Assign Age Group in Main Data
  df$age_group <- cut(
    df$age,
    breaks = bins,
    labels = age_order,
    include.lowest = FALSE # (14, 24] -> 15-24
  )
  
  # Merge reference hours
  pop_merged <- left_join(df, WORKING_HRS, by = c("age_group", "gender"))
  
  # Assign Hours
  pop_merged$working_hours <- pop_merged$hours_mean
  
  # Handle NA (if any) and Zero logic
  pop_merged$working_hours[is.na(pop_merged$working_hours)] <- 0
  pop_merged$working_hours[pop_merged$working_hours == 0] <- EPSILON
  
  # Clean up extra columns from merge if desired (hours_mean)
  pop_merged$hours_mean <- NULL
  
  return(pop_merged)
}

# --- 7. Generate Sample Population (Main) ---

generate_sample_population <- function(n = 1000, 
                                       seed_num = 42, 
                                       hh_configs = SAMPLE_DATA_CFG) {
  
  if (!is.null(seed_num)) set.seed(seed_num)
  
  # Probability vector for configs
  probs <- sapply(hh_configs, function(x) x$prob)
  probs <- probs / sum(probs) # Normalize
  
  people_data_list <- list()
  hh_id <- 1
  current_n <- 0
  
  # Loop until N reached
  while (current_n < n) {
    
    # Choose config
    config_idx <- sample(seq_along(hh_configs), 1, prob = probs)
    config <- hh_configs[[config_idx]]
    
    # Head Age Logic
    if (config$S > 0 && config$A == 0) {
      head_age <- floor(runif(1, 65, 91)) # randint(65, 90) -> 65..90 inclusive
    } else if (config$C > 0) {
      head_age <- round(rnorm(1, 40, 8))
      head_age <- clip_val(head_age, 20, 60)
    } else if (config$S > 0 && config$A > 0) {
      head_age <- floor(runif(1, 55, 71)) # randint(55, 70)
    } else {
      head_age <- round(rnorm(1, 35, 12))
      head_age <- clip_val(head_age, 18, 64)
    }
    
    members <- list()
    
    # Create Adults
    if (config$A > 0) {
      for (i in 1:config$A) {
        if (i == 1 && config$S == 0) {
          age <- head_age
        } else {
          age <- head_age + floor(runif(1, -5, 6)) # randint(-5, 5)
        }
        members[[length(members) + 1]] <- list(role = "Adult", age = clip_val(age, 18, 64))
      }
    }
    
    # Create Seniors
    if (config$S > 0) {
      for (i in 1:config$S) {
        if (config$A == 0 && i == 1) {
          age <- head_age
        } else if (config$A > 0) {
          age <- head_age + floor(runif(1, 20, 31)) # randint(20, 30)
        } else {
          age <- head_age + floor(runif(1, -5, 6))
        }
        members[[length(members) + 1]] <- list(role = "Senior", age = max(65, age))
      }
    }
    
    # Create Children
    if (config$C > 0) {
      for (i in 1:config$C) {
        age <- head_age - floor(runif(1, 20, 43)) # randint(20, 42)
        members[[length(members) + 1]] <- list(role = "Child", age = clip_val(age, 0, 17))
      }
    }
    
    # Assign Attributes
    hh_region <- sample(
      c("Auckland", "Wellington", "Christchurch", "Others"), 
      1, 
      prob = c(0.34, 0.10, 0.08, 0.48)
    )
    
    # Add to main list
    for (m in members) {
      if (current_n >= n + 50) break # Buffer break
      
      m$household_id <- hh_id
      m$household_type <- config$code
      m$region <- hh_region
      m$gender <- sample(c("Male", "Female"), 1, prob = c(0.49, 0.51))
      
      people_data_list[[length(people_data_list) + 1]] <- m
      current_n <- current_n + 1
    }
    hh_id <- hh_id + 1
  }
  
  # Convert list of lists to DF
  df <- bind_rows(people_data_list)
  df <- head(df, n) # Slice exact n
  df$id <- seq_len(nrow(df))
  
  # --- 3. Demographic & Socio-Economic Logic ---
  
  # A. Ethnicity
  df$ethnicity <- "European"
  eth_cats <- c("European", "Maori", "Pacific", "Asian", "Other")
  
  # Masks
  mask_under20 <- df$age < 20
  mask_working <- df$age >= 20 & df$age < 65
  mask_senior <- df$age >= 65
  
  if (any(mask_under20)) {
    df$ethnicity[mask_under20] <- sample(eth_cats, sum(mask_under20), replace=TRUE, prob=c(0.50, 0.25, 0.12, 0.10, 0.03))
  }
  if (any(mask_working)) {
    df$ethnicity[mask_working] <- sample(eth_cats, sum(mask_working), replace=TRUE, prob=c(0.60, 0.15, 0.08, 0.14, 0.03))
  }
  if (any(mask_senior)) {
    df$ethnicity[mask_senior] <- sample(eth_cats, sum(mask_senior), replace=TRUE, prob=c(0.85, 0.07, 0.03, 0.04, 0.01))
  }
  
  # B. Education (18+)
  df$education_level <- "Not finished"
  mask_edu <- df$age >= 18
  if (any(mask_edu)) {
    df$education_level[mask_edu] <- sample(
      c("Not finished", "School", "Vocational", "University"),
      size = sum(mask_edu),
      replace = TRUE,
      prob = c(0.1, 0.35, 0.35, 0.2)
    )
  }
  
  # C. Employment & Income pipelines
  df <- obtain_employment_status(df, EMP_PARAMS)
  df <- obtain_market_income(df)
  df <- obtain_benefit_income(df)
  df <- obtain_working_hours(df)
  
  # --- 4. Final Data Cleanup ---
  
  # Household stats
  hh_stats <- df %>%
    group_by(household_id) %>%
    summarise(
      n_adults = sum(age >= 18 & age < 65),
      n_seniors = sum(age >= 65),
      n_children = sum(age < 18),
      .groups = "drop"
    )
  
  df <- left_join(df, hh_stats, by = "household_id")
  
  # Employed Flag & Working Hours cleanup
  df$employed <- as.numeric(df$market_income > 0)
  df$working_hours[df$employed == 0] <- EPSILON
  
  cols <- c(
    "id", "household_id", "household_type", "role", "age", "gender", 
    "ethnicity", "region", "education_level", "is_employed", 
    "market_income", "benefit_income", "n_adults", "n_seniors", 
    "n_children", "employed", "working_hours"
  )
  
  # Write Output
  if (!dir.exists(SAMPLE_DATA_DIR)) {
    dir.create(SAMPLE_DATA_DIR, recursive = TRUE)
  }
  
  pop_data_path <- file.path(SAMPLE_DATA_DIR, "pop_data.parquet")
  write_parquet(df %>% select(all_of(cols)), pop_data_path)
  
  return(df)
}


simulation_sample_ruf <- function(df_input) {
  # Transforms raw household census/survey data into the specific format 
  # required for the dashboard/model.
  # 
  # Args:
  #     df_input (data.frame): The raw dataframe containing 'id', 'household_id', 
  #                            'age', 'market_income', 'working_hours', etc.
  #                            
  # Returns:
  #     data.frame: A dataframe with calculated weekly incomes, age groups, 
  #                 and household aggregates.
  
  # Create a copy (R does "copy-on-modify" automatically, but explicit assignment creates the local variable)
  df <- df_input
  
  # 1. Calculate Market Income Per Week
  # Assumption: Input 'market_income' is Annual. 
  # If input is already weekly, remove the `/ 52`.
  # Python: .round(0).astype(int)
  df$market_income_per_week <- as.integer(round(df$market_income / 52, 0))
  
  # 2. Create Age Groups
  # Bins: 0-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65+
  # Python cut(right=False) means intervals are [a, b). 
  # R cut(right=FALSE) achieves the same.
  bins <- c(0, 15, 25, 35, 45, 55, 65, 150)
  labels <- c('0-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65+')
  
  df$age_group <- cut(df$age, breaks = bins, labels = labels, right = FALSE)
  
  # 3. Create Household Market Income Per Week
  # Sums weekly income for everyone with the same household_id
  # Python's transform('sum') broadcasts the sum to all rows in the group
  df <- df %>%
    group_by(household_id) %>%
    mutate(household_market_income_per_week = sum(market_income_per_week, na.rm = TRUE)) %>%
    ungroup()
  
  # 4. Create Market Income Per Hour
  # Handle division by zero for non-workers
  # Using vectorized ifelse instead of apply/lambda for performance in R
  df$market_income_per_hour <- ifelse(
    df$working_hours > EPSILON + 1.0,
    df$market_income_per_week / df$working_hours,
    EPSILON
  )
  
  # 5. Helper Columns 
  # 'hours_mean' appears to be a direct copy of working_hours in your target schema
  df$hours_mean <- df$working_hours
  df$selected <- TRUE
  
  # 6. Select and Order Columns
  target_cols <- c(
    'household_id', 'id', 'age', 'gender', 'market_income_per_week', 
    'working_hours', 'age_group', 'hours_mean', 
    'household_market_income_per_week', 'market_income_per_hour', 'selected'
  )
  
  # Return only the columns requested
  return(df[, target_cols])
}


simulation_sample_heckman <- function(df_input, weeks_per_year = 52) {
  # Note: The calculation implies market_income is the total income over the period,
  # and we are deriving an hourly rate (or weekly rate per hour unit) depending on interpretation,
  # but strictly following the Python arithmetic:

  df_input$market_income_per_week <- df_input$market_income / (
    df_input$working_hours * weeks_per_year
  )
  df_input$market_income_per_week[df_input$working_hours < EPSILON + 0.1] <- EPSILON

  
  return(df_input[, c("id", "age", "gender", "education_level", "market_income_per_week", "employed")])
}

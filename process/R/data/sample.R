
# Ensure the output directory exists
SAMPLE_DATA_DIR <- file.path("etc", "sample")

SAMPLE_DATA_CFG <- list(
  household_types = list(
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
)

EMP_PARAMS <- list(
  # Note: In R lists, keys are strings. 
  # We will handle the parsing of numeric ranges inside the function.
  age_base_prob = list(
    "0,14" = 0.00,
    "15,19" = 0.40,
    "20,29" = 0.75,
    "30,54" = 0.88,
    "55,64" = 0.7,
    "65,74" = 0.02,
    "75,100" = 0.0
  ),
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
  gender    = c("Male", "Female", "Male", "Female", "Male", "Female", 
                "Male", "Female", "Male", "Female", "Male", "Female"),
  hours_mean = c(27.5, 23.5, 41.0, 35.0, 42.0, 29.5, 
                 41.0, 34.0, 39.0, 33.0, 26.0, 22.0)
)

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
  
  # Copy implicit in R assignment usually, but explicit copy ensures independence if using environments
  pop <- pop_input 
  
  # Map ethnicity offsets
  # Using named vector matching
  pop$b_eth <- eth_offset_map[pop$ethnicity]
  
  # Calculate "True" rate
  # R's pnorm is the CDF (equivalent to scipy.stats.norm.cdf)
  z_score <- b0 + (b_age * pop$age) + pop$b_eth
  pop$true_rate <- pnorm(z_score)
  
  # Calculate actual number of deaths (Binomial expectation)
  # rbinom(n, size, prob)
  pop$deaths <- rbinom(n = nrow(pop), size = 1, prob = pop$true_rate)
  
  # Select specific columns
  mortality_data <- pop %>% select(age, ethnicity, deaths)
  
  # Define bins matching range(0, 111, 10) -> 0, 10, ... 110
  # Python: right=False means [0, 10)
  bins <- seq(0, 110, by = 10)
  labels <- paste0(bins[-length(bins)], "-", bins[-length(bins)] + 9)
  
  # Create the 'age_group' column
  mortality_data$age_group <- cut(
    mortality_data$age,
    breaks = bins,
    labels = labels,
    right = FALSE,
    include.lowest = TRUE
  )
  
  # Group by 'age_group' and 'ethnicity'
  # summarise behaves like groupby().sum().reset_index()
  mortality_data <- mortality_data %>%
    group_by(age_group, ethnicity) %>%
    summarise(deaths = sum(deaths), .groups = "drop") %>%
    arrange(ethnicity, age_group) %>%
    rename(ethnicity_group = ethnicity)
  
  return(mortality_data)
}

generate_sample_supplements <- function(required_data_types = c("mortality")) {
  
  proc_data_path <- file.path(SAMPLE_DATA_DIR, "pop_data.parquet")
  pop_data <- read_parquet(proc_data_path)
  
  if ("mortality" %in% required_data_types) {
    mortality_data <- simulation_sample_mortality(pop_data)
    mortality_data_path <- file.path(SAMPLE_DATA_DIR, "mortality_data.parquet")
    
    if (!dir.exists(SAMPLE_DATA_DIR)) {
      dir.create(SAMPLE_DATA_DIR, recursive = TRUE)
    }
    write_parquet(mortality_data, mortality_data_path)
  }
}

obtain_employment_status <- function(df, params) {
  df <- df # R creates a copy on write by default
  
  # --- A. Apply Age Probabilities (The Base) ---
  
  # Parse the dictionary keys (which are strings in the R list) to get bins and values
  age_prob_list <- params$age_base_prob
  
  # Sort keys to ensure order (mimicking sorted_age_keys) based on the start number
  keys <- names(age_prob_list)
  # Extract numeric start value for sorting
  starts <- as.numeric(sapply(str_split(keys, ","), `[`, 1))
  ends <- as.numeric(sapply(str_split(keys, ","), `[`, 2))
  
  sorted_indices <- order(starts)
  sorted_keys <- keys[sorted_indices]
  
  # Construct bins
  # Python logic: bins.append(sorted_age_keys[0][0]) then loop ends
  # Note: Python's pd.cut(include_lowest=True) usually handles the first edge
  bins <- c(starts[sorted_indices[1]])
  labels <- numeric()
  
  for (k in sorted_keys) {
    # Extract end value
    end_val <- as.numeric(unlist(str_split(k, ","))[2])
    bins <- c(bins, end_val)
    labels <- c(labels, age_prob_list[[k]])
  }
  
  # Map Age to Base Probability
  # Python: include_lowest=True, right=True (default). 
  # We use cut with include.lowest = TRUE.
  df$base_prob <- as.numeric(as.character(cut(
    df$age,
    breaks = bins,
    labels = labels,
    include.lowest = TRUE,
    ordered_result = FALSE
  )))
  
  # Handle NaN (ages outside defined bins)
  df$base_prob[is.na(df$base_prob)] <- 0.0
  
  # --- B. Apply Multipliers (Gender & Education) ---
  # Using named vector lookup
  gender_factor <- params$gender_multiplier[df$gender]
  gender_factor[is.na(gender_factor)] <- 1.0
  
  edu_factor <- params$education_multiplier[df$education_level]
  edu_factor[is.na(edu_factor)] <- 1.0
  
  # --- C. Calculate Joint Probability ---
  df$prob_employed <- df$base_prob * gender_factor * edu_factor
  
  # --- D. Add Uncertainty (Noise) ---
  # rnorm is equivalent to numpy.random.normal
  noise <- rnorm(n = nrow(df), mean = 0.0, sd = 0.07)
  df$prob_employed <- df$prob_employed + noise
  
  # --- E. Final Clip and Draw ---
  # Clip to 0.0 to 1.0 using pmin/pmax
  df$prob_employed <- pmin(pmax(df$prob_employed, 0), 1)
  
  # Generate boolean status
  random_draws <- runif(nrow(df))
  df$is_employed <- random_draws < df$prob_employed
  
  return(df)
}

obtain_market_income <- function(df) {
  
  # D. Market Income Calculation
  # 1. Base Wage (Annual, Full-time equivalent baseline)
  # rlnorm takes meanlog and sdlog
  base_wage_dist <- rlnorm(n = nrow(df), meanlog = 10.6, sdlog = 0.1)
  
  # 2. Education Multiplier
  educ_map <- c(
    "Not finished" = 0.15,
    "School" = 0.85,
    "Vocational" = 1.10,
    "University" = 1.55
  )
  edu_factor <- educ_map[df$education_level]
  
  # 3. Age Multiplier (The "Career Arc")
  # Clip age to 18 to avoid issues
  age_calc <- pmin(pmax(df$age, 18), 80)
  age_factor <- 1 + 0.025 * (age_calc - 20) - 0.00035 * (age_calc - 20)^2
  age_factor <- pmax(age_factor, 0.5)
  
  # 4. Gender Multiplier (Statistical Wage Gap)
  # ifelse is vectorised
  gender_factor <- ifelse(df$gender == "Male", 1.0, 0.90)
  
  # 5. Calculate Final Market Income
  df$market_income <- base_wage_dist * edu_factor * age_factor * gender_factor
  
  # 6. Apply Unemployment (Zero market income if not employed)
  df$market_income[!df$is_employed] <- 0
  
  # Rounding
  df$market_income <- round(df$market_income, 0)
  
  return(df)
}

obtain_benefit_income <- function(df) {
  # E. Benefit Income (Simple Superannuation)
  df$benefit_income <- 0.0
  # NZ Super: Universal for 65+, approx 26k net
  df$benefit_income[df$age >= 65] <- 26000
  return(df)
}

generate_sample_population <- function(n = 1000, seed_num = 42, hh_configs = SAMPLE_DATA_CFG$household_types) {
  if (!is.null(seed_num)) {
    set.seed(seed_num)
  }
  
  # --- 1. Define Household Compositions ---
  probs <- sapply(hh_configs, function(x) x$prob)
  probs <- probs / sum(probs) # Normalise
  
  # --- 2. Generate Households Loop ---
  people_data <- list()
  hh_id <- 1
  current_n <- 0
  
  # Pre-calculate indices to sample from (0-indexed to match python range logic or 1-indexed for R)
  hh_indices <- 1:length(hh_configs)
  
  while (current_n < n) {
    # choice equivalent in R is sample
    config_idx <- sample(hh_indices, 1, prob = probs)
    config <- hh_configs[[config_idx]]
    members <- list()
    
    # -- (Existing Age Logic Preserved) --
    if (config$S > 0 && config$A == 0) {
      # randint(65, 90) -> sample(65:89, 1) in Python usually excludes endpoint, 
      # but numpy.random.randint includes low, excludes high.
      # Python: randint(65, 90) -> [65, 89]. R sample(65:89, 1).
      head_age <- sample(65:89, 1)
    } else if (config$C > 0) {
      head_age <- as.integer(rnorm(1, 40, 8))
      head_age <- pmin(pmax(head_age, 20), 60)
    } else if (config$S > 0 && config$A > 0) {
      head_age <- sample(55:69, 1) # Python randint(55, 70)
    } else {
      head_age <- as.integer(rnorm(1, 35, 12))
      head_age <- pmin(pmax(head_age, 18), 64)
    }
    
    # Create Adults
    if (config$A > 0) {
      for (i in 1:config$A) {
        # Logic: i == 0 (1st iteration) and config['S'] == 0
        is_first_adult <- (i == 1 && config$S == 0)
        
        if (is_first_adult) {
          age <- head_age
        } else {
          # randint(-5, 6) -> range [-5, 5]
          offset <- sample(-5:5, 1)
          age <- head_age + offset
        }
        members[[length(members) + 1]] <- list(role = "Adult", age = pmin(pmax(age, 18), 64))
      }
    }
    
    # Create Seniors
    if (config$S > 0) {
      for (i in 1:config$S) {
        if (config$A == 0 && i == 1) {
          age <- head_age
        } else if (config$A > 0) {
          # randint(20, 30) -> [20, 29]
          offset <- sample(20:29, 1)
          age <- head_age + offset
        } else {
          # randint(-5, 6) -> [-5, 5]
          offset <- sample(-5:5, 1)
          age <- head_age + offset
        }
        members[[length(members) + 1]] <- list(role = "Senior", age = max(65, age))
      }
    }
    
    # Create Children
    if (config$C > 0) {
      for (i in 1:config$C) {
        # randint(20, 42) -> [20, 41]
        offset <- sample(20:41, 1)
        age <- head_age - offset
        members[[length(members) + 1]] <- list(role = "Child", age = pmin(pmax(age, 0), 17))
      }
    }
    
    # Assign Attributes
    hh_region <- sample(
      c("Auckland", "Wellington", "Christchurch", "Others"),
      1,
      prob = c(0.34, 0.10, 0.08, 0.48)
    )
    
    for (m in members) {
      if (current_n >= n + 50) break
      
      # Update member with common attributes
      m$household_id <- hh_id
      m$household_type <- config$code
      m$region <- hh_region
      m$gender <- sample(c("Male", "Female"), 1, prob = c(0.49, 0.51))
      
      people_data[[length(people_data) + 1]] <- m
      current_n <- current_n + 1
    }
    hh_id <- hh_id + 1
  }
  
  # Convert list of lists to DataFrame
  # bind_rows is efficient for this
  df <- bind_rows(people_data)
  
  # Slice to exactly n
  df <- df[1:n, ]
  df$id <- 1:n
  
  # --- 3. Demographic & Socio-Economic Logic (UPDATED) ---
  
  # A. Ethnicity
  df$ethnicity <- "European"
  eth_cats <- c("European", "Maori", "Pacific", "Asian", "Other")
  
  # Vectorized assignment
  mask_under_20 <- df$age < 20
  if (sum(mask_under_20) > 0) {
    df$ethnicity[mask_under_20] <- sample(
      eth_cats,
      size = sum(mask_under_20),
      replace = TRUE,
      prob = c(0.50, 0.25, 0.12, 0.10, 0.03)
    )
  }
  
  mask_20_65 <- (df$age >= 20) & (df$age < 65)
  if (sum(mask_20_65) > 0) {
    df$ethnicity[mask_20_65] <- sample(
      eth_cats,
      size = sum(mask_20_65),
      replace = TRUE,
      prob = c(0.60, 0.15, 0.08, 0.14, 0.03)
    )
  }
  
  mask_over_65 <- df$age >= 65
  if (sum(mask_over_65) > 0) {
    df$ethnicity[mask_over_65] <- sample(
      eth_cats,
      size = sum(mask_over_65),
      replace = TRUE,
      prob = c(0.85, 0.07, 0.03, 0.04, 0.01)
    )
  }
  
  # B. Education Assignment (18+)
  df$education_level <- "Not finished"
  mask_edu <- df$age >= 18
  if (sum(mask_edu) > 0) {
    df$education_level[mask_edu] <- sample(
      c("Not finished", "School", "Vocational", "University"),
      size = sum(mask_edu),
      replace = TRUE,
      prob = c(0.1, 0.35, 0.35, 0.2)
    )
  }
  
  # C. Employment Status
  df <- obtain_employment_status(df, EMP_PARAMS)
  df <- obtain_market_income(df)
  df <- obtain_benefit_income(df)
  df <- obtain_working_hours(df)
  
  # --- 4. Final Data Cleanup ---
  # HH Stats aggregation
  hh_stats <- df %>%
    group_by(household_id) %>%
    summarise(
      n_adults = sum(age >= 18 & age < 65),
      n_seniors = sum(age >= 65),
      n_children = sum(age < 18),
      .groups = "drop"
    )
  
  # Merge
  df <- left_join(df, hh_stats, by = "household_id")
  
  cols <- c(
    "id",
    "household_id",
    "household_type",
    "role",
    "age",
    "gender",
    "ethnicity",
    "region",
    "education_level",
    "is_employed",
    "market_income",
    "benefit_income",
    "n_adults",
    "n_seniors",
    "n_children"
  )
  
  # Write output
  if (!dir.exists(SAMPLE_DATA_DIR)) {
    dir.create(SAMPLE_DATA_DIR, recursive = TRUE)
  }
  
  pop_data_path <- file.path(SAMPLE_DATA_DIR, "pop_data.parquet")
  
  # Write strictly the columns requested
  write_parquet(df[, cols], pop_data_path)
  
  return(df)
}


obtain_working_hours <- function(df) {
  # Assumes WORKING_HRS is already a dataframe in your global environment
  df_hours <- WORKING_HRS
  
  # 1. Get ordered labels from the unique values in the lookup table
  age_order <- unique(df_hours$age_group)
  
  # 2. Define bins (14 to ensure 15 is included, 999 as catch-all)
  # R's cut() includes the rightmost value by default, just like pandas
  bins <- c(14, 24, 34, 44, 54, 64, 999)
  
  # 3. Pipeline: Binning -> Joining -> Assigning
  pop_merged <- df %>%
    mutate(
      # cut() creates a factor (Categorical) automatically
      age_group = cut(age, breaks = bins, labels = age_order)
    ) %>%
    # Left join is equivalent to merge(..., how='left')
    left_join(df_hours, by = c("age_group", "gender")) %>%
    mutate(
      working_hours = hours_mean
    )
  
  pop_merged$hours_mean <- NULL
  
  return(pop_merged)
}

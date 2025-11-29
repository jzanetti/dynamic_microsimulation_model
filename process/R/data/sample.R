

SAMPLE_DATA_CFG <- list(
  list(code = '1A',       A = 1, S = 0, C = 0, prob = 0.14),
  list(code = '1S',       A = 0, S = 1, C = 0, prob = 0.10),
  list(code = '2A',       A = 2, S = 0, C = 0, prob = 0.15),
  list(code = '1A+1S',    A = 1, S = 1, C = 0, prob = 0.03),
  list(code = '2S',       A = 0, S = 2, C = 0, prob = 0.08),
  list(code = '2A+1C',    A = 2, S = 0, C = 1, prob = 0.10),
  list(code = '2A+2C',    A = 2, S = 0, C = 2, prob = 0.12),
  list(code = '2A+3C',    A = 2, S = 0, C = 3, prob = 0.05),
  list(code = '1A+1C',    A = 1, S = 0, C = 1, prob = 0.05),
  list(code = '1A+2C',    A = 1, S = 0, C = 2, prob = 0.04),
  list(code = '3A',       A = 3, S = 0, C = 0, prob = 0.05),
  list(code = '2A+1S',    A = 2, S = 1, C = 0, prob = 0.03),
  list(code = '2A+1S+1C', A = 2, S = 1, C = 1, prob = 0.02),
  list(code = 'Others',   A = 2, S = 0, C = 0, prob = 0.04)
)
SAMPLE_DATA_DIR = "etc/sample/"

simulation_sample_mortality <- function(pop_input, 
                                        b0 = -4.5, 
                                        b_age = 0.095, 
                                        eth_offset_map = list(
                                          "Maori" = 0.4, 
                                          "Pacific" = 0.35, 
                                          "Asian" = -0.3, 
                                          "European" = 0.0, 
                                          "Other" = 0.0
                                        )) {
  
  # Create a deep copy (in data.table, copy() ensures we don't modify the input by reference)
  pop <- copy(pop_input)
  
  # Map ethnicity offsets
  # unlist helps match the dictionary lookup style
  pop[, b_eth := as.numeric(unlist(eth_offset_map)[ethnicity])]
  
  # Handle any NAs if ethnicity wasn't found (default to 0 or handle logic)
  pop[is.na(b_eth), b_eth := 0]
  
  # Calculate "True" rate
  # Python: scipy.stats.norm.cdf -> R: pnorm
  pop[, z_score := b0 + (b_age * age) + b_eth]
  pop[, true_rate := pnorm(z_score)]
  
  # Calculate actual number of deaths (Binomial expectation)
  # Python: numpy.random.binomial -> R: rbinom
  pop[, deaths := rbinom(.N, size = 1, prob = true_rate)]
  
  # Filter for deaths only
  mortality_data <- pop[deaths == 1, .(age, ethnicity, deaths)]
  
  # Binning Logic
  # Python: range(0, 111, 10) -> R: seq(0, 110, 10)
  bins <- seq(0, 110, 10)
  # labels logic: paste0(i, "-", i+9)
  bin_labels <- paste0(head(bins, -1), "-", head(bins, -1) + 9)
  
  # Python: cut(..., right=False) -> R: cut(..., right = FALSE)
  mortality_data[, age_group := cut(age, breaks = bins, labels = bin_labels, right = FALSE)]
  
  # Group by 'age_group' and 'ethnicity'
  # data.table aggregation: dt[, .(sum_col = sum(col)), by = .(groups)]
  # We use complete.cases or expand.grid logic if we need to see observed=False behavior,
  # but standard aggregation is usually sufficient.
  mortality_agg <- mortality_data[, .(deaths = sum(deaths)), by = .(age_group, ethnicity)]
  
  # Sort
  setorder(mortality_agg, ethnicity, age_group)
  
  # rename the ethnicity column
  mortality_agg <- mortality_agg %>%
    rename(ethnicity_group = ethnicity)
  
  return(mortality_agg)
}

add_sample_population_status <- function(pop) {
  pop[, life_stage := "live"]
  return(pop)
}

# Helper function to mimic numpy.clip
clip_val <- function(x, lower, upper) {
  pmax(lower, pmin(x, upper))
}

generate_sample_population <- function(n = 1000, seed_num = 42, hh_configs = SAMPLE_DATA_CFG) {
  
  if (!is.null(seed_num)) set.seed(seed_num)
  
  # --- 1. Define Household Compositions (Default Configs) ---
  if (is.null(hh_configs)) {
    hh_configs <- list(
      list(code = '1A',       A = 1, S = 0, C = 0, prob = 0.14),
      list(code = '1S',       A = 0, S = 1, C = 0, prob = 0.10),
      list(code = '2A',       A = 2, S = 0, C = 0, prob = 0.15),
      list(code = '1A+1S',    A = 1, S = 1, C = 0, prob = 0.03),
      list(code = '2S',       A = 0, S = 2, C = 0, prob = 0.08),
      list(code = '2A+1C',    A = 2, S = 0, C = 1, prob = 0.10),
      list(code = '2A+2C',    A = 2, S = 0, C = 2, prob = 0.12),
      list(code = '2A+3C',    A = 2, S = 0, C = 3, prob = 0.05),
      list(code = '1A+1C',    A = 1, S = 0, C = 1, prob = 0.05),
      list(code = '1A+2C',    A = 1, S = 0, C = 2, prob = 0.04),
      list(code = '3A',       A = 3, S = 0, C = 0, prob = 0.05),
      list(code = '2A+1S',    A = 2, S = 1, C = 0, prob = 0.03),
      list(code = '2A+1S+1C', A = 2, S = 1, C = 1, prob = 0.02),
      list(code = 'Others',   A = 2, S = 0, C = 0, prob = 0.04)
    )
  }
  
  # Extract probabilities and normalize
  probs <- sapply(hh_configs, function(x) x$prob)
  probs <- probs / sum(probs)
  
  # --- 2. Generate Households Loop ---
  people_list <- list()
  hh_id <- 1
  current_n <- 0
  
  # Define region probabilities once
  regions <- c('Auckland', 'Wellington', 'Christchurch', 'Others')
  region_probs <- c(0.34, 0.10, 0.08, 0.48)
  
  while (current_n < n) {
    # Pick a configuration index
    config_idx <- sample(seq_along(hh_configs), 1, prob = probs)
    config <- hh_configs[[config_idx]]
    
    members <- list()
    
    # --- Age Generation Logic ---
    
    # 1. Determine Head Age
    if (config$S > 0 && config$A == 0) {
      # Senior only
      head_age <- sample(65:90, 1)
    } else if (config$C > 0) {
      # Family
      head_age <- round(rnorm(1, 40, 8))
      head_age <- clip_val(head_age, 20, 60)
    } else if (config$S > 0 && config$A > 0) {
      # Mixed Adult/Senior
      head_age <- sample(55:70, 1)
    } else {
      # Standard Adult
      head_age <- round(rnorm(1, 35, 12))
      head_age <- clip_val(head_age, 18, 64)
    }
    
    # 2. Create Adults (18-64)
    if (config$A > 0) {
      for (i in 1:config$A) {
        if (i == 1 && config$S == 0) {
          age <- head_age
        } else {
          age <- head_age + sample(-5:5, 1)
        }
        age <- clip_val(age, 18, 64)
        members[[length(members) + 1]] <- list(role = 'Adult', age = age)
      }
    }
    
    # 3. Create Seniors (65+)
    if (config$S > 0) {
      for (i in 1:config$S) {
        if (config$A == 0 && i == 1) {
          age <- head_age
        } else {
          if (config$A > 0) {
            age <- head_age + sample(20:30, 1)
          } else {
            age <- head_age + sample(-5:5, 1)
          }
        }
        age <- max(65, age)
        members[[length(members) + 1]] <- list(role = 'Senior', age = age)
      }
    }
    
    # 4. Create Children (0-17)
    if (config$C > 0) {
      for (i in 1:config$C) {
        age <- head_age - sample(20:42, 1)
        age <- clip_val(age, 0, 17)
        members[[length(members) + 1]] <- list(role = 'Child', age = age)
      }
    }
    
    # --- Assign Common Attributes ---
    hh_region <- sample(regions, 1, prob = region_probs)
    
    for (m in members) {
      # Stop buffer
      if (current_n >= n + 50) break
      
      # Convert list element to dataframe row
      person <- data.frame(
        household_id = hh_id,
        household_type = config$code,
        role = m$role,
        age = m$age,
        region = hh_region,
        gender = sample(c('Male', 'Female'), 1, prob = c(0.49, 0.51)),
        stringsAsFactors = FALSE
      )
      
      people_list[[length(people_list) + 1]] <- person
      current_n <- current_n + 1
    }
    hh_id <- hh_id + 1
  }
  
  # Bind all rows
  df <- bind_rows(people_list)
  # Trim to exactly n
  df <- df[1:n, ]
  df$id <- 1:n
  
  # --- 3. Demographic & Socio-Economic Logic ---
  
  # Ethnicity
  df$ethnicity <- 'European'
  eth_cats <- c('European', 'Maori', 'Pacific', 'Asian', 'Other')
  
  # Assign based on age masks
  mask_young <- df$age < 20
  if (any(mask_young)) {
    df$ethnicity[mask_young] <- sample(eth_cats, sum(mask_young), replace = TRUE, prob = c(0.50, 0.25, 0.12, 0.10, 0.03))
  }
  
  mask_adult <- df$age >= 20 & df$age < 65
  if (any(mask_adult)) {
    df$ethnicity[mask_adult] <- sample(eth_cats, sum(mask_adult), replace = TRUE, prob = c(0.60, 0.15, 0.08, 0.14, 0.03))
  }
  
  mask_old <- df$age >= 65
  if (any(mask_old)) {
    df$ethnicity[mask_old] <- sample(eth_cats, sum(mask_old), replace = TRUE, prob = c(0.85, 0.07, 0.03, 0.04, 0.01))
  }
  
  # Education (18+)
  df$education_level <- 'None'
  mask_edu <- df$age >= 18
  if (any(mask_edu)) {
    df$education_level[mask_edu] <- sample(c('Low', 'Medium', 'High'), sum(mask_edu), replace = TRUE, prob = c(0.35, 0.40, 0.25))
  }
  
  # Employment (18-64)
  df$employment_status <- 'Not in Labor Force'
  df$labour_income <- 0.0
  mask_work <- df$age >= 18 & df$age < 65
  
  if (any(mask_work)) {
    df$employment_status[mask_work] <- sample(c('Employed', 'Unemployed'), sum(mask_work), replace = TRUE, prob = c(0.95, 0.05))
  }
  
  # Wages
  # rlnorm takes meanlog and sdlog
  base_wage <- rlnorm(n, meanlog = 10.6, sdlog = 0.5)
  
  educ_map <- c('None' = 0, 'Low' = 0.85, 'Medium' = 1.0, 'High' = 1.35)
  # Match education level to multiplier
  educ_mult <- educ_map[df$education_level]
  df$wage_offer <- base_wage * educ_mult
  
  emp_mask <- df$employment_status == 'Employed'
  df$labour_income[emp_mask] <- df$wage_offer[emp_mask]
  
  # Wealth & Investments
  age_factor <- df$age / 50.0
  wealth_base <- rlnorm(n, meanlog = 10.5, sdlog = 1.8)
  df$wealth_stock <- wealth_base * age_factor
  df$wealth_stock[df$age < 18] <- 0
  df$investment_income <- df$wealth_stock * 0.04
  
  # NZ Super (Universal for 65+)
  super_mask <- df$age >= 65
  df$labour_income[super_mask] <- df$labour_income[super_mask] + 26000
  
  # --- 4. Final Aggregations ---
  df$disposable_income <- df$labour_income + df$investment_income
  
  # Calculate Household Composition Columns
  hh_stats <- df %>%
    group_by(household_id) %>%
    summarise(
      n_adults = sum(age >= 18 & age < 65),
      n_seniors = sum(age >= 65),
      n_children = sum(age < 18),
      .groups = 'drop'
    )
  
  # Join stats back to main dataframe
  df <- left_join(df, hh_stats, by = "household_id")
  
  # Reorder columns
  cols <- c('id', 'household_id', 'household_type', 'role', 'age', 'n_adults', 'n_seniors', 'n_children',
            'gender', 'region', 'ethnicity', 'disposable_income')
  remaining <- setdiff(names(df), cols)
  
  pop_data <- as.data.table(df[, c(cols, remaining)])
  pop_data <- add_sample_population_status(pop_data)
  mortality_data <- simulation_sample_mortality(pop_data)
 
  dir.create(SAMPLE_DATA_DIR, showWarnings = FALSE)
  
  write_parquet(as.data.frame(pop_data), paste0(SAMPLE_DATA_DIR, "/pop_data.parquet"))
  write_parquet(as.data.frame(mortality_data), paste0(SAMPLE_DATA_DIR, "/mortality_data.parquet"))
}
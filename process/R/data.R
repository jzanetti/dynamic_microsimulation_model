
#' Generate sample population data
#' @param n Number of population
#' @return A data.table representing the initial population
#' The function will write a CSV file to the etc/data directory.

generate_sample_population <- function(n = 1000, seed = 42) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # --- 1. Basic Demographics ---
  ids <- 1:n
  
  # NZ Age structure approximation
  # Define bins: 0-14, 15-29, 30-44, 45-59, 60-74, 75-99
  # We select a bin based on weights, then a random age within that bin
  age_weights <- c(0.19, 0.20, 0.20, 0.19, 0.15, 0.07)
  # Sample which bin each person falls into (1 to 6)
  age_groups <- sample(1:6, n, replace = TRUE, prob = age_weights)
  
  # Define min/max for each bin
  bin_mins <- c(0, 15, 30, 45, 60, 75)
  bin_maxs <- c(14, 29, 44, 59, 74, 99)
  
  # Generate specific ages uniformly within the assigned bins
  ages <- floor(runif(n, min = bin_mins[age_groups], max = bin_maxs[age_groups] + 1))
  
  # Gender (approx 51% female)
  genders <- sample(c('Male', 'Female'), n, replace = TRUE, prob = c(0.49, 0.51))
  
  # Region (Auckland ~34%, Wellington ~10%, etc.)
  regions <- sample(
    c('Auckland', 'Wellington', 'Christchurch', 'Others'),
    n, replace = TRUE,
    prob = c(0.34, 0.10, 0.08, 0.48)
  )
  
  # Create Data Frame
  df <- data.frame(
    id = ids,
    age = ages,
    gender = genders,
    region = regions,
    stringsAsFactors = FALSE
  )
  
  # --- 2. Education Logic (Dependent on Age) ---
  df$education_level <- "None"
  df$in_education <- FALSE
  
  # School age (5-17)
  mask_school <- df$age >= 5 & df$age <= 17
  df$in_education[mask_school] <- TRUE
  df$education_level[mask_school] <- "Low"
  
  # Adult Education (18+)
  mask_adult <- df$age >= 18
  num_adults <- sum(mask_adult)
  if (num_adults > 0) {
    df$education_level[mask_adult] <- sample(
      c('Low', 'Medium', 'High'),
      num_adults, replace = TRUE,
      prob = c(0.35, 0.40, 0.25)
    )
  }
  
  # University students (18-24)
  mask_uni <- df$age >= 18 & df$age <= 24
  num_uni <- sum(mask_uni)
  if (num_uni > 0) {
    df$in_education[mask_uni] <- sample(c(TRUE, FALSE), num_uni, replace = TRUE, prob = c(0.4, 0.6))
  }
  
  # --- 3. Employment & Income ---
  df$employment_status <- 'Not in Labor Force'
  df$labour_income <- 0
  df$wage_offer <- 0
  
  # Working age (18-64)
  mask_work_age <- df$age >= 18 & df$age < 65
  num_work_age <- sum(mask_work_age)
  
  if (num_work_age > 0) {
    df$employment_status[mask_work_age] <- sample(
      c('Employed', 'Unemployed'),
      num_work_age, replace = TRUE,
      prob = c(0.95, 0.05)
    )
  }
  
  # Base wage (lognormal approx 50k median)
  base_wage <- rlnorm(n, meanlog = 10.8, sdlog = 0.4)
  
  # Multipliers map
  # We use a named vector as a lookup table
  edu_mult <- c("None" = 0, "Low" = 0.8, "Medium" = 1.0, "High" = 1.4)
  df$wage_offer <- base_wage * edu_mult[df$education_level]
  
  # Assign labour income only to employed
  mask_employed <- df$employment_status == 'Employed'
  df$labour_income[mask_employed] <- df$wage_offer[mask_employed]
  
  # --- 4. Wealth & Investment ---
  # Wealth correlates with age
  age_factor <- df$age / 50.0
  wealth_base <- rlnorm(n, meanlog = 11, sdlog = 1.5)
  df$wealth_stock <- wealth_base * age_factor
  
  # Kids have 0 wealth
  df$wealth_stock[df$age < 18] <- 0
  
  # Investment income (3% return)
  df$investment_income <- df$wealth_stock * 0.03
  
  # --- 5. Health & Children ---
  # Health declines with age (5.5 base - age penalty + noise)
  health_raw <- 5.5 - (df$age / 25.0) + rnorm(n, 0, 0.5)
  # Clip between 1 and 5 and round
  df$health_status <- pmin(pmax(round(health_raw), 1), 5)
  
  df$disability <- FALSE
  # Disability mask: Low health & 30% random chance
  mask_disabled <- (df$health_status <= 2) & (runif(n) < 0.3)
  df$disability[mask_disabled] <- TRUE
  
  # Mental health score
  df$ghq_score <- rnorm(n, 12, 5)
  
  # Children (Assigned to age 25-55)
  df$num_children <- 0
  mask_parents <- df$age >= 25 & df$age <= 55
  num_parents <- sum(mask_parents)
  if (num_parents > 0) {
    df$num_children[mask_parents] <- sample(
      0:4, num_parents, replace = TRUE,
      prob = c(0.3, 0.2, 0.3, 0.15, 0.05)
    )
  }
  
  # --- 6. Total Income ---
  # Add NZ Super for 65+ (approx 25k)
  mask_super <- df$age >= 65
  df$labour_income[mask_super] <- df$labour_income[mask_super] + 25000
  
  df$disposable_income <- df$labour_income + df$investment_income
  
  # --- 7. Lagged Variables ---
  df$lag_health_status <- df$health_status
  df$lag_employment_status <- df$employment_status
  
  # Income Quintiles (1 to 5)
  # Using rank and ceiling to calculate quintiles manually to avoid external pkg dependency
  ranks <- rank(df$disposable_income, ties.method = "first")
  df$lag_income_quintile <- ceiling(ranks / (n / 5))
  
  # --- 8. Partnerships (Assortative Mating) ---
  df$partner_id <- NA
  
  # Get indices of people > 18
  eligible_idx <- which(df$age >= 18)
  
  # Create a sorting key: Age + random noise
  # This ensures people are paired with others of similar age
  sort_key <- df$age[eligible_idx] + rnorm(length(eligible_idx), 0, 2)
  
  # Order the eligible indices by this key
  sorted_eligible <- eligible_idx[order(sort_key)]
  
  # Loop through pairs (step by 2)
  # We stop at length-1 to avoid index out of bounds if odd number
  for (i in seq(1, length(sorted_eligible) - 1, by = 2)) {
    # 70% chance to form a couple
    if (runif(1) < 0.70) {
      p1 <- sorted_eligible[i]
      p2 <- sorted_eligible[i+1]
      
      df$partner_id[p1] <- df$id[p2]
      df$partner_id[p2] <- df$id[p1]
    }
  }
  
  return(df)
}
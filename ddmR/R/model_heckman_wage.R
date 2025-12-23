heckman_wage_model <- function(df, 
                               selection_col, 
                               outcome_col, 
                               select_exog, 
                               outcome_exog, 
                               add_intercept = TRUE) {
  
  # --- 1. Prepare Data ---
  # Identify all relevant columns
  all_cols <- unique(c(selection_col, select_exog, outcome_exog))
  
  # Drop rows where independent variables are missing
  # (Base R equivalent to df.dropna(subset=all_cols))
  data <- df[complete.cases(df[, all_cols]), ]
  
  # --- Step 1: Selection Equation (Probit) ---
  
  # Construct the formula: selection_col ~ x1 + x2 + ...
  # In R, intercepts are added by default. If add_intercept is FALSE, we add " - 1"
  rhs_select <- paste(select_exog, collapse = " + ")
  if (!add_intercept) rhs_select <- paste(rhs_select, "- 1")
  
  f_select <- as.formula(paste(selection_col, "~", rhs_select))
  
  # Fit Probit (GLM with binomial family and probit link)
  probit_model <- glm(f_select, data = data, family = binomial(link = "probit"))
  
  # Calculate Inverse Mills Ratio (Lambda)
  # predict(..., type = "link") gives the linear predictor (Z-scores)
  z_scores <- predict(probit_model, type = "link")
  
  # IMR = pdf(z) / cdf(z)
  # dnorm is PDF, pnorm is CDF
  data$imr <- dnorm(z_scores) / pnorm(z_scores)
  
  # --- Step 2: Outcome Equation (OLS) ---
  
  # Filter for selected observations (participation == 1) AND outcome is not NA
  # Note: R's lm() handles NA in outcome automatically, but we filter to match logic
  data_selected <- data[data[[selection_col]] == 1 & !is.na(data[[outcome_col]]), ]
  
  # Construct the formula: outcome_col ~ x1 + x2 + imr
  rhs_outcome <- paste(c(outcome_exog, "imr"), collapse = " + ")
  if (!add_intercept) rhs_outcome <- paste(rhs_outcome, "- 1")
  
  f_outcome <- as.formula(paste(outcome_col, "~", rhs_outcome))
  
  # Fit OLS
  ols_model <- lm(f_outcome, data = data_selected)
  
  # --- Return Results ---
  return(list(
    selection = probit_model,
    outcome = ols_model,
    data = data,
    outcome_exog = outcome_exog
  ))
}
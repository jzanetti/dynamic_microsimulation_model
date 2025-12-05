

heckman_wage_model <- function(df,
                               selection_col = NULL,
                               outcome_col = NULL,
                               select_exog = NULL,
                               outcome_exog = NULL,
                               add_intercept = TRUE) {
  
  # """
  # Performs Heckman's Two-Step correction for selection bias.
  # """
  # 1. Prepare Data
  # Drop rows where independent variables are missing to avoid alignment errors
  # Python: all_cols = list(set([selection_col] + select_exog + outcome_exog))
  # We use unique() to replicate set(), and c() to combine lists
  all_cols <- unique(c(selection_col, select_exog, outcome_exog))
  
  # Python: data = df.dropna(subset=all_cols).copy()
  # We use drop_na from tidyr/dplyr logic, or base R complete.cases
  data <- df %>% 
    tidyr::drop_na(all_of(all_cols)) %>%
    as.data.frame() # Ensure it's a standard dataframe for modeling
  
  # --- Step 1: Selection Equation (Probit) ---
  # Construct formula for GLM
  # Equivalent to sm.add_constant, R adds intercept by default (y ~ x). 
  # If add_intercept is FALSE, we use (y ~ x - 1).
  select_formula_str <- paste(selection_col, "~", paste(select_exog, collapse = " + "))
  if (!add_intercept) {
    select_formula_str <- paste(select_formula_str, "- 1")
  }
  select_formula <- as.formula(select_formula_str)
  
  # Fit Probit
  # Python: probit_model = sm.Probit(y_select, X_select).fit(disp=0)
  probit_model <- glm(select_formula, family = binomial(link = "probit"), data = data)
  
  # Calculate Inverse Mills Ratio (Lambda)
  # Get the predicted Z-scores (index)
  # Python: z_scores = probit_model.predict(X_select, linear=True)
  # In R, type="link" returns the linear predictor (Z-score)
  z_scores <- predict(probit_model, type = "link")
  
  # IMR = pdf(z) / cdf(z)
  # Python: data["imr"] = norm.pdf(z_scores) / norm.cdf(z_scores)
  data$imr <- dnorm(z_scores) / pnorm(z_scores)
  
  # --- Step 2: Outcome Equation (OLS) ---
  # Filter for selected observations (e.g., where participation == 1)
  # We must also ensure the outcome_col is not NaN
  data_selected <- data[data[[selection_col]] == 1 & !is.na(data[[outcome_col]]), ]
  
  # Python: y_outcome = data_selected[outcome_col]
  # Python: X_outcome = data_selected[outcome_exog + ["imr"]]
  
  # Construct formula for OLS
  # We add "imr" to the independent variables
  outcome_vars <- c(outcome_exog, "imr")
  outcome_formula_str <- paste(outcome_col, "~", paste(outcome_vars, collapse = " + "))
  
  if (!add_intercept) {
    outcome_formula_str <- paste(outcome_formula_str, "- 1")
  }
  outcome_formula <- as.formula(outcome_formula_str)
  
  # Fit OLS
  ols_model <- lm(outcome_formula, data = data_selected)
  
  # --- Step 3: derive Rho and Sigma (Optional diagnostics) ---
  # (Commented out in source, preserved as comments here)
  # beta_lambda = coef(ols_model)["imr"]
  # sigma_e_sq = summary(ols_model)$sigma^2 # approximate MSE resid
  # sigma_u = sqrt(sigma_e_sq + beta_lambda^2 * var(data_selected$imr))
  # rho = beta_lambda / sigma_u
  
  # Python: X_outcome_all = data[outcome_exog + ["imr"]]
  # Python: X_outcome_all = sm.add_constant(X_outcome_all, has_constant="add")
  # Python: data["latent_market_income"] = ols_model.predict(X_outcome_all)
  
  # In R, predicting on 'newdata' applies the model coefficients (including intercept) 
  # to the full dataset provided.
  # data$latent_market_income <- predict(ols_model, newdata = data)
  
  return (
    list(selection = probit_model, outcome = ols_model, data = data)
  )
}
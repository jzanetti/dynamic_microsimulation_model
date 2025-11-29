library(caret)
library(dplyr)

fit_aggregated_rate_model <- function(df, target_col, predictor_cols, population_col) {
  
  # Ensure proper types
  df <- df %>%
    mutate(across(all_of(c(target_col, population_col)), as.numeric))
  
  df <- df %>%
    mutate(across(where(is.character), as.factor))

  df$rate <- df[[target_col]] / df[[population_col]]

  df$rate[is.nan(df$rate)] <- 1.0
  
  formula_str <- paste("rate", "~", paste(predictor_cols, collapse = " + "))

  model <- train(
    as.formula(formula_str),
    data = df,
    method = "glm")
  
  # We need to store the names of categorical columns to fix NAs in prediction later
  model$formula_str <- formula_str
  # Test:
  # new_data_df <- data.frame(age_group = c("50-59", "50-59"), ethnicity_group = c("Asian", "Maori"))
  # preds_linear <- predict(model, newdata = new_data_df)
  
  return(model)
}

predict_expected_deaths <- function(model, new_data_df) {
    results <- predict(model, newdata = new_data_df)
    return(results)
}
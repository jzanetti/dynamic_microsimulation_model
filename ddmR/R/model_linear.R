library(caret)
library(dplyr)

linear_model <- function(df, target_col, predictor_cols, population_col = NULL, use_rate = FALSE) {
  # For example, Obtain mortality probabilities, the input is sth like:
  #     age_group ethnicity_group  deaths  base_year  count
  # 0        0-9           Asian       0       2025    165
  # 1      10-19           Asian       0       2025    141
  # 2      20-29           Asian       2       2025    119
  # 3      30-39           Asian      19       2025    260
  # 4      40-49           Asian      75       2025    240
  # 5      50-59           Asian      60       2025     85
  # the function will predict the death rate (e.g., deaths/count) based on
  # selected predictors
  # Ensure proper types
  
  df[[target_col]] <- as.numeric(df[[target_col]])
  # df <- df %>%
  #   mutate(across(all_of(target_col), as.numeric))
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  if (use_rate == TRUE) {
    df[[population_col]] <- as.numeric(df[[population_col]])
    #df <- df %>%
    #  mutate(across(all_of(population_col), as.numeric))
    df$target <- df[[target_col]] / df[[population_col]]
    df$target[is.nan(df$target)] <- 1.0
  } else {
    df$target <- df[[target_col]]
  }
  
  #df <- df %>%
  #  mutate(across(all_of(c(target_col, population_col)), as.numeric))
  
  #df <- df %>%
  #  mutate(across(where(is.character), as.factor))

  #df$rate <- df[[target_col]] / df[[population_col]]

  #df$rate[is.nan(df$rate)] <- 1.0
  
  formula_str <- paste("target", "~", paste(predictor_cols, collapse = " + "))

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

# predict_expected_deaths <- function(model, new_data_df) {
#     results <- predict(model, newdata = new_data_df)
#     return(results)
# }
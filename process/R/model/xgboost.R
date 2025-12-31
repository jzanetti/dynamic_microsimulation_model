

xgboost_model <- function(df_for_train,
                          target_col,
                          predictor_cols,
                          population_col = NULL) {
  
  train_features <- as.matrix(df_for_train[, ..predictor_cols])
  train_label <- df_for_train[[target_col]]
  
  # Assuming xgboost_model logic:
  model <- xgboost(
    data = train_features,
    label = train_label,
    nrounds = 300,
    params = list(objective = "reg:tweedie"),
    verbose = 0
  )
  
  return (model)
}
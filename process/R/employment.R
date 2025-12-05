

forward <- function(pop_data, cfg) {
  pop_data$employed <- as.numeric(pop_data$market_income > 0)
  return(run_heckman_model(pop_data, cfg))
}


run_heckman_model <- function(pop_data, cfg, heckman_age_groups = c("0-18", "18-65", "65-999")) {

  model_wrapper_env$run_heckman_wage_model(pop_data, cfg, heckman_age_groups = heckman_age_groups)
  pop_data <- run_heckman_wage_model_prediction(pop_data, cfg, heckman_age_groups = heckman_age_groups)
  return(pop_data)
}


run_heckman_wage_model_prediction <- function(pop_data, cfg, heckman_age_groups) {

  all_data <- list()
  for (proc_ages_str in heckman_age_groups) {
    model_outputpath <- file.path(
      cfg$output_dirs$models,
      paste0("model_heckman_wage_", proc_ages_str, ".rds")
    )
    models <- readRDS(model_outputpath)
    proc_data <- models$data
    proc_data$latent_market_income <- predict(models$outcome, newdata = proc_data)
    all_data[[length(all_data) + 1]] <- proc_data
  }
  
  all_data_df <- bind_rows(all_data)
  if ("latent_market_income" %in% colnames(pop_data)) {
    pop_data$latent_market_income <- NULL
  }
  
  # Select only id and latent_income from results to merge back
  merge_cols <- all_data_df %>% select(id, latent_market_income)
  pop_data <- left_join(pop_data, merge_cols, by = "id")
  
  return (pop_data)
  
}


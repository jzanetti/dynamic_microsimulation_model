create_outputs <- function(data_dir, model_dir) {
  
  # Construct path and load data
  data_path <- file.path(data_dir, "results.parquet")
  
  if (!file.exists(data_path)) {
    stop(paste("File not found:", data_path))
  }
  
  data <- read_parquet(data_path)
  
  # <><><><><><><><><><><><><><><><><><><><>
  # 1. Produce population status
  # <><><><><><><><><><><><><><><><><><><><>
  pop_stats <- data %>%
    group_by(year, life_stage) %>%
    tally(name = "count") %>% # tally() is equivalent to summarise(n())
    ungroup()
  
  # Python: filter alive -> drop_duplicates -> groupby year -> count
  hhd_stats <- data %>%
    filter(life_stage == "alive") %>%
    select(year, household_id) %>%
    distinct() %>% # drop_duplicates
    count(year, name = "count")
  
  # <><><><><><><><><><><><><><><><><><><><>
  # 2. Produce employment status
  # <><><><><><><><><><><><><><><><><><><><>
  employment_stats <- list()
  keys_to_process <- c("market_income_per_week", "latent_market_income_per_week")
  
  for (proc_key in keys_to_process) {
    # Dynamic column name for the result
    col_name <- paste0("mean_", proc_key)
    
    # Calculate mean
    # Note: R's mean() requires na.rm = TRUE to match Python's default behavior of ignoring NaNs
    stat_df <- data %>%
      filter(life_stage == "alive") %>%
      group_by(year) %>%
      summarise(!!col_name := mean(.data[[proc_key]], na.rm = TRUE), .groups = "drop")
    
    employment_stats[[col_name]] <- stat_df
  }
  
  return(list(
    population = list(pop = pop_stats, hhd = hhd_stats),
    employment = list(income = employment_stats)
  ))
}
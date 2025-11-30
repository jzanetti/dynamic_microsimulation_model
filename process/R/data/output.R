
create_outputs <- function(data_dir) {
  data_path <- file.path(data_dir, "results.parquet")
  data <- read_parquet(data_path) %>%
    as_tibble()
  
  # <><><><><><><><><><><><><><><><><><><><>
  # 1. Produce population status
  # <><><><><><><><><><><><><><><><><><><><>
  pop_stats <- data %>%
    group_by(year, life_stage) %>%
    summarise(count = n(), .groups = "drop")
  
  hhd_stats <- data %>%
    # Filter for alive people (data[data["life_stage"] == "alive"])
    filter(life_stage == "alive") %>%
    # Select year and household_id and get unique combinations (.drop_duplicates())
    distinct(year, household_id) %>%
    # Group by year
    group_by(year) %>%
    # Count the number of unique household IDs (groupby("year")["household_id"].count())
    summarise(count = n(), .groups = "drop")
  
  # The return structure in R is typically a named list
  return(list(
    population = list(
      pop = pop_stats,
      hhd = hhd_stats
    )
  ))
}
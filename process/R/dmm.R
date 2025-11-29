
run_dmm <- function(population_data, cfg, start_year, years = 5) {
  print("Starting DMM Processing")
  
  end_year <- start_year + years - 1
  for (proc_year in start_year:end_year) {
    print(paste0("Processing Year: ", proc_year))
    person_env$forward(population_data, proc_year, cfg)
  }
  
}

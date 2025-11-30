
run_dmm <- function(population_data, cfg, start_year, years = 5) {
  print("Starting DMM Processing")
  
  results <- list()
  end_year <- start_year + years - 1
  
  results_index <- 1
  for (proc_year in start_year:end_year) {
    print(paste0("Processing Year: ", proc_year))

    proc_pop <- person_env$forward(population_data, proc_year, cfg)
    
    results[[results_index]] <- proc_pop
    
    results_index <- results_index + 1
  }
  
  results <- bind_rows(results)
  
  output_path <- paste0(cfg[["output_dirs"]][["outputs"]], "/results.parquet")
  
  print(paste0("Writing outputs: ", output_path))
  
  dir.create(cfg[["output_dirs"]][["outputs"]], recursive = TRUE, showWarnings = FALSE)
  
  write_parquet(as.data.frame(results), output_path)
  
}

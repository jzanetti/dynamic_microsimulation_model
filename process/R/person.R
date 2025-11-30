forward <- function(data, 
                    forward_year, 
                    cfg, 
                    id_col_name = "id", 
                    required_data_types = c("pop", "mortality")) {

  # 1. Validation: Check if keys exist in the list (dictionary)
  for (proc_data_type in required_data_types) {
    if (!proc_data_type %in% names(data)) {
      stop(sprintf("Data type %s is missing ...", proc_data_type))
    }
  }
  
  # 2. Extract population data
  pop <- data[["pop"]]
  
  # 3. Calculate year difference
  # Note: We assume base_year is uniform, taking the first unique value
  current_base_year <- as.integer(unique(pop$base_year))
  year_diff <- forward_year - current_base_year
  
  pop$year <- forward_year
  
  # 4. Early return if no time has passed
  if (year_diff == 0) {
    return(pop)
  }
  
  # <><><><><><><><><><><><><><><><>
  # Update Age
  # <><><><><><><><><><><><><><><><>
  pop$age <- pop$age + year_diff
  
  # <><><><><><><><><><><><><><><><>
  # Calculate mortality
  # <><><><><><><><><><><><><><><><>
  # Assuming run_mortality is a defined R function available in your environment
  pop <- mortality_env$run_mortality(pop, id_col_name = id_col_name, cfg = cfg)
  
  # The "x = 3" in your snippet seemed to be a placeholder or typo.
  # Based on the type hint "-> DataFrame", we return the modified pop df.
  return(pop)
}
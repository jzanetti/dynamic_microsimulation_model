forward <- function(pop, 
                    forward_year, 
                    cfg, 
                    id_col_name = "id") {

  # Calculate year difference
  current_base_year <- as.integer(unique(pop$base_year))
  year_diff <- forward_year - current_base_year
  
  # 4. Early return if no time has passed
  if (year_diff == 0) {
    return(pop)
  }
  
  pop$year <- forward_year
  
  # <><><><><><><><><><><><><><><><>
  # Update Age
  # <><><><><><><><><><><><><><><><>
  pop$age <- pop$age + year_diff
  
  # <><><><><><><><><><><><><><><><>
  # Calculate mortality
  # <><><><><><><><><><><><><><><><>
  pop <- mortality_env$run_mortality(pop, id_col_name = id_col_name, cfg = cfg)
  
  return(pop)
}
forward <- function(pop, 
                    forward_year, 
                    cfg, 
                    id_col_name = "id") {

  # Calculate year difference
  year_diff <- forward_year - as.integer(unique(pop$year))
  
  # 4. Early return if no time has passed
  if (year_diff == 0) {
    return(pop)
  }
  
  pop$year <- forward_year
  
  # <><><><><><><><><><><><><><><><>
  # Update Age
  # <><><><><><><><><><><><><><><><>
  pop$age <- pop$age + year_diff
  
  return(pop)
}
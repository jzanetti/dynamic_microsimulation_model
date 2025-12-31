cal_tax <- function(income_series) {
  
  # Ensure input is numeric
  total_tax <- rep(0, length(income_series))
  remaining_income <- income_series
  
  # Assuming TAX_BRACKETS is a list of lists or a matrix/data frame 
  # with columns: threshold and rate
  # e.g., TAX_BRACKETS <- list(list(48000, 0.33), list(14000, 0.175), list(0, 0.105))
  for (bracket in TAX_BRACKETS) {
    threshold <- bracket[[1]]
    rate <- bracket[[2]]
    
    # Calculate income falling into this specific bracket
    # pmax is the element-wise version of max()
    taxable_at_rate <- pmax(0, remaining_income - threshold)
    total_tax <- total_tax + (taxable_at_rate * rate)
    
    # Update remaining income to the threshold for the next bracket down
    # pmin is the element-wise version of min()
    remaining_income <- pmin(remaining_income, threshold)
  }
  
  return(total_tax)
}
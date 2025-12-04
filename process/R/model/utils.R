
preprocess_data <- function(df, cat_cols, reference_groups = NULL) {
  # """
  # Converts categorical columns to dummy variables and returns the new DataFrame
  # plus a map of original column names to new dummy column names.
  # """
  
  # Copy the dataframe to avoid modifying the original in place (R does this by default on assignment, but explicit is good)
  df_encoded <- df
  new_cols_map <- list()
  
  if (is.null(reference_groups)) {
    reference_groups <- list()
  }
  
  for (col in cat_cols) {
    # --- 1. Generate dummies for this column ---
    # Python: pd_get_dummies(df_encoded[col], prefix=col, dtype=int)
    
    # We get unique sorted levels to match Pandas' alphabetical sorting
    # We use na.omit because pd.get_dummies(dummy_na=False) ignores NAs by default
    vals <- df_encoded[[col]]
    levels <- sort(unique(na.omit(vals)))
    
    # Create a temporary list to build the dummy dataframe
    dummy_list <- list()
    for (lvl in levels) {
      col_name <- paste0(col, "_", lvl)
      # Create binary vector (1/0)
      # Pandas dtype=int results in integers, so we use as.integer
      vec <- as.integer(vals == lvl)
      # Handle NAs: if original is NA, the check is NA. Pandas makes these 0.
      vec[is.na(vec)] <- 0 
      dummy_list[[col_name]] <- vec
    }
    
    # Convert list to DataFrame
    dummies <- as.data.frame(dummy_list)
    
    # --- 2. Determine which category to drop ---
    if (col %in% names(reference_groups)) {
      ref_val <- reference_groups[[col]]
      ref_col_name <- paste0(col, "_", ref_val)
      
      if (ref_col_name %in% colnames(dummies)) {
        # Python: dummies = dummies.drop(columns=[ref_col])
        dummies[[ref_col_name]] <- NULL
        print(sprintf("[%s] Reference group set to: '%s'", col, ref_val))
      } else {
        # Fallback
        print(sprintf("Warning: Reference '%s' not found in '%s'. Dropping first.", ref_val, col))
        # Python: dummies.iloc[:, 1:]
        # drop=FALSE ensures it stays a dataframe even if 1 col remains
        dummies <- dummies[, -1, drop = FALSE] 
      }
    } else {
      # Default behavior: Drop the first column (alphabetical)
      dropped_col_name <- colnames(dummies)[1]
      
      # Python: dropped_col.replace(col+'_', '')
      # We strip the prefix to log the reference value
      ref_val_extracted <- sub(paste0("^", col, "_"), "", dropped_col_name)
      
      dummies <- dummies[, -1, drop = FALSE]
      print(sprintf("[%s] Reference group set to: '%s' (Default)", col, ref_val_extracted))
    }
    
    # --- 3. Store the list of new column names ---
    new_cols_map[[col]] <- colnames(dummies)
    
    # --- 4. Add to main DF and remove original string column ---
    # Python: pd_concat([df_encoded, dummies], axis=1)
    df_encoded <- bind_cols(df_encoded, dummies)
    
    # Python: df_encoded.drop(columns=[col], inplace=True)
    df_encoded[[col]] <- NULL
  }
  
  # Return a list (Tuple equivalent in R for named returns)
  return(list(df_encoded = df_encoded, new_cols_map = new_cols_map))
}
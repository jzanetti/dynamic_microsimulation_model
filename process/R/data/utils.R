library(dplyr)
library(stringr)
library(tidyr)


# Returns a list(bins=..., labels=...) or NULL
parse_bins_if_ranges <- function(labels) {
  parsed_data <- list()
  is_range_based <- FALSE
  
  for (lbl in labels) {
    lbl_str <- as.character(lbl)
    
    # Check for "0-9"
    range_match <- str_match(lbl_str, "^(\\d+)\\s*-\\s*(\\d+)$")
    # Check for "80+"
    plus_match <- str_match(lbl_str, "^(\\d+)\\s*\\+$")
    
    if (!is.na(range_match[1,1])) {
      is_range_based <- TRUE
      low <- as.numeric(range_match[1,2])
      high <- as.numeric(range_match[1,3]) + 1 # Matching Python logic: upper bound exclusive
      parsed_data <- append(parsed_data, list(list(label = lbl, low = low, high = high)))
      
    } else if (!is.na(plus_match[1,1])) {
      is_range_based <- TRUE
      low <- as.numeric(plus_match[1,2])
      high <- Inf # R uses Inf, Python uses np.inf
      parsed_data <- append(parsed_data, list(list(label = lbl, low = low, high = high)))
      
    } else {
      # Standard category like "Asian"
    }
  }
  
  if (!is_range_based || length(parsed_data) == 0) {
    return(NULL)
  }
  
  # Sort by 'low' value
  # Convert list of lists to a dataframe for easier sorting/extraction
  df_bins <- bind_rows(parsed_data) %>% arrange(low)
  
  # Construct bins vector: all 'low' values + the last 'high'
  bins <- c(df_bins$low, tail(df_bins$high, 1))
  labels_sorted <- df_bins$label
  
  return(list(bins = bins, labels = labels_sorted))
}

aggregate_population <- function(df, id_col, column_mappings) {
  
  # Work on a copy (R does this by default/value, but we define a new var for clarity)
  working_df <- df
  # We iterate over the names of the list (equivalent to dict.items())
  for (col_name in names(column_mappings)) {
    labels <- column_mappings[[col_name]]
    
    if (!col_name %in% names(working_df)) {
      stop(sprintf("Column '%s' not found in DataFrame.", col_name))
    }
    
    # 1. Check if these labels imply numerical binning
    auto_res <- parse_bins_if_ranges(labels)
    
    if (!is.null(auto_res)) {
      # CASE A: Numeric Binning (e.g., Age)
      # R's cut(right = FALSE) is equivalent to pandas cut(right = False) -> [a, b)
      working_df[[col_name]] <- cut(
        working_df[[col_name]],
        breaks = auto_res$bins,
        labels = auto_res$labels,
        right = FALSE,
        include.lowest = TRUE # Often needed to catch the exact start value
      )
    } else {
      # CASE B: Strict Categorical (e.g., Ethnicity)
      # Convert to Factor (R's version of Categorical)
      # Values not in 'labels' become NA
      working_df[[col_name]] <- factor(
        working_df[[col_name]],
        levels = labels,
        ordered = TRUE
      )
    }
  }
  
  # --- Aggregation ---
  group_cols <- names(column_mappings)
  result_df <- working_df %>%
    group_by(across(all_of(group_cols)), .drop = FALSE) %>%
    summarise(count = sum(!is.na(.data[[id_col]])), .groups = "drop")

  # --- Renaming Columns ---
  cols_to_rename <- group_cols
  
  result_df <- result_df %>%
    rename_with(
      .fn = ~ paste0(., "_group"),
      .cols = all_of(cols_to_rename)
    )
  
  return(result_df)
}
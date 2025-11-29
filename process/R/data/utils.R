library(dplyr)
library(stringr)
library(tidyr)

# --- Helper: Check if labels look like ranges ---
parse_bins_if_ranges <- function(labels) {
  # Returns list(bins=..., labels=...) if ranges, else list(bins=NULL, labels=NULL)
  
  parsed_data <- list()
  is_range_based <- FALSE
  
  for (label in labels) {
    label_str <- as.character(label)
    
    # Check for "0-9"
    range_match <- str_match(label_str, "^(\\d+)\\s*-\\s*(\\d+)$")
    # Check for "80+"
    plus_match <- str_match(label_str, "^(\\d+)\\s*\\+$")
    
    if (!any(is.na(range_match[,1]))) {
      is_range_based <- TRUE
      low <- as.integer(range_match[,2])
      high <- as.integer(range_match[,3]) + 1
      parsed_data[[length(parsed_data) + 1]] <- list(label = label, low = low, high = high)
      
    } else if (!any(is.na(plus_match[,1]))) {
      is_range_based <- TRUE
      low <- as.integer(plus_match[,2])
      high <- Inf # R uses Inf, numpy uses inf
      parsed_data[[length(parsed_data) + 1]] <- list(label = label, low = low, high = high)
    }
  }
  
  if (!is_range_based || length(parsed_data) == 0) {
    return(list(bins = NULL, labels = NULL))
  }
  
  # Sort based on low value
  # Bind list to dataframe for easier sorting
  parsed_df <- bind_rows(parsed_data) %>% arrange(low)
  
  # Create bins: all 'lows' + the 'high' of the last one
  bins <- c(parsed_df$low, tail(parsed_df$high, 1))
  labels_sorted <- parsed_df$label
  
  return(list(bins = bins, labels = labels_sorted))
}

group_data <- function(df, column_mappings, overwrite_col = FALSE) {
  working_df <- df # R copies by default
  
  for (col_name in names(column_mappings)) {
    labels <- column_mappings[[col_name]]
    
    if (overwrite_col) {
      col_name_to_write <- col_name
    } else {
      col_name_to_write <- paste0(col_name, "_group")
    }
    
    if (!col_name %in% names(working_df)) {
      stop(sprintf("Column '%s' not found in DataFrame.", col_name))
    }
    
    # 1. Check if these labels imply numerical binning
    parse_res <- parse_bins_if_ranges(labels)
    auto_bins <- parse_res$bins
    auto_labels <- parse_res$labels
    
    if (!is.null(auto_bins)) {
      # CASE A: Numeric Binning (e.g., Age)
      # right = FALSE is equivalent to Python's right=False ( [0, 10) )
      working_df[[col_name_to_write]] <- cut(
        working_df[[col_name]], 
        breaks = auto_bins, 
        labels = auto_labels, 
        right = FALSE,
        include.lowest = TRUE # Often needed to catch the absolute start
      )
    } else {
      # CASE B: Strict Categorical (e.g., Ethnicity)
      # factor in R is equivalent to pd.Categorical
      # Values not in 'levels' become NA automatically
      working_df[[col_name_to_write]] <- factor(
        working_df[[col_name]], 
        levels = labels, 
        ordered = TRUE
      )
    }
  }
  
  return(working_df)
}

aggregate_population <- function(df, id_col, column_mappings) {
  # Group data together
  working_df <- group_data(df, column_mappings, overwrite_col = TRUE)
  
  group_cols <- names(column_mappings)
  
  # Aggregation
  result_df <- working_df %>%
    group_by(across(all_of(group_cols)), .drop = FALSE) %>%
    summarise(count = sum(!is.na(.data[[id_col]])), .groups = "drop")
  
  # --- Renaming Columns ---
  cols_to_rename <- group_cols
  result_df <- result_df %>%
    rename_with(
      .fn = ~ paste0(., "_group"),
      .cols = all_of(cols_to_rename))

  return(result_df)
}

assign_groups <- function(df, id_col, column_mappings) {
  # Select columns
  cols_to_keep <- c(id_col, names(column_mappings))
  df_working <- df[, cols_to_keep, drop = FALSE]
  
  return(group_data(df_working, column_mappings))
}

assign_random_status <- function(df, selected_col_name = NULL, options = NULL, condition = NULL) {
  
  # Identify rows to process
  if (!is.null(condition)) {
    # We use which() to get indices, similar to .index in pandas
    selected_indices <- which(df[[selected_col_name]] == condition)
    
    if (length(selected_indices) == 0) {
      return(df)
    }
  } else {
    selected_indices <- 1:nrow(df)
  }
  
  # Get probability
  prob_col_name <- paste0(selected_col_name, "_prob")
  
  # In R, extracting a single value from the first selected row
  prob <- df[selected_indices[1], prob_col_name]
  
  # Calculate counts
  n_total <- length(selected_indices)
  n_selected_a <- as.integer(round(n_total * prob))
  n_selected_b <- n_total - n_selected_a
  
  # Create list of statuses
  options_results <- c(rep(options$a, n_selected_a), rep(options$b, n_selected_b))
  
  # Shuffle
  options_results <- sample(options_results)
  
  # Assign back to specific indices
  df[selected_indices, selected_col_name] <- options_results
  
  return(df)
}
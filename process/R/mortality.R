library(dplyr)
library(purrr) # For map functions if needed, though basic loops work too

run_mortality <- function(pop, id_col_name, cfg) {
  
  # 1. Construct Path
  # In R, we use file.path instead of os.path.join
  # We assume the file is converted to .rds (R's native format)
  mortality_model_path <- file.path(cfg$output_dirs$models, "model_mortality.rds")
  mortality_model <- readRDS(mortality_model_path)
  
  # 2. Extract Predictors
  # Assuming mortality_model$trained_data is a dataframe
  # and predictors is a character vector of column names
  predictors <- names(mortality_model$trained_data[mortality_model$predictors])
  
  # 3. Create Column Mapping
  col_mapping <- list()
  for (proc_key in predictors) {
    # .replace in Python -> gsub in R
    clean_key <- gsub("_group", "", proc_key)
    col_mapping[[clean_key]] <- unique(mortality_model$trained_data[[proc_key]])
  }
  
  # 4. Assign Groups
  pop_working <- data_utils_env$assign_groups(pop, id_col_name, col_mapping)
  
  # 5. First Merge (Left Join)
  # We grab ID and life_stage from original pop
  pop_subset <- pop %>% select(all_of(id_col_name), life_stage)
  pop_working <- left_join(pop_working, pop_subset, by = id_col_name)
  
  # 6. Prediction
  pop_working$life_stage_prob <- predict(
    mortality_model$model, 
    newdata = pop_working, 
    na.action = na.pass)
  
  # 7. Clip Probabilities (0 to 1)
  pop_working$life_stage_prob <- pmax(pmin(pop_working$life_stage_prob, 1), 0)
  # Sometimes life_stage_prob is NA, this is usually caused by the age goes beyond the 
  # a normal range (e.g., age = 105 etc.), in this case, we set the prob to 1.0
  pop_working$life_stage_prob[is.na(pop_working$age_group)] <- 1.0
  
  # 8. Groupby and Apply Random Status
  pop_working <- pop_working %>%
    group_by(age, ethnicity) %>%
    group_modify(~ data_utils_env$assign_random_status(
      .x,
      selected_col_name = "life_stage",
      options = list(a = "dead", b = "alive"),
      condition = "alive"
    )) %>%
    ungroup()
  
  # 9. Drop old column and Merge new results
  # Remove 'life_stage' from original pop
  pop <- pop %>% select(-life_stage)
  
  # Merge the updated life_stage back
  # We select only ID and life_stage from the working df
  cols_to_merge <- pop_working %>% select(all_of(id_col_name), life_stage)
  
  pop <- left_join(pop, cols_to_merge, by = id_col_name)
  
  return(pop)
}
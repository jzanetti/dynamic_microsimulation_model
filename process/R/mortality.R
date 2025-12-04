library(dplyr)
library(purrr) # For map functions if needed, though basic loops work too

forward <- function(pop, cfg, id_col_name = "id") {
  pop <- run_mortality(pop, id_col_name = id_col_name, cfg = cfg)
  return (pop)
}


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
  pop_working <- pop_working %>%
    left_join(pop[, c(id_col_name, "life_stage")], by = id_col_name)
  
  pop_working_selected <- pop_working %>%
    filter(life_stage == "alive")
  
  # 5. First Merge (Left Join)
  # We grab ID and life_stage from original pop
  # pop_subset <- pop %>% select(all_of(id_col_name), life_stage)
  # pop_working_selected <- left_join(pop_working_selected, pop_subset, by = id_col_name)
  
  # 6. Prediction
  pop_working_selected$life_stage_prob <- predict(
    mortality_model$model, 
    newdata = pop_working_selected, 
    na.action = na.pass)
  
  # 7. Clip Probabilities (0 to 1)
  pop_working_selected$life_stage_prob <- pmax(pmin(pop_working_selected$life_stage_prob, 1), 0)
  # Sometimes life_stage_prob is NA, this is usually caused by the age goes beyond the 
  # a normal range (e.g., age = 105 etc.), in this case, we set the prob to 1.0
  pop_working_selected$life_stage_prob[is.na(pop_working_selected$age_group)] <- 1.0
  
  # 8. Groupby and Apply Random Status
  pop_working_selected <- pop_working_selected %>%
    group_by(age, ethnicity) %>%
    group_modify(~ data_utils_env$assign_random_status(
      .x,
      selected_col_name = "life_stage",
      options = list(a = "dead", b = "alive"),
      condition = "alive"
    )) %>%
    ungroup()
  
  pop <- pop %>%
    left_join(pop_working_selected[
      , c(id_col_name, "life_stage")], 
      by = id_col_name, suffix = c("_old", ""))
  
  pop <- pop %>%
    mutate(life_stage_old = coalesce(life_stage, life_stage_old)) %>%
    select(-life_stage) %>%
    rename(life_stage = life_stage_old)
  
  return(pop)
}
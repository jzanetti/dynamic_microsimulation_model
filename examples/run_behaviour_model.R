source("process/R/deps.R")
source("process/R/data/utils.R", local = data_utils_env)
source("process/R/data/input.R", local = data_input_env)
source("process/R/data/output.R", local = data_output_env)
source("process/R/data/filename.R", local = data_filename_env)
source("process/R/data/sample.R", local = data_sample_env)
source("process/R/data/tawa.R", local = data_tawa_env)
source("process/R/model/wrapper.R", local = model_wrapper_env)
source("process/R/model/linear.R", local = model_linear_env)
source("process/R/model/heckman_wage.R", local = model_heckman_wage_env)
source("process/R/vis.R", local = vis_env)
source("process/R/dmm.R", local = dmm_env)
source("process/R/person.R", local = person_env)
source("process/R/mortality.R", local = mortality_env)
source("process/R/employment.R", local = employment_env)
source("process/R/model/utils.R", local = model_utils_env)
source("process/R/model/random_utility_function.R", local = model_ruf_env)
source("process/R/model/validation.R", local = model_validation_env)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
run_model       <- TRUE
run_calib       <- TRUE
run_validation  <- TRUE
run_sensitivity <- TRUE
run_predict     <- TRUE

output_dir <- "etc/app/runs"

# Input Parameters (Python Dict -> R Named List)
input_params <- list(
  total_hours       = 80.0,
  min_hourly_wage   = 23.0,
  leisure_value     = 23.0,
  exclude_seniors   = TRUE,
  hours_options     = c(0, 10, 20, 30, 40),
  
  # Python: None -> R: NULL
  apply_people_income_filter = NULL, 
  # apply_people_income_filter = list(min = 0.1, max = 0.5),
  
  apply_household_income_filter = list(min = 0.7, max = 0.9),
  apply_household_size_filter   = NULL,
  apply_earner_type_filter      = NULL
  
  # Example of nested list for commented out section:
  # apply_household_size_filter = list(
  #   H_Counts_Adults = c(1, 3),
  #   H_Counts_DependentKids = c(0, 3)
  # )
)

# ------------------------------------------------------------------------------
# Data Loading
# ------------------------------------------------------------------------------
# Using fread for performance (equivalent to read_csv but faster)
tawa_data <- list(
  input = fread("etc/app/Synthetic-HES23-single-period.csv"),
  sq    = fread("etc/app/TY25_BEFU24_SQ.csv.gz"),
  sijin = fread("etc/app/TY25_BEFU24_sijin.csv.gz"),
  test2 = fread("etc/app/TY25_BEFU24_test2.csv.gz")
)

# ------------------------------------------------------------------------------
# Execution jobs (model, calibration, validation, sensitivity study, predict)
# ------------------------------------------------------------------------------
if (run_model) {
  data_tawa_env$tawa_data_preprocess(
    tawa_data = tawa_data, 
    input_params = input_params, 
    tawa_data_name = "sq", 
    output_dir = output_dir
  )
  
  model_ruf_env$utility_func(
    params = input_params, # Note: function signature might expect 'params' or 'input_paramshjh'
    tawa_data_name = "sq",
    hours_options = input_params$hours_options,
    output_dir = output_dir
  )
}

if (run_calib) {
  model_ruf_env$run_ruf_calibrate(
    input_params = input_params, 
    tawa_data_name = "sq", 
    output_dir = output_dir
  )
}

if (run_validation) {
  model_validation_env$run_ruf_validation(
    input_params = input_params, 
    tawa_data_name = "sq", 
    output_dir = output_dir
  )
  
  # Assuming plot_intermediate function exists in your R environment
  vis_env$plot_intermediate(
    input_params,
    "ruf_validation",
    tawa_data_name = "sq",
    output_dir = output_dir
  )
}

if (run_sensitivity) {
  model_validation_env$run_ruf_sensitivity(
    input_params = input_params, 
    tawa_data_name = "sq", 
    output_dir = output_dir
  )
  
  vis_env$plot_intermediate(
    input_params,
    "ruf_sensitivity",
    tawa_data_name = "sq",
    output_dir = output_dir
  )
}

if (run_predict) {
  data_tawa_env$run_tawa_predict(
    tawa_data = tawa_data,
    output_dir = output_dir,
    input_params = input_params,
    updated_tawa_data_path = "etc/app/Synthetic-HES23-single-period-updated2.csv"
  )
}

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


run_model = FALSE
run_calib = FALSE
run_validation = TRUE
run_sensitivity = TRUE
output_dir = "etc/app/runs"

tawa_wrapper <- function(run_model = TRUE, 
                         run_calib = TRUE,
                         tawa_data = list(
                           input = read.csv("etc/app/Synthetic-HES23-single-period.csv"),
                           output = read.csv("C:/Work/Github/AIMapp/apps/TAWAapp/TY25_BEFU24_SQ.csv.gz")
                         ),
                         input_params = list(
                           "total_hours" = 80.0,
                           "min_hourly_wage" = 23.0,
                           "leisure_value" = 23.0,
                           "exclude_seniors" = TRUE,
                           "hours_options" = c(0, 10, 20, 30, 40),
                           "apply_household_income_filter" = list("min" = 0.1, "max" = 0.7),
                           "apply_earner_type_filter" = NULL, 
                           "apply_household_size_filter" = NULL
                         )) {
  
  if (run_model) {
    data = data_tawa_env$tawa_data_preprocess(
      tawa_data,
      min_hourly_wage=input_params[["min_hourly_wage"]],
      hours_options=input_params[["hours_options"]],
      exclude_seniors=input_params[["exclude_seniors"]],
      apply_household_income_filter = input_params[["apply_household_income_filter"]],
      apply_earner_type_filter=input_params[["apply_earner_type_filter"]],
      apply_household_size_filter = input_params[["apply_household_size_filter"]])
    
    model_ruf_env$utility_func(
      data,
      input_params,
      income_name="income_per_hour",
      working_hours_name="working_hours",
      output_dir = output_dir,
      recreate_data = TRUE
    )
  }
  
  if (run_calib) {
      model_ruf_env$run_ruf_calibrate(input_params, output_dir=output_dir)
  }
}

tawa_wrapper(run_model = TRUE, run_calib = TRUE)

if (run_validation) {
  model_validation_env$run_ruf_validation(input_params, output_dir=output_dir)
}

if (run_sensitivity) {
  model_validation_env$run_ruf_sensitivity(input_params, output_dir=output_dir)
  vis_env$plot_intermediate(
    input_params,
    "utility_func", 
    output_dir = output_dir)
}


source("process/R/deps.R")
source("process/R/data/sample.R", local = data_sample_env)
source("process/R/data/utils.R", local = data_utils_env)
source("process/R/data/input.R", local = data_input_env)
source("process/R/model/wrapper.R", local = model_wrapper_env)
source("process/R/model/linear.R", local = model_linear_env)
source("process/R/vis.R", local = vis_env)
source("process/R/dmm.R", local = dmm_env)
source("process/R/person.R", local = person_env)
source("process/R/mortality.R", local = mortality_env)

# ---------------------------
# Load configuration file
# ---------------------------
cfg <- read_yaml("examples/cfg.yml")

# ---------------------------
# Create a sample population data
# ---------------------------
# data_sample_env$generate_sample_population()

# ---------------------------
# Create input data for DMM
# ---------------------------
sample_pop <- data_input_env$create_inputs(
  "etc/sample", 
  required_data_types = c("pop", "mortality"), 
  data_type = "parquet", 
  base_year=cfg[["base_year"]])

# ---------------------------
# Plot input population data
# ---------------------------
vis_env$plot_inputs(
  sample_pop[["pop"]], 
  exclude_col=c("id", "household_id", "base_year") , 
  output_dir = cfg[["output_dirs"]][["figures"]])

# ---------------------------
# Create necessary models
# ---------------------------
for (proc_model_name in c("mortality")) {
  model_wrapper_env$run_rate_model(
    sample_pop,
    proc_model_name,
    cfg[["models"]][[proc_model_name]],
    cfg[["output_dirs"]][["models"]]
  )
}

# ---------------------------
# Run DMM processing
# ---------------------------
dmm_env$run_dmm(sample_pop, cfg, start_year=2025, years=5)

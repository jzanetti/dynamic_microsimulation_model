source("process/R/deps.R")
source("process/R/data.R", local = data_env)
source("process/R/vis.R", local = vis_env)

# ---------------------------
# Load configuration file
# ---------------------------
cfg <- read_yaml("cfg.yml")

# ---------------------------
# Create a sample population data
# ---------------------------
sample_pop <- data_env$generate_sample_population()
vis_env$plot_inputs(
  sample_pop, 
  exclude_col="id", 
  output_dir = cfg[["output_dirs"]][["figures"]])
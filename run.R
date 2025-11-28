source("process/R/deps.R")
source("process/R/data/sample.R", local = sample_env)
source("process/R/vis.R", local = vis_env)

# ---------------------------
# Load configuration file
# ---------------------------
cfg <- read_yaml("cfg.yml")

# ---------------------------
# Create a sample population data
# ---------------------------
sample_pop <- sample_env$generate_sample_population()
vis_env$plot_inputs(
  sample_pop[["population"]], 
  exclude_col=c("id", "household_id") , 
  output_dir = cfg[["output_dirs"]][["figures"]])
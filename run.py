from process.Python.data import generate_sample_population
from process.Python.vis import plot_inputs
from yaml import safe_load as yaml_safe_load


# ---------------------------
# Load configuration file
# ---------------------------
cfg = yaml_safe_load(open("cfg.yml"))

# ---------------------------
# Create a sample population data
# ---------------------------
sample_pop = generate_sample_population()
plot_inputs(sample_pop, exclude_col="id", output_dir=cfg["output_dirs"]["figures"])
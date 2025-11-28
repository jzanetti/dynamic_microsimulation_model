from process.Python.data.sample import generate_sample_population
from process.Python.vis import plot_inputs
from yaml import safe_load as yaml_safe_load
# from process.Python.dmm import run_dmm


# ---------------------------
# Load configuration file
# ---------------------------
cfg = yaml_safe_load(open("cfg.yml"))

# ---------------------------
# Create a sample population data
# ---------------------------
sample_pop = generate_sample_population()
plot_inputs(sample_pop["population"], exclude_cols=["id", "household_id"], output_dir=cfg["output_dirs"]["figures"])


# ---------------------------
# Run DMM processing
# ---------------------------
# run_dmm(sample_pop, start_year=2020, years=5)
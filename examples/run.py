from process.Python.data.sample import generate_sample_population
from process.Python.data.input import create_inputs
from process.Python.vis import plot_inputs
from yaml import safe_load as yaml_safe_load
from process.Python.model.wrapper import run_rate_model
from process.Python.dmm import run_dmm


# ---------------------------
# Load configuration file
# ---------------------------
cfg = yaml_safe_load(open("examples/cfg.yml"))

# ---------------------------
# Create a sample population data
# ---------------------------
generate_sample_population()

# ---------------------------
# Create input data for DMM
# ---------------------------
sample_pop = create_inputs(
    "etc/sample/",
    required_data_types=["pop", "mortality"],
    data_type="parquet",
    base_year=cfg["base_year"],
)

# ---------------------------
# Plot input population data
# ---------------------------
plot_inputs(
    sample_pop["pop"],
    exclude_cols=["id", "household_id", "base_year"],
    output_dir=cfg["output_dirs"]["figures"],
)

# ---------------------------
# Create necessary models
# ---------------------------
for proc_model_name in ["mortality"]:
    run_rate_model(
        sample_pop,
        proc_model_name,
        cfg=cfg["models"][proc_model_name],
        output_dir=cfg["output_dirs"]["models"],
    )

# ---------------------------
# Run DMM processing
# ---------------------------
run_dmm(sample_pop, cfg, start_year=2025, years=5)

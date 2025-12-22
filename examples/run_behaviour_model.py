from process.Python.model.random_utlity_function import utility_func, run_ruf_calibrate
from pandas import read_csv
from process.Python.model.validation import run_ruf_validation, run_ruf_sensitivity
from pandas import read_csv

from process.Python.data.tawa import tawa_data_preprocess
from process.Python.vis import plot_intermediate


run_model = True
run_calib = True
run_validation = True
run_sensitivity = True
output_dir = "etc/app/runs"
input_params = {
    "total_hours": 80.0,
    "min_hourly_wage": 23.0,
    "leisure_value": 23.0,
    "exclude_seniors": True,
    "hours_options": [0, 10, 20, 30, 40],
    "apply_household_income_filter": {"min": 0.1, "max": 0.7},
    # "apply_earner_type_filter": None, # "primary", # primary/others 
    "apply_household_size_filter": None,
    "apply_earner_type_filter": None
    #"apply_household_size_filter": {
    #    "H_Counts_Adults": [1, 1],
    #    "H_Counts_DependentKids": [1, 1]}
}


if run_model:
    tawa_data = {
        "input": read_csv("etc/app/Synthetic-HES23-single-period.csv"),
        "output": read_csv("C:/Work/Github/AIMapp/apps/TAWAapp/TY25_BEFU24_SQ.csv.gz")
    }

    data = tawa_data_preprocess(
        tawa_data,
        min_hourly_wage=input_params["min_hourly_wage"],
        hours_options=input_params["hours_options"],
        exclude_seniors=input_params["exclude_seniors"],
        apply_household_income_filter = input_params["apply_household_income_filter"],
        apply_earner_type_filter=input_params["apply_earner_type_filter"],
        apply_household_size_filter = input_params["apply_household_size_filter"])

    utility_func(
        data,
        input_params,
        income_name="income_per_hour",
        working_hours_name="working_hours",
        output_dir = output_dir,
        recreate_data = True
    )

if run_calib:
    run_ruf_calibrate(input_params, output_dir=output_dir)

if run_validation:
    run_ruf_validation(input_params, output_dir=output_dir)

if run_sensitivity:
    run_ruf_sensitivity(input_params, output_dir=output_dir)
    plot_intermediate(
        input_params,
        "utility_func", 
        output_dir = output_dir)


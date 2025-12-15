from process.Python.model.random_utlity_function import utility_func
from pandas import read_csv
from process.Python.model.validation import run_ruf_validation, run_ruf_sensitivity
from pandas import read_csv

from process.Python.data.tawa import tawa_data_preprocess
from process.Python.vis import plot_intermediate


run_model = False
run_validation = False
run_sensitivity = True

input_params = {
    "total_hours": 80.0,
    "min_hourly_wage": 23.0,
    "leisure_value": 23.0,
    "exclude_seniors": False,
    "hours_options": [0, 10, 20, 30, 40],
    "apply_household_income_filter": {"min": 0.1, "max": 0.7},
    "apply_earner_type_filter": None, 
    "apply_household_size_filter": None
    # "apply_earner_type_filter": "primary",
    #"apply_household_size_filter": {
    #    "H_Counts_Adults": [2, 2],
    #    "H_Counts_DependentKids": [0, 1]}
}


if run_model:
    hes_data = read_csv("etc/app/Synthetic-HES23-single-period.csv")

    data = tawa_data_preprocess(
        hes_data,
        min_hourly_wage=input_params["min_hourly_wage"],
        hours_options=input_params["hours_options"],
        exclude_seniors=input_params["exclude_seniors"],
        apply_household_income_filter = input_params["apply_household_income_filter"],
        apply_earner_type_filter=input_params["apply_earner_type_filter"],
        apply_household_size_filter = input_params["apply_household_size_filter"])

    utility_func(
        data,
        input_params,
        income_name={"market": "market_income_per_hour"},
        working_hours_name="working_hours",
        output_dir = "etc/app",
        recreate_data = True
    )

if run_validation:
    run_ruf_validation(input_params, output_dir="etc/app")

if run_sensitivity:
    run_ruf_sensitivity(input_params, output_dir="etc/app/")
    plot_intermediate(
        input_params,
        "utility_func", 
        output_dir = "etc/app/")


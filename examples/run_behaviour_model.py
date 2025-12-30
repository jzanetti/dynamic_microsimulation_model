from process.Python.model.random_utlity_function import utility_func, run_ruf_calibrate
from pandas import read_csv
from process.Python.model.validation import run_ruf_validation, run_ruf_sensitivity
from pandas import read_csv

from process.Python.data.tawa import run_tawa_predict, tawa_data_preprocess
from process.Python.vis import plot_intermediate


run_model = False
run_calib = False
run_validation = False
run_sensitivity = False
run_predict = True
output_dir = "etc/app/runs"
input_params = {
    "total_hours": 80.0,
    "min_hourly_wage": 23.0,
    "leisure_value": 23.0,
    "exclude_seniors": True,
    "hours_options": [0, 10, 20, 30, 40],
    # "apply_people_income_filter": {"min": 0.1, "max": 0.5},
    "apply_people_income_filter": None,
    "apply_household_income_filter": {"min": 0.7, "max": 0.9},
    "apply_household_size_filter": None,
    "apply_earner_type_filter": None,
    
    #"apply_household_size_filter": {
    #    "H_Counts_Adults": [1, 3],
    #    "H_Counts_DependentKids": [0, 3]}
}

tawa_data = {
    "input": read_csv("etc/app/Synthetic-HES23-single-period.csv"),
    "sq": read_csv("etc/app/TY25_BEFU24_SQ.csv.gz"),
    "sq2": read_csv("etc/app/TY25_BEFU24_SQ2.csv.gz"),
    "sijin": read_csv("etc/app/TY25_BEFU24_sijin.csv.gz"),
    "test2": read_csv("etc/app/TY25_BEFU24_test2.csv.gz")
}

if run_model:
    tawa_data_preprocess(
        tawa_data, input_params, tawa_data_name = "sq", output_dir = output_dir)
    utility_func(
        input_params,
        tawa_data_name = "sq", 
        hours_options = input_params["hours_options"],
        output_dir = output_dir
    )

if run_calib:
    run_ruf_calibrate(input_params, tawa_data_name = "sq", output_dir=output_dir)

if run_validation:
    run_ruf_validation(input_params, tawa_data_name = "sq", output_dir=output_dir)
    plot_intermediate(
        input_params,
        "ruf_validation",
        tawa_data_name = "sq",
        output_dir = output_dir)
    
if run_sensitivity:
    run_ruf_sensitivity(input_params, tawa_data_name = "sq", output_dir=output_dir)
    plot_intermediate(
        input_params,
        "ruf_sensitivity",
        tawa_data_name = "sq",
        output_dir = output_dir) 


if run_predict:
    run_tawa_predict(
        tawa_data, 
        output_dir, 
        input_params,
        "test2",
        updated_tawa_data_path = "etc/app/Synthetic-HES23-single-period-updated.csv")

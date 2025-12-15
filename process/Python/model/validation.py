
from pyarrow.parquet import read_table as pq_read_table
from pandas import read_csv
from pandas import DataFrame
from numpy import arange, array
from numpy import where, isclose, mean
from process.Python.model.random_utlity_function import predict as ruf_predict
from process.Python.vis import plot_intermediate
from logging import getLogger
from process.Python.data.filename import create_hash_filename

logger = getLogger()


def run_ruf_sensitivity(
    input_params: dict, 
    income_scaler: list = arange(0.5, 2.0, 0.1), 
    output_dir: str = "",
    plot_using_ratio: bool = True
):

    filename_hash = create_hash_filename(input_params)
    data_output_path = f"{output_dir}/utility_func_data_{filename_hash}.parquet"
    model_output_path = f'{output_dir}/utility_func_parameters_{filename_hash}.csv'
    accuracy_output_path = f'{output_dir}/validation_score_{filename_hash}.csv'
    results_path = f"{output_dir}/sensitivity_tests_{filename_hash}.csv"

    logger.info(f"Model estimated paramaters are read from {model_output_path}")
    model_params = read_csv(model_output_path)
    model_params = model_params.set_index('parameter')['Value'].to_dict()

    logger.info(f"Model training data are read from {data_output_path}")
    data_to_check = pq_read_table(data_output_path)
    data_to_check = data_to_check.to_pandas()

    logger.info(f"Model accuracy is read from {accuracy_output_path}")
    model_accuracy = read_csv(accuracy_output_path)
    model_accuracy = model_accuracy.set_index('scores')['value'].to_dict()

    results = {
        "full_time": [],
        "part_time": [],
        "total_employment_hrs": [],
        "scaler": income_scaler
    }

    for scaler in income_scaler:

        logger.info(f"Processing sensitivity study for scaler: {scaler}")
        predicted_choices = ruf_predict(data_to_check, model_params, method = "top30", scaler = scaler)

        full_time_employment_rate = round(
            len(predicted_choices[predicted_choices["option_hours"] >= input_params["hours_options"][-1]])
            / len(predicted_choices)
            * 100,
            2,
        )
        part_time_employment_rate = round(
            len(
                predicted_choices[
                    (predicted_choices["option_hours"] >= input_params["hours_options"][1]) & (
                       predicted_choices["option_hours"] < input_params["hours_options"][-1] 
                    )
                ]
            )
            / len(predicted_choices)
            * 100,
            2,
        )

        results["full_time"].append(full_time_employment_rate)
        results["part_time"].append(part_time_employment_rate)
        #results["total_employment_hrs"].append(
        #    sum(predicted_choices["option_hours"] * predicted_choices["is_chosen"])
        #)

        #predicted_choices = data_to_check.loc[
        #    data_to_check.groupby("people_id")["utlity"].idxmax()
        #]

        results["total_employment_hrs"].append(
            sum(predicted_choices.groupby("people_id")["option_hours"].mean())
        )

    if plot_using_ratio:
        index = where(isclose(results["scaler"], 1.0, atol=1.0/1e5))[0][0]
        for proc_key in ["full_time", "part_time", "total_employment_hrs"]:
            results[proc_key] = array(results[proc_key])
            results[proc_key] = results[proc_key] / results[proc_key][index]

    logger.info(f"Writing sensitivity study results to {results_path}")
    DataFrame(results).to_csv(results_path)
    




def run_ruf_validation(input_params: dict, output_dir: str, method = "top30"):

    filename_hash = create_hash_filename(input_params)

    data = pq_read_table(f"{output_dir}/utility_func_data_{filename_hash}.parquet")
    data = data.to_pandas()
    params = read_csv(f"{output_dir}/utility_func_parameters_{filename_hash}.csv")
    params = params.set_index('parameter')['Value'].to_dict()

    predicted_choices = ruf_predict(data, params, method=method)

    pred_n = len(predicted_choices[predicted_choices["is_chosen"] == 1])
    truth_n = len(data[data["is_chosen"] == 1])
    truth_hrs = data[data["is_chosen"] == 1]["option_hours"].sum()

    if method == "top30":
        pred_hrs = sum(data.groupby("people_id")["option_hours"].mean())
    else:
        pred_hrs = predicted_choices[predicted_choices["is_chosen"] == 1]["option_hours"].sum()

    scores = DataFrame(
        {
            "scores": ["highest_utility_accuracy", "total_hrs_accuracy"],
            "value": [100.0 * pred_n / truth_n, 100.0 * pred_hrs / truth_hrs]
        }
    )

    scores.to_csv(f"{output_dir}/validation_score_{filename_hash}.csv")
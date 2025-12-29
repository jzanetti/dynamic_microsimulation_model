
from pyarrow.parquet import read_table as pq_read_table
from pandas import read_csv
from pandas import DataFrame
from numpy import arange, array, histogram
from numpy import where, isclose
from process.Python.model.random_utlity_function import predict as ruf_predict
from logging import getLogger
from process.Python.data.filename import create_hash_filename
from process.Python import RUF_METHOD
from process.Python.model.random_utlity_function import negative_log_likelihood
from numpy import log as np_log

logger = getLogger()


def run_ruf_sensitivity(
    input_params: dict, 
    income_scaler: list = arange(0.5, 2.0, 0.1), 
    output_dir: str = "",
    tawa_data_name: str = "sq",
    plot_using_ratio: bool = True,
    method = RUF_METHOD
):

    filename_hash = create_hash_filename(input_params, filename_suffix=tawa_data_name)
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
        "total_employment_hrs": [],
        "scaler": income_scaler
    }

    for scaler in income_scaler:
        logger.info(f"Processing sensitivity study for scaler: {scaler}")
        predicted_choices = ruf_predict(data_to_check, model_params, method = method, scaler=scaler)

        results["total_employment_hrs"].append(
            sum(predicted_choices.groupby("people_id")["hours"].mean())
        )

    if plot_using_ratio:
        index = where(isclose(results["scaler"], 1.0, atol=1.0/1e5))[0][0]
        for proc_key in ["total_employment_hrs"]:
            results[proc_key] = array(results[proc_key])
            results[proc_key] = results[proc_key] / results[proc_key][index]

    logger.info(f"Writing sensitivity study results to {results_path}")
    DataFrame(results).to_csv(results_path)
    

def run_ruf_validation(input_params: dict, tawa_data_name: str, output_dir: str, method=RUF_METHOD):

    filename_hash = create_hash_filename(input_params, filename_suffix=tawa_data_name)

    data = pq_read_table(f"{output_dir}/utility_func_data_{filename_hash}.parquet")
    data = data.to_pandas()
    params = read_csv(f"{output_dir}/utility_func_parameters_{filename_hash}.csv")
    params = params.set_index('parameter')['Value'].to_dict()


    # 1. Calculate r2_mcfadden
    options_n = len(data["option_hours"].unique())
    params_list = []
    for proc_key in ["beta_income_hhld", 
                     "beta_income_hhld2", 
                     "beta_leisure", 
                     "beta_leisure2", 
                     "beta_interaction"]:
        params_list.append(params[proc_key])
    
    ll_model = -negative_log_likelihood(params_list, data, options_n)
    n_people = len(data) // options_n
    ll_null = n_people * np_log(1 / options_n)
    r2_mcfadden = 1 - (ll_model / ll_null)

    # 2. Calculate utility accuracy
    # data = data[(data["people_id"] == 9) & (data["option_hours_id"].isin([1175, 1176]))]
    df_clean = data.drop(columns=["calibrated_err"])
    predicted_choices = ruf_predict(df_clean, params, method=method)

    pred_n = len(predicted_choices[predicted_choices["is_chosen"] == 1])
    truth_n = len(data[data["is_chosen"] == 1])

    # 3. Calculate emplpyment hours accuracy
    truth_hrs = data[data["is_chosen"] == 1]["option_hours"].sum()
    if method == "top30":
        pred_hrs = sum(data.groupby("people_id")["option_hours"].mean())
    else:
        pred_hrs = predicted_choices[predicted_choices["is_chosen"] == 1]["hours"].sum()

    # 4. Show calibration distribution
    # data["cal_err_ratio"] = data["calibrated_err"] / data["utility"]
    counts, bin_edges = histogram(data["calibrated_err"], bins=50)
    # counts, bin_edges = histogram(data["cal_err_ratio"], bins=50)
    err_dist = DataFrame({
        'bin_start': bin_edges[:-1],
        'bin_end': bin_edges[1:],
        'count': counts
    })
    output_path = f"{output_dir}/validation_err_{filename_hash}.csv"
    logger.info(f"Validation (distribution) is written to {output_path}")
    err_dist.to_csv(output_path, index=False)

    scores = DataFrame(
        {
            "scores": ["highest_utility_accuracy", "total_hrs_accuracy", "r2_mcfadden"],
            "value": [100.0 * pred_n / truth_n, 100.0 * pred_hrs / truth_hrs, r2_mcfadden]
        }
    )

    output_path = f"{output_dir}/validation_score_{filename_hash}.csv"

    logger.info(f"Validation (score) is written to {output_path}")
    scores.to_csv(output_path, index=False)
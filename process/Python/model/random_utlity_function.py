from pandas import DataFrame
from pandas import Series as pd_series
from numpy import exp as np_exp
from numpy import sum as np_sum
from numpy import exp as np_exp
from numpy import log as np_log
from numpy.random import uniform as np_uniform
from scipy.optimize import minimize as scipy_minimize
from logging import getLogger
from process.Python.data.input import prepare_ruf_inputs
from pyarrow.parquet import read_table as pq_read_table
from os.path import exists
from process.Python import RUN_LOG
from process.Python.data.filename import create_hash_filename
from pandas import read_csv
from pyarrow.parquet import write_table as pq_write_table
import pyarrow as pa
logger = getLogger()


def cal_utility(data: DataFrame, params: dict, income_scaler: float = 1.0) -> DataFrame:
    scaled_leisure = data["leisure"]
    data["utility"] = quadratic_utility(
        (
            params["beta_income_hhld"], 
            params["beta_income_hhld2"], 
            params["beta_leisure"], 
            params["beta_leisure2"], 
            params["beta_interaction"]
        ), 
        data["income"] * income_scaler, 
        data["income_hhld"] * income_scaler, 
        scaled_leisure
    )

    if "calibrated_err" in data:
        data["utility_calibrated"] = data["utility"] + data["calibrated_err"]

    return data


def run_ruf_calibrate(input_params: dict, output_dir: str):

    def _obtain_err(group):
        # Identify the observed choice (where is_chosen == 1)
        k_idx = group[group['is_chosen'] == 1].index[0]
        V_k = group.loc[k_idx, 'utility']
        
        # Draw error for the chosen alternative (Standard Gumbel)
        u_k = np_uniform(0, 1)
        eps_k = -np_log(-np_log(u_k))
        
        epsilons = pd_series(index=group.index, dtype=float)
        epsilons.loc[k_idx] = eps_k
        
        # Draw errors for unchosen alternatives (Truncated Gumbel)
        # Ensures V_k + eps_k > V_j + eps_j
        for j_idx in group.index:
            if j_idx == k_idx:
                continue
            V_j = group.loc[j_idx, 'utility']
            u_j = np_uniform(0, 1)
            
            # Upper bound for the unchosen error
            threshold = eps_k + V_k - V_j
            eps_j = -np_log(np_exp(-threshold) - np_log(u_j))
            epsilons.loc[j_idx] = eps_j
            
        return epsilons
    
    logger.info("Calibrating the RUF function ...")
    filename_hash = create_hash_filename(input_params)

    data = pq_read_table(f"{output_dir}/utility_func_data_{filename_hash}.parquet")
    data = data.to_pandas()
    params = read_csv(f"{output_dir}/utility_func_parameters_{filename_hash}.csv")
    params = params.set_index('parameter')['Value'].to_dict()

    data = cal_utility(data, params)
    data["calibrated_err"] = data.groupby("people_id", group_keys=False).apply(_obtain_err)
    data = data.drop(columns=['utility'])
    # data['utlity_calibrated'] = data['utlity'] + data['calibrated_err']
    # Check if the chosen option now has the maximum utility
    # verification = data.groupby('people_id').apply(
    #   lambda x: x.loc[x['is_chosen']==1, 'utlity_calibrated'].values[0] == x['utlity_calibrated'].max())
    # logger.info(f"Calibration successful for all people: {verification.all()}")

    pq_write_table(pa.Table.from_pandas(data), f"{output_dir}/utility_func_data_{filename_hash}.parquet")
    


def predict(data: DataFrame, 
            params: dict, 
            method: str = "top30", 
            scaler: float = 1.0):

    data = cal_utility(data, params, income_scaler=scaler)

    utility_name = "utility"
    if "utility_calibrated" in data:
        utility_name = "utility_calibrated"

    if method == "top30": # the threshold is from top 30%
        utility_thresholds = data.groupby(
            "people_id")[utility_name].transform(lambda x: x.quantile(1 - 0.3))
        predictions = data.loc[
            data[utility_name] >= utility_thresholds]
    else:
        predictions = data.loc[
            data.groupby("people_id")[utility_name].idxmax()]

    return predictions



def negative_log_likelihood(
    params,
    df,
    options_n
):
    # 1. Calculate Utility for all rows
    V = quadratic_utility(
        params, 
        df["income"].values, 
        df["income_hhld"].values, 
        df["leisure"].values
    )

    # 2. Reshape to (Num_People, 3_Options)
    n_people = len(df) // options_n
    V_matrix = V.reshape((n_people, options_n))

    # 3. Calculate Probabilities (Softmax)
    # Subtract max for stability
    max_V = V_matrix.max(axis=1, keepdims=True)
    exp_V = np_exp(V_matrix - max_V)
    sum_exp_V = exp_V.sum(axis=1, keepdims=True)
    probs = exp_V / sum_exp_V

    # 4. Get probability of the ACTUAL choice
    # We use the 'is_chosen' mask
    choice_mask = df["is_chosen"].values.reshape((n_people, options_n))
    chosen_probs = probs[choice_mask == 1]

    return -np_sum(np_log(chosen_probs + 1e-10))


def quadratic_utility(params, income, income_hhld, leisure, show_debug = False, apply_log: bool = RUN_LOG):
    """
    Utility Function: U = b_c*C + b_c2*C^2 + b_l*L + b_l2*L^2 + b_cl*(C*L)
    """
    b_hhld_i, b_hhld_i2, b_l, b_l2, b_cl = params

    if apply_log:
        income_hhld_to_use = np_log(income_hhld)
        income_to_use = np_log(income)
        leisure_to_use = np_log(leisure)
    else:
        income_hhld_to_use = income_hhld
        income_to_use = income
        leisure_to_use = leisure
    
    util = (
        b_hhld_i * income_hhld_to_use
        + b_hhld_i2 * (income_hhld_to_use**2)
        + b_l * leisure_to_use
        + b_l2 * (leisure_to_use**2)
        + b_cl * (income_to_use * leisure_to_use)
    )

    if show_debug:
        debug_info = f"Total util {sum(util)}"
        print(debug_info)

    return util


def utility_func(
    df_input: DataFrame,
    params: dict,
    output_dir: str = "",
    income_name: str = "income_per_hour",
    working_hours_name: str = "working_hours",
    params_dict: dict = {
        "beta_income_hhld": {"initial": 0.1, "bound": (1e-6, 15.0)},
        "beta_income_hhld2": {"initial": -0.01, "bound": (-15.0, -1e-6)},
        "beta_leisure": {"initial": 0.1, "bound": (1e-6, 15.0)},
        "beta_leisure2": {"initial": -0.01, "bound": (-15.0, -1e-6)},
        "beta_interaction": {"initial": 0.1, "bound": (-15.0, 15.0)}
    },
    recreate_data: bool = True
):

    hours_options = params["hours_options"]
    total_hours = params["total_hours"]
    leisure_value = params["leisure_value"]

    filename_hash = create_hash_filename(params)

    data_output_path = f"{output_dir}/utility_func_data_{filename_hash}.parquet"
    model_output_path = f'{output_dir}/utility_func_parameters_{filename_hash}.csv'

    if recreate_data or (not exists(data_output_path)):
        prepare_ruf_inputs(
            df_input,
            hours_options,
            total_hours,
            leisure_value,
            income_name,
            working_hours_name,
            data_output_path=data_output_path
        )

    proc_data = pq_read_table(data_output_path)
    proc_data = proc_data.to_pandas()

    initial_guess = [v["initial"] for v in params_dict.values()]
    bounds = [v["bound"] for v in params_dict.values()]

    result = scipy_minimize(
        fun=negative_log_likelihood,
        x0=initial_guess,
        args=(
            proc_data,
            len(hours_options)
        ),
        method="L-BFGS-B",
        bounds=bounds,
        options={'iprint': 1, 'eps': 1e-8} # Add this
    )

    result_params = {}
    for i, proc_param_var in enumerate(params_dict):
        result_params[proc_param_var] = result.x[i]

    # Write outputs
    results_params_df = df = DataFrame.from_dict(
        result_params, orient='index', columns=['Value'])
    results_params_df.index.name = 'parameter'

    logger.info(f"The model estimated paramaters are written to {model_output_path}")
    df.to_csv(model_output_path)

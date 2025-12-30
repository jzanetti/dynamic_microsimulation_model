from pandas import DataFrame, merge
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
from process.Python import RUN_LOG, RUF_METHOD
from process.Python.data.filename import create_hash_filename
from pandas import read_csv
from pyarrow.parquet import write_table as pq_write_table
import pyarrow as pa
logger = getLogger()


def cal_utility(data: DataFrame, params: dict, income_scaler: float = 1.0) -> DataFrame:

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
        data["leisure"]
    )

    if "calibrated_err" in data:
        data["utility_calibrated"] = data["utility"] + data["calibrated_err"]

    return data


def run_ruf_calibrate(input_params: dict, tawa_data_name: str, output_dir: str):

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

            # for numerical stability, when the threshold is too big, the RUF model gives too much error
            if not V_k + eps_k > V_j + eps_j:
                eps_j = -99999.0

            epsilons.loc[j_idx] = eps_j
            
        return epsilons
    
    logger.info("Calibrating the RUF function ...")

    filename_hash = create_hash_filename(input_params, filename_suffix=tawa_data_name)
    ruf_data_path = f"{output_dir}/utility_func_data_{filename_hash}.parquet"
    ruf_params_path = f"{output_dir}/utility_func_parameters_{filename_hash}.csv"

    data = pq_read_table(ruf_data_path)
    data = data.to_pandas()
    params = read_csv(ruf_params_path)

    data = cal_utility(data, params.set_index("parameter")["Value"].to_dict())
    data["calibrated_err"] = data.groupby("people_id", group_keys=False).apply(_obtain_err)
    data = data.drop(columns=["utility"])

    pq_write_table(pa.Table.from_pandas(data), ruf_data_path)
    


def predict(data: DataFrame, 
            params: dict, 
            method: str = RUF_METHOD, 
            scaler: float = 1.0,
            use_hhld: bool = False):

    data = cal_utility(data, params, income_scaler=scaler)

    utility_name = "utility"
    if "utility_calibrated" in data:
        utility_name = "utility_calibrated"

    if method == "top30": # the threshold is from top 30%
        utility_thresholds = data.groupby(
            "people_id")[utility_name].transform(lambda x: x.quantile(1 - 0.3))
        predictions = data.loc[
            data[utility_name] >= utility_thresholds]
        predictions["hours"] = predictions.groupby(["household_id", "people_id"])["option_hours"].transform("mean")
        # predictions = predictions[["household_id", "people_id", "hours"]].drop_duplicates()
    else:
        if use_hhld:
            hhld_total_utility = data.groupby([
                "household_id", "option_hours_id"], as_index=False)[utility_name].sum()
            hhld_best_option = hhld_total_utility.loc[hhld_total_utility.groupby('household_id')[utility_name].idxmax()]
            predictions = merge(
                data, 
                hhld_best_option[['household_id', 'option_hours_id']], 
                on=['household_id', 'option_hours_id'], 
                how='inner'
            )
        else:
            predictions = data.loc[
                data.groupby(["people_id"])[utility_name].idxmax()]

        predictions = predictions.rename(columns={
                "option_hours": "hours"
        })

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
    params: dict,
    tawa_data_name: str = "sq",
    output_dir: str = "",
    hours_options: list = [0, 20, 40],
    params_dict: dict = {
        "beta_income_hhld": {"initial": 0.1, "bound": (1e-6, None)},
        "beta_income_hhld2": {"initial": -0.01, "bound": (None, -1e-6)},
        "beta_leisure": {"initial": 0.1, "bound": (1e-6, None)},
        "beta_leisure2": {"initial": -0.01, "bound": (None, -1e-6)},
        "beta_interaction": {"initial": 0.1, "bound": (None, None)}
    },
):

    filename_hash = create_hash_filename(params, filename_suffix=tawa_data_name)

    data_output_path = f"{output_dir}/utility_func_data_{filename_hash}.parquet"
    model_output_path = f'{output_dir}/utility_func_parameters_{filename_hash}.csv'

    proc_data = pq_read_table(data_output_path)
    proc_data = proc_data.to_pandas()

    initial_guess = [v["initial"] for v in params_dict.values()]
    bounds = [v["bound"] for v in params_dict.values()]

    print("Start running optimization ...")
    proc_data = proc_data.sort_values(by=[
        "household_id", "people_id", "option_hours"]).reset_index(drop=True)

    result = scipy_minimize(
        fun=negative_log_likelihood,
        x0=initial_guess,
        args=(
            proc_data[["income", "income_hhld", "leisure", "is_chosen"]],
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


"""
# proc_data2 = pq_read_table("test2.parquet")
proc_data2 = pq_read_table("etc/app/runs/utility_func_data_aee9ef9b4875e727f4357144600222d2_sq.parquet")
proc_data2 = proc_data2.to_pandas()
sort_cols = ['household_id', 'people_id', 'option_hours', 'option_hours_id']

# 2. Sort and reset index for BOTH dataframes
df2 = proc_data2.sort_values(by=sort_cols).reset_index(drop=True)
df1 = proc_data.sort_values(by=sort_cols).reset_index(drop=True)

df2["option_hours_id"] = df2["option_hours_id"].astype(int)

import pandas as pd
pd.testing.assert_frame_equal(
    df2.astype(object), 
    df1.astype(object), 
    check_dtype=False
)
negative_log_likelihood(initial_guess, proc_data, len(hours_options))
proc_data2  = proc_data2.sort_values(by=[
    "household_id", "people_id", "option_hours"]).reset_index(drop=True)
"""
#proc_data2 = pq_read_table("etc/app/runs/utility_func_data_aee9ef9b4875e727f4357144600222d2_sq.parquet")
#proc_data2 = proc_data2.to_pandas()
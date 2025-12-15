from pandas import DataFrame
from numpy import exp as np_exp
from numpy import sum as np_sum
from numpy import exp as np_exp
from numpy import log as np_log
from scipy.optimize import minimize as scipy_minimize
from logging import getLogger
from process.Python.data.input import prepare_ruf_inputs
from pyarrow.parquet import read_table as pq_read_table


from process.Python.data.filename import create_hash_filename

logger = getLogger()


def predict(data: DataFrame, params: dict, method: str = "top30", scaler: float = 1.0):
    scaled_income_hhld = data["income_hhld"] * scaler
    scaled_income = data["income"] * scaler
    scaled_leisure = data["leisure"]

    data["utlity"] = (
        params["beta_income_hhld"] * scaled_income_hhld
        + params["beta_income_hhld2"] * (scaled_income_hhld**2)
        + params["beta_leisure"] * scaled_leisure
        + params["beta_leisure2"] * (scaled_leisure**2)
        + params["beta_interaction"] * (scaled_income * scaled_leisure)
    )
    if method == "top30": # the threshold is from top 30%
        utility_thresholds = data.groupby(
            "people_id")["utlity"].transform(lambda x: x.quantile(1 - 0.3))
        predictions = data.loc[
            data["utlity"] >= utility_thresholds]
    else:
        predictions = data.loc[
            data.groupby("people_id")["utlity"].idxmax()]

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

    # 5. Sum Log Likelihoods
    return -np_sum(np_log(chosen_probs + 1e-10))


def quadratic_utility(params, income, income_hhld, leisure, show_debug = False):
    """
    Utility Function: U = b_c*C + b_c2*C^2 + b_l*L + b_l2*L^2 + b_cl*(C*L)
    """
    b_hhld_i, b_hhld_i2, b_l, b_l2, b_cl = params

    util = (
        b_hhld_i * income_hhld
        + b_hhld_i2 * (income_hhld**2)
        + b_l * leisure
        + b_l2 * (leisure**2)
        + b_cl * (income * leisure)
    )

    if show_debug:
        debug_info = f"Total util {sum(util)}"
        print(debug_info)

    return util


def utility_func(
    df_input: DataFrame,
    params: dict,
    output_dir: str = "",
    income_name: dict or None = {"market_income": "test"},
    working_hours_name: str = "working_hours",
    params_dict: dict = {
        "beta_income_hhld": {"initial": 1.0, "bound": (1e-6, None)},
        "beta_income_hhld2": {"initial": -0.01, "bound": (None, -1e-6)},
        "beta_leisure": {"initial": 1.0, "bound": (1e-6, None)},
        "beta_leisure2": {"initial": -0.01, "bound": (None, -1e-6)},
        "beta_interaction": {"initial": 1.0, "bound": (None, None)}
    },
    recreate_data: bool = True
):

    hours_options = params["hours_options"]
    total_hours = params["total_hours"]
    leisure_value = params["leisure_value"]

    filename_hash = create_hash_filename(params)

    data_output_path = f"{output_dir}/utility_func_data_{filename_hash}.parquet"
    model_output_path = f'{output_dir}/utility_func_parameters_{filename_hash}.csv'

    if recreate_data:
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

    # logger.info(f"The model training data are written to {data_output_path}")
    # pq_write_table(pa.Table.from_pandas(proc_data), data_output_path)

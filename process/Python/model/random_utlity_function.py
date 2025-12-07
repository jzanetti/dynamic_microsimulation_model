from pandas import DataFrame
import statsmodels.api as sm
import statsmodels.api as sm
from copy import deepcopy
from numpy import array as np_array
from numpy import max as np_max
from numpy import exp as np_exp
from numpy import sum as np_sum
from numpy import abs as np_abs
from numpy import exp as np_exp
from numpy import log as np_log
from scipy.optimize import minimize as scipy_minimize
from logging import getLogger
from numpy import arange

from process.Python.vis import plot_intermediate

logger = getLogger()


def results_validation(data_to_check: DataFrame, params: dict):

    results = {
        "full_time": [],
        "part_time": [],
        "total_employment_hrs": [],
        "scaler": arange(0.3, 3.0, 0.1),
    }

    for scaler in results["scaler"]:
        scaled_income = data_to_check["income"] * scaler
        scaled_leisure = data_to_check["leisure"]
        # Create employment:
        data_to_check["utlity"] = (
            params["beta_income"] * scaled_income
            + params["beta_income2"] * (scaled_income**2)
            + params["beta_leisure"] * scaled_leisure
            + params["beta_leisure2"] * (scaled_leisure**2)
            + params["beta_interaction"] * (scaled_income * scaled_leisure)
        )
        predicted_choices = data_to_check.loc[
            data_to_check.groupby("people_id")["utlity"].idxmax()
        ]

        full_time_employment_rate = round(
            len(predicted_choices[predicted_choices["leisure"] < 0.6])
            / len(predicted_choices)
            * 100,
            2,
        )
        part_time_employment_rate = round(
            len(
                predicted_choices[
                    (predicted_choices["leisure"] > 0.6)
                    & (predicted_choices["leisure"] < 0.9)
                ]
            )
            / len(predicted_choices)
            * 100,
            2,
        )
        logger.info(
            f"Predicted employment rate (full-time) {scaler}: {full_time_employment_rate} %"
        )

        logger.info(
            f"Predicted employment rate (part-time) {scaler}: {part_time_employment_rate} %"
        )

        results["full_time"].append(full_time_employment_rate)
        results["part_time"].append(part_time_employment_rate)
        results["total_employment_hrs"].append(
            sum(predicted_choices["option_hours"] * predicted_choices["is_chosen"])
        )

    plot_intermediate(results, "utility_func")


def negative_log_likelihood(params, df):
    # 1. Calculate Utility for all rows
    V = quadratic_utility(params, df["income"].values, df["leisure"].values)

    # 2. Reshape to (Num_People, 3_Options)
    n_people = len(df) // 3
    V_matrix = V.reshape((n_people, 3))

    # 3. Calculate Probabilities (Softmax)
    # Subtract max for stability
    max_V = V_matrix.max(axis=1, keepdims=True)
    exp_V = np_exp(V_matrix - max_V)
    sum_exp_V = exp_V.sum(axis=1, keepdims=True)
    probs = exp_V / sum_exp_V

    # 4. Get probability of the ACTUAL choice
    # We use the 'is_chosen' mask
    choice_mask = df["is_chosen"].values.reshape((n_people, 3))
    chosen_probs = probs[choice_mask == 1]

    # 5. Sum Log Likelihoods
    return -np_sum(np_log(chosen_probs + 1e-10))


def quadratic_utility(params, income, leisure):
    """
    Utility Function: U = b_c*C + b_c2*C^2 + b_l*L + b_l2*L^2 + b_cl*(C*L)
    """
    b_i, b_i2, b_l, b_l2, b_cl = params

    # Scale variables for numerical stability (optional but recommended)
    # income = income / 100.0
    # leisure = leisure / 80.0

    util = (
        b_i * income
        + b_i2 * (income**2)
        + b_l * leisure
        + b_l2 * (leisure**2)
        + b_cl * (income * leisure)
    )
    return util


def prepare_inputs(df_input: DataFrame, hours_options: list, total_hours: int):

    def _map_to_closest_choice(actual_hours, options):
        """
        e.g., Maps 27.5 -> 20, 42.0 -> 40, etc. based on minimal distance.
        """
        actual_hours = float(actual_hours)
        # Find the option that minimizes abs(actual - option)
        best_option = min(options, key=lambda x: abs(x - actual_hours))
        return best_option

    long_data = []

    for index, row in df_input.iterrows():
        person_wage = row["latent_market_income_per_week"]
        actual_hours = row["working_hours"]

        # Identify which option this person actually picked (Observed Choice)
        observed_choice = _map_to_closest_choice(actual_hours, hours_options)

        # Generate the 3 scenarios for this person
        for option_hours in hours_options:

            # Scenario 1: Calculate Leisure
            simulated_leisure = total_hours - option_hours

            # Scenario 2: Calculate Income
            # Note: If they don't work (0 hours), income is 0 (or benefits - simplified here)
            # We also subtract working_costs if they work (assuming costs are incurred for any work)
            if option_hours > 0:
                gross_income = person_wage * option_hours
                # Subtracting the working_costs observed in data (or 0 if not provided)
                # Simplification: We assume the cost in the data applies if they work
                # net_income = gross_income - row["working_costs"]

                # Simple tax rule (approx 20% tax for simulation)
                # net_income = net_income * 0.8
                net_income = gross_income
            else:
                net_income = 0  # Or add welfare benefit logic here

            # Ensure consumption isn't negative for log/utility calc
            simulated_income = max(net_income, 1.0)

            # Record this scenario
            long_data.append(
                {
                    "people_id": row["id"],
                    "option_hours": option_hours,
                    "is_chosen": 1 if option_hours == observed_choice else 0,
                    "income": simulated_income,
                    "leisure": simulated_leisure,
                }
            )

    return DataFrame(long_data)


def utility_func(
    df_input: DataFrame,
    hours_options: list,
    total_hours: float,
    income_name: str = "latent_market_income_per_week",
    working_hours_name: str = "working_hours",
    params_dict: dict = {
        "beta_income": {"initial": 1.0, "bound": (1e-6, None)},
        "beta_income2": {"initial": -0.1, "bound": (None, -1e-6)},
        "beta_leisure": {"initial": 1.0, "bound": (1e-6, None)},
        "beta_leisure2": {"initial": -1.0, "bound": (None, -1e-6)},
        "beta_interaction": {"initial": 0.01, "bound": (None, None)},
    },
):

    # select relevant features
    df_input = deepcopy(
        df_input[["id", "household_id", income_name, working_hours_name, "employed"]]
    )

    df_input["leisure_hours"] = total_hours - df_input[working_hours_name]

    # employment_rate_raw = len(df_input[df_input["employed"] == 1]) / len(df_input)

    proc_data = prepare_inputs(df_input, hours_options, total_hours)

    proc_data["income"] = proc_data["income"] / 100.0
    proc_data["leisure"] = proc_data["leisure"] / 80.0

    initial_guess = [v["initial"] for v in params_dict.values()]
    bounds = [v["bound"] for v in params_dict.values()]

    result = scipy_minimize(
        fun=negative_log_likelihood,
        x0=initial_guess,
        args=(proc_data,),
        method="L-BFGS-B",
        bounds=bounds,
    )

    result_params = {}
    for i, proc_param_var in enumerate(params_dict):
        result_params[proc_param_var] = result.x[i]
        logger.info(f"{proc_param_var}: round({result_params[proc_param_var]}, 3)")

    # Check predictions
    logger.info("--- Model Check ---")
    logger.info(f"Success: {result.success}")
    logger.info(f"Message: {result.message}")
    validation = results_validation(proc_data, result_params)

    return result_params, validation

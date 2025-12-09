from pandas import DataFrame
import statsmodels.api as sm  # Kept as is, though not used in the provided code
from copy import deepcopy
from numpy import array as np_array
from numpy import max as np_max
from numpy import exp as np_exp
from numpy import sum as np_sum
from numpy import abs as np_abs
from numpy import log as np_log
from numpy import where as np_where
from logging import getLogger
from numpy import arange, mean
from process.Python.vis import plot_intermediate
import torch
import torch.optim as optim
import numpy as np
import pandas as pd

logger = getLogger()


def obtain_accuracy(data_to_check: DataFrame, params: dict):
    scaled_income_hhld = data_to_check["income_hhld"]
    scaled_income = data_to_check["income"]
    scaled_leisure = data_to_check["leisure"]

    data_to_check["utlity"] = (
        params["beta_income_hhld"] * scaled_income_hhld
        + params["beta_income_hhld2"] * (scaled_income_hhld**2)
        + params["beta_leisure"] * scaled_leisure
        + params["beta_leisure2"] * (scaled_leisure**2)
        + params["beta_interaction"] * (scaled_income * scaled_leisure)
        + params["beta_interaction2"] * (scaled_income / scaled_income_hhld)
    )
    predicted_choices = data_to_check.loc[
        data_to_check.groupby("people_id")["utlity"].idxmax()
    ]

    pred_n = len(predicted_choices[predicted_choices["is_chosen"] == 1])
    truth_n = len(data_to_check[data_to_check["is_chosen"] == 1])

    accuracy_ratio = pred_n / truth_n

    return accuracy_ratio


def results_validation(
    data_to_check: DataFrame, params: dict, total_hours: int, hours_options: list
):
    results = {
        "full_time": [],
        "part_time": [],
        "total_employment_hrs": [],
        "scaler": arange(0.1, 5.0, 0.01),
    }
    leisure_time = [total_hours - x for x in hours_options]
    for scaler in results["scaler"]:
        scaled_income_hhld = data_to_check["income_hhld"] * scaler
        scaled_income = data_to_check["income"] * scaler
        scaled_leisure = data_to_check["leisure"]
        # Create employment:
        data_to_check["utlity"] = (
            params["beta_income_hhld"] * scaled_income_hhld
            + params["beta_income_hhld2"] * (scaled_income_hhld**2)
            + params["beta_leisure"] * scaled_leisure
            + params["beta_leisure2"] * (scaled_leisure**2)
            + params["beta_interaction"] * (scaled_income * scaled_leisure)
            + params["beta_interaction2"] * (scaled_income / scaled_income_hhld)
        )
        predicted_choices = data_to_check.loc[
            data_to_check.groupby("people_id")["utlity"].idxmax()
        ]
        full_time_employment_rate = round(
            len(predicted_choices[predicted_choices["leisure"] == min(leisure_time)])
            / len(predicted_choices)
            * 100,
            2,
        )
        part_time_employment_rate = round(
            len(
                predicted_choices[
                    (predicted_choices["leisure"] >= mean(leisure_time) - 1.0)
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


def quadratic_utility(params, income, income_hhld, leisure):
    """
    Utility Function: U = b_c*C + b_c2*C^2 + b_l*L + b_l2*L^2 + b_cl*(C*L)
    """
    b_hhld_i, b_hhld_i2, b_l, b_l2, b_cl, b_hi = params
    util = (
        b_hhld_i * income_hhld
        + b_hhld_i2 * (income_hhld**2)
        + b_l * leisure
        + b_l2 * (leisure**2)
        + b_cl * (income * leisure)
        + b_hi * (income / income_hhld)
    )
    return util


def prepare_inputs(
    df_input: DataFrame, hours_options: list, total_hours: int, income_name: str
):
    import itertools

    all_household_ids = df_input["household_id"].unique()
    long_data = []
    for index1, proc_hhld_id in enumerate(all_household_ids):

        print(index1 / len(all_household_ids))

        proc_hhld = df_input[df_input["household_id"] == proc_hhld_id]

        num_people = len(proc_hhld)
        all_possible_hours_combination = list(
            itertools.product(hours_options, repeat=num_people)
        )

        chosen_combination = min(
            all_possible_hours_combination,
            key=lambda c: sum(
                (a - b) ** 2 for a, b in zip(c, list(proc_hhld["working_hours"].values))
            ),
        )

        for proc_combination in all_possible_hours_combination:

            is_chosen = 0
            if proc_combination == chosen_combination:
                is_chosen = 1

            proc_data_list = []
            proc_hhld_income = 0
            for index2, proc_hours in enumerate(proc_combination):
                proc_person = proc_hhld.iloc[index2]
                person_wage = proc_person[income_name]
                simulated_leisure = total_hours - proc_hours
                if proc_hours > 0:
                    gross_income_person = person_wage * proc_hours
                else:
                    gross_income_person = 0

                proc_hhld_income += gross_income_person

                proc_data_list.append(
                    {
                        "household_id": int(proc_hhld["household_id"].values[0]),
                        "people_id": int(proc_person["id"]),
                        "option_hours": proc_hours,
                        "is_chosen": is_chosen,
                        "income": gross_income_person,
                        "leisure": simulated_leisure,
                    }
                )

            for proc_data in proc_data_list:
                if proc_hhld_income == 0:
                    proc_hhld_income = 1e-9
                proc_data["income_hhld"] = proc_hhld_income

            long_data.extend(proc_data_list)

    return DataFrame(long_data)


def negative_log_likelihood_pytorch(
    params,
    income_tensor,  # person income
    income_hhld_tensor,  # hhld income
    leisure_tensor,
    is_chosen_tensor,
    options_n,
    hours_values_tensor,
    target_total_hours,
    penalty_weight=0.5,
    run_calibrate=True,
):
    # 1. Calculate Utility for all rows
    V = quadratic_utility(params, income_tensor, income_hhld_tensor, leisure_tensor)

    # 2. Reshape to (n_people, options_n)
    n_people = income_tensor.shape[0] // options_n
    V_matrix = V.view(n_people, options_n)

    # 3. Calculate Probabilities (Softmax with stability)
    max_V = V_matrix.max(dim=1, keepdim=True)[0]
    exp_V = torch.exp(V_matrix - max_V)
    sum_exp_V = exp_V.sum(dim=1, keepdim=True)
    probs = exp_V / sum_exp_V

    # 4. Get probability of the ACTUAL choice
    choice_mask = is_chosen_tensor.view(n_people, options_n).bool()
    chosen_probs = probs[choice_mask]

    # 5. Negative Log Likelihood
    nll = -torch.sum(torch.log(chosen_probs + 1e-10))

    if run_calibrate:
        # Calibration: Total Expected Hours
        expected_hours_person = torch.sum(probs * hours_values_tensor, dim=1)
        total_expected_hours = torch.sum(expected_hours_person)
        calibration_penalty = (
            penalty_weight * (total_expected_hours - target_total_hours) ** 2
        )
        return nll + calibration_penalty
    else:
        return nll


def optimize_with_pytorch(
    proc_data: pd.DataFrame,
    initial_guess: np.ndarray,
    bounds: list,
    options_n: int,
    hours_options: list,
    calibration_target_hours: float,
    penalty_weight: float = 0.5,
    run_calibrate: bool = True,
    learning_rate: float = 0.01,
    max_epochs: int = 1000,
    device: str = "cpu",  # or 'cuda' if GPU available
):
    # Convert data to PyTorch tensors
    income_tensor = torch.tensor(
        proc_data["income"].values, dtype=torch.float32, device=device
    )  # person
    income_hhld_tensor = torch.tensor(
        proc_data["income_hhld"].values, dtype=torch.float32, device=device
    )  # hhld
    leisure_tensor = torch.tensor(
        proc_data["leisure"].values, dtype=torch.float32, device=device
    )
    is_chosen_tensor = torch.tensor(
        proc_data["is_chosen"].values, dtype=torch.float32, device=device
    )
    hours_values_tensor = torch.tensor(
        hours_options, dtype=torch.float32, device=device
    )
    target_total_hours_tensor = torch.tensor(
        calibration_target_hours, dtype=torch.float32, device=device
    )

    # Parameters to optimize
    params = torch.tensor(
        initial_guess, dtype=torch.float32, device=device, requires_grad=True
    )

    # Optimizer (Adam is good for adaptive learning rates and handling scaling issues)
    optimizer = optim.Adam([params], lr=learning_rate)

    # Bounds as tensors for clamping
    lower_bounds = torch.tensor(
        [b[0] if b[0] is not None else float("-inf") for b in bounds],
        dtype=torch.float32,
        device=device,
    )
    upper_bounds = torch.tensor(
        [b[1] if b[1] is not None else float("inf") for b in bounds],
        dtype=torch.float32,
        device=device,
    )

    # Training loop
    for epoch in range(max_epochs):
        optimizer.zero_grad()
        loss = negative_log_likelihood_pytorch(
            params,
            income_tensor,
            income_hhld_tensor,
            leisure_tensor,
            is_chosen_tensor,
            options_n,
            hours_values_tensor,
            target_total_hours_tensor,
            penalty_weight,
            run_calibrate,
        )
        loss.backward()  # Autograd computes gradients
        optimizer.step()

        # Clamp parameters to bounds after step
        with torch.no_grad():
            params.clamp_(lower_bounds, upper_bounds)

        if epoch % 100 == 0:
            print(f"Epoch {epoch}: Loss = {loss.item()}")

    # Return optimized parameters
    return params.detach().cpu().numpy()


def utility_func(
    df_input: DataFrame,
    hours_options: list,
    total_hours: float,
    income_names: dict = {
        "hhld": "household_disposable_income_per_week",
        "person": "disposable_income_per_week",
    },
    working_hours_name: str = "working_hours",
    params_dict: dict = {
        "beta_income_hhld": {"initial": 1.0, "bound": (1e-6, None)},
        "beta_income_hhld2": {"initial": -0.1, "bound": (None, -1e-6)},
        "beta_leisure": {"initial": 1.0, "bound": (1e-6, None)},
        "beta_leisure2": {"initial": -0.1, "bound": (None, -1e-6)},
        "beta_interaction": {"initial": 0.1, "bound": (None, None)},
        "beta_interaction2": {"initial": 0.1, "bound": (None, None)},
    },
    penalty_weight=1.0,
):
    # select relevant features
    df_input = deepcopy(
        df_input[
            [
                "id",
                "household_id",
                # income_names["hhld"],
                income_names["person"],
                working_hours_name,
            ]
        ]
    )
    df_input["leisure_hours"] = total_hours - df_input[working_hours_name]
    proc_data = prepare_inputs(
        df_input,
        hours_options,
        total_hours,
        income_names["person"],
    )

    # proc_data = proc_data.merge(
    #    df_input[["household_id", income_names["hhld"]]].drop_duplicates(),
    #    on="household_id",
    # )
    # proc_data = proc_data.rename(columns={income_names["hhld"]: "income_hhld"})
    # scaler = (
    #    proc_data[proc_data["is_chosen"] == 1]["income"].median()
    #    / proc_data[proc_data["is_chosen"] == 1]["leisure"].median()
    # )
    # scaler = scaler * 1.0
    # proc_data["leisure"] = proc_data["leisure"] * scaler

    # 2. Fix Zero Income for Household (Prevents division by zero)
    # Replace 0 with a small number (e.g. 1 cent)
    proc_data.loc[proc_data["income_hhld"] <= 0, "income_hhld"] = 0.001

    # 3. Fix Negative Incomes (Log and Square roots hate them, though you use Squares)
    # It is safer to clamp them to 0 or a small positive for economic modelling
    proc_data.loc[proc_data["income"] < 0, "income"] = 0
    proc_data.loc[proc_data["income_hhld"] < 0, "income_hhld"] = 0.001

    income_scaler = 1000.0
    proc_data["income"] = proc_data["income"] / income_scaler
    proc_data["income_hhld"] = proc_data["income_hhld"] / income_scaler

    # 2. Scale Leisure to "Hundreds of Hours"
    # e.g., 40 hours becomes 0.4
    leisure_scaler = 100.0
    proc_data["leisure"] = proc_data["leisure"] / leisure_scaler

    # proc_data["leisure"] = proc_data["leisure"] * (
    #    proc_data["income"].median()
    #    * proc_data["option_hours"].median()
    #    / proc_data["leisure"].median()
    # )

    logger.info("Data scaled: Income / 1000, Leisure / 100")

    # auto_scaler_person = proc_data["income"].mean() / proc_data["leisure"].mean()
    # proc_data["income"] = proc_data["income"] / auto_scaler_person
    # proc_data["income_hhld"] = proc_data["income_hhld"] / auto_scaler_person
    initial_guess = [v["initial"] for v in params_dict.values()]
    bounds = [v["bound"] for v in params_dict.values()]
    calibration_target_hours = df_input[working_hours_name].sum()
    optimized_params = optimize_with_pytorch(
        proc_data=proc_data,
        initial_guess=initial_guess,
        bounds=bounds,
        options_n=len(hours_options),
        hours_options=hours_options,
        calibration_target_hours=calibration_target_hours,
        penalty_weight=penalty_weight,
        run_calibrate=False,
        learning_rate=0.001,  # Adjustable
        max_epochs=15000,  # Adjustable
        device="cuda" if torch.cuda.is_available() else "cpu",
    )
    result_params = {}
    for i, proc_param_var in enumerate(params_dict):
        result_params[proc_param_var] = optimized_params[i]
        logger.info(f"{proc_param_var}: round({result_params[proc_param_var]}, 3)")

    # Check predictions
    logger.info("--- Model Check ---")
    # No direct 'success' or 'message' in PyTorch; you can add convergence check if needed
    logger.info("Optimization completed.")
    results_validation(proc_data, result_params, total_hours, hours_options)
    accuacry = obtain_accuracy(proc_data, result_params)

    print(accuacry)

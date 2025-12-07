from pandas import DataFrame
from process.Python.model.wrapper import run_heckman_wage_model
from process.Python.model.wrapper import run_utility_func_model_fit
import statsmodels.api as sm
from pandas import concat
from logging import getLogger
from os.path import join
from pickle import load as pickle_load
from itertools import product as iter_prod
from numpy.random import choice as np_choice
from process.Python.data import EPLISON

logger = getLogger()


def forward(
    pop_data: DataFrame,
    cfg: dict,
    weeks_per_year: int = 50,
    utility_age_groups: list = ["18-65"],
    run_utility_func: bool = False,
):

    # pop_data["employed"] = (pop_data["market_income"] > 0).astype(float)
    # pop_data["latent_working_hours"] = pop_data["working_hours"]
    # pop_data.loc[pop_data["employed"] == False, "working_hours"] = EPLISON
    pop_data["market_income_per_week"] = pop_data["market_income"] / (
        pop_data["working_hours"] * weeks_per_year
    )
    possible_working_hrs = []
    for proc_hr in cfg["behaviour_model"]["employment"]["possible_working_hrs"]:
        if proc_hr == 0.0:
            proc_hr = EPLISON
        possible_working_hrs.append(proc_hr)

    # ------------------------
    # Run Heckman model and Utility function
    # ------------------------
    for proc_age_group in utility_age_groups:

        proc_age_group_range = proc_age_group.split("-")
        proc_data = pop_data[
            (pop_data["age"] >= int(proc_age_group_range[0]))
            & (pop_data["age"] < int(proc_age_group_range[1]))
        ]

        pop_data = run_heckman_model(proc_data, cfg, proc_age_group)

        if run_utility_func:
            run_utility(
                pop_data,
                proc_age_group,
                cfg["output_dirs"]["models"],
                hours_options=possible_working_hrs,
                total_hours=cfg["behaviour_model"]["employment"]["total_hrs"],
            )

    return pop_data


def run_utility(
    pop_data: DataFrame,
    utility_age_group: str,
    output_dir: str,
    hours_options: list,
    total_hours: float = 80.0,
):

    proc_ages = utility_age_group.split("-")

    run_utility_func_model_fit(
        pop_data, output_dir, proc_ages, hours_options, total_hours
    )

    return pop_data


def run_heckman_model(pop_data: DataFrame, cfg: dict, proc_age_group: str) -> DataFrame:

    run_heckman_wage_model(pop_data, cfg, heckman_age_group=proc_age_group)
    updated_results = run_heckman_wage_model_prediction(
        cfg["output_dirs"]["models"], proc_age_group
    )

    if "latent_market_income_per_week" in pop_data:
        pop_data = pop_data.drop(
            columns=["latent_market_income_per_week"], errors="ignore"
        )

    pop_data = pop_data.merge(
        updated_results[["id", "latent_market_income_per_week"]], on="id", how="left"
    )

    return pop_data


def run_heckman_wage_model_prediction(data_dir, proc_age_group):

    model_outputpath = join(data_dir, f"model_heckman_wage_{proc_age_group}.pkl")
    models = pickle_load(open(model_outputpath, "rb"))
    all_data = models["data"]
    X_outcome_all = all_data[models["outcome_exog"] + ["imr"]]
    X_outcome_all = sm.add_constant(X_outcome_all, has_constant="add")
    all_data["latent_market_income_per_week"] = models["outcome"].predict(X_outcome_all)

    return all_data

from pandas import DataFrame
from process.Python.model.wrapper import run_heckman_wage_model
import statsmodels.api as sm
from pandas import concat
from logging import getLogger
from os.path import join
from pickle import load as pickle_load
from itertools import product as iter_prod
from numpy.random import choice as np_choice
from process.Python.data import EPLISON
from process.Python.model.random_utlity_function import utility_func
from process.Python.model.validation import run_ruf_validation
from copy import deepcopy

logger = getLogger()


def forward(
    pop_data: DataFrame,
    cfg: dict
):
    pop_data = run_heckman_model(pop_data, cfg)
    pop_data = run_ruf(pop_data, cfg)
    return pop_data


def run_ruf(pop_data: DataFrame, cfg: dict):
    return pop_data


def run_heckman_model(pop_data: DataFrame, cfg: dict) -> DataFrame:

    updated_results = run_heckman_wage_model_prediction(
        pop_data, cfg["output_dirs"]["models"]
    )

    income_keys = ["market_income_per_week", "latent_market_income_per_week"]

    for proc_key in income_keys:
        if proc_key in pop_data:
            pop_data = pop_data.drop(
                columns=[proc_key], errors="ignore"
            )

    pop_data = pop_data.merge(
        updated_results[["id"] + income_keys], on="id", how="left"
    )

    return pop_data


def run_heckman_wage_model_prediction(pop_data, data_dir):

    model_outputpath = join(data_dir, f"model_heckman_wage.pkl")
    models = pickle_load(open(model_outputpath, "rb"))
    all_data = models["data"]
    X_outcome_all = all_data[models["outcome_exog"] + ["imr"]]
    X_outcome_all = sm.add_constant(X_outcome_all, has_constant="add")
    all_data["latent_market_income_per_week"] = models["outcome"].predict(X_outcome_all)

    return all_data

from pandas import DataFrame
from process.Python.model.wrapper import run_heckman_wage_model
import statsmodels.api as sm
from pandas import concat
from logging import getLogger
from os.path import join
from pickle import load as pickle_load

logger = getLogger()


def forward(pop_data: DataFrame, cfg: dict):

    pop_data["employed"] = (pop_data["market_income"] > 0).astype(float)

    return run_heckman_model(pop_data, cfg)


def run_heckman_model(
    pop_data: DataFrame,
    cfg: dict,
    heckman_age_groups: list = ["0-18", "18-65", "65-999"],
) -> DataFrame:

    run_heckman_wage_model(pop_data, cfg, heckman_age_groups=heckman_age_groups)
    pop_data = run_heckman_wage_model_prediction(pop_data, cfg, heckman_age_groups)

    return pop_data


def run_heckman_wage_model_prediction(pop_data, cfg, heckman_age_groups):

    all_data = []
    for proc_ages_str in heckman_age_groups:
        model_outputpath = join(
            cfg["output_dirs"]["models"], f"model_heckman_wage_{proc_ages_str}.pkl"
        )
        models = pickle_load(open(model_outputpath, "rb"))
        proc_data = models["data"]
        X_outcome_all = proc_data[models["outcome_exog"] + ["imr"]]
        X_outcome_all = sm.add_constant(X_outcome_all, has_constant="add")
        proc_data["latent_market_income"] = models["outcome"].predict(X_outcome_all)
        all_data.append(proc_data)

    all_data = concat(all_data)

    if "latent_market_income" in pop_data:
        pop_data = pop_data.drop(columns=["latent_market_income"], errors="ignore")
    pop_data = pop_data.merge(
        all_data[["id", "latent_market_income"]], on="id", how="left"
    )

    return pop_data

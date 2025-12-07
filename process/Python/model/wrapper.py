from pandas import DataFrame, merge
from logging import getLogger
from os import makedirs
from os.path import exists, join
from pickle import dump as pickle_dump
from copy import deepcopy
from process.Python.data.utils import aggregate_population
from process.Python.model.linear import linear_model
from process.Python.model.utils import preprocess_data
from process.Python.model.heckman_wage import heckman_wage_model
from process.Python.model.random_utlity_function import utility_func

logger = getLogger()


def run_utility_func_model_fit(
    df_input: DataFrame,
    output_dir: dict,
    proc_ages: str,
    hours_options: list,
    total_hours: float,
):

    logger.info(f"Utility model for {proc_ages}")

    model_outputpath = f"{output_dir}/utility_params_{"-".join(proc_ages)}.pkl"

    results, validation = utility_func(df_input, hours_options, total_hours)

    pickle_dump(
        {"params": results, "validation": validation},
        open(model_outputpath, "wb"),
    )


def run_heckman_wage_model(
    pop_data: DataFrame,
    cfg: dict,
    heckman_age_group: str,
) -> DataFrame:

    pop_data_input = deepcopy(pop_data)

    employment_cfg = cfg["behaviour_model"]["employment"]

    cal_cols = list(
        set(cfg["cat_cols"])
        & set(
            employment_cfg["predictors"]["selection"]
            + employment_cfg["predictors"]["outcome"]
        )
    )

    logger.info(
        f"Identified categoriec features are {cal_cols}, and start one-shot preprocessing ...",
    )
    pop_data_input, cal_cols_map = preprocess_data(
        pop_data_input, cal_cols, reference_groups=None
    )

    heckman_model_predictors = {"selection": [], "outcome": []}
    for proc_pred_type in heckman_model_predictors:
        for proc_var in employment_cfg["predictors"][proc_pred_type]:
            if proc_var in cal_cols:
                heckman_model_predictors[proc_pred_type].extend(cal_cols_map[proc_var])
            else:
                heckman_model_predictors[proc_pred_type].append(proc_var)

    logger.info(f"Heckman model for {heckman_age_group}")

    model_outputpath = (
        f"{cfg["output_dirs"]["models"]}/model_heckman_wage_{heckman_age_group}.pkl"
    )

    heckman_age_group = heckman_age_group.split("-")

    results = heckman_wage_model(
        pop_data_input,
        selection_col="employed",
        outcome_col="market_income_per_week",
        select_exog=heckman_model_predictors["selection"],
        outcome_exog=heckman_model_predictors["outcome"],
    )

    pickle_dump(
        results,
        open(model_outputpath, "wb"),
    )


def run_model(
    pop_data: dict, target_data_name: str, cfg: dict, output_dir: str
) -> DataFrame:
    pop = pop_data["pop"]
    target_data = pop_data[target_data_name]

    # create mortality data mapping
    data_mapping = {}
    for proc_key in cfg["predictors"]:
        proc_group_key = f"{proc_key}_group"
        if proc_group_key not in target_data:
            raise Exception(f"{proc_group_key} is required for mortality model")
        else:
            data_mapping[proc_key] = list(target_data[proc_group_key].unique())

    pop_to_use = aggregate_population(
        pop[cfg["predictors"] + ["id"]], "id", data_mapping
    )

    predictors_new = []
    for proc_key in cfg["predictors"]:
        predictors_new.append(f"{proc_key}_group")

    data_to_use = merge(target_data, pop_to_use, on=predictors_new, how="left")

    # Obtain mortality probabilities
    model = linear_model(
        data_to_use,
        cfg["target"],
        predictors_new,
        population_col="count",
        use_rate=True,
    )

    model = {"model": model, "predictors": predictors_new, "trained_data": data_to_use}

    model_path = join(output_dir, f"model_{target_data_name}.pkl")

    logger.info(f"Saving {target_data_name} model to {model_path}")

    if not exists(output_dir):
        makedirs(output_dir)

    with open(model_path, "wb") as file:
        pickle_dump(model, file)

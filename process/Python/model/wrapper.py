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
from process.Python.model.validation import run_ruf_validation

logger = getLogger()

def run_ruf_model(
    df_input: DataFrame,
    cfg: dict,
    recreate_data: bool = True):

    input_params = {
        "total_hours": cfg["behaviour_model"]["employment"]["ruf"]["total_hrs"],
        "min_hourly_wage": cfg["behaviour_model"]["employment"]["ruf"]["min_hourly_wage"],
        "leisure_value": cfg["behaviour_model"]["employment"]["ruf"]["leisure_value"],
        "exclude_seniors": True,
        "hours_options": cfg["behaviour_model"]["employment"]["ruf"]["possible_working_hrs"],
        "apply_household_income_filter": {"min": 0.1, "max": 0.7},
        "apply_earner_type_filter": None, 
        "apply_household_size_filter": None
    }

    output_dir = f"{cfg["output_dirs"]["models"]}"


    utility_func(
        df_input,
        input_params,
        income_name={"market": "market_income_per_hour"},
        working_hours_name="working_hours",
        output_dir = output_dir,
        recreate_data = recreate_data
    )


    run_ruf_validation(input_params, output_dir=output_dir)


def run_heckman_wage_model(
    pop_data: DataFrame,
    cfg: dict
) -> DataFrame:

    pop_data_input = deepcopy(pop_data)

    heckman_cfg = cfg["behaviour_model"]["employment"]["heckman"]

    cal_cols = list(
        set(cfg["cat_cols"])
        & set(
            heckman_cfg["selection"]
            + heckman_cfg["outcome"]
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
        for proc_var in heckman_cfg[proc_pred_type]:
            if proc_var in cal_cols:
                heckman_model_predictors[proc_pred_type].extend(cal_cols_map[proc_var])
            else:
                heckman_model_predictors[proc_pred_type].append(proc_var)

    model_outputpath = (
        f"{cfg["output_dirs"]["models"]}/model_heckman_wage.pkl"
    )

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

from pandas import DataFrame, merge
from logging import getLogger
from os import makedirs
from os.path import exists, join
from pickle import dump as pickle_dump

from process.Python.data.utils import aggregate_population
from process.Python.model.linear import linear_model

logger = getLogger()


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

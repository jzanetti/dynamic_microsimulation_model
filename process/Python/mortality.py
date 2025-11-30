from os.path import join
from pickle import load as pickle_load
from process.Python.data.utils import assign_groups, assign_random_status
from pandas import DataFrame
from numpy import isnan


def run_mortality(pop: DataFrame, id_col_name: str, cfg: dict):
    mortality_model_path = join(cfg["output_dirs"]["models"], "model_mortality.pkl")
    mortality_model = pickle_load(open(mortality_model_path, "rb"))

    predictors = list(
        mortality_model["trained_data"][mortality_model["predictors"]].columns
    )

    col_mapping = {}
    for proc_key in predictors:
        col_mapping[proc_key.replace("_group", "")] = list(
            mortality_model["trained_data"][proc_key].unique()
        )

    pop_working = assign_groups(
        pop,
        id_col_name,
        col_mapping,
    )

    pop_working = pop_working.merge(
        pop[[id_col_name, "life_stage"]], on=id_col_name, how="left"
    )

    pop_working_selected = pop_working[pop_working["life_stage"] == "alive"]

    pop_working_selected["life_stage_prob"] = mortality_model["model"].predict(
        pop_working_selected
    )
    pop_working_selected["life_stage_prob"] = pop_working_selected[
        "life_stage_prob"
    ].clip(lower=0, upper=1)
    # when the age goes beyond range (e.g., 105), set the prob to 1.0
    pop_working_selected.loc[
        pop_working_selected["age_group"].isna(), "life_stage_prob"
    ] = 1.0

    pop_working_selected = pop_working_selected.groupby(
        ["age", "ethnicity"], group_keys=False
    ).apply(
        assign_random_status,
        selected_col_name="life_stage",
        options={"a": "dead", "b": "alive"},
        condition="alive",
    )

    pop = pop.merge(
        pop_working_selected[[id_col_name, "life_stage"]],
        on=id_col_name,
        how="left",
        suffixes=("_old", ""),
    )

    pop["life_stage_old"] = pop["life_stage"].combine_first(pop["life_stage_old"])
    pop = pop.drop(columns=["life_stage"])
    pop = pop.rename(columns={"life_stage_old": "life_stage"})

    return pop

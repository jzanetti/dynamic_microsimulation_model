from pandas import DataFrame
from os.path import join
from pickle import load as pickle_load
from process.Python.data.utils import assign_groups
from process.Python.mortality import run_mortality


def forward(
    data: dict,
    forward_year: int,
    cfg: dict,
    id_col_name: str = "id",
    required_data_types: list = ["pop", "mortality"],
) -> DataFrame:

    for proc_data_type in required_data_types:
        if proc_data_type not in data:
            raise Exception(f"Data type {proc_data_type} is missing ...")

    pop = data["pop"]

    year_diff = forward_year - int(pop["base_year"].unique())

    if year_diff == 0:
        return pop

    # <><><><><><><><><><><><><><><><>
    # Calculate mortality
    # <><><><><><><><><><><><><><><><>
    pop["age"] = pop["age"] + year_diff

    # <><><><><><><><><><><><><><><><>
    # Calculate mortality
    # <><><><><><><><><><><><><><><><>
    pop = run_mortality(pop, id_col_name=id_col_name, cfg=cfg)

    x = 3

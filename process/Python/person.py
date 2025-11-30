from pandas import DataFrame
from os.path import join
from pickle import load as pickle_load
from process.Python.data.utils import assign_groups
from process.Python.mortality import run_mortality


def forward(
    pop: DataFrame, forward_year: int, cfg: dict, id_col_name: str = "id"
) -> DataFrame:

    year_diff = forward_year - int(pop["year"].unique())

    if year_diff == 0:
        return pop

    pop["year"] = forward_year

    # <><><><><><><><><><><><><><><><>
    # Calculate mortality
    # <><><><><><><><><><><><><><><><>
    pop["age"] = pop["age"] + year_diff

    # <><><><><><><><><><><><><><><><>
    # Calculate mortality
    # <><><><><><><><><><><><><><><><>
    pop = run_mortality(pop, id_col_name=id_col_name, cfg=cfg)

    return pop

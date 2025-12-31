from pyarrow.parquet import read_table as pq_read_table
from pyarrow.parquet import write_table as pq_write_table
import pyarrow as pa
from os.path import join
from pandas import DataFrame
from copy import deepcopy
from itertools import product as iter_product
from process.Python.model.xgboost import xgboost_model
from logging import getLogger
from process.Python.model.tax_calculator import cal_tax

logger = getLogger()


def add_initial_pop_status(pop: DataFrame) -> DataFrame:
    pop["life_stage"] = "alive"

    return pop


def create_inputs(
    data_dir: str,
    required_data_types: list = ["pop"],
    data_type: str = "parquet",
    base_year: int or None = None,
):
    inputs = {}
    for proc_data_type in required_data_types:
        proc_data_path = join(data_dir, f"{proc_data_type}_data.{data_type}")
        proc_table = pq_read_table(proc_data_path)
        proc_table = proc_table.to_pandas()

        if proc_data_type == "pop":
            proc_table = add_initial_pop_status(proc_table)

        if base_year is not None:
            proc_table["base_year"] = base_year

        inputs[proc_data_type] = proc_table

    return inputs


def prepare_ruf_inputs(
    df_input: DataFrame,
    hours_options: list,
    total_hours: int,
    leisure_value: float,
    market_income_name_per_hour: str,
    benefit_income_name_per_week: str,
    working_hours_name: str,
    data_scaler: float = 1000.0,
    data_output_path: str = None,
):

    # select relevant features
    df_processed = deepcopy(
        df_input[
            [
                "id",
                "household_id",
                "selected",
                market_income_name_per_hour,
                benefit_income_name_per_week,
                working_hours_name,
            ]
        ]
    )

    all_household_ids = df_processed["household_id"].unique()
    long_data = []
    for index1, proc_hhld_id in enumerate(all_household_ids):

        if index1 % 10 == 0:
            logger.info(
                f"Processing input: {round(100 * index1 / len(all_household_ids), 3)}%"
            )

        proc_hhld = df_processed[df_processed["household_id"] == proc_hhld_id]

        num_people = len(proc_hhld)
        all_possible_hours_combination = list(
            iter_product(hours_options, repeat=num_people)
        )

        chosen_combination = min(
            all_possible_hours_combination,
            key=lambda c: sum(
                (a - b) ** 2 for a, b in zip(c, list(proc_hhld["working_hours"].values))
            ),
        )

        for i_comb, proc_combination in enumerate(all_possible_hours_combination):

            is_chosen = 0
            if proc_combination == chosen_combination:
                is_chosen = 1

            proc_data_list = []
            proc_hhld_market_income = 0
            proc_hhld_benefit_income = 0
            for index2, proc_hours in enumerate(proc_combination):
                proc_person = proc_hhld.iloc[index2]
                person_wage = proc_person[market_income_name_per_hour]
                leisure_hours = total_hours - proc_hours

                market_income_person = person_wage * proc_hours
                benefit_income_person = proc_person[benefit_income_name_per_week]

                proc_hhld_market_income += market_income_person
                proc_hhld_benefit_income += benefit_income_person

                if proc_person["selected"]:
                    proc_data_list.append(
                        {
                            "option_hours_id": i_comb,
                            "household_id": int(proc_hhld["household_id"].values[0]),
                            "people_id": int(proc_person["id"]),
                            "option_hours": proc_hours,
                            "is_chosen": is_chosen,
                            "market_income": market_income_person,
                            "benefit_income": benefit_income_person,
                            "leisure": leisure_hours * leisure_value,
                        }
                    )

            for proc_data in proc_data_list:
                proc_data["market_income_hhld"] = proc_hhld_market_income
                proc_data["benefit_income_hhld"] = proc_hhld_benefit_income
            long_data.extend(proc_data_list)

    results = DataFrame(long_data)
    income_cols = [
        "market_income",
        "benefit_income",
        "market_income_hhld",
        "benefit_income_hhld",
    ]
    results[income_cols] = results[income_cols].clip(lower=1e-9)
    # results = results.drop_duplicates()
    results = obtain_benefit_income(results)

    # make the data scaled: e.g., income is between 0-2, leisure is between 0-1
    for proc_key in ["income", "income_hhld", "leisure"]:
        results[proc_key] = results[proc_key] / data_scaler
    results["leisure"] = results["leisure"] * 23.0

    results = results.sort_values(
        by=["household_id", "people_id", "option_hours"]
    ).reset_index(drop=True)

    pq_write_table(pa.Table.from_pandas(results), data_output_path)


def obtain_benefit_income(df: DataFrame) -> DataFrame:

    logger.info("Predict benefit income using XGBoost model")

    df_for_train = df[df["is_chosen"] == 1].reset_index(drop=True)
    model = xgboost_model(
        df_for_train, "benefit_income", ["market_income", "market_income_hhld"]
    )
    import xgboost as xgb

    dpredict = xgb.DMatrix(
        df[["market_income", "market_income_hhld"]].values.astype("float32")
    )
    df["benefit_income_pred"] = model.predict(dpredict)
    # df["benefit_income_pred"] = model.predict(
    #    df[["market_income", "market_income_hhld"]]
    # )

    df["income"] = df["market_income"] + df["benefit_income_pred"]
    df["tax"] = cal_tax(df["income"])
    df["income"] = df["income"] - df["tax"]
    df["income_hhld"] = df.groupby(["option_hours_id", "household_id"])[
        "income"
    ].transform("sum")
    df = df[
        [
            "option_hours_id",
            "household_id",
            "people_id",
            "option_hours",
            "is_chosen",
            "leisure",
            "market_income",
            "benefit_income_pred",
            "income",
            "income_hhld",
        ]
    ]
    return df


"""
import matplotlib.pyplot as plt

plt.scatter(
    df["market_income_hhld"],
    df["benefit_income_pred"],
    alpha=0.3,
    s=5,
)
plt.scatter(
    df["market_income_hhld"],
    df["benefit_income"],
    alpha=0.3,
    c="r",
    s=5,
)
plt.savefig("test.png")
plt.close()
df2 = df[
    [
        "people_id",
        "household_id",
        "benefit_income",
        "benefit_income_pred",
        "market_income",
        "market_income_hhld",
    ]
]
"""

from os.path import join
from pyarrow.parquet import read_table as pq_read_table


def create_outputs(data_dir: str):
    data_path = join(data_dir, "results.parquet")
    data = pq_read_table(data_path)
    data = data.to_pandas()

    # <><><><><><><><><><><><><><><><><><><><>
    # 1. Produce population status
    # <><><><><><><><><><><><><><><><><><><><>
    pop_stats = data.groupby(["year", "life_stage"]).size().reset_index(name="count")
    hhd_stats = (
        data[data["life_stage"] == "alive"][["year", "household_id"]]
        .drop_duplicates()
        .groupby("year")["household_id"]
        .count()
        .reset_index(name="count")
    )

    # <><><><><><><><><><><><><><><><><><><><>
    # 2. Produce employment status
    # <><><><><><><><><><><><><><><><><><><><>
    employment_stats = {}
    for proc_key in ["market_income", "latent_market_income"]:
        employment_stats[f"mean_{proc_key}"] = (
            data[(data["life_stage"] == "alive")]
            .groupby("year")[proc_key]
            .mean()
            .reset_index(name=f"mean_{proc_key}")
        )
    return {
        "population": {"pop": pop_stats, "hhd": hhd_stats},
        "employment": {"income": employment_stats},
    }

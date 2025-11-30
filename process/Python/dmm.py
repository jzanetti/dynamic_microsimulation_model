from logging import getLogger
from process.Python.person import forward as person_forward
from os.path import exists
from os import makedirs
from pandas import concat
from pyarrow.parquet import write_table as pq_write_table
import pyarrow as pa
from copy import deepcopy

logger = getLogger(__name__)


def run_dmm(population_data: dict, cfg: dict, start_year: int, years: int = 5):

    logger.info("Starting DMM Processing")

    results = []

    start_pop = deepcopy(population_data["pop"])

    start_pop["year"] = start_pop["base_year"]

    for proc_year in range(start_year, start_year + years):

        logger.info(f"Processing Year: {proc_year}")

        proc_pop = person_forward(start_pop, proc_year, cfg)

        results.append(proc_pop)

        start_pop = deepcopy(proc_pop)

    results = concat(results)

    results = results.reset_index()

    output_path = f"{cfg["output_dirs"]["outputs"]}/results.parquet"

    print(f"Writing outputs: {output_path}")

    if not exists(cfg["output_dirs"]["outputs"]):
        makedirs(cfg["output_dirs"]["outputs"])

    pq_write_table(pa.Table.from_pandas(results), output_path)

from logging import getLogger
from process.Python.person import forward as person_forward
from pandas import DataFrame

logger = getLogger(__name__)


def run_dmm(population_data: dict, cfg: dict, start_year: int, years: int = 5):

    logger.info("Starting DMM Processing")

    for proc_year in range(start_year, start_year + years):
        logger.info(f"Processing Year: {proc_year}")

        person_forward(population_data, proc_year, cfg)

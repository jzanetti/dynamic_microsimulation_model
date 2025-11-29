from pyarrow.parquet import read_table as pq_read_table
from os.path import join


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

        if base_year is not None:
            proc_table["base_year"] = base_year

        inputs[proc_data_type] = proc_table

    return inputs

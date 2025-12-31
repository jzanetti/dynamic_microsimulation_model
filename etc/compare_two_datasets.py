from pyarrow.parquet import read_table as pq_read_table
from process.Python.model.random_utlity_function import negative_log_likelihood
import pandas as pd

# Python
df1 = pq_read_table(
    "etc/app/runs/utility_func_data_c1eaf5bf91f7b1ac8ff16f735da624b8_sq.parquet"
)
df1 = df1.to_pandas()

# R
df2 = pq_read_table(
    "etc/app/runs/utility_func_data_b96bb9b8f927dd59a9a062cbd7512d1e_sq.parquet"
)
df2 = df2.to_pandas()
df2["option_hours_id"] = df2["option_hours_id"].astype(int)
sort_cols = ["household_id", "people_id", "option_hours", "option_hours_id"]


pd.testing.assert_frame_equal(df2.astype(object), df1.astype(object), check_dtype=False)
# negative_log_likelihood(initial_guess, proc_data, len(hours_options))
# proc_data2 = proc_data2.sort_values(
#     by=["household_id", "people_id", "option_hours"]
# ).reset_index(drop=True)

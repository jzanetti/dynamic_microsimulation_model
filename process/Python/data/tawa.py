from process.Python.data.sample import obtain_working_hours
from process.Python import RUF_METHOD
from pandas import Series
from numpy import where
from process.Python.data.input import prepare_ruf_inputs
from process.Python.model.random_utlity_function import predict as ruf_predict
from process.Python.data.filename import create_hash_filename
from pyarrow.parquet import read_table as pq_read_table

from pathlib import Path
from pandas import read_csv, merge
import pandas as pd
from logging import getLogger
from os.path import join
from process.Python.data.filename import create_hash_filename
from pyarrow.parquet import read_table as pq_read_table

logger = getLogger()


def run_tawa_predict(
    tawa_data: dict,
    output_dir: str,
    input_params: dict,
    reform_name: str,
    updated_tawa_data_path: str,
):

    filename_hash = create_hash_filename(input_params, filename_suffix="sq")
    data_sq = pq_read_table(f"{output_dir}/utility_func_data_{filename_hash}.parquet")
    data_sq = data_sq.to_pandas()

    tawa_data_preprocess(
        tawa_data,
        input_params,
        tawa_data_name=reform_name,
        output_dir=output_dir,
        ref_ids=list(data_sq["people_id"].unique()),
    )

    run_ruf_for_tawa(
        tawa_data,
        input_params,
        updated_tawa_data_path=updated_tawa_data_path,
        tawa_data_name=reform_name,
        output_dir=output_dir,
    )


def add_utility_err_to_tawa(
    input_params: dict,
    tawa_data_name: str,
    output_dir: str,
    join_keys: list = ["household_id", "people_id", "option_hours", "option_hours_id"],
) -> str:
    """
    Merges calibrated error terms from the status quo (sq) dataset into
    the reform dataset based on household and person identifiers.
    """

    # 1. Load Reform Data
    reform_hash = create_hash_filename(input_params, filename_suffix=tawa_data_name)
    reform_file = join(output_dir, f"utility_func_data_{reform_hash}.parquet")
    df_reform = pd.read_parquet(reform_file)

    # 2. Load SQ Data (Optimized)
    sq_hash = create_hash_filename(input_params, filename_suffix="sq")
    sq_file = join(output_dir, f"utility_func_data_{sq_hash}.parquet")
    df_sq = pd.read_parquet(sq_file, columns=join_keys + ["calibrated_err"])

    # 3. Merge
    df_merged = pd.merge(
        df_reform, df_sq.drop_duplicates(subset=join_keys), on=join_keys, how="left"
    )

    # 4. Write Output
    output_file = join(output_dir, f"utility_func_data_{reform_hash}_updated.parquet")
    df_merged.to_parquet(output_file, index=False)

    return output_file


def run_ruf_for_tawa(
    tawa_data: dict,
    input_params: dict,
    updated_tawa_data_path: str = "test.csv",
    tawa_data_name: str = "reform1",
    output_dir: str = ".",
    method=RUF_METHOD,
):

    logger.info("Adding utility error from SQ to Reform ...")
    add_utility_err_to_tawa(input_params, tawa_data_name, output_dir)

    logger.info("Obtain RUF paramaters ...")
    filename_hash = create_hash_filename(input_params, filename_suffix="sq")
    ruf_params = read_csv(f"{output_dir}/utility_func_parameters_{filename_hash}.csv")
    ruf_params = ruf_params.set_index("parameter")["Value"].to_dict()

    logger.info("Get TAWA RUF data (Reform)")
    filename_hash = create_hash_filename(input_params, filename_suffix=tawa_data_name)
    data_reform = pq_read_table(
        f"{output_dir}/utility_func_data_{filename_hash}_updated.parquet"
    )
    data_reform = data_reform.to_pandas()
    data_reform = ruf_predict(data_reform, ruf_params, method=method, use_hhld=True)

    logger.info("Get TAWA RUF data (SQ)")
    filename_hash = create_hash_filename(input_params, filename_suffix="sq")
    data_sq = pq_read_table(f"{output_dir}/utility_func_data_{filename_hash}.parquet")
    data_sq = data_sq.to_pandas()
    data_sq = data_sq[data_sq["is_chosen"] == 1]
    data_sq = data_sq[["household_id", "people_id", "option_hours"]]
    data_sq = data_sq.rename(columns={"option_hours": "hours"})

    logger.info("Merge SQ and Reform data together ...")
    employment_scaler_data = merge(
        data_reform,
        data_sq,
        on=["household_id", "people_id"],
        suffixes=("_reform", "_sq"),
    )
    employment_scaler_data["employment_scaler"] = (
        employment_scaler_data["hours_reform"] / employment_scaler_data["hours_sq"]
    )

    employment_num1 = len(
        employment_scaler_data[employment_scaler_data["employment_scaler"] > 1]
    )
    employment_num2 = len(
        employment_scaler_data[employment_scaler_data["employment_scaler"] < 1]
    )
    logger.info(f"Number of people with inreased working hours {employment_num1}")
    logger.info(f"Number of people with decreased working hours {employment_num2}")

    logger.info("Update TAWA data ...")
    employment_scaler_data = employment_scaler_data[
        ["household_id", "people_id", "employment_scaler"]
    ]

    employment_scaler_data = employment_scaler_data.rename(
        columns={"household_id": "snz_hes_hhld_uid", "people_id": "snz_hes_uid"}
    )

    tawa_data_input = tawa_data["input"].merge(
        employment_scaler_data, on=["snz_hes_hhld_uid", "snz_hes_uid"], how="left"
    )

    tawa_data_input["employment_scaler"] = tawa_data_input["employment_scaler"].fillna(
        1.0
    )

    for tawa_data_key in ["P_Income_SelfEmployed", "P_Income_WageSalary"]:
        tawa_data_input[tawa_data_key] = (
            tawa_data_input[tawa_data_key] * tawa_data_input["employment_scaler"]
        )

    tawa_data_input = tawa_data_input.drop(["employment_scaler"], axis=1)

    tawa_data_input.to_csv(updated_tawa_data_path, index=False)


def tawa_data_preprocess(
    tawa_data: dict,
    input_params: dict,
    tawa_data_name: str = "sq",
    income_name: str = "income_per_hour",
    working_hours_name: str = "working_hours",
    ref_ids: list or None = None,
    output_dir: str = ".",
):

    # ---------------------------------------------
    # Step 0: Obtain parameters
    # ---------------------------------------------
    total_hours = input_params["total_hours"]
    leisure_value = input_params["leisure_value"]
    min_hourly_wage = input_params["min_hourly_wage"]
    hours_options = input_params["hours_options"]
    exclude_seniors = input_params["exclude_seniors"]
    apply_earner_type_filter = input_params["apply_earner_type_filter"]
    apply_household_size_filter = input_params["apply_household_size_filter"]
    apply_people_income_filter = input_params["apply_people_income_filter"]
    apply_household_income_filter = input_params["apply_household_income_filter"]

    # ---------------------------------------------
    # Step 1: Merge tawa input and output
    # ---------------------------------------------
    df_input = tawa_data["input"][
        [
            "snz_hes_hhld_uid",
            "snz_hes_uid",
            "H_Counts_Adults",
            "H_Counts_DependentKids",
            "P_Attributes_Age",
            "P_Attributes_Sex",
            "P_Job_TotalHoursPerWeek",
        ]
    ]

    df_output = tawa_data[tawa_data_name][
        [
            "snz_hes_hhld_uid",
            "snz_hes_uid",
            "P_Income_SelfEmployed",
            "P_Income_WageSalary",
            "P_Benefits_All_Taxable",
            "P_Benefits_All_NonTaxable",
            "P_Income_TaxPayable",
        ]
    ]

    df = df_input.merge(df_output, on=["snz_hes_uid", "snz_hes_hhld_uid"])

    df["market_income_person"] = df["P_Income_SelfEmployed"] + df["P_Income_WageSalary"]
    df["benefit_income_person"] = (
        df["P_Benefits_All_Taxable"] + df["P_Benefits_All_NonTaxable"]
    )
    df = df[
        [
            "snz_hes_hhld_uid",
            "snz_hes_uid",
            "H_Counts_Adults",
            "H_Counts_DependentKids",
            "P_Attributes_Age",
            "P_Attributes_Sex",
            "P_Job_TotalHoursPerWeek",
            "market_income_person",
            "benefit_income_person",
            "P_Income_TaxPayable",
        ]
    ]

    """
    # ---------------------------------------------
    # Step 2: Apply household filter
    # ---------------------------------------------
    df["market_income_person"] = df["P_Income_SelfEmployed"] + df["P_Income_WageSalary"]
    df["market_income_hhld"] = df.groupby("snz_hes_hhld_uid")[
        ["market_income_person"]
    ].transform("sum")

    from process.Python.model.linear import linear_model

    model = linear_model(
        df, "P_Benefits_All_Taxable", ["market_income_person", "market_income_hhld"]
    )
    """

    # ---------------------------------------------
    # Step 2: Apply household filter
    # ---------------------------------------------
    if apply_household_size_filter is not None:
        mask = Series(True, index=df.index)
        for col, (min_val, max_val) in apply_household_size_filter.items():
            mask = mask & df[col].between(min_val, max_val)
        df = df[mask]

    # ---------------------------------------------
    # Step 3: Apply Person filter (only people over 18 will be considered)
    # ---------------------------------------------
    if exclude_seniors:
        df = df[df["P_Attributes_Age"] < 65]
    df = df[df["P_Attributes_Age"] >= 18]

    # ---------------------------------------------
    # Step 3: Rename the data and convert year income to weekly
    # ---------------------------------------------
    df["market_income_person_per_week"] = df["market_income_person"] / 52.0
    df["benefit_income_person_per_week"] = df["benefit_income_person"] / 52.0
    df["total_income_person_per_week"] = (
        df["market_income_person_per_week"] + df["benefit_income_person_per_week"]
    )

    df = df.rename(
        columns={
            "snz_hes_hhld_uid": "household_id",
            "snz_hes_uid": "id",
            "P_Attributes_Age": "age",
            "P_Attributes_Sex": "gender",
            "market_income_person_per_week": "market_income_per_week",
            "benefit_income_person_per_week": "benefit_income_per_week",
            "total_income_person_per_week": "total_income_per_week",
            "P_Job_TotalHoursPerWeek": "working_hours_tawa",
        }
    )[
        [
            "household_id",
            "id",
            "age",
            "gender",
            "market_income_per_week",
            "benefit_income_per_week",
            "total_income_per_week",
            "working_hours_tawa",
        ]
    ]
    df["gender"] = where(df["gender"] == 1, "Male", "Female")

    # ---------------------------------------------
    # Step 5: Obtain working hours
    # ---------------------------------------------
    df = obtain_working_hours(df)
    df["working_hours"] = df[["working_hours", "working_hours_tawa"]].max(axis=1)
    df["working_hours"] = where(
        df["working_hours"] < hours_options[1],
        1e-9,
        df["working_hours"],
    )

    df["market_income_per_hour"] = df["market_income_per_week"] / df["working_hours"]
    df["market_income_per_hour"] = df["market_income_per_hour"].clip(
        lower=min_hourly_wage
    )
    df["working_hours"] = df["market_income_per_week"] / df["market_income_per_hour"]

    # ---------------------------------------------
    # Step 6: Data quality control
    # ---------------------------------------------
    if apply_household_income_filter is not None:
        df["total_income_per_week_hhld"] = df.groupby("household_id")[
            "total_income_per_week"
        ].transform("sum")

        thres_min = df["total_income_per_week_hhld"].quantile(
            apply_household_income_filter["min"]
        )
        thres_max = df["total_income_per_week_hhld"].quantile(
            apply_household_income_filter["max"]
        )
        df = df[
            (df["total_income_per_week_hhld"] >= thres_min)
            & (df["total_income_per_week_hhld"] <= thres_max)
        ]

    if apply_people_income_filter is not None:
        thres_min = df["total_income_per_week"].quantile(
            apply_people_income_filter["min"]
        )
        thres_max = df["total_income_per_week"].quantile(
            apply_people_income_filter["max"]
        )
        df = df[
            (df["total_income_per_week"] >= thres_min)
            & (df["total_income_per_week"] <= thres_max)
        ]

    # ---------------------------------------------
    # Step 6: Obtain primary/secondary earner
    # ---------------------------------------------
    if apply_earner_type_filter is not None:
        df = df.sort_values(
            ["household_id", "market_income_per_week"], ascending=[True, False]
        )
        if apply_earner_type_filter.lower() == "primary":
            # df_primary = df[df["gender"] == "Male"]
            df_primary = df.groupby("household_id").nth(0).reset_index()
            selected_id = df_primary["id"].unique()
        elif apply_earner_type_filter.lower() == "others":
            df_rest = df.groupby("household_id").nth(slice(1, None)).reset_index()
            # df_secondary = df[df["gender"] == "Female"]
            # df_secondary = df.groupby("household_id").nth(1).reset_index()
            selected_id = df_rest["id"].unique()

        df["selected"] = where(df["id"].isin(selected_id), True, False)
    else:
        df["selected"] = True

    if ref_ids is not None:
        df = df[df["id"].isin(ref_ids)]

    # ---------------------------------------------
    # Step 7: Prepare RUF inputs
    # ---------------------------------------------
    filename_hash = create_hash_filename(input_params, filename_suffix=tawa_data_name)
    prepare_ruf_inputs(
        df,
        hours_options,
        total_hours,
        leisure_value,
        "market_income_per_hour",
        "benefit_income_per_week",
        working_hours_name,
        data_output_path=f"{output_dir}/utility_func_data_{filename_hash}.parquet",
    )

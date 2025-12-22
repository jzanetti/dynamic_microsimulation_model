

from process.Python.data.sample import obtain_working_hours
from pandas import DataFrame, Series
from pandas import DataFrame
from numpy import where
from process.Python.data import EPLISON

def tawa_data_preprocess(
    tawa_data: dict, 
    hours_options: list = [0, 20, 40],
    min_hourly_wage:float = 23.0, 
    exclude_seniors: bool =False,
    income_types: list = [
        "P_Income_Disposable"
    ],
    apply_earner_type_filter: str or None = None,
    apply_household_income_filter: dict or None = {
        "min": 0.3, "max": 0.7
    },
    apply_people_income_filter: dict or None = {
        "min": 0.1, "max": 0.9
    },
    apply_household_size_filter: dict or None = {
        "H_Counts_Adults": [2, 2],
        "H_Counts_DependentKids": [1, 3]
    },
    yearly_income: bool = True
):
    
    # ---------------------------------------------
    # Step 0: Apply household filter
    # ---------------------------------------------
    df_input = tawa_data["input"][[
        "snz_hes_hhld_uid",
        "snz_hes_uid",
        "H_Counts_Adults", 
        "H_Counts_DependentKids",
        "P_Attributes_Age",
        "P_Attributes_Sex",
        "P_Job_TotalHoursPerWeek"]]
    
    df_output = tawa_data["output"][[
        "snz_hes_hhld_uid",
        "snz_hes_uid",
        "P_Income_Disposable"]]

    df = df_input.merge(df_output, on=["snz_hes_uid", "snz_hes_hhld_uid"])

    # ---------------------------------------------
    # Step 1: Apply household filter
    # ---------------------------------------------
    if apply_household_size_filter is not None:
        mask = Series(True, index=df.index)
        for col, (min_val, max_val) in apply_household_size_filter.items():
            mask = mask & df[col].between(min_val, max_val)
        df = df[mask]

    if exclude_seniors:
        # df = df[df.groupby("snz_hes_hhld_uid")["P_Attributes_Age"].transform("min") < 65]
        df = df[df["P_Attributes_Age"] < 65]

    # ---------------------------------------------
    # Step 2: Apply Person filter (only people over 18 will be considered)
    # ---------------------------------------------
    df = df[df["P_Attributes_Age"] > 18]

    # ---------------------------------------------
    # Step 3:Rename the data
    # ---------------------------------------------
    df['P_Total_income'] = df[income_types].sum(axis=1)

    if yearly_income:
        df['P_Total_income'] = df['P_Total_income'] / 52.0

    df = df.rename(
        columns={
            "snz_hes_hhld_uid": "household_id",
            "snz_hes_uid": "id",
            "P_Attributes_Age": "age",
            "P_Attributes_Sex": "gender",
            "P_Total_income": "income_per_week",
            "P_Job_TotalHoursPerWeek": "working_hours",
        }
    )[
        [
            "household_id",
            "id",
            "age",
            "gender",
            "income_per_week",
            "working_hours",
        ]
    ]
    df["gender"] = where(df["gender"] == 1, "Male", "Female")

    # ---------------------------------------------
    # Step 4: Quality control for the market income
    # ---------------------------------------------
    if apply_people_income_filter is not None:
        thres_min = df['income_per_week'].quantile(apply_people_income_filter["min"])
        thres_max = df['income_per_week'].quantile(apply_people_income_filter["max"])
        df = df[
            (df["income_per_week"] >= thres_min)
            & (df["income_per_week"] <= thres_max)
        ]

    # ---------------------------------------------
    # Step 5: Obtain working hours
    # ---------------------------------------------
    df = obtain_working_hours(df)
    df["working_hours"] = where(
        df["working_hours"] < hours_options[1],
        1e-9,
        df["working_hours"],
    )

    # ---------------------------------------------
    # Step 6: Data quality control
    # ---------------------------------------------
    # 6.1: Remove outlier households 
    if apply_household_income_filter is not None:
        df["household_income_per_week"] = df.groupby("household_id")[
            "income_per_week"
        ].transform("sum")

        thres_min = df['household_income_per_week'].quantile(
            apply_household_income_filter["min"])
        thres_max = df['household_income_per_week'].quantile(
            apply_household_income_filter["max"])
        df = df[
            (df["household_income_per_week"] >= thres_min)
            & (df["household_income_per_week"] <= thres_max)
        ]

    # 5.2: Correct wage for people who are not working (here
    #      we use minimum wage)
    df["income_per_hour"] = where(
        df["working_hours"] == 1e-9,
        min_hourly_wage,
        df["income_per_week"] / df["working_hours"],
    )

    # ---------------------------------------------
    # Step 6: Obtain primary/secondary earner
    # ---------------------------------------------
    if apply_earner_type_filter is not None:
        df = df.sort_values(
            ["household_id", "income_per_week"], ascending=[True, False]
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

        df['selected'] = where(df['id'].isin(selected_id), True, False)
    else:
        df['selected'] = True

    return df
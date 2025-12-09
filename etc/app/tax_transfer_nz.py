from process.Python.model.random_utlity_function import utility_func
from process.Python.data.sample import obtain_working_hours
from pandas import read_csv
from pandas import DataFrame
from numpy import where


def data_preprocess(
    df: DataFrame, min_hourly_wage=23.0, max_hourly_wage=100.0, exclude_seniors=False
):

    df = df[(df["H_Counts_DependentKids"] == 0)]
    # df = df[(df["H_Counts_Adults"] >= 0) & (df["H_Counts_DependentKids"] >= 2)]
    df["P_Total_income"] = 0
    for proc_key in [
        # "P_Income_OtherRegularNZ",
        # "P_Income_OtherStudentBursary",
        # "P_Income_PensionBenefitOverseas",
        "P_Income_SelfEmployed",
        # "P_Income_StudentAllowance",
        # "P_Income_Totals_Dividend",
        # "P_Income_Totals_Interest",
        # "P_Income_Totals_OtherChargeable",
        # "P_Income_Totals_OtherNonTaxable",
        # "P_Income_Totals_OtherTaxable",
        "P_Income_WageSalary",
    ]:
        df["P_Total_income"] = df["P_Total_income"] + df[proc_key]

    # income_median = df["P_Total_income"].median()
    # df = df[
    #    (df["P_Total_income"] >= income_median * 0.75)
    #    & (df["P_Total_income"] <= income_median * 1.25)
    # ]
    df2 = df.rename(
        columns={
            "snz_hes_hhld_uid": "household_id",
            "snz_hes_uid": "id",
            "P_Attributes_Age": "age",
            "P_Attributes_Sex": "gender",
            # "P_Income_WageSalary": "market_income_per_week",
            # "P_Job_WageSalaryHoursPerWeek": "working_hours",
            "P_Total_income": "market_income_per_week",
            "P_Job_TotalHoursPerWeek": "working_hours",
        }
    )[
        [
            "household_id",
            "id",
            "age",
            "gender",
            "market_income_per_week",
            "working_hours",
        ]
    ]

    # df2 = df2[
    #    (df2["market_income_per_week"] >= 0.0)
    #    & (df2["market_income_per_week"] <= max_hourly_wage * 40)
    # ]

    df2["household_market_income_per_week"] = df2.groupby("household_id")[
        "market_income_per_week"
    ].transform("sum")

    income_median = df2["household_market_income_per_week"].median()
    df2 = df2[
        (df2["household_market_income_per_week"] >= income_median * 0.5)
        & (df2["household_market_income_per_week"] <= income_median * 1.5)
    ]

    if exclude_seniors:
        df2 = df2[df2.groupby("household_id")["age"].transform("min") < 65]

    df2["gender"] = where(df2["gender"] == 1, "Male", "Female")
    df2 = obtain_working_hours(df2)

    hours_options = [
        0.0,
        df2["working_hours"].median() * 0.3,
        df2["working_hours"].median() * 0.6,
        df2["working_hours"].median() * 0.8,
        df2["working_hours"].median() * 1.0,
        df2["working_hours"].median() * 1.2,
    ]

    df2 = df2[
        (df2["market_income_per_week"] >= 0.0)
        & (df2["market_income_per_week"] <= max_hourly_wage * 40)
    ]

    working_hours_cutoff = hours_options[1]

    df2["working_hours"] = where(
        df2["market_income_per_week"] < working_hours_cutoff,
        1e-9,
        df2["working_hours"],
    )

    df2["market_income_per_hour"] = where(
        df2["working_hours"] == 1e-9,
        # min_hourly_wage,
        1e-9,
        df2["market_income_per_week"] / df2["working_hours"],
    )

    # df2["market_income_hhld_per_week"] = df2.groupby("household_id")[
    #    "market_income_per_week"
    # ].transform("sum")

    return {"data": df2, "hours_options": hours_options}


if __name__ == "__main__":

    df = read_csv("etc/app/Synthetic-HES23-single-period.csv")

    data = data_preprocess(df, exclude_seniors=True)

    total_hours = 60.0

    data_input = data["data"]

    # mask = data_input.groupby("household_id")["household_id"].transform("size") <= 4

    # Filter the dataframe
    # data_input = data_input[mask]

    data_input = data_input.sort_values(
        ["household_id", "market_income_per_week"], ascending=[True, False]
    )

    # 2. Group and take the 2nd row (index 1)
    # Note: Households with only 1 member will be automatically dropped.
    data_input = data_input.groupby("household_id").nth(1).reset_index()

    # data_input = data_input.loc[
    #    data_input.groupby("household_id")["market_income_per_week"].idxmax()
    # ]

    # data_input = data_input[
    #    (data_input["age"] >= 18)
    #    & (data_input["age"] < 25)
    #    & (data_input["gender"] == "Male")
    # ]

    utility_func(
        data_input,
        data["hours_options"],
        total_hours=total_hours,
        income_name="market_income_per_hour",
        working_hours_name="working_hours",
        penalty_weight=1.0,
    )

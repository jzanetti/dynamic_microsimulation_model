from numpy import zeros_like, maximum, minimum
from pandas import DataFrame
from process.Python import TAX_BRACKETS


def cal_tax(income_series: DataFrame) -> DataFrame:

    total_tax = zeros_like(income_series, dtype=float)
    remaining_income = income_series.values.copy()

    for threshold, rate in TAX_BRACKETS:
        # Calculate income falling into this specific bracket
        taxable_at_rate = maximum(0, remaining_income - threshold)
        total_tax += taxable_at_rate * rate
        # Update remaining income to the threshold for the next bracket down
        remaining_income = minimum(remaining_income, threshold)

    return total_tax

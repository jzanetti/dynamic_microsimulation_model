import pandas as pd
import numpy as np
from sklearn.linear_model import PoissonRegressor
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from pandas import DataFrame
from numpy import nan
from sklearn.linear_model import LinearRegression


def fit_aggregated_rate_model(
    df: DataFrame, target_col, predictor_cols, population_col
):
    """
    Fits a Regression model for aggregated count data.

    Args:
        df (pd.DataFrame): The training data.
        death_col (str): Column containing count of deaths (e.g., 5, 10).
        population_col (str): Column containing total people in that group (exposure).
        predictor_cols (list): List of columns to use as predictors (age, ethnicity).

    Returns:
        model: A trained Pipeline.
    """

    # 1. Prepare Target (y) and Weights
    y = df[target_col] / df[population_col]
    y = y.replace(nan, 1.0)
    X = df[predictor_cols]

    # 2. Identify Column Types
    numeric_features = X.select_dtypes(include=["int64", "float64"]).columns.tolist()
    categorical_features = X.select_dtypes(
        include=["object", "category"]
    ).columns.tolist()

    # 3. Create Transformers (Same as before)
    numeric_transformer = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
        ]
    )

    categorical_transformer = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="constant", fill_value="missing")),
            ("encoder", OneHotEncoder(handle_unknown="ignore")),
        ]
    )

    preprocessor = ColumnTransformer(
        transformers=[
            ("num", numeric_transformer, numeric_features),
            ("cat", categorical_transformer, categorical_features),
        ]
    )

    # 4. Create Pipeline with PoissonRegressor
    model_pipeline = Pipeline(
        steps=[
            ("preprocessor", preprocessor),
            ("regressor", LinearRegression()),
        ]
    )
    # 5. Fit the model
    model_pipeline.fit(X, y)

    new_data_df = DataFrame(
        {
            "age_group": ["50-59", "50-59"],
            "ethnicity_group": ["Asian", "Maori"],
        }
    )

    model_pipeline.predict(new_data_df)

    return model_pipeline


def predict_expected_deaths(model, new_data_df, population_col_name=None):
    """
    Predicts the death RATE.
    If a population column is provided, it calculates expected DEATH COUNTS.
    """
    # 1. Predict the Rate (Probability of death)
    predicted_rate = model.predict(new_data_df)

    # 2. If we know the population of the new data, calculate Counts
    if population_col_name and population_col_name in new_data_df.columns:
        return predicted_rate * new_data_df[population_col_name]

    # Otherwise just return the probability/rate (0 to 1)
    return predicted_rate

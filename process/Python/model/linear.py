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


def linear_model(
    df: DataFrame,
    target_col,
    predictor_cols,
    population_col: None = None,
    use_rate: bool = False,
):
    """
    Fits a Regression model for aggregated count data.
    For example, Obtain mortality probabilities, the input is sth like:
        age_group ethnicity_group  deaths  base_year  count
    0        0-9           Asian       0       2025    165
    1      10-19           Asian       0       2025    141
    2      20-29           Asian       2       2025    119
    3      30-39           Asian      19       2025    260
    4      40-49           Asian      75       2025    240
    5      50-59           Asian      60       2025     85
    the function will predict the death rate (e.g., deaths/count) based on
    selected predictors

    Args:
        df (pd.DataFrame): The training data.
        death_col (str): Column containing count of deaths (e.g., 5, 10).
        population_col (str): Column containing total people in that group (exposure).
        predictor_cols (list): List of columns to use as predictors (age, ethnicity).

    Returns:
        model: A trained Pipeline.
    """

    if use_rate:
        if population_col is None:
            raise Exception("Use rate is enabled, but population col is set to None")
        y = df[target_col] / df[population_col]
        y = y.replace(nan, 1.0)
    else:
        y = df[target_col]

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

    # new_data_df = DataFrame(
    #    {
    #        "age_group": ["50-59", "50-59"],
    #        "ethnicity_group": ["Asian", "Maori"],
    #    }
    # )

    # model_pipeline.predict(new_data_df)

    return model_pipeline

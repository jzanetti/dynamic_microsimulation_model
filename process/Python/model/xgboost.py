import pandas as pd
import numpy as np
from xgboost import XGBRegressor
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer

import pandas as pd
import xgboost as xgb


def xgboost_model(
    df_for_train: pd.DataFrame,
    target_col: str,
    predictor_cols: list,
    population_col: str = None,
):
    # 1. Prepare features - equivalent to as.matrix(df[, ..predictor_cols])
    # IMPORTANT: R's as.matrix converts all data to one type.
    # Ensure no categorical strings are left; they must be numeric.
    X = df_for_train[predictor_cols].values.astype("float32")
    y = df_for_train[target_col].values.astype("float32")

    # 2. Create DMatrix (The core data structure for XGBoost)
    # Using DMatrix directly is the only way to guarantee parity with R's xgboost()
    dtrain = xgb.DMatrix(X, label=y)

    # 3. Define Parameters
    # We must explicitly set parameters that might have different defaults
    params = {
        "objective": "reg:tweedie",
        "eta": 0.3,  # R default for 'eta'
        "max_depth": 6,  # R default for 'max_depth'
        "tree_method": "exact",  # Forces deterministic split calculation
        "nthread": 1,  # Multi-threading can cause slight float variations
        "seed": 42,  # Ensure the seed is set inside the params
    }

    # 4. Train using the core API
    # nrounds in R is num_boost_round in Python
    model = xgb.train(
        params=params, dtrain=dtrain, num_boost_round=300, verbose_eval=False
    )

    return model

import statsmodels.api as sm
from scipy.stats import norm
from numpy import sqrt as np_sqrt


def heckman_wage_model(
    df, selection_col, outcome_col, select_exog, outcome_exog, add_intercept=True
):
    """
    Performs Heckman's Two-Step correction for selection bias.

    Parameters:
    -----------
    df : pandas.DataFrame
        The dataset containing all variables.
    selection_col : str
        Name of the binary column (0/1) indicating participation/selection.
    outcome_col : str
        Name of the dependent variable (e.g., wage). Can be NaN for non-selected rows.
    select_exog : list of str
        List of column names for the Selection Equation (Probit).
        MUST include the Exclusion Restriction variable.
    outcome_exog : list of str
        List of column names for the Outcome Equation (OLS).
    add_intercept : bool, default True
        If True, automatically adds a constant (intercept) to both equations.

    Returns:
    --------
    dict
        Contains:
        - 'probit_model': The fitted statsmodels Probit object.
        - 'ols_model': The fitted statsmodels OLS object (outcome equation).
        - 'imr': The Inverse Mills Ratio series (aligned to original df).
        - 'rho': Estimated correlation (derived from IMR coefficient).
        - 'sigma': Estimated standard error of residuals (derived).
    """

    # 1. Prepare Data
    # Drop rows where independent variables are missing to avoid alignment errors
    all_cols = list(set([selection_col] + select_exog + outcome_exog))
    data = df.dropna(subset=all_cols).copy()

    # --- Step 1: Selection Equation (Probit) ---
    y_select = data[selection_col]
    X_select = data[select_exog]

    if add_intercept:
        X_select = sm.add_constant(X_select)

    # Fit Probit
    probit_model = sm.Probit(y_select, X_select).fit(disp=0)

    # Calculate Inverse Mills Ratio (Lambda)
    # Get the predicted Z-scores (index)
    z_scores = probit_model.predict(X_select, linear=True)

    # IMR = pdf(z) / cdf(z)
    data["imr"] = norm.pdf(z_scores) / norm.cdf(z_scores)

    # --- Step 2: Outcome Equation (OLS) ---
    # Filter for selected observations (e.g., where participation == 1)
    # We must also ensure the outcome_col is not NaN
    data_selected = data[
        (data[selection_col] == 1) & (data[outcome_col].notna())
    ].copy()

    y_outcome = data_selected[outcome_col]
    X_outcome = data_selected[outcome_exog + ["imr"]]  # Add IMR to regressors

    if add_intercept:
        X_outcome = sm.add_constant(X_outcome)

    # Fit OLS
    ols_model = sm.OLS(y_outcome, X_outcome).fit()

    # --- Step 3: derive Rho and Sigma (Optional diagnostics) ---
    # The coefficient on IMR is (rho * sigma_u)
    beta_lambda = ols_model.params["imr"]

    # Estimated residual variance from the OLS step
    # Note: This is slightly biased in the two-step method but provides an approximation
    sigma_e_sq = ols_model.mse_resid
    sigma_u = np_sqrt(
        sigma_e_sq + beta_lambda**2 * data_selected["imr"].var()
    )  # Approximation
    rho = beta_lambda / sigma_u

    return {
        "probit_model": probit_model,
        "ols_model": ols_model,
        "data_with_imr": data[["imr"]],  # Returns IMR for the whole dataset
        "lambda_param": beta_lambda,
        "rho_approx": rho,
    }

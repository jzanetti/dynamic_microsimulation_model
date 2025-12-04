from pandas import get_dummies as pd_get_dummies
from pandas import concat as pd_concat
from logging import getLogger

logger = getLogger()


def preprocess_data(df, cat_cols, reference_groups=None):
    """
    Converts categorical columns to dummy variables and returns the new DataFrame
    plus a map of original column names to new dummy column names.

    Parameters:
    -----------
    df : pandas.DataFrame
        The raw data containing string/categorical columns.
    cat_cols : list of str
        List of column names to encode (e.g., ['education', 'gender']).
    reference_groups : dict, optional
        Dictionary specifying which category to drop (use as reference) for specific columns.
        Example: {'education': 'Low', 'gender': 'Male'}
        If None (or column not listed), defaults to alphabetical 'drop_first'.

    Returns:
    --------
    df_encoded : pandas.DataFrame
        The processed DataFrame with dummy variables.
    new_cols_map : dict
        A dictionary mapping the original column name to the list of new dummy column names.
        Example: {'education': ['education_High', 'education_Medium'], ...}
    """
    df_encoded = df.copy()
    new_cols_map = {}
    reference_groups = reference_groups or {}

    for col in cat_cols:
        # Generate dummies for this column
        # We initially keep all to manually drop the reference later
        dummies = pd_get_dummies(df_encoded[col], prefix=col, dtype=int)

        # Determine which category to drop
        if col in reference_groups:
            ref_col = f"{col}_{reference_groups[col]}"
            if ref_col in dummies.columns:
                dummies = dummies.drop(columns=[ref_col])
                print(f"[{col}] Reference group set to: '{reference_groups[col]}'")
            else:
                # Fallback if the user made a typo in the reference name
                print(
                    f"Warning: Reference '{reference_groups[col]}' not found in '{col}'. Dropping first."
                )
                dummies = dummies.iloc[:, 1:]
        else:
            # Default behavior: Drop the first column (alphabetical)
            dropped_col = dummies.columns[0]
            dummies = dummies.iloc[:, 1:]
            logger.info(
                f"[{col}] Reference group set to: '{dropped_col.replace(col+'_', '')}' (Default)"
            )

        # Store the list of new column names created for this variable
        new_cols_map[col] = dummies.columns.tolist()

        # Add to main DF and remove original string column
        df_encoded = pd_concat([df_encoded, dummies], axis=1)
        df_encoded.drop(columns=[col], inplace=True)

    return df_encoded, new_cols_map

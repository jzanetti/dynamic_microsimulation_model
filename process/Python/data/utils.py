from pandas import DataFrame, Categorical
from numpy import inf as np_inf
from re import search as re_search
from pandas import cut as pd_cut


def aggregate_population(
    df: DataFrame, id_col: str, column_mappings: dict
) -> DataFrame:
    """
    Aggregates population based on a dictionary of column definitions.
    Automatically distinguishes between columns that need binning (Age)
    and columns that need strict categorical filtering (Ethnicity).

    Args:
        df: Input DataFrame with unit records.
        id_col: The column counting the individuals (e.g., 'id').
        column_mappings: Dict where Key is column name, Value is list of labels.
                         e.g. {'age': ['0-9', '10+'], 'ethnicity': ['Asian', 'Maori']}

    Returns:
        DataFrame aggregated by the keys in column_mappings.
    """

    # Work on a copy to avoid modifying the original data
    working_df = df.copy()

    # --- Helper: Check if labels look like ranges ---
    def _parse_bins_if_ranges(labels):
        """
        Returns (bins, labels) if the input looks like ranges ('0-9'),
        Returns (None, None) if the input looks like standard categories ('Asian').
        """
        parsed_data = []
        is_range_based = False

        for label in labels:
            # Check for "0-9"
            range_match = re_search(r"(\d+)\s*-\s*(\d+)", str(label))
            # Check for "80+"
            plus_match = re_search(r"(\d+)\s*\+", str(label))

            if range_match:
                is_range_based = True
                low = int(range_match.group(1))
                high = int(range_match.group(2)) + 1
                parsed_data.append({"label": label, "low": low, "high": high})
            elif plus_match:
                is_range_based = True
                low = int(plus_match.group(1))
                high = np_inf
                parsed_data.append({"label": label, "low": low, "high": high})
            else:
                # If we encounter a label like "Asian" that matches neither regex,
                # and we haven't seen range matches yet, assume it's not a range.
                pass

        if not is_range_based or not parsed_data:
            return None, None

        # Prepare bins if ranges were found
        parsed_data.sort(key=lambda x: x["low"])
        bins = [x["low"] for x in parsed_data] + [parsed_data[-1]["high"]]
        labels_sorted = [x["label"] for x in parsed_data]
        return bins, labels_sorted

    # --- Main Processing Loop ---

    for col_name, labels in column_mappings.items():
        if col_name not in working_df.columns:
            raise ValueError(f"Column '{col_name}' not found in DataFrame.")

        # 1. Check if these labels imply numerical binning
        auto_bins, auto_labels = _parse_bins_if_ranges(labels)

        if auto_bins:
            # CASE A: Numeric Binning (e.g., Age)
            working_df[col_name] = pd_cut(
                working_df[col_name], bins=auto_bins, labels=auto_labels, right=False
            )
        else:
            # CASE B: Strict Categorical (e.g., Ethnicity)
            # We convert to pd.Categorical using the EXACT labels provided.
            # Any value in the DF not in 'labels' becomes NaN and is dropped.
            working_df[col_name] = Categorical(
                working_df[col_name], categories=labels, ordered=True
            )

    # --- Aggregation ---
    group_cols = list(column_mappings.keys())

    # observed=False ensures that even if 'EU' has 0 count,
    # it appears in the result because it exists in the Categorical definition.
    result_df = (
        working_df.groupby(group_cols, observed=False)[id_col].count().reset_index()
    )

    # rename the output columns from * to *_group
    group_cols_new = {}
    for proc_key in group_cols:
        group_cols_new[proc_key] = f"{proc_key}_group"
    group_cols_new[id_col] = "count"

    return result_df.rename(columns=group_cols_new)

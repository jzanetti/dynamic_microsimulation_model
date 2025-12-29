from numpy import array, arange, clip, nan, sum
from numpy.random import binomial, choice, lognormal, normal, randint, seed, random
from pandas import DataFrame, qcut, cut, Series, Categorical, merge
from scipy.stats import norm as scipy_norm
from copy import deepcopy
from pyarrow.parquet import write_table as pq_write_table
from os.path import exists, join
from os import makedirs
import pyarrow as pa
from pyarrow.parquet import read_table as pq_read_table
from numpy import maximum, where, select
from numpy.random import normal
from process.Python.data import EPLISON

SAMPLE_DATA_DIR = "etc/sample/"
SAMPLE_DATA_CFG = {
    "household_types": [
        # --- Single Person ---
        {"code": "1A", "A": 1, "S": 0, "C": 0, "prob": 0.14},  # Single Worker
        {"code": "1S", "A": 0, "S": 1, "C": 0, "prob": 0.10},  # Single Retiree
        # --- Couples (No Kids) ---
        {"code": "2A", "A": 2, "S": 0, "C": 0, "prob": 0.15},  # DINKs / Pre-kids
        {"code": "1A+1S", "A": 1, "S": 1, "C": 0, "prob": 0.03},  # Bridging retirement
        {"code": "2S", "A": 0, "S": 2, "C": 0, "prob": 0.08},  # Retired Couple
        # --- Families with Children ---
        {"code": "2A+1C", "A": 2, "S": 0, "C": 1, "prob": 0.10},
        {"code": "2A+2C", "A": 2, "S": 0, "C": 2, "prob": 0.12},
        {"code": "2A+3C", "A": 2, "S": 0, "C": 3, "prob": 0.05},
        {"code": "1A+1C", "A": 1, "S": 0, "C": 1, "prob": 0.05},  # Solo Parent 1 kid
        {"code": "1A+2C", "A": 1, "S": 0, "C": 2, "prob": 0.04},  # Solo Parent 2 kids
        # --- Multi-Generational / Flatting ---
        {
            "code": "3A",
            "A": 3,
            "S": 0,
            "C": 0,
            "prob": 0.05,
        },  # Flatting/Adult Kids at home
        {
            "code": "2A+1S",
            "A": 2,
            "S": 1,
            "C": 0,
            "prob": 0.03,
        },  # Couple + Elderly parent
        {"code": "2A+1S+1C", "A": 2, "S": 1, "C": 1, "prob": 0.02},  # 3-Generation home
        {"code": "Others", "A": 2, "S": 0, "C": 0, "prob": 0.04},  # Catch-all
    ]
}

EMP_PARAMS = {
    "age_base_prob": {
        (0, 14): 0.00,
        (15, 19): 0.40,
        (20, 29): 0.75,
        (30, 54): 0.88,  # Prime age peak
        (55, 64): 0.7,
        (65, 74): 0.02,
        (75, 100): 0.0,
    },
    "gender_multiplier": {"Male": 1.0, "Female": 0.88},  # 12% penalty relative to base
    "education_multiplier": {
        "Not finished": 0.30,  # Significant penalty
        "School": 0.90,  # Slight penalty
        "Vocational": 1.05,  # Slight boost
        "University": 1.2,  # Boost
    },
}

WORKING_HRS = [
    {"age_group": "15-24", "gender": "Male", "hours_mean": 27.5},
    {"age_group": "15-24", "gender": "Female", "hours_mean": 23.5},
    {"age_group": "25-34", "gender": "Male", "hours_mean": 41.0},
    {"age_group": "25-34", "gender": "Female", "hours_mean": 35.0},
    {"age_group": "35-44", "gender": "Male", "hours_mean": 42.0},
    {"age_group": "35-44", "gender": "Female", "hours_mean": 29.5},
    {"age_group": "45-54", "gender": "Male", "hours_mean": 41.0},
    {"age_group": "45-54", "gender": "Female", "hours_mean": 34.0},
    {"age_group": "55-64", "gender": "Male", "hours_mean": 39.0},
    {"age_group": "55-64", "gender": "Female", "hours_mean": 33.0},
    {"age_group": "65+", "gender": "Male", "hours_mean": 26.0},
    {"age_group": "65+", "gender": "Female", "hours_mean": 22.0},
]


def simulation_sample_mortality(
    pop_input: DataFrame,
    b0: float = -4.5,
    b_age: float = 0.095,
    eth_offset_map: dict = {
        "Maori": 0.4,
        "Pacific": 0.35,
        "Asian": -0.3,
        "European": 0.0,
        "Other": 0.0,
    },
) -> DataFrame:

    pop = deepcopy(pop_input)

    pop["b_eth"] = pop["ethnicity"].map(eth_offset_map)

    # Calculate "True" rate
    # R's pnorm is the Cumulative Distribution Function (CDF).
    # In Python, this is norm.cdf from scipy.stats.
    z_score = b0 + (b_age * pop["age"]) + pop["b_eth"]
    pop["true_rate"] = scipy_norm.cdf(z_score)

    # Calculate actual number of deaths (Binomial expectation)
    # np.random.binomial is the equivalent of rbinom
    pop["deaths"] = binomial(n=1.0, p=pop["true_rate"])

    # Select specific columns
    mortality_data = pop[["age", "ethnicity", "deaths"]]
    # mortality_data = mortality_data[mortality_data["deaths"] == 1]

    bins = range(0, 111, 10)
    labels = [f"{i}-{i+9}" for i in bins[:-1]]

    # Create the 'age_group' column
    mortality_data["age_group"] = cut(
        mortality_data["age"], bins=bins, labels=labels, right=False
    )

    # Group by the new 'age_group' and 'ethnicity'
    # observed=False ensures you see all age groups even if counts are 0 (optional)
    mortality_data = (
        mortality_data.groupby(["age_group", "ethnicity"], observed=False)["deaths"]
        .sum()
        .reset_index()
    )

    # Sort for better viewing (optional, but matches data.table structure)
    mortality_data = mortality_data.sort_values(
        by=["ethnicity", "age_group"]
    ).reset_index(drop=True)

    return mortality_data.rename(columns={"ethnicity": "ethnicity_group"})


def generate_sample_supplements(required_data_types: list = ["mortality"]):

    proc_data_path = join(SAMPLE_DATA_DIR, f"pop_data.parquet")
    pop_data = pq_read_table(proc_data_path)
    pop_data = pop_data.to_pandas()

    if "mortality" in required_data_types:
        mortality_data = simulation_sample_mortality(pop_data)
        mortality_data_path = f"{SAMPLE_DATA_DIR}/mortality_data.parquet"
        pq_write_table(pa.Table.from_pandas(mortality_data), mortality_data_path)

    if "ruf" in required_data_types:
        ruf_data = simulation_sample_ruf(pop_data)
        ruf_data_path = f"{SAMPLE_DATA_DIR}/ruf_data.parquet"
        pq_write_table(pa.Table.from_pandas(ruf_data), ruf_data_path)

    if "heckman" in required_data_types:
        heckman_data = simulation_sample_heckman(pop_data)
        heckman_data_path = f"{SAMPLE_DATA_DIR}/heckman_data.parquet"
        pq_write_table(pa.Table.from_pandas(heckman_data), heckman_data_path)

def obtain_employment_status(df, params):
    df = df.copy()

    # --- A. Apply Age Probabilities (The Base) ---
    # We use pd.cut to map continuous age to the buckets in the dict

    # Extract bins and values from the dict
    bins = []
    labels = []

    # We sort keys to ensure bins are in order
    sorted_age_keys = sorted(params["age_base_prob"].keys())

    # Construct bins for pd.cut (e.g., [0, 14, 19, 29...])
    # Note: pd.cut includes the right edge by default
    bins.append(sorted_age_keys[0][0])  # Start point (0)
    for r in sorted_age_keys:
        bins.append(r[1])  # End points
        labels.append(params["age_base_prob"][r])

    # Map Age to Base Probability
    df["base_prob"] = cut(
        df["age"], bins=bins, labels=labels, include_lowest=True, ordered=False
    ).astype(
        float
    )  # Ensure float type

    # Handle NaN (ages outside defined bins)
    df["base_prob"] = df["base_prob"].fillna(0.0)

    # --- B. Apply Multipliers (Gender & Education) ---
    # simple .map() functions are very fast
    gender_factor = df["gender"].map(params["gender_multiplier"]).fillna(1.0)
    edu_factor = df["education_level"].map(params["education_multiplier"]).fillna(1.0)

    # --- C. Calculate Joint Probability ---
    # Formula: Base * Gender_Scaler * Edu_Scaler
    df["prob_employed"] = df["base_prob"] * gender_factor * edu_factor

    # --- D. Add Uncertainty (Noise) ---
    # Add +/- noise (mean 0, std 0.07)
    noise = normal(loc=0.0, scale=0.07, size=len(df))
    df["prob_employed"] = df["prob_employed"] + noise

    # --- E. Final Clip and Draw ---
    # Ensure probability is strictly 0.0 to 1.0
    df["prob_employed"] = df["prob_employed"].clip(0, 1)

    # Generate boolean status
    random_draws = random(size=len(df))
    df["is_employed"] = random_draws < df["prob_employed"]

    # Cleanup temp columns (optional)
    # df = df.drop(columns=["base_prob", "prob_employed"])

    return df


def obtain_market_income(df: DataFrame):

    # D. Market Income Calculation
    # 1. Base Wage (Annual, Full-time equivalent baseline)
    base_wage_dist = lognormal(
        mean=10.6, sigma=0.1, size=len(df)
    )  # Centered approx $40k-$50k before modifiers

    # 2. Education Multiplier
    educ_map = {
        "Not finished": 0.15,
        "School": 0.85,
        "Vocational": 1.10,
        "University": 1.55,
    }
    edu_factor = df["education_level"].map(educ_map)

    # 3. Age Multiplier (The "Career Arc")
    # Quadratic curve: Income rises, peaks around 45-50, then softens
    # Formula: 1 + 0.02*(Age-20) - 0.0003*(Age-20)^2
    # We clip age to 18 to avoid issues
    age_calc = clip(df["age"], 18, 80)
    age_factor = 1 + 0.025 * (age_calc - 20) - 0.00035 * (age_calc - 20) ** 2
    age_factor = maximum(age_factor, 0.5)  # Prevent negative or too low multipliers

    # 4. Gender Multiplier (Statistical Wage Gap)
    # 1.0 for Male, approx 0.90 for Female (10% gap)
    gender_factor = where(df["gender"] == "Male", 1.0, 0.90)

    # 5. Calculate Final Market Income
    df["market_income"] = base_wage_dist * edu_factor * age_factor * gender_factor

    # 6. Apply Unemployment (Zero market income if not employed)
    df.loc[~df["is_employed"], "market_income"] = 0

    # Rounding
    df["market_income"] = df["market_income"].round(0)

    return df


def obtain_benefit_income(df: DataFrame):
    # E. Benefit Income (Simple Superannuation)
    df["benefit_income"] = 0.0
    # NZ Super: Universal for 65+, approx 26k net
    df.loc[df["age"] >= 65, "benefit_income"] = 26000
    return df


def generate_sample_population(
    n: int = 1000, seed_num: int = 42, hh_configs=SAMPLE_DATA_CFG["household_types"]
) -> DataFrame:
    if seed_num is not None:
        seed(seed_num)

    # --- 1. Define Household Compositions ---
    probs = [x["prob"] for x in hh_configs]
    probs = array(probs) / sum(probs)

    # --- 2. Generate Households Loop ---
    people_data = []
    hh_id = 1
    current_n = 0

    while current_n < n:
        config_idx = choice(range(len(hh_configs)), p=probs)
        config = hh_configs[config_idx]
        members = []

        # -- (Existing Age Logic Preserved) --
        if config["S"] > 0 and config["A"] == 0:
            head_age = randint(65, 90)
        elif config["C"] > 0:
            head_age = int(normal(40, 8))
            head_age = clip(head_age, 20, 60)
        elif config["S"] > 0 and config["A"] > 0:
            head_age = randint(55, 70)
        else:
            head_age = int(normal(35, 12))
            head_age = clip(head_age, 18, 64)

        # Create Adults
        for i in range(config["A"]):
            age = (
                head_age if (i == 0 and config["S"] == 0) else head_age + randint(-5, 6)
            )
            members.append({"role": "Adult", "age": clip(age, 18, 64)})

        # Create Seniors
        for i in range(config["S"]):
            if config["A"] == 0 and i == 0:
                age = head_age
            elif config["A"] > 0:
                age = head_age + randint(20, 30)
            else:
                age = head_age + randint(-5, 6)
            members.append({"role": "Senior", "age": max(65, age)})

        # Create Children
        for i in range(config["C"]):
            age = head_age - randint(20, 42)
            members.append({"role": "Child", "age": clip(age, 0, 17)})

        # Assign Attributes
        hh_region = choice(
            ["Auckland", "Wellington", "Christchurch", "Others"],
            p=[0.34, 0.10, 0.08, 0.48],
        )

        for m in members:
            if current_n >= n + 50:
                break
            m.update(
                {
                    "household_id": hh_id,
                    "household_type": config["code"],
                    "region": hh_region,
                    "gender": choice(["Male", "Female"], p=[0.49, 0.51]),
                }
            )
            people_data.append(m)
            current_n += 1
        hh_id += 1

    df = DataFrame(people_data).iloc[:n].copy()
    df["id"] = arange(1, n + 1)

    # --- 3. Demographic & Socio-Economic Logic (UPDATED) ---
    # A. Ethnicity
    df["ethnicity"] = "European"
    eth_cats = ["European", "Maori", "Pacific", "Asian", "Other"]

    # Vectorized assignment for speed
    df.loc[df["age"] < 20, "ethnicity"] = choice(
        eth_cats, size=(df["age"] < 20).sum(), p=[0.50, 0.25, 0.12, 0.10, 0.03]
    )
    df.loc[(df["age"] >= 20) & (df["age"] < 65), "ethnicity"] = choice(
        eth_cats,
        size=((df["age"] >= 20) & (df["age"] < 65)).sum(),
        p=[0.60, 0.15, 0.08, 0.14, 0.03],
    )
    df.loc[df["age"] >= 65, "ethnicity"] = choice(
        eth_cats, size=(df["age"] >= 65).sum(), p=[0.85, 0.07, 0.03, 0.04, 0.01]
    )

    # B. Education Assignment (18+)
    # Adjusted to align with income multipliers
    df["education_level"] = "Not finished"  # Under 18 or no qual
    mask_edu = df["age"] >= 18
    # Distribution: No Qual/School, Vocational/Trade, Bachelor+
    df.loc[mask_edu, "education_level"] = choice(
        ["Not finished", "School", "Vocational", "University"],
        size=mask_edu.sum(),
        p=[0.1, 0.35, 0.35, 0.2],
    )

    # C. Employment Status (The "Unemployed" Logic)
    # We assign probability of being employed based on Age Group
    df = obtain_employment_status(df, EMP_PARAMS)
    df = obtain_market_income(df)
    df = obtain_benefit_income(df)
    df = obtain_working_hours(df)

    # --- 4. Final Data Cleanup ---
    hh_stats = (
        df.groupby("household_id")["age"]
        .apply(
            lambda x: Series(
                {
                    "n_adults": ((x >= 18) & (x < 65)).sum(),
                    "n_seniors": (x >= 65).sum(),
                    "n_children": (x < 18).sum(),
                }
            )
        )
        .unstack()
    )

    df = df.merge(hh_stats, on="household_id", how="left")

    df["employed"] = (df["market_income"] > 0).astype(float)
    df.loc[df["employed"] == False, "working_hours"] = EPLISON

    cols = [
        "id",
        "household_id",
        "household_type",
        "role",
        "age",
        "gender",
        "ethnicity",
        "region",
        "education_level",
        "is_employed",
        "market_income",
        "benefit_income",
        "n_adults",
        "n_seniors",
        "n_children",
        "employed",
        "working_hours",
    ]

    # Postprocess:

    # Write output
    if not exists(SAMPLE_DATA_DIR):
        makedirs(SAMPLE_DATA_DIR)

    pop_data_path = f"{SAMPLE_DATA_DIR}/pop_data.parquet"
    pq_write_table(pa.Table.from_pandas(df[cols]), pop_data_path)

    return df


def obtain_working_hours(df: DataFrame):
    df_hours = DataFrame(WORKING_HRS)

    # Optional: Set categorical order for age_group to ensure correct plotting
    age_order = list(df_hours.age_group.unique())
    df_hours["age_group"] = Categorical(
        df_hours["age_group"], categories=age_order, ordered=True
    )

    bins = [14, 24, 34, 44, 54, 64, 999]  # 150 is catch-all for old age
    # labels = ["15-24", "25-34", "35-44", "45-54", "55-64", "65+"]

    df["age_group"] = cut(df["age"], bins=bins, labels=age_order)

    pop_merged = merge(df, df_hours, on=["age_group", "gender"], how="left")

    # Method A: Simple Average (Deterministic)
    pop_merged["working_hours"] = pop_merged["hours_mean"]

    pop_merged.loc[pop_merged["working_hours"] == 0, "working_hours"] = EPLISON

    return pop_merged.drop(columns = ["hours_mean"])



def simulation_sample_ruf(df_input):
    """
    Transforms raw household census/survey data into the specific format 
    required for the dashboard/model.
    
    Args:
        df_input (pd.DataFrame): The raw dataframe containing 'id', 'household_id', 
                                 'age', 'market_income', 'working_hours', etc.
                                 
    Returns:
        pd.DataFrame: A dataframe with calculated weekly incomes, age groups, 
                      and household aggregates.
    """
    # Create a copy to avoid SettingWithCopy warnings on the original df
    df = df_input.copy()
    
    # 1. Calculate Market Income Per Week
    # Assumption: Input 'market_income' is Annual. 
    # If input is already weekly, remove the `/ 52`.
    df['market_income_per_week'] = (df['market_income'] / 52).round(0).astype(int)

    # 2. Create Age Groups
    # Bins: 0-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65+
    bins = [0, 15, 25, 35, 45, 55, 65, 150]
    labels = ['0-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65+']
    df['age_group'] = cut(df['age'], bins=bins, labels=labels, right=False)

    # 3. Create Household Market Income Per Week
    # Sums weekly income for everyone with the same household_id
    df['household_market_income_per_week'] = df.groupby('household_id')['market_income_per_week'].transform('sum')

    # 4. Create Market Income Per Hour
    # Handle division by zero for non-workers
    df['market_income_per_hour'] = df.apply(
        lambda x: x['market_income_per_week'] / x['working_hours'] if x['working_hours'] > 0 else 0.0, 
        axis=1
    )
    
    # 5. Helper Columns 
    # 'hours_mean' appears to be a direct copy of working_hours in your target schema
    df['hours_mean'] = df['working_hours']
    df['selected'] = True

    # 6. Select and Order Columns
    target_cols = [
        'household_id', 'id', 'age', 'gender', 'market_income_per_week', 
        'working_hours', 'age_group', 'hours_mean', 
        'household_market_income_per_week', 'market_income_per_hour', 'selected'
    ]
    
    # Return only the columns requested
    return df[target_cols]


def simulation_sample_heckman(df_input, weeks_per_year: int = 52):
    df_input["market_income_per_week"] = df_input["market_income"] / (
        df_input["working_hours"] * weeks_per_year
    )

    return df_input[["id", "age", "gender", "education_level", "market_income_per_week", "employed"]]

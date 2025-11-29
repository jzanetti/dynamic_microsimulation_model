from numpy import array, arange, clip, nan, sum
from numpy.random import binomial, choice, lognormal, normal, randint, seed, random
from pandas import DataFrame, qcut, cut, Series
from scipy.stats import norm as scipy_norm
from copy import deepcopy
from pyarrow.parquet import write_table as pq_write_table
from os.path import exists
from os import makedirs
import pyarrow as pa


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
    mortality_data = mortality_data[mortality_data["deaths"] == 1]

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


def add_sample_population_status(pop: DataFrame) -> DataFrame:
    pop["life_stage"] = "alive"

    return pop


def generate_sample_population(
    n: int = 1000, seed_num: int = 42, hh_configs=SAMPLE_DATA_CFG["household_types"]
) -> DataFrame:
    if seed_num is not None:
        seed(seed_num)

    # --- 1. Define Household Compositions (The Archetypes) ---
    # Probabilities approximated from Stats NZ Household Composition data
    # A = Adult (18-64), S = Senior (65+), C = Child (0-17)

    # Validate probabilities sum to 1
    probs = [x["prob"] for x in hh_configs]
    # Normalize just in case of float errors
    probs = array(probs) / sum(probs)

    # --- 2. Generate Households Loop ---
    people_data = []
    hh_id = 1
    current_n = 0

    while current_n < n:
        # Pick a configuration
        config_idx = choice(range(len(hh_configs)), p=probs)
        config = hh_configs[config_idx]

        members = []

        # --- Age Generation Logic ---
        # We need a reference "Head" age to ensure family ages make sense
        # (e.g., parents must be older than children).

        # 1. Determine Head Age based on composition type
        if config["S"] > 0 and config["A"] == 0:
            # Senior only household
            head_age = randint(65, 90)
        elif config["C"] > 0:
            # Family: Head usually 25-55
            head_age = int(normal(40, 8))
            head_age = clip(head_age, 20, 60)
        elif config["S"] > 0 and config["A"] > 0:
            # Mixed Adult/Senior: Head likely near 60
            head_age = randint(55, 70)
        else:
            # Standard Adult: 18-64
            head_age = int(normal(35, 12))
            head_age = clip(head_age, 18, 64)

        # 2. Create Adults (18-64)
        for i in range(config["A"]):
            if i == 0 and config["S"] == 0:
                # Use head age if this is the first adult and no seniors exist
                age = head_age
            else:
                # Other adults (partners/flatmates) usually close in age
                age = head_age + randint(-5, 6)

            # Force constrain to definition
            age = clip(age, 18, 64)
            members.append({"role": "Adult", "age": age})

        # 3. Create Seniors (65+)
        for i in range(config["S"]):
            if config["A"] == 0 and i == 0:
                age = head_age  # Head is the senior
            else:
                # If living with adults, might be much older (parent) or same age (partner)
                if config["A"] > 0:
                    age = head_age + randint(20, 30)  # Likely parent of the adult
                else:
                    age = head_age + randint(-5, 6)  # Partner of senior head

            age = max(65, age)  # Enforce definition
            members.append({"role": "Senior", "age": age})

        # 4. Create Children (0-17)
        for i in range(config["C"]):
            # Kids usually 20-40 years younger than head
            age = head_age - randint(20, 42)
            age = clip(age, 0, 17)  # Enforce definition
            members.append({"role": "Child", "age": age})

        # --- Assign Common Attributes ---
        # Region: Assigned per household so they live together
        hh_region = choice(
            ["Auckland", "Wellington", "Christchurch", "Others"],
            p=[0.34, 0.10, 0.08, 0.48],
        )

        for m in members:
            # Stop if we hit exact N limit (optional, depends if you want partial families)
            if current_n >= n + 50:
                break

            m["household_id"] = hh_id
            m["household_type"] = config["code"]
            m["region"] = hh_region
            m["gender"] = choice(["Male", "Female"], p=[0.49, 0.51])
            people_data.append(m)
            current_n += 1

        hh_id += 1

    # Convert to DataFrame
    df = DataFrame(people_data).iloc[:n].copy()
    df["id"] = arange(1, n + 1)

    # --- 3. Demographic & Socio-Economic Logic (Applied to Individuals) ---
    # Ethnicity (Age Dependent)
    df["ethnicity"] = "European"
    eth_cats = ["European", "Maori", "Pacific", "Asian", "Other"]

    mask_young = df["age"] < 20
    if mask_young.any():
        df.loc[mask_young, "ethnicity"] = choice(
            eth_cats, size=mask_young.sum(), p=[0.50, 0.25, 0.12, 0.10, 0.03]
        )

    mask_adult = (df["age"] >= 20) & (df["age"] < 65)
    if mask_adult.any():
        df.loc[mask_adult, "ethnicity"] = choice(
            eth_cats, size=mask_adult.sum(), p=[0.60, 0.15, 0.08, 0.14, 0.03]
        )

    mask_old = df["age"] >= 65
    if mask_old.any():
        df.loc[mask_old, "ethnicity"] = choice(
            eth_cats, size=mask_old.sum(), p=[0.85, 0.07, 0.03, 0.04, 0.01]
        )

    # Education (Only relevant for 18+)
    df["education_level"] = "None"
    mask_edu = df["age"] >= 18
    df.loc[mask_edu, "education_level"] = choice(
        ["Low", "Medium", "High"], size=mask_edu.sum(), p=[0.35, 0.40, 0.25]
    )

    # Employment (Only 18-64)
    df["employment_status"] = "Not in Labor Force"
    df["labour_income"] = 0.0
    mask_work = (df["age"] >= 18) & (df["age"] < 65)

    if mask_work.any():
        df.loc[mask_work, "employment_status"] = choice(
            ["Employed", "Unemployed"], size=mask_work.sum(), p=[0.95, 0.05]
        )

    # Wages
    base_wage = lognormal(mean=10.6, sigma=0.5, size=n)
    educ_map = {"None": 0, "Low": 0.85, "Medium": 1.0, "High": 1.35}
    df["wage_offer"] = base_wage * df["education_level"].map(educ_map)

    emp_mask = df["employment_status"] == "Employed"
    df.loc[emp_mask, "labour_income"] = df.loc[emp_mask, "wage_offer"]

    # Wealth & Investments
    age_factor = df["age"] / 50.0
    wealth_base = lognormal(mean=10.5, sigma=1.8, size=n)
    df["wealth_stock"] = wealth_base * age_factor
    df.loc[df["age"] < 18, "wealth_stock"] = 0
    df["investment_income"] = df["wealth_stock"] * 0.04

    # NZ Super (Universal for 65+)
    # Note: We don't check employment status, as you can work and get Super,
    # though tax rates differ. We just add the income.
    super_mask = df["age"] >= 65
    df.loc[super_mask, "labour_income"] += 26000  # Approx annual net super

    # --- 4. Final Aggregations ---
    df["disposable_income"] = df["labour_income"] + df["investment_income"]

    # Calculate Household Composition Columns (for easy filtering later)
    # We group by ID and count specific age brackets
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

    # Join these counts back to the main dataframe
    df = df.merge(hh_stats, on="household_id", how="left")

    # Reorder for clarity
    cols = [
        "id",
        "household_id",
        "household_type",
        "role",
        "age",
        "n_adults",
        "n_seniors",
        "n_children",
        "gender",
        "region",
        "ethnicity",
        "disposable_income",
    ]
    remaining = [c for c in df.columns if c not in cols]

    pop_data = df[cols + remaining]
    pop_data = add_sample_population_status(pop_data)
    mortality_data = simulation_sample_mortality(pop_data)

    # Write output to parquet
    pop_data_path = f"{SAMPLE_DATA_DIR}/pop_data.parquet"
    mortality_data_path = f"{SAMPLE_DATA_DIR}/mortality_data.parquet"

    if not exists(SAMPLE_DATA_DIR):
        makedirs(SAMPLE_DATA_DIR)

    pq_write_table(pa.Table.from_pandas(pop_data), pop_data_path)
    pq_write_table(pa.Table.from_pandas(mortality_data), mortality_data_path)

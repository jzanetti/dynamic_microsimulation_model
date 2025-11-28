
from numpy.random import choice, normal, lognormal
from numpy import arange
from numpy import nan
from pandas import DataFrame
import numpy as np
import pandas as pd

def generate_sample_population(n: int = 1000, seed: int or None = 42) -> DataFrame:
    """
    Generate sample population data, sort of matching NZ demographics.
    
    Args:
        n (int, optional): Number of population. Defaults to 1000.
    """
    np.random.seed(seed)  # For reproducibility

    # --- 1. Basic Demographics (Weighted to NZ Stats) ---
    ids = np.arange(1, n + 1)
    
    # NZ Age structure approximation (skewed towards younger/mid, tapering at old age)
    # Probabilities for 0-14, 15-29, 30-44, 45-59, 60-74, 75+
    age_bins = [0, 15, 30, 45, 60, 75, 100]
    age_weights = [0.19, 0.20, 0.20, 0.19, 0.15, 0.07] 
    
    # Generate broad age bins first, then uniform within bin
    age_groups = np.random.choice(len(age_bins)-1, n, p=age_weights)
    ages = np.array([np.random.randint(age_bins[i], age_bins[i+1]) for i in age_groups])
    
    # Gender (approx 51% female in NZ)
    genders = np.random.choice(['Male', 'Female'], n, p=[0.49, 0.51])
    
    # Region (Stats NZ approx: Auck 34%, Wlg 10%, Chch 8%, Others 48%)
    regions = np.random.choice(
        ['Auckland', 'Wellington', 'Christchurch', 'Others'], 
        n, 
        p=[0.34, 0.10, 0.08, 0.48]
    )

    df = pd.DataFrame({
        'id': ids,
        'age': ages,
        'gender': genders,
        'region': regions
    })

    # --- 2. Education Logic (Dependent on Age) ---
    # Init as None
    df['education_level'] = 'None'
    df['in_education'] = False

    # School age (5-17)
    mask_school = (df['age'] >= 5) & (df['age'] <= 17)
    df.loc[mask_school, 'in_education'] = True
    df.loc[mask_school, 'education_level'] = 'Low'

    # Adult Education Distribution (Approx NZ: 25% High, 40% Med, 35% Low)
    mask_adult = df['age'] >= 18
    # Assign weighted education to adults
    educ_probs = [0.35, 0.40, 0.25] # Low, Med, High
    df.loc[mask_adult, 'education_level'] = np.random.choice(
        ['Low', 'Medium', 'High'], size=mask_adult.sum(), p=educ_probs
    )
    
    # University students (Age 18-24 often in education)
    mask_uni = (df['age'] >= 18) & (df['age'] <= 24)
    df.loc[mask_uni, 'in_education'] = np.random.choice([True, False], size=mask_uni.sum(), p=[0.4, 0.6])

    # --- 3. Employment & Income (Life-cycle Hypothesis) ---
    df['employment_status'] = 'Not in Labor Force'
    df['labour_income'] = 0.0
    
    # Working age population (18-64)
    mask_work_age = (df['age'] >= 18) & (df['age'] < 65)
    
    # Employment rate approx 95% (simplified for simulation)
    emp_status_choices = np.random.choice(
        ['Employed', 'Unemployed'], size=mask_work_age.sum(), p=[0.95, 0.05]
    )
    df.loc[mask_work_age, 'employment_status'] = emp_status_choices

    # Calculate Wages based on Education Premium
    # Base wage ~50k, multipliers for Ed level
    base_wage = np.random.lognormal(mean=10.8, sigma=0.4, size=n) # ~50k median
    
    educ_multiplier = {'None': 0, 'Low': 0.8, 'Medium': 1.0, 'High': 1.4}
    df['wage_offer'] = base_wage * df['education_level'].map(educ_multiplier)
    
    # Only employed people get labour income
    df.loc[df['employment_status'] == 'Employed', 'labour_income'] = \
        df.loc[df['employment_status'] == 'Employed', 'wage_offer']

    # --- 4. Wealth & Investment (Accumulation over time) ---
    # Wealth correlates strongly with Age. Young = Low, Old = High.
    # We add an age factor to the lognormal distribution
    age_factor = (df['age'] / 50.0) # Peaks around 50-60
    wealth_base = np.random.lognormal(mean=11, sigma=1.5, size=n) # Higher variance
    df['wealth_stock'] = wealth_base * age_factor
    df.loc[df['age'] < 18, 'wealth_stock'] = 0 # Kids have no money
    
    # Investment income approx 3% of wealth
    df['investment_income'] = df['wealth_stock'] * 0.03

    # --- 5. Health & Children ---
    # Health declines with age
    # Base 5, subtract age penalty, add random variance
    health_raw = 5.5 - (df['age'] / 25.0) + np.random.normal(0, 0.5, n)
    df['health_status'] = np.clip(np.round(health_raw), 1, 5).astype(int)
    
    df['disability'] = False
    # Disability correlates with poor health/age
    mask_disabled = (df['health_status'] <= 2) & (np.random.random(n) < 0.3)
    df.loc[mask_disabled, 'disability'] = True

    df['ghq_score'] = np.random.normal(12, 5, n)

    # Children (Assigned mostly to 25-50 age bracket)
    df['num_children'] = 0
    mask_parents = (df['age'] >= 25) & (df['age'] <= 55)
    df.loc[mask_parents, 'num_children'] = np.random.choice(
        [0, 1, 2, 3, 4], size=mask_parents.sum(), p=[0.3, 0.2, 0.3, 0.15, 0.05]
    )

    # --- 6. Total Income ---
    # Add NZ Super for 65+ (Approx 25k)
    super_mask = df['age'] >= 65
    df.loc[super_mask, 'labour_income'] += 25000 
    
    df['disposable_income'] = df['labour_income'] + df['investment_income']

    # --- 7. Lagged Variables (Initialized to current state for simplicity) ---
    df['lag_health_status'] = df['health_status']
    df['lag_employment_status'] = df['employment_status']
    # Quintiles
    df['lag_income_quintile'] = pd.qcut(df['disposable_income'].rank(method='first'), 5, labels=[1,2,3,4,5])

    # --- 8. Partnerships (Assortative Mating by Age) ---
    df['partner_id'] = np.nan
    
    # Filter singles > 18
    eligible_singles = df[df['age'] >= 18].copy()
    
    # Add small noise to age to shuffle within same-age groups, then sort
    # This ensures 25yos get paired with 24-26yos, not random 60yos.
    eligible_singles['sort_key'] = eligible_singles['age'] + np.random.normal(0, 2, len(eligible_singles))
    eligible_singles = eligible_singles.sort_values('sort_key')
    
    single_ids = eligible_singles['id'].values
    
    # Pair up adjacent sorted IDs (approx 60% of adults are partnered)
    # We step by 2 to create pairs
    for i in range(0, len(single_ids) - 1, 2):
        # 70% chance they are a couple
        if np.random.random() < 0.70:
            p1, p2 = single_ids[i], single_ids[i+1]
            df.loc[df['id'] == p1, 'partner_id'] = p2
            df.loc[df['id'] == p2, 'partner_id'] = p1

    return df
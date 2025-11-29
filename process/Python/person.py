
from pandas import DataFrame

def forward(pop_data: dict) -> DataFrame:
  
    pop = pop_data["population"]
    mortality_data = pop_data["params"]["mortality"]

    pop["age"] += 1

    # Obtain mortality probabilities



  # 1. Increment Age
  pop[, age := age + 1]
  
  # 2. Mortality (Probit)
  # Probit P(Death) = pnorm(beta0 + beta1*Age + ...)
  pop[, prob_death := pnorm(PARAMS$mortality$intercept + PARAMS$mortality$age_coef * age)]
  
  # Random draw
  pop[, death_draw := runif(.N)]
  
  # Identify deaths
  deaths <- pop[death_draw < prob_death, id]
  
  # Remove dead agents (in a real sim, we might archive them instead)
  if(length(deaths) > 0) {
    cat(sprintf("   -> Deaths this year: %d\n", length(deaths)))
    pop <- pop[!id %in% deaths]
  }
  
  # 3. Child Maturation & Leaving Home
  # (Placeholder: Randomly assign new households to 19-year-olds)
  # Real logic: Probit on leaving home
  
  # Cleanup temp cols
  pop[, c("prob_death", "death_draw") := NULL]
  return(pop)
}
# Dynamic Microsimulation Model (DMM) - Core

This is the core model for the Dynamic Microsimulation Model (DMM). The model is available in both `Python` and `R`. 

The model is created based on the paper [Bronka et al.](https://www.microsimulation.pub/articles/00318)


## How to install the package


## A quick start:

### Step 1: Create a sample population dataset

A sample population dataset can be created using:
```python
from process.Python.data import generate_sample_population
sample_pop = generate_sample_population()
```
Or in R:
```R
source("process/R/deps.R")
source("process/R/data.R", local = data_env)
sample_pop <- data_env$generate_population()
```


## Developers

### Python
The package environment can be installed using `conda env create -f env.yml`




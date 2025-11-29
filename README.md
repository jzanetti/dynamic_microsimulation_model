# Dynamic Microsimulation Model (DMM) - Core

This is the core model for the Dynamic Microsimulation Model (DMM). The model is available in both `Python` and `R`.

The model is created based on the paper [Bronka et al.](https://www.microsimulation.pub/articles/00318)

## How to install the package

## A quick start:

### Step 1: Create a sample population dataset

A sample population dataset can be created using:

```python
from process.Python.data.sample import generate_sample_population
generate_sample_population()
```

Or in R:

```R
source("process/R/deps.R")
source("process/R/data/sample.R", local = sample_env)
sample_env$generate_population()
```

Sample data is written in the directory `etc/sample` in the `parquet` format

## Developers

### Python

The package environment can be installed using `conda env create -f env.yml`

# Dynamic Microsimulation Model (DMM) Core

This repository contains the core implementation of a **Dynamic Microsimulation Model (DMM)**, designed for projecting individual and household life-course trajectories. The model supports analyses in domains such as demography, education, health, household composition, labour supply, and policy impacts (e.g., tax-benefit systems).

The core model is implemented in **both Python and R**, allowing users to choose their preferred language while maintaining equivalent functionality. Inspired by open-source dynamic microsimulation frameworks, this implementation provides a flexible, modular structure for research and policy analysis.

## Features

- Modular design for easy extension and customization
- Support for partial modules (e.g., employment behaviour only) or full dynamic simulations
- Cross-language compatibility (Python ↔ R)

## Python vs. R: Side-by-Side Comparison

| Aspect                      | Python                                      | R                                           |
|-----------------------------|---------------------------------------------|---------------------------------------------|
| **Environment Setup**       | `conda create -n ddm_core python=3.XX`<br>`conda activate ddm_core` | `renv::init(bare = TRUE)`<br>`renv::hydrate()` |
| **Run Employment Behaviour Module** | `examples/run_behaviour_model.py`          | `examples/run_behaviour_model.R`            |
| **Run Full Dynamic Model**  | `examples/run_full.py`                      | `examples/run_full.R`                       |

Both implementations produce comparable results and share the same example structure.

## Installation

### Python
Use `conda` for reproducible environments
```bash
# Recommended: Create a conda environment
conda create -n ddm_core python=3.10
conda activate ddm_core
```

### R:
Use `renv` for reproducible environments
```R
renv::init(bare = TRUE)
renv::hydrate()
```

## Quick Start
The model can be run in two modes:

- Employment Behaviour Module Only – Useful for testing specific components.
- Full Dynamic Model – Complete life-course simulation.

### Examples
Run the scripts in the `examples/` directory:

- Employment behaviour:
  - `Python`: `examples/run_behaviour_model.py`
  - `R`: `examples/run_behaviour_model.R`

- Full model:
  - `Python`: `python examples/run_full.py`
  - `R`: `examples/run_full.R`


These scripts demonstrate data loading, model initialization, simulation runs, and basic output inspection.

## Contributing
Contributions are welcome! Please open issues or pull requests.

## Developers
TBA. Contact the maintainers for collaboration opportunities.

## License
TBA
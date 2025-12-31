library(devtools)
library(data.table)
library(ggplot2)
library(base64enc)
library(yaml)
library(dplyr)
library(caret)
library(stringr)
library(tidyr)
library(arrow)
library(purrr)
library(stats)
library(gridExtra)
library(jsonlite)
library(digest)
library(arrow)
library(data.table)
library(readr)
library(lbfgsb3c)
library(nloptr)
library(xgboost)
library(patchwork)
# ------------------------
# Local environments
# ------------------------
data_sample_env <- new.env()
data_utils_env <- new.env()
data_input_env <- new.env()
data_output_env <- new.env()
data_filename_env <- new.env()
data_tawa_env <- new.env()
vis_env <- new.env()
model_linear_env <- new.env()
model_xgboost_env <- new.env()
model_wrapper_env <- new.env()
model_utils_env <- new.env()
model_ruf_env <- new.env()
model_validation_env <- new.env()
model_tax_calculator_env <- new.env()
dmm_env <- new.env()
person_env <- new.env()
mortality_env <- new.env()
employment_env <- new.env()
model_heckman_wage_env <- new.env()


# ------------------------
# Constants
# ------------------------
TEST_RUN = FALSE
RUN_LOG = TRUE
RUF_METHOD = NULL # top30 or NULL
TAX_BRACKETS <- list(
  list(180000, 0.39),
  list(78100, 0.33),
  list(53500, 0.30),
  list(15600, 0.175),
  list(0, 0.105)
)
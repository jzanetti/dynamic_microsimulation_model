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


# ------------------------
# Local environments
# ------------------------
data_sample_env <- new.env()
data_utils_env <- new.env()
data_input_env <- new.env()
vis_env <- new.env()
model_linear_env <- new.env()
model_wrapper_env <- new.env()
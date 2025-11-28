library(devtools)
library(data.table)
library(ggplot2)
library(base64enc)
library(yaml)
library(dplyr)

# ------------------------
# Local environments
# ------------------------
sample_env <- new.env()
vis_env <- new.env()
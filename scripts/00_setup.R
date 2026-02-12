# 00_setup.R
# Setup for ML model + project paths

library(tidyverse)
library(tidymodels)
library(textrecipes)
library(rvest)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(tibble)
library(readr)
library(zoo)
library(tokenizers)
library(kernlab)

tidymodels_prefer()
set.seed(1)

# Paths (single source of truth) 
source(file.path("scripts", "Paths.R"))

# Ensure key directories exist
fs::dir_create(paths$data, recurse = TRUE)
fs::dir_create(paths$raw, recurse = TRUE)
fs::dir_create(paths$interim, recurse = TRUE)
fs::dir_create(paths$output, recurse = TRUE)
fs::dir_create(paths$archive, recurse = TRUE)

fs::dir_create(paths$confidence_build, recurse = TRUE)
fs::dir_create(paths$confidence_test, recurse = TRUE)
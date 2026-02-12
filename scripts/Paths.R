# Paths
# Acts as a single source of truth - tells every script where to read/write

# paths00.R
# Centralised paths + small I/O helpers
# Loads once at the top of scripts so everything reads/writes consistently.

library(fs)

# Project root
# Uses the current working directory as root.
# If you use an RStudio Project, setwd() will already be the project root.
# scripts/paths00.R
# Centralised project paths (relative to project root)

paths <- list(
  root    = getwd(),                         # assumes you set WD to project root (recommended via .Rproj)
  data    = file.path(getwd(), "data"),
  raw     = file.path(getwd(), "data", "raw"),
  interim = file.path(getwd(), "data", "interim"),
  output  = file.path(getwd(), "data", "output"),
  scripts = file.path(getwd(), "scripts"),
  archive = file.path(getwd(), "archive")
)

# Convenience subpaths you actually use
paths$pbp_raw <- file.path(paths$raw, "Raw PBP Data")

paths$pbp_raw_test <- file.path(paths$raw, "Hybrid Model Test Events")

paths$gold1 <- file.path(paths$raw, "MMA Foul Analysis Gold Standard Table.csv")

paths$gold2 <- file.path(paths$raw, "MMA Foul Analysis Gold Standard Table 2.csv")

paths$gold_test <- file.path(paths$raw, "Hybrid Test Gold Standard Table.csv")

paths$confidence_build <- file.path(paths$interim, "confidence_build_model")

paths$sentences <- file.path(paths$interim, "training_sentences")

paths$confidence_test <- file.path(paths$interim, "confidence_test_model")

paths$model_v0_all <- file.path(paths$output, "model_v0_all_events.csv")

paths$model_v2_clean <- file.path(paths$output, "model_v2_clean.csv")

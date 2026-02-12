# Build rule-confidence sentence dataset across events and run quick diagnostics
source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

# Note: no setwd() in the Github version - handled in 00_paths.R: setwd("/Users/lucanasillo/Documents/MMA Foul Analysis")
#Packages
library(purrr)
library(readr)
library(dplyr)
library(stringr)


# Define event IDs (NOT filenames)

confidence_event_ids <- c(
  # Former confidence_events (training group A)
  "UFC-tsarukyan-hooker-22-11-2025",
  "UFC321-aspinall-gane-25-10-2025",
  "UFC-imavov-borralho-06-09-2025",
  "UFCvegas-105-emmett-murphy-05-04-2025",
  "UFC-covington-buckley-14-12-2024",
  "UFC-lewis-teixeira-12-06-2025",
  "UFC322-dellamaddalena-makhachev-15-11-2025",
  "UFCvegas-107-blanchfield-barber-31-05-2025",
  "UFCvegas-106-burns-morales-17-05-2025",
  "UFC320-ankalaev-pereira-04-10-2025",
  "UFC-deridder-allen-18-10-2025",
  
  # Former confidence_events2 (training group B)
  "UFC-lopes-silva-13-09-2025",
  "UFC-oliveira-gamrot-11-10-2025",
  "UFC-ulberg-reyes-27-09-2025",
  "UFC-walker-zhang-23-08-2025",
  "UFC-whittaker-deridder-26-07-2025",
  "UFC318-holloway-poirier-19-07-2025",
  "UFC319-duplessis-chimaev-16-08-2025",
  "UFC323-dvalishvili-yan-06-12-2025",
  "UFCVegas-108-taira-park-02-08-2025",
  "UFCVegas-109-dolidze-hernandez-09-08-2025",
  "UFCVegas-110-garcia-onama-01-11-2025",
  "UFCVegas-111-bonfim-brown-08-11-2025"
)

# (Optional) guard against duplicates
stopifnot(length(confidence_event_ids) == length(unique(confidence_event_ids)))


# Convert event IDs -> filenames -> full paths

confidence_files <- paste0("sentence_conf_", confidence_event_ids, ".csv")
confidence_paths <- file.path(paths$confidence_build, confidence_files)

# Fail early if anything is missing (nice for debugging)
missing_files <- confidence_files[!file.exists(confidence_paths)]
if (length(missing_files) > 0) {
  stop("Missing sentence_conf files in confidence_build:\n", paste(missing_files, collapse = "\n"))
}


# Load and bind

confidence_list_all <- purrr::map_dfr(
  confidence_paths,
  readr::read_csv,
  show_col_types = FALSE
)

View(confidence_list_all)


# Diagnostic Summary: Compare rule_conf distributions for foul vs non-foul sentences. Trying to allign the rule confidences with the model's has_foul logic as the confidences were a later addition

confidence_list_all %>% summarise(
  mean_conf_hasfoul = mean(rule_conf[has_foul]),
  mean_conf_notfoul = mean(rule_conf[!has_foul]),
  p95_notfoul = quantile(rule_conf[!has_foul], 0.95),
  p95_hasfoul = quantile(rule_conf[has_foul], 0.95)
)

# Inspect top-scoring non-foul sentences (rule false-positive candidates)

confidence_list_all %>%
  filter(!has_foul) %>%
  arrange(desc(rule_conf)) %>%
  select(sentence, rule_conf, rule_reasons_pos, rule_reasons_neg) %>%
  head(20)


# Inspect true fouls ranked by rule_conf (sanity check / threshold intuition)

confidence_diag <- filter(confidence_list_all, has_foul == TRUE) %>% 
  arrange(desc(rule_conf))



# Export: true-foul sentences with confidence scores for manual review

write_csv(confidence_diag, file.path(paths$interim, "confidence_true.csv"))


# Threshold check: count fouls below threshold and non-fouls above threshold
confidence_list_all %>%
  summarise(
    nonfoul_above_07 = sum(!has_foul & rule_conf >= 0.7),
    foul_below_07    = sum(has_foul & rule_conf < 0.7),
    total_nonfoul     = sum(!has_foul),
    total_foul        = sum(has_foul)
  )

# Next step: join rule confidence + ML probabilities to build the hybrid scoring table
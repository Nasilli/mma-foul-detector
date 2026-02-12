# 02_build_sentence_datasets.R

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

library(dplyr)
library(purrr)
library(stringr)
# Building the sentence-level datasets for ML foul detection (train/test), fouls labelled using rule-based detections, feature engineering via add_flags()
# Fouls determined by model_v0 and model_v2, hence ml_df and ml_df2, were confirmed to be 100% precise before used for training.

# Training Events A: sent_all objects created by process_event_v2
# Build sentence-level table + labels from model_v0_clean (created using prior iteration of process_event)
# Load sent_all_dbg CSVs from disk
read_sent_dbg <- function(event_id) {
  f <- file.path(paths$sentences, paste0("sent_all_dbg__", event_id, ".csv"))
  if (!file.exists(f)) stop("Missing sentence file: ", f)
  readr::read_csv(f, show_col_types = FALSE) %>%
    mutate(event = event_id)
}

# Named list ensures event ID is preserved when binding rows
# 00_paths.R note: this is one of the places you’ll eventually replace hardcoded lists with something like “load all training sent_all outputs from disk” (or auto-detect events).

training_event_ids <- c(
  "UFC-deridder-allen-18-10-2025",
  "UFC320-ankalaev-pereira-04-10-2025",
  "UFCvegas-106-burns-morales-17-05-2025",
  "UFC-covington-buckley-14-12-2024",
  "UFC-imavov-borralho-06-09-2025",
  "UFC-lewis-teixeira-12-06-2025",
  "UFC-tsarukyan-hooker-22-11-2025",
  "UFC321-aspinall-gane-25-10-2025",
  "UFC322-dellamaddalena-makhachev-15-11-2025",
  "UFCvegas-105-emmett-murphy-05-04-2025",
  "UFCvegas-107-blanchfield-barber-31-05-2025"
)

sent_all_all_events_training <- purrr::map_dfr(training_event_ids, read_sent_dbg)
# Create normalised join key for matching sentences across tables
sent_all_labeled <- sent_all_all_events_training %>%
  mutate(
    sentence_key = str_squish(str_to_lower(sentence))
  )


# Creating lookup of sentences flagged as fouls by the rule-based model (training period)
model_v0_keyed <- model_v0_clean %>%
  mutate(
    sentence_key = str_squish(str_to_lower(context))
  ) %>%
  select(event, sentence_key) %>%
  distinct()

# Binary labels: any sentence appearing in model output is treated as a foul (1)
foul_labels <- model_v0_keyed %>% 
  select(event, sentence_key) %>% 
  distinct() %>% 
  mutate(has_foul =1L)

# Left join labels onto all sentences & default to non-foul (0) when no match
sent_all_labeled <- sent_all_labeled %>% 
  left_join(
    foul_labels,
    by = c("event", "sentence_key")
  )%>% 
  mutate(
    has_foul.y = if_else(is.na(has_foul.y), 0L, has_foul.y)
  ) %>% 
  rename(has_foul = has_foul.y)

# Keep minimal fields needed for ML dataset construction
# 00_paths.R note: model_v0_clean should eventually be read from an output path (not assumed to exist in memory).
sent_all_labeled <- sent_all_labeled %>% 
  select(bout, sentence_key, sid, has_foul, event)

# Training Events B :sent_all objects created by process_event_v2 
# Upcoming events were originally to be used for testing a prior iteration of the ML model but now are joined with the previous model as one large training/testing dataset (split applied to avoid testing on trained data)
# # Build sentence-level table + labels from model_v2_clean (created using process_event_v2 and confirmed to be a 100% match with the relevant gold standard table)
test_event_ids <- c(
  "UFC323-dvalishvili-yan-06-12-2025",
  "UFCVegas-111-bonfim-brown-08-11-2025",
  "UFCVegas-110-garcia-onama-01-11-2025",
  "UFC-oliveira-gamrot-11-10-2025",
  "UFC-ulberg-reyes-27-09-2025",
  "UFC-lopes-silva-13-09-2025",
  "UFC-walker-zhang-23-08-2025",
  "UFC319-duplessis-chimaev-16-08-2025",
  "UFCVegas-109-dolidze-hernandez-09-08-2025",
  "UFCVegas-108-taira-park-02-08-2025",
  "UFC-whittaker-deridder-26-07-2025",
  "UFC318-holloway-poirier-19-07-2025"
)

sent_all_all_events_test <- purrr::map_dfr(test_event_ids, read_sent_dbg)

# Named list ensures event ID is preserved when binding rows



# Create normalised join key for matching sentences across tables
sent_all_labeled_test <- sent_all_all_events_test %>%
  mutate(
    sentence_key = str_squish(str_to_lower(sentence))
  )

# Creating lookup of sentences flagged as fouls by the rule-based model
model_v2_keyed <- model_v2_clean %>%
  mutate(
    sentence_key = str_squish(str_to_lower(context))
  ) %>%
  select(event, sentence_key) %>%
  distinct()

# Binary labels: any sentence appearing in model output is treated as a foul (1)
foul_labels_test <- model_v2_keyed %>% 
  select(event, sentence_key) %>% 
  distinct() %>% 
  mutate(has_foul =1L)

# Left join labels onto all sentences & default to non-foul (0) when no match
sent_all_labeled_test <- sent_all_labeled_test %>% 
  left_join(
    foul_labels_test,
    by = c("event", "sentence_key")
  )%>% 
  mutate(
    has_foul.y = if_else(is.na(has_foul.y), 0L, has_foul.y)
  ) %>% 
  rename(has_foul = has_foul.y)

# Keep minimal fields needed for ML dataset construction
# 00_paths.R note: model_v0_clean should eventually be read from an output path (not assumed to exist in memory).
sent_all_labeled_test <- sent_all_labeled_test %>% 
  select(bout, sentence_key, sid, has_foul, event)


# Formal test dataset into ML schema (bout, sid, text, has_foul)

test_df <- sent_all_labeled_test %>%
  transmute(
    bout = bout,
    sid = sid,
    text = sentence_key,
    has_foul = factor(has_foul, levels = c(0, 1))
  ) %>%
  filter(!is.na(text), text != "")

# Formal test dataset into ML schema (bout, sid, text, has_foul)
test2_df <- sent_all_labeled %>%
  transmute(
    bout = bout,
    sid = sid,
    text = sentence_key,
    has_foul = factor(has_foul, levels = c(0, 1))
  ) %>%
  filter(!is.na(text), text != "")

# Applying same feature engineering used in training (keyword flags, regex indicators, etc.)
test_df2 <- add_flags(test_df)

test2_df <- add_flags(test2_df)



# Ensure target (has_foul) has identical factor levels in both datasets
test2_df  <- test2_df  %>% mutate(has_foul = factor(has_foul, levels = c(0,1)))
test_df2  <- test_df2  %>% mutate(has_foul = factor(has_foul, levels = c(0,1)))

# Keep only shared columns to prevent schema mismatch when binding
common_cols <- intersect(names(test2_df), names(test_df2))

# Combine Events A & Events B into one labelled dataset: downstream scripts create train/test splits
ml_data <- bind_rows(
  test_df2 %>% select(all_of(common_cols)) %>% mutate(source = "test_df2"),
  test2_df %>% select(all_of(common_cols)) %>% mutate(source = "test2_df")
)

# ========================= IMPORTANT: Reproducibility Note =========================
# This script can be run in TWO ways:
#
# (A) Reproduce the accuracy metrics reported in this project (recommended):
#     - You MUST use the exact same Sherdog play-by-play HTML files used to build the blind test set.
#     - Download those HTML pages and place them in:
#           data/raw/Hybrid Model Test Events/
#       (or whatever `paths$pbp_raw_test` points to)
#     - The gold standard file `Hybrid Test Gold Standard Table.csv` must match those exact events/sentences.
#     - If you use different HTML files, the gold table will not align and the precision/recall numbers will be invalid.
#
# (B) Run the hybrid detector on NEW/OTHER events (no comparable accuracy metrics):
#     - You may place any Sherdog play-by-play HTML files in the folder and the script will produce predictions.
#     - HOWEVER: the reported accuracy metrics will NOT be meaningful unless you create a new gold standard
#       table for those exact events and ensure sentence normalisation/join keys match.
#
# In short: accuracy metrics require the same HTML set + matching gold table.
# ================================================================================

# Purpose: Validate the Hybrid foul-detection logic on a fully blind set of events (separate HTML + separate gold standard table)

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))
source(file.path("scripts", "process_event_v2.R"))

# This script rebuilds sentence-level data, joins rule + ML confidences, applies the hybrid decision rule and evaluate precision/recall vs gold


library(purrr)
library(dplyr)

test_dir <- paths$pbp_raw_test

test_html_files <- list.files(
  path = test_dir,
  pattern = "\\.html$",
  full.names = TRUE
)

# Run parser across all HTMLs in the folder
test_model_v2 <- purrr::map_df(test_html_files, process_event_v2)

# Note: test_model_v2 is the raw detector output for these events; this test is evaluated at the sentence level, so the downstream pipeline uses sent_all_dbg + gold contexts instead of model_v2 rows


# --- Helper: extract event_id from html filename ---
# Assumes html files are named like: "UFC-317-topuria-oliveira-28-06-2025.html"
get_event_id_from_html <- function(path) {
  nm <- basename(path)
  str_remove(nm, "\\.html$")
}

# --- Helper: read sent_all dbg CSV for an event_id ---
# Assumes your saved sentence files are named like:
# data/interim/training_sentences/sent_all_dbg__<event_id>.csv
read_sent_dbg <- function(event_id) {
  fp <- file.path(paths$sentences, paste0("sent_all_dbg__", event_id, ".csv"))
  if (!file.exists(fp)) {
    stop("Missing sent_all_dbg file for event_id: ", event_id, "\nExpected: ", fp)
  }
  readr::read_csv(fp, show_col_types = FALSE) %>%
    mutate(event = event_id)
}

# --- Choose which event_ids to use for THIS run ---
# Option 1 (recommended): derive from whatever HTML files are in the folder
test_event_ids <- purrr::map_chr(test_html_files, get_event_id_from_html)

sent_all_all_hybrid_events_test <- purrr::map_dfr(test_event_ids, read_sent_dbg)

# Standardise to the schema you use downstream
sent_all_labeled_hybrid_test <- sent_all_all_hybrid_events_test %>%
  transmute(
    event,
    bout,
    sid,
    sentence_key = sentence
  ) %>%
  mutate(sentence_key = str_squish(str_to_lower(sentence_key)))


# Gold labels (blind set): each gold row represents a true foul sentence (Context).
# Converts gold contexts into has_foul=1 labels; all other sentences in the event are has_foul=0

## IF YOU DIDNT DOWNLOAD THE FILES I USED THEN YOU CANNOT CHECK ACCURACY OF THE OUTPUT

gold_hybrid <- readr::read_csv(
  file.path(paths$raw, "Hybrid Test Gold Standard Table.csv"),
  show_col_types = FALSE
) %>% 
  select(-(1:2))


gold_hybrid_clean <- gold_hybrid %>% 
  rename(
    sentence_key = Context,
    event = Event,
    bout = Bout,
    fouler = Fouler,
    foul_type = Foul,
    fight_paused = `Pause?`,
    point_deduction = `Deduction?`,
    foul_idx = `Foul Index`
  ) %>% 
  mutate(
    fight_paused = str_to_lower(fight_paused) == "yes",
    point_deduction = str_to_lower(point_deduction) == "yes",
    bout_key = bout %>%
      str_to_lower() %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish() %>%
      str_split(" vs ") %>%
      map_chr(~ {
        if (length(.x) != 2) return(NA_character_)
        paste(
          word(.x[1], -1),  # last word of fighter 1
          "vs",
          word(.x[2], -1)   # last word of fighter 2
        )
      })
  )



hybrid_foul_labels_test <- gold_hybrid_clean %>% 
  select(event, sentence_key) %>% 
  distinct() %>% 
  mutate(has_foul =1L,
         sentence_key = str_squish(str_to_lower(sentence_key))
         )

# Sentence-key normalisation: fixes encoding artefacts (mojibake), quote/dash variants, and hyphen spacing so gold contexts match scraped sentences exactly
# This is critical because joins are string-exact at (event, sentence_key, sid
clean_sentence_key <- function(x) {
  x %>%
    as.character() %>%
    
    # fix common mojibake sequences first
    str_replace_all("\u00E2\u0080\u0094", "—") %>%  # â€” -> em dash
    str_replace_all("\u00E2\u0080\u0093", "–") %>%  # â€“ -> en dash
    str_replace_all("\u00E2\u0080\u0099", "’") %>%  # â€™ -> right quote
    str_replace_all("\u00E2\u0080\u0098", "‘") %>%  # â€˜ -> left quote
    str_replace_all("\u00E2\u0080\u009C", "“") %>%  # â€œ -> left dbl quote
    str_replace_all("\u00E2\u0080\u009D", "”") %>%  # â€� -> right dbl quote
    
    # catch literal typed sequences
    str_replace_all("â€™", "’") %>%
    str_replace_all("â€˜", "‘") %>%
    str_replace_all("â€œ", "“") %>%
    str_replace_all("â€\u009d", "”") %>%
    
    # standardise punctuation variants
    str_replace_all("[\u2018\u2019`']", "'") %>%     # quotes -> '
    str_replace_all("[\u201C\u201D]", "\"") %>%      # dbl quotes -> "
    str_replace_all("[\u2013\u2014]", "-") %>%       # en/em dash -> -
    
    str_replace_all("\\s*-\\s*", "-") %>%            # remove spaces around hyphens
    
    str_squish() %>%
    str_to_lower()
}


sent_all_labeled_hybrid_test <- sent_all_labeled_hybrid_test %>%
  mutate(sentence_key = clean_sentence_key(sentence_key))

hybrid_foul_labels_test <- hybrid_foul_labels_test %>%
  mutate(sentence_key = clean_sentence_key(sentence_key))

# Joining all sentences with confirmed foul sentences
sent_all_labeled_hybrid_test <- sent_all_labeled_hybrid_test %>% 
  left_join(
    hybrid_foul_labels_test,
    by = c("event", "sentence_key")
  ) %>% 
  mutate(
    has_foul = if_else(is.na(has_foul), 0L, has_foul)
  ) %>% 
  select(event, bout, sentence_key, sid, has_foul)

sent_all_labeled_hybrid_test %>% 
  count(has_foul == 1)

# Debug check: gold contexts that do not appear in the scraped sentence list (join failures)
# If non-empty, accuracy metrics will be understated because some true fouls are unreachable for the model
missing <- hybrid_foul_labels_test %>%
  anti_join(sent_all_labeled_hybrid_test, by = c("event", "sentence_key"))

# Built table showing all has_foul sentences from gold standard table. Now we need to add rule confidence column, ML confidence column and the threshold condition column and test for accuracy.

# Building Rule Confidence Table:
# Build rule-confidence filenames from the event ids used in THIS run
hybrid_confidence_paths <- file.path(
  paths$confidence_test,
  paste0("sentence_conf_", test_event_ids, ".csv")
)

# Guardrail: tell user exactly what's missing
missing_conf <- hybrid_confidence_paths[!file.exists(hybrid_confidence_paths)]
if (length(missing_conf) > 0) {
  stop(
    "Missing rule-confidence files for one or more test events.\n",
    "Expected files like: sentence_conf_<event_id>.csv in: ", paths$confidence_test, "\n\n",
    "Missing:\n- ", paste(basename(missing_conf), collapse = "\n- ")
  )
}

hybrid_confidence_list <- purrr::map_dfr(
  hybrid_confidence_paths,
  readr::read_csv,
  show_col_types = FALSE
)


hybrid_confidence_list <- hybrid_confidence_list %>% 
  select(
    event, sentence, rule_conf, rule_reasons_pos, rule_reasons_neg, sid
  )


# Building ML Confidence Table:
# Apply the trained production ML model (LR + TF-IDF + domain flags) to the blind sentences to obtain ml_prob
ml_hybrid_test_data <- sent_all_labeled_hybrid_test %>%
  mutate(text = sentence_key) %>%   
  add_flags()

ml_hybrid_pred <- ml_hybrid_test_data %>%
  bind_cols(predict(fit_lr_flags, new_data = ml_hybrid_test_data, type = "prob")) %>%
  mutate(ml_prob = .pred_1)

ml_confidence_table_test <- ml_hybrid_pred %>%
  transmute(
    event,
    bout,
    sentence_key,
    ml_prob,
    sid
  )


# Final Assembly Point: Joining Has_foul table with Rule_conf and ML_conf tables

hybrid_confidence_list <- hybrid_confidence_list %>% 
  mutate(
    sentence_key = sentence
  ) %>% 
  select(
    event, sentence_key, rule_conf, rule_reasons_pos, rule_reasons_neg, sid
  )

hybrid_confidence_list <- hybrid_confidence_list %>%
  mutate(sentence_key = str_to_lower(sentence_key))

# Join sentence-level gold labels + rule_conf + ml_prob using (event, sentence_key, sid)
# sid is included to disambiguate repeated/near-duplicate sentences

hybrid_full <- sent_all_labeled_hybrid_test %>%
  left_join(hybrid_confidence_list, by = c("event", "sentence_key", "sid")) %>%
  left_join(ml_confidence_table_test,   by = c("event", "sentence_key", "sid"))



# Need to add a new column onto the joined table which incorporates the hybrid decision logic:

# Hybrid decision rule (blind test):
# 1) If rule_conf is extremely high (>= 0.90), accept as foul even if ML is low (rules are high-precision at this level). DIFFERENT TO PREVIOUSLY CONFIRMED HYBRID LOGIC - UPDATED LOGIC IN LIGHT OF BLIND TEST
# 2) Otherwise accept if ML is above tuned threshold (ml_prob >= 0.32).
# 3) Otherwise “rescue” ML misses only when rules are confidently positive (rule_conf >= 0.70 AND ml_prob < 0.32).
# Motivation: prioritise recall while controlling false positives; the >=0.90 rule acts as a precision-gated override for unambiguous rule hits

t_ml   <- 0.32
t_rule <- 0.7

hybrid_full <- hybrid_full %>%
  mutate(
    hybrid_foul = factor(
      if_else(
        rule_conf >= 0.90 |
          ml_prob >= 0.32 |
          (rule_conf >= 0.7 & ml_prob < 0.32),
        "1", "0"
      ),
      levels = c("0","1")
    )
  )

hybrid_full <- hybrid_full %>% 
  select(
    event,
    bout.x,
    sentence_key,
    sid,
    has_foul,
    rule_conf,
    rule_reasons_pos,
    rule_reasons_neg,
    ml_prob,
    hybrid_foul
  )

hybrid_full <- hybrid_full %>% 
  rename(
    bout = bout.x
  )

hybrid_full_filtered <- hybrid_full %>% 
  filter( has_foul == 1) %>% 
  select(
    event,
    bout,
    sentence_key,
    sid
  )


# Accuracy Checks:
hybrid_full %>% 
  count(hybrid_foul == 1)

# How many fouls the hybrid predicts:
table(hybrid_full$hybrid_foul)

# Evaluate hybrid_foul against blind gold has_foul labels (sentence-level classification)
table(
  truth = hybrid_full$has_foul,
  pred  = hybrid_full$hybrid_foul
)
# 106 fouls predicted aganist 89 real fouls

# 84 true positives
# 5 False Negatives = 5 real fouls missed so recall = 0.944
# 22 False Positives, so precision = 0.792



# Error analysis: capture false negatives to identify systematic misses (phrasing gaps, rule pattern gaps, key-normalisation issues)

false_negatives <- hybrid_full %>%
  filter(
    has_foul == 1,
    hybrid_foul == "0"
  ) %>%
  select(
    event,
    sid,
    sentence_key,
    ml_prob,
    rule_conf,
    rule_reasons_pos,
    rule_reasons_neg
  )


readr::write_csv(hybrid_full, file.path(paths$output, "hybrid_full_blind_test.csv"))


# Freezing model here, can improve it in the future but i want to upload it in its current form then continue working on it.
# Next step is to build the final output table which takes the fouls determined by hybrid_foul and then determines the fight, fouler, foul type, whether the fight was paused and whether the foul resulted in a points deduction.

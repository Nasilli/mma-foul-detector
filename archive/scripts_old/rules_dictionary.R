## EARLY EXPERIMENT SCRIPT - DID NOT END UP USING THIS PIPLEIN AS I CREATED RULE CONFIDENCE USING THE SAME REGEX FROM PROCESS_EVENT_V2


# Rules_dictionary.R


# Building Confidence Metrics for Rule-based model, in order for effective hybrid regex-rules/ML detecton model
# Need to ensre I dont try to assign confidence for every individual regex, instead build rule-confidence at a grouped level (groin/eye-poke/fence-grab, etc.) and then calibrate with signals (pause, deduction, negation blocks, etc.)
# Rule confidence should approximate: P(has_foul = 1| rule family triggeres, and not blocked), so we need a function that returns: rule_hit(TRUE/FALSE), rule_family(.e.g. "eye", "groin", etc.), rule_conf (0-1), (optional), matched_rules (debug string)

library(stringr)
library(tibble)

# --- RULE FAMILIES ---
rules_tbl <- tibble::tribble(
  ~rule_id,            ~family,        ~pattern,
  "R_GROIN_LOW",       "groin",         "low\\s+blow|below\\s+the\\s+belt|south\\s+of\\s+the\\s+border|\\bgroin\\b|\\bcup\\b|cup\\s+shot",
  "R_EYE_POKE",        "eye",           "eye\\s+poke|eyepoke|eye\\s+goug|poke.*eye|finger.*eye|thumb.*eye|eye\\s*socket|eyeball",
  "R_FENCE_GRAB",      "fence",         "(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?|tug(s|ged|ging)?).{0,25}(fence|cage|chain\\s+links?)|(fence|cage|chain\\s+links?).{0,25}(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?|tug(s|ged|ging)?)",
  "R_TOES_FENCE",      "fence",         "toes?.{0,40}(fence|cage|chain\\s+links?)|hooks?\\s+(his|her|their)\\s+toes?.{0,30}(in|into|inside|on)\\s+(the\\s+)?(fence|cage|chain\\s+links?)",
  "R_BACK_HEAD",       "back_head",     "back\\s+of\\s+(the\\s+|his\\s+|her\\s+|[A-Za-z]+(?:['’`]?\\s*)?)?head|rabbit\\s+punch|behind\\s+the\\s+ear",
  "R_HEADBUTT",        "headbutt",      "head\\s*butt|headbutt|clash\\s+of\\s+heads",
  "R_GROUNDED_KNEE",   "grounded_knee", "knee\\s+to\\s+(a\\s+)?grounded\\s+opponent|illegal\\s+knee",
  "R_ILLEGAL_UPKICK",  "illegal_upkick","illegal\\s+upkick|illegal\\s+(kick|upkick)\\s+to\\s+(a\\s+)?grounded\\s+opponent|\\bupkick\\b.{0,50}\\b(downed|grounded)\\b",
  "R_CROWN_STRIKE",    "illegal_strike","crown\\s+of\\s+the\\s+head.{0,40}illegal|illegal\\s+strike"
)

rules_tbl <- rules_tbl %>%
  mutate(re = purrr::map(pattern, ~ regex(.x, ignore_case = TRUE)))


# Rules Scoring:
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Returns one row per sentence with rule hits + top family + debug
score_rules <- function(df, text_col = "text", rules_tbl) {
  stopifnot(text_col %in% names(df))
  
  df0 <- df %>%
    mutate(.row_id = row_number(),
           .text   = .data[[text_col]] %>% str_to_lower() %>% str_squish())
  
  hits_long <- df0 %>%
    select(.row_id, .text) %>%
    tidyr::crossing(rules_tbl %>% select(rule_id, family, re)) %>%
    mutate(
      hit = purrr::map2_lgl(.text, re, ~ stringr::str_detect(.x, .y))
    ) %>%
    filter(hit) %>%
    group_by(.row_id) %>%
    summarise(
      rule_hit = TRUE,
      families = paste(unique(family), collapse = "|"),
      rule_ids = paste(rule_id, collapse = "|"),
      n_rules  = n(),
      .groups = "drop"
    )
  
  df0 %>%
    left_join(hits_long, by = ".row_id") %>%
    mutate(
      rule_hit = if_else(is.na(rule_hit), FALSE, rule_hit),
      families = if_else(is.na(families), "", families),
      rule_ids = if_else(is.na(rule_ids), "", rule_ids),
      n_rules  = if_else(is.na(n_rules), 0L, n_rules)
    ) %>%
    rename(sentence_id = .row_id) %>% 
    select(-.text)
}

train_scored <- score_rules(train_df, text_col = "text", rules_tbl = rules_tbl)
test_scored <- score_rules(test_df, text_col = "text", rules_tbl = rules_tbl)

scored_all <- bind_rows(
  train_scored %>% mutate(dataset = "train"),
  test_scored %>% mutate(dataset = "test")
)

## Rule Calibration
# Caliibrationg the rule confience from labelled data is the core of this being effective.
library(dplyr)
library(tidyr)
library(purrr)

# has_foul must be factor("0","1") or numeric 0/1
fit_rule_calibration <- function(scored_all, label_col = "has_foul") {
  
  y <- scored_all[[label_col]]
  y_num <- if (is.factor(y)) as.integer(as.character(y)) else as.integer(y)
  
  df <- scored_all %>%
    mutate(.y = y_num)
  
  rules_long_id <- df %>%
    select(sentence_id, .y, rule_hit, rule_ids) %>%
    filter(rule_hit) %>%
    mutate(rule_id = strsplit(rule_ids, "\\|")) %>%
    tidyr::unnest_longer(rule_id)
  
  rules_long_family <- df %>%
    select(sentence_id, .y, rule_hit, families) %>%
    filter(rule_hit) %>%
    mutate(family = strsplit(families, "\\|")) %>%
    tidyr::unnest_longer(family)
  
  rule_id_stats <- rules_long_id %>%
    distinct(sentence_id, rule_id, .keep_all = TRUE) %>%
    group_by(rule_id) %>%
    summarise(
      n_fired = n(),
      tp = sum(.y == 1),
      fp = sum(.y == 0),
      conf = (tp + 1) / (tp + fp + 2),
      .groups = "drop"
    )
  
  family_stats <- rules_long_family %>%
    distinct(sentence_id, family, .keep_all = TRUE) %>%
    group_by(family) %>%
    summarise(
      n_fired = n(),
      tp = sum(.y == 1),
      fp = sum(.y == 0),
      conf = (tp + 1) / (tp + fp + 2),
      .groups = "drop"
    )
  
  list(rule_id_stats = rule_id_stats, family_stats = family_stats)
}

cal <- fit_rule_calibration(scored_all, label_col = "has_foul")
cal$rule_id_stats %>% arrange(desc(n_fired))
cal$family_stats %>% arrange(desc(n_fired))

# Saving Calibration Outputs:
# save as RDS (best)
saveRDS(cal, "rule_calibration_train.rds")

# optional: save as CSV for inspection
readr::write_csv(cal$rule_id_stats, "rule_id_calibration_train.csv")
readr::write_csv(cal$family_stats,  "family_calibration_train.csv")


rule_conf_map <- cal$rule_id_stats %>%
  select(rule_id, conf)

scored_all_conf <- scored_all %>%
  mutate(rule_id_list = if_else(rule_hit, strsplit(rule_ids, "\\|"), list(character(0)))) %>%
  tidyr::unnest_longer(rule_id_list, values_to = "rule_id", keep_empty = TRUE) %>%
  left_join(rule_conf_map, by = "rule_id") %>%
  group_by(row_number()) %>%
  summarise(
    across(everything(), ~ dplyr::first(.x)),
    rule_conf = if_else(any(!is.na(conf)), max(conf, na.rm = TRUE), 0),
    .groups = "drop"
  )
# Will compte a confidence for each row in tain_scored using max rule confidence among triggeres rules, fallback to family confidence and then fallback to a global prior (overall precision when rules fire)

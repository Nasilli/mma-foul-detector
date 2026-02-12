# Creating Hybrid Table: Sentences+ Rule Confidence + ML Confidence:
# Helping to construct a hybrid decision policy (e.g. take foul if rule_conf <0.8 AND ML_conf <0.2)
# Debug why ML misses rule-obvious cases

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

# NOTE: has_foul is derived from the (validated) foul-detector output. 
# For the events used here, model_v2_clean was audited against the gold standard and sentence-level foul labels were checked to be 100% aligned.
# For the final test of the hybrid model (in a later script), has_foul was derived from a manually constructed gold-standard table.

library(dplyr)
library(stringr)

# Making both tables 'join-ready'

make_sentence_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[\u2018\u2019]", "'") %>%        # curly apostrophes -> straight
    str_replace_all("[^a-z0-9\\s']", " ") %>%         # drop punctuation (keep letters/numbers/spaces/')
    str_squish()
}

# Load if previous scripts were not run before (ml_confidence_table.csv is created in 04_evaluate_and_threshold.R)
ml_confidence_table <- readr::read_csv(file.path(paths$interim, "ml_confidence_table.csv"), show_col_types = FALSE)

# ML confidence table
ml_tbl <- ml_confidence_table %>%
  transmute(
    bout = str_squish(bout),
    sid = as.integer(sid),
    text,
    has_foul = as.integer(has_foul),
    ml_prob = as.numeric(ml_prob),
    pred_foul = as.integer(pred_foul),
    sentence_key = make_sentence_key(text)
  )

# Run Rule Confidence.R before this script as confidence_list_all is created there
# Rules confidence table
rules_tbl <- confidence_list_all %>%
  transmute(
    bout = str_squish(bout),
    sid = as.integer(sid),
    sentence,
    has_foul_rule = as.integer(has_foul),
    rule_conf = as.numeric(rule_conf),
    rule_score = as.numeric(rule_score),
    rule_reasons_pos,
    rule_reasons_neg,
    sentence_key = make_sentence_key(sentence)
  )


# Fixing duplicate sentence problem:
# Duplicate sentences can appear multiple times in a bout; sent_ix disambiguates repeats. Assumes both tables preserve the same ordering within bout.
ml_tbl2 <- ml_tbl %>%
  group_by(bout, sentence_key) %>%
  mutate(sent_ix = row_number()) %>%
  ungroup()

rules_tbl2 <- rules_tbl %>%
  group_by(bout, sentence_key) %>%
  mutate(sent_ix = row_number()) %>%
  ungroup()


hybrid_df <- ml_tbl2 %>%
  left_join(
    rules_tbl2 %>% select(bout, sentence_key, sent_ix, rule_conf, rule_score, rule_reasons_pos, rule_reasons_neg),
    by = c("bout", "sentence_key", "sent_ix")
  )


# How many failed matches?
sum(is.na(hybrid_df$rule_conf))
# Match Rate
mean(!is.na(hybrid_df$rule_conf))

# Sanity Check whether misses cluster in a small number of bouts
hybrid_df %>%
  mutate(missed = is.na(rule_conf)) %>%
  count(bout, missed) %>%
  group_by(bout) %>%
  summarise(miss_rate = mean(missed), n=n()) %>%
  arrange(desc(miss_rate)) %>%
  head(20)

# See examples of misses
hybrid_df %>%
  filter(is.na(rule_conf)) %>%
  select(bout, text, sentence_key, ml_prob) %>%
  head(25)


# Check if there's still duplicates 
ml_tbl2 %>% count(bout, sentence_key, sent_ix) %>% filter(n > 1)
rules_tbl2 %>% count(bout, sentence_key, sent_ix) %>% filter(n > 1)




# Disagreement Analysis:
# Compare ML vs rule “high-confidence” decisions at fixed operating thresholds
hybrid_disagreement <- hybrid_df %>%
  mutate(
    ml_high = ml_prob >= 0.32, # ML operating threshold chosen in 04_evaluate_and_threshold.R
    rule_high = rule_conf >= 0.7 # rule operating threshold chosen from confidence diagnostics
  ) 


# Confusion grid: how often ML and rules agree/disagree, split by has_foul label
hybrid_disagreement %>% 
  count(ml_high, rule_high, has_foul)


# Focus: cases labelled as foul (has_foul==1) where rules are high but ML is low.
# These are priority ML false negatives (missed by ML despite strong rule evidence).

ml_fn <-hybrid_disagreement %>%
  filter(
    has_foul == 1,
    ml_high == FALSE,
    rule_high == TRUE
  ) %>%
  select(text, ml_prob, rule_conf, rule_reasons_pos) %>%
  arrange(ml_prob) %>%
  head(30)

ml_fn

# Insight: many misses appear unambiguous (high rule evidence), suggesting ML is
# sensitive to phrasing seen in training (distributional bias) and under-weights
# explicit “illegality/referee” cues in unfamiliar wording.
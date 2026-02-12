# 04_evaluate_and_threshold.R

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

# Testing model on v2 data and picking optimal threshold

# Define operating threshold (chosen to balance precision/recall given project objectives)
t_star <- 0.32  

# Assemble sentence-level datasets from earlier scripts (legacy “test” + “train” pools)
test_df <- sent_all_labeled_test %>%
  transmute(
    bout = bout,
    sid = sid,
    text = sentence_key,
    has_foul = factor(has_foul, levels = c(0, 1))
  ) %>%
  filter(!is.na(text), text != "")

test2_df <- sent_all_labeled %>%
  transmute(
    bout = bout,
    sid = sid,
    text = sentence_key,
    has_foul = factor(has_foul, levels = c(0, 1))
  ) %>%
  filter(!is.na(text), text != "")

# Apply the same engineered flags used during model training
test_df2 <- add_flags(test_df)

test2_df <- add_flags(test2_df)

library(dplyr)

# Ensure same target type in both
test2_df  <- test2_df  %>% mutate(has_foul = factor(has_foul, levels = c(0,1)))
test_df2  <- test_df2  %>% mutate(has_foul = factor(has_foul, levels = c(0,1)))

# Harmonise columns and bind into a single dataset for evaluation/splitting
common_cols <- intersect(names(test2_df), names(test_df2))

ml_data <- bind_rows(
  test_df2 %>% select(all_of(common_cols)) %>% mutate(source = "test_df2"),
  test2_df %>% select(all_of(common_cols)) %>% mutate(source = "test2_df")
)
# Add engineered flags required by wf_flags recipe (wf_flags expects these columns)
ml_data2 <- add_flags(ml_data)


# Create stratified hold-out split to evaluate model performance before choosing threshold
# NOTE: split uses ml_data2 (flagged dataset) because wf_flags recipe requires flag_ columns
set.seed(123)
split  <- initial_split(ml_data2, prop = 0.8, strata = has_foul)
train_ml <- training(split)
test_ml  <- testing(split)

# Fit the chosen workflow on training split and score probabilities on test split

fit_lr_flags_test <- fit(wf_flags, data = train_ml)

pred_test <- test_ml %>%
  bind_cols(predict(fit_lr_flags_test, test_ml, type = "prob")) %>%
  mutate(ml_prob = .pred_1)

# Sanity-check discriminative performance (ROC AUC + PR AUC)
roc_auc(pred_test, truth = has_foul, ml_prob, event_level = "second")
pr_auc(pred_test, truth = has_foul, ml_prob, event_level = "second")



# Sweep thresholds to see precision/recall/F1 trade-offs and locate the best operating region
# Threshold Sweep:
thresholds <- seq(0.01, 0.7, by = 0.005)

score_tbl_v2 <- purrr::map_dfr(thresholds, function(t) {
  pred <- pred_test %>%
    mutate(pred_foul = factor(if_else(ml_prob >= t, "1", "0"), levels = c("0","1")))
  
  precision_t <- suppressWarnings(
    precision(pred, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  )
  recall_t <- recall(pred, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  f1_t <- suppressWarnings(
    f_meas(pred, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  )
  
  tibble(threshold = t, precision = precision_t, recall = recall_t, f1 = f1_t)
}) %>%
  filter(!is.na(precision))

score_tbl_v2

# Identify best-performing thresholds (by F1) from coarse grid
score_tbl_v2 %>%
  arrange(desc(f1)) %>%
  slice(1:30)

# Optimal operating threshold is very likely around 0.3-0.4 as this is where F1 peaks, a more precise sweep is needed to identify:

# Fine sweep around the F1 peak to choose a stable threshold
thresholds_fine <- seq(0.3, 0.4, by = 0.002)

score_tbl_v2_fine <- purrr::map_dfr(thresholds_fine, function(t) {
  pred <- pred_test %>%
    mutate(pred_foul = factor(if_else(ml_prob >= t, "1", "0"), levels = c("0","1")))
  
  precision_t <- suppressWarnings(
    precision(pred, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  )
  recall_t <- recall(pred, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  f1_t <- suppressWarnings(
    f_meas(pred, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  )
  
  tibble(threshold = t, precision = precision_t, recall = recall_t, f1 = f1_t)
}) %>%
  filter(!is.na(precision))

score_tbl_v2_fine %>% arrange(desc(f1)) %>% slice(1:15)

readr::write_csv(score_tbl_v2,      file.path(paths$interim, "threshold_sweep_coarse.csv"))
readr::write_csv(score_tbl_v2_fine, file.path(paths$interim, "threshold_sweep_fine.csv"))

# Locking in Threshold classification:
# At threshold 0.32 precision = 0.825, recall = 0.917, f1 = 0.868
# Comparisons were made between the domain-flags model and a previous model with only text tuning, optimal threshold for text tuning model gave f1 of 0.743, recall = 0.815, precision = 0.682, so numerically, domain-flags model appear the better choice.

# Apply chosen threshold on hold-out predictions and report confusion matrix + precision/recall
pred_v2_hard <- pred_test %>%
  mutate(pred_foul = factor(if_else(ml_prob >= t_star, "1", "0"),
                            levels = c("0","1")))

ml_confidence_table <- pred_v2_hard %>% 
  select(sid, bout, text, has_foul, ml_prob, pred_foul)

conf_mat(pred_v2_hard, truth = has_foul, estimate = pred_foul)
recall(pred_v2_hard, truth = has_foul, estimate = pred_foul, event_level="second")
precision(pred_v2_hard, truth = has_foul, estimate = pred_foul, event_level="second")

# Re-score the full dataset with the final trained model to produce ML probability + hard classification for downstream use
# Assumes fit_lr_flags and ml_data2 exist from 03_train_models.R / 02_build_sentence_datasets.R
pred_all <- ml_data2 %>%
  bind_cols(predict(fit_lr_flags, ml_data2, type = "prob")) %>%
  mutate(ml_prob = .pred_1)

ml_confidence_table <- pred_all %>%
  mutate(
    pred_foul = factor(if_else(ml_prob >= t_star, "1", "0"),
                       levels = c("0","1"))
  ) %>%
  select(sid, bout, text, has_foul, ml_prob, pred_foul)

readr::write_csv(ml_confidence_table, file.path(paths$interim, "ml_confidence_table.csv"))
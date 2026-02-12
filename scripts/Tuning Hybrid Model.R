# Tuning Hybrid Model:

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

# Objective: tune (t_ml, t_rule) thresholds for a hybrid decision rule that priorities recall while controlling false positives
# Hybrid rule: classify as foul if ML >= t_ml OR (Rule >= t_rule AND ML < t_ml) to let rules “rescue” ML misses
# Note: metrics are evaluated against has_foul (validated sentence-level label), not an external gold label
library(dplyr)
library(tidyr)
library(purrr)
library(yardstick)

# Threshold search space: ML probability threshold (fine grid) and rule confidence threshold (coarser grid)
# Rule grid is coarser because rule_conf is already well-separated and we only need conservative cutoffs
ml_grid   <- seq(0.01, 0.5, by = 0.01)
rule_grid <- seq(0.5, 0.95, by = 0.05)

# Helper: apply a given (t_ml, t_rule) to hybrid_df and return precision/recall/F1
# Original design choice: rules only contribute when ML is below threshold (prevents double-counting and keeps rules conservative)
eval_hybrid <- function(t_ml, t_rule) {
  
  pred_tbl <- hybrid_df %>%
    mutate(
      has_foul = factor(as.character(has_foul), levels = c("0","1")),
      pred_foul = factor(
        if_else(
          (ml_prob >= t_ml) |
            (rule_conf >= t_rule & ml_prob < t_ml),
          "1", "0"
        ),
        levels = c("0","1")
      )
    )
  
  tibble(
    precision = yardstick::precision(pred_tbl, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate,
    recall    = yardstick::recall(   pred_tbl, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate,
    f1        = yardstick::f_meas(   pred_tbl, truth = has_foul, estimate = pred_foul, event_level = "second")$.estimate
  )
}

# Evaluate all combinations of thresholds and collect metrics for comparison
# Result: hyb_scores contains (t_ml, t_rule) and corresponding precision/recall/F1
hyb_scores <- expand_grid(
  t_ml = ml_grid,
  t_rule = rule_grid
) %>%
  mutate(scores = map2(t_ml, t_rule, eval_hybrid)) %>%
  unnest(scores)

# Rank candidate thresholds by F1 (optionally filtering out degenerate low-recall configurations).
# F1 is used as a balanced summary, but final selection is guided by project objective (high recall for foul detection)
hyb_scores %>%
  filter(recall >= 0.3) %>%
  arrange(desc(f1)) %>%
  slice(1:20)
# hyb_scores plateau: with t_ml=0.32, metrics are identical (P=0.837, R=1.000, F1=0.911) for t_rule=0.50–0.95, so t_rule=0.70 appears a conservative default


# Selected operating point: t_ml = 0.32 from ML threshold sweep; t_rule = 0.70 from rule confidence diagnostics
# Rationale: maximise recall while keeping precision acceptable; rules are applied conservatively to recover ML false negatives
# Note: reported metrics depend on the current dataset/split; treat as indicative rather than universal. With this, against the test split dataset, recall = 1, precision = 0.84, fq = 0.91
t_ml <- 0.32
t_rule <- 0.7


# Apply the chosen hybrid decision rule to generate the final hybrid classification for analysis
# Hybrid_df created in `Building Hybrid Model.R` script
hybrid_df <- hybrid_df %>%
  mutate(
    has_foul = factor(as.character(has_foul), levels = c("0","1")),
    hybrid_foul = factor(
      if_else(
        (ml_prob >= 0.32) |
          (rule_conf >= 0.7 & ml_prob < 0.32),
        "1", "0"
      ),
      levels = c("0","1")
    )
  )

# Compute hybrid performance metrics at the chosen operating thresholds
hybrid_metrics <- tibble(
  precision = precision(hybrid_df, truth = has_foul, estimate = hybrid_foul,
                        event_level = "second")$.estimate,
  recall    = recall(hybrid_df, truth = has_foul, estimate = hybrid_foul,
                     event_level = "second")$.estimate,
  f1        = f_meas(hybrid_df, truth = has_foul, estimate = hybrid_foul,
                     event_level = "second")$.estimate
)

hybrid_metrics

# Sanity check: inspect cases where rules flipped a negative ML decision into a positive hybrid decision
hybrid_df %>%
  filter(
    ml_prob < 0.32,
    rule_conf >= 0.7,
    hybrid_foul == "1"
  ) %>%
  select(bout, text, ml_prob, rule_conf, rule_reasons_pos)

# Next step: refit the ML model on the full labelled dataset, & then apply this hybrid logic to a new blind gold set
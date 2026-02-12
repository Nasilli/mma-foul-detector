# Purpose: Show working for model choice (LR vs SVM) + tuning decisions:

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))
source(file.path("scripts", "01_load_clean_gold_and_models.R"))
source(file.path("scripts", "02_build_sentence_datasets.R"))
source(file.path("scripts", "03_train_models.R"))  

# Demonstrate model selection (LR vs SVM) and tuning (penalty, max_tokens) on held-out split
# Assumes sent_all_labeled, sent_all_labeled_test, and ml_data2 already exist from earlier scripts


# Build minimal train/test datasets (same schema) for baseline model comparison
train_df <- sent_all_labeled %>%
  transmute(
    text = sentence_key,
    has_foul = factor(has_foul, levels = c(0, 1))
  ) %>%
  filter(!is.na(text), text != "")

test_df <- sent_all_labeled_test %>%
  transmute(
    text = sentence_key,
    has_foul = factor(has_foul, levels = c(0, 1))
  ) %>%
  filter(!is.na(text), text != "")

# Baseline TF-IDF recipe (unigrams + bigrams) used for model comparison

rec <- recipe(has_foul ~ text, data = train_df) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%  # 1-grams + 2-grams
  step_tokenfilter(text, max_tokens = 2000) %>%
  step_tfidf(text)

# Fit and evaluate baseline Logistic Regression (good under class imbalance)
############# LR Model ############# 
model_lr <- logistic_reg(penalty = 0.01, mixture = 1) %>%  # lasso-ish
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_lr)

fit_lr <- fit(wf, data = train_df)

pred_prob_lr <- predict(fit_lr, test_df, type="prob") %>%
  bind_cols(test_df)

# Evaluate on ROC AUC and PR AUC (PR AUC is more informative under imbalance)

lr_roc <- roc_auc(pred_prob_lr, truth = has_foul, .pred_1, event_level="second")
lr_pr <- pr_auc(pred_prob_lr, truth = has_foul, .pred_1, event_level="second")  


# Fit and evaluate linear SVM baseline for comparison.
############# SVM Model ############# 
model_svm_kern <- svm_linear(cost = 1) %>%
  set_engine("kernlab", prob.model = TRUE) %>%
  set_mode("classification")

wf_svm_kern <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_svm_kern)

fit_svm_kern <- fit(wf_svm_kern, data = train_df)

pred_prob_svm <- predict(fit_svm_kern, test_df, type = "prob") %>%
  bind_cols(test_df %>% select(has_foul))

svm_roc <- roc_auc(pred_prob_svm, truth = has_foul, .pred_1, event_level="second") 
svm_pr <- pr_auc(pred_prob_svm, truth = has_foul, .pred_1, event_level="second") 

# Compare LR vs SVM; choose LR due to higher PR AUC
# Selection:
lr_roc # ROC AUC of 0.968
lr_pr #PR AUC of 0.575 
svm_roc #ROC AUC of 0.946
svm_pr # PR AUC of 0.355

# SVM shows a clear downgrade in PR AUC so LR Regression is the logical choice: 
# LR > SVM on PR AUC under the severe class imbalance of the datasets
# Next step is to tune the TF-IDF-LR Pipeline:

# Tune LR hyperparameters using stratified train/test split on ml_data2 (includes engineered flags)

library(tidymodels)
library(dplyr)
library(stringr)

set.seed(123)

# Ensure target type is consistent; ml_data2 is the full sentence-level dataset with flags
ml_data2 <- ml_data2 %>%
  mutate(has_foul = factor(has_foul, levels = c(0, 1))) %>%
  filter(!is.na(text), text != "")

# Create stratified split for tuning evaluation
split <- initial_split(ml_data2, prop = 0.8, strata = has_foul)
train_ml <- training(split)
test_ml  <- testing(split)


best_max <- 5000  

# Helper: build TF-IDF + engineered-flag recipe with configurable vocabulary size
make_rec_flags <- function(data, max_tokens) {
  recipe(has_foul ~ text + flag_warn + flag_illegal + flag_eye + flag_groin + flag_fence + flag_backhd +
           flag_strong_anchor + flag_headbutt + flag_upkick + flag_grounded_ctx + flag_upkick_downed +
           flag_illegal_knee_grounded + flag_eye_poke_explicit + flag_fence_grab + flag_toes_in_fence +
           flag_ref_context + flag_pause_context + flag_eye_warning_only + flag_injury_eye,
         data = data) %>%
    step_tokenize(text) %>%
    step_stopwords(text) %>%
    step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%
    step_tokenfilter(text, max_tokens = max_tokens) %>%
    step_tfidf(text)
}


# Tune LR penalty over a small grid; select by PR AUC
pen_grid <- c(0.005, 0.01, 0.02, 0.04)

# Fit model for a given penalty and return PR/ROC AUC on test split
eval_pen <- function(pen) {
  rec <- make_rec_flags(train_ml, best_max)
  
  model_lr <- logistic_reg(penalty = pen, mixture = 1) %>% set_engine("glmnet")
  wf <- workflow() %>% add_recipe(rec) %>% add_model(model_lr)
  
  fit_obj <- fit(wf, data = train_ml)
  
  pred <- predict(fit_obj, test_ml, type = "prob") %>%
    bind_cols(test_ml %>% select(has_foul))
  
  tibble(
    penalty = pen,
    pr_auc  = pr_auc(pred, truth = has_foul, .pred_1, event_level = "second")$.estimate,
    roc_auc = roc_auc(pred, truth = has_foul, .pred_1, event_level = "second")$.estimate
  )
}

pen_results <- purrr::map_dfr(pen_grid, eval_pen) %>% arrange(desc(pr_auc))
pen_results

best_pen <- pen_results$penalty[1]
best_pen
# best_pen = 0.005




# Tune vocabulary size (max_tokens) using best_pen - select by PR AUC.
max_grid <- c(3000, 5000, 8000)

eval_vocab <- function(mx) {
  rec <- make_rec_flags(train_ml, mx)
  
  model_lr <- logistic_reg(penalty = best_pen, mixture = 1) %>% set_engine("glmnet")
  wf <- workflow() %>% add_recipe(rec) %>% add_model(model_lr)
  fit_obj <- fit(wf, data = train_ml)
  
  pred <- predict(fit_obj, test_ml, type = "prob") %>%
    bind_cols(test_ml %>% select(has_foul))
  
  tibble(
    max_tokens = mx,
    pr_auc = pr_auc(pred, truth = has_foul, .pred_1, event_level="second")$.estimate
  )
}

vocab_results <- purrr::map_dfr(max_grid, eval_vocab) %>% arrange(desc(pr_auc))
vocab_results

best_max <- vocab_results$max_tokens[1]
best_max
# Best_max = 5000



# Refit tuned LR on de-duplicated data and report final PR/ROC AUC on held-out split
ml_data2_dedup <- ml_data2 %>% distinct(text, .keep_all = TRUE)
split <- initial_split(ml_data2_dedup, prop = 0.8, strata = has_foul)
train_ml <- training(split)
test_ml  <- testing(split)
# Recipe using tuned max_tokens
rec_best <- make_rec_flags(train_ml, best_max)

# LR using tuned penalty 
model_lr_best <- logistic_reg(penalty = best_pen, mixture = 1) %>%
  set_engine("glmnet")

wf_best <- workflow() %>%
  add_recipe(rec_best) %>%
  add_model(model_lr_best)

fit_lr_best <- fit(wf_best, data = train_ml)


pred <- predict(fit_lr_best, test_ml, type = "prob") %>%
  bind_cols(test_ml %>% select(has_foul))

roc_auc(pred, truth = has_foul, .pred_1, event_level = "second")
pr_auc(pred, truth = has_foul, .pred_1, event_level = "second")

# Testing on held-out split after implementing best_pen and best_max gives: 
# ROC AUC = 0.999
# PR AUC = 0.936
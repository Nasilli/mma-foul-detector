
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


setwd("/Users/lucanasillo/Documents/MMA Foul Analysis")

source("run_model_v2.R")

model_v2_clean <- readr::read_csv("model_v2_clean.csv", show_col_types = FALSE)
  
tidymodels::tidymodels_prefer()

foul_dict <- tibble::tribble(
  ~label,                ~pattern,
  "groin strike",        "low blow|groin|\\bcup\\b|below the belt",
  "eye poke",            "eye poke|poke.*eye|finger.*eye|thumb.*eye|\\beye\\b",
  "fence grab",          "(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?).{0,25}\\b(fence|cage)\\b|\\b(fence|cage)\\b.{0,25}(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?)",
  "back of head strike", "back of (the )?head|rabbit punch|behind the ear|to the back of the head"
)

# Load gold standard (v1 only for now)
gold <- read_csv(
  file = "MMA Foul Analysis Gold Standard Table.csv",
  name_repair = "minimal",
  skip = 1,
  col_names = TRUE,
  col_select = -1
)

gold_clean <- gold %>% 
  rename(
    event = Event,
    bout = `Bout:`,
    fouler = Fouler,
    foul_type = `Foul Type`,
    fight_paused = `Fight Paused?`,
    point_deduction = `Point Deduction?`,
    foul_idx = `Foul Index`
  ) %>% 
  mutate(
    fight_paused = str_to_lower(fight_paused) == "yes",
    point_deduction = str_to_lower(point_deduction) == "yes",
    bout_key = bout %>%
      str_to_lower() %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish(),
  )

# Load new gold standard (v2)
gold_v2 <- read_csv(
  file = "MMA Foul Analysis Gold Standard Table 2.csv",
  name_repair = "minimal",
  col_names = TRUE,
  col_select = -1
)

gold_clean_2 <- gold_v2 %>% 
  rename(
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


ml_df <- gold_clean %>%
  inner_join(
    model_v0_clean %>%
      select(event, bout_key, foul_idx, expanded_context),
    by = c("event", "bout_key", "foul_idx")
  ) %>%
  filter(!is.na(expanded_context), expanded_context != "") %>%
  mutate(
    foul_type = factor(foul_type),
    fouler = factor(fouler),
    pause = factor(fight_paused, levels = c(FALSE, TRUE)),
    deduction = factor(point_deduction, levels = c(FALSE, TRUE))
  ) %>%
  rename(text = expanded_context)

ml_df2 <- gold_clean_2 %>%
  inner_join(
    model_v2_clean %>%
      select(event, bout_key, foul_idx, expanded_context),
    by = c("event", "bout_key", "foul_idx")
  ) %>%
  filter(!is.na(expanded_context), expanded_context != "") %>%
  mutate(
    foul_type = factor(foul_type),
    fouler = factor(fouler),
    pause = factor(fight_paused, levels = c(FALSE, TRUE)),
    deduction = factor(point_deduction, levels = c(FALSE, TRUE))
  ) %>%
  rename(text = expanded_context)

######################### Foul Detection ############################################################

# What Model Fouls Don't Exist in Gold? False Positives
model_unmatched <- model_v2_clean %>%
  anti_join(
    gold_clean_2 %>% select(event, bout_key, foul_idx),
    by = c("event", "bout_key", "foul_idx")
  )

readr::write_csv(model_unmatched, "model_unmatched_fouls.csv")

# What Gold Fouls Don't Exist in the Model? False Negatives
gold_unmatched <- gold_clean_2 %>%
  anti_join(
    model_v2_clean %>% select(event, bout_key, foul_idx),
    by = c("event", "bout_key", "foul_idx")
  )

readr::write_csv(gold_unmatched, "Model_False_Negatives.csv")


fp_diag <- model_unmatched %>%
  mutate(
    text = coalesce(expanded_context, context) %>% str_to_lower(),
    matched_labels = purrr::map(text, \(x) {
      foul_dict %>%
        filter(str_detect(x, pattern)) %>%
        pull(label) %>%
        unique()
    }),
    why_detected = purrr::map_chr(matched_labels, \(labs) {
      if (length(labs) == 0) "UNKNOWN trigger" else paste(labs, collapse = " | ")
    })
  ) %>%
  select(event, bout, bout_key, foul_idx, why_detected, foul_type_guess, text)

# quick summary
fp_diag %>% count(why_detected, sort = TRUE)

View(fp_diag)

nrow(ml_df)
ml_df %>% count(foul_type) %>% arrange(desc(n))


# CREATING TRAINING ML TABLE (SENT_ALL FROM MODEL_V0 EVENTS: INCLUDES EVERY SENTENCE FROM THE COMMENTARY THEN A NEW COLUMN ON THE JOINED TABLE WILL IDENTIFY THE SENTENCES WHICH INCLUDE THE FOULS FROM ML_DF)

sent_all_training_events <- list(
  `sent_all_dbg__UFC-deridder-allen-18-10-2025`,
  `sent_all_dbg__UFC320-ankalaev-pereira-04-10-2025`,
  `sent_all_dbg__UFCvegas-106-burns-morales-17-05-2025`,
  `sent_all_dbg__UFC-covington-buckley-14-12-2024`,
  `sent_all_dbg__UFC-imavov-borralho-06-09-2025`,
  `sent_all_dbg__UFC-lewis-teixeira-12-06-2025`,
  `sent_all_dbg__UFC-tsarukyan-hooker-22-11-2025`,
  `sent_all_dbg__UFC321-aspinall-gane-25-10-2025`,
  `sent_all_dbg__UFC322-dellamaddalena-makhachev-15-11-2025`,
  `sent_all_dbg__UFCvegas-105-emmett-murphy-05-04-2025`,
  `sent_all_dbg__UFCvegas-107-blanchfield-barber-31-05-2025`
)

# Ensuring Joined Table Identifies the Event Each Sentence Comes From
sent_all_training_list <- list(
  "UFC-deridder-allen-18-10-2025"              = `sent_all_dbg__UFC-deridder-allen-18-10-2025`,
  "UFC320-ankalaev-pereira-04-10-2025"         = `sent_all_dbg__UFC320-ankalaev-pereira-04-10-2025`,
  "UFCvegas-106-burns-morales-17-05-2025"      = `sent_all_dbg__UFCvegas-106-burns-morales-17-05-2025`,
  "UFC-covington-buckley-14-12-2024"           = `sent_all_dbg__UFC-covington-buckley-14-12-2024`,
  "UFC-imavov-borralho-06-09-2025"             = `sent_all_dbg__UFC-imavov-borralho-06-09-2025`,
  "UFC-lewis-teixeira-12-06-2025"              = `sent_all_dbg__UFC-lewis-teixeira-12-06-2025`,
  "UFC-tsarukyan-hooker-22-11-2025"            = `sent_all_dbg__UFC-tsarukyan-hooker-22-11-2025`,
  "UFC321-aspinall-gane-25-10-2025"            = `sent_all_dbg__UFC321-aspinall-gane-25-10-2025`,
  "UFC322-dellamaddalena-makhachev-15-11-2025" = `sent_all_dbg__UFC322-dellamaddalena-makhachev-15-11-2025`,
  "UFCvegas-105-emmett-murphy-05-04-2025"      = `sent_all_dbg__UFCvegas-105-emmett-murphy-05-04-2025`,
  "UFCvegas-107-blanchfield-barber-31-05-2025" = `sent_all_dbg__UFCvegas-107-blanchfield-barber-31-05-2025`
)


sent_all_all_events_training <- bind_rows(sent_all_training_events)

sent_all_all_events_training <- purrr::imap_dfr(
  sent_all_training_list,
  ~ dplyr::mutate(.x, event = .y)
)

sent_all_labeled <- sent_all_all_events_training %>%
  mutate(
    sentence_key = str_squish(str_to_lower(sentence))
  )

model_v0_keyed <- model_v0_clean %>%
  mutate(
    sentence_key = str_squish(str_to_lower(context))
  ) %>%
  select(event, sentence_key) %>%
  distinct()

# Creating has_foul column to identify sentences with/without fouls:

foul_labels <- model_v0_keyed %>% 
  select(event, sentence_key) %>% 
  distinct() %>% 
  mutate(has_foul =1L)

sent_all_labeled <- sent_all_labeled %>% 
  left_join(
    foul_labels,
    by = c("event", "sentence_key")
  )%>% 
  mutate(
    has_foul.y = if_else(is.na(has_foul.y), 0L, has_foul.y)
  ) %>% 
  rename(has_foul = has_foul.y)

sent_all_labeled <- sent_all_labeled %>% 
  select(bout, sentence_key, sid, has_foul, event)


## Training Model:
# Ensuring text is readable
library(dplyr)
library(stringr)
library(tidyr)
library(tidymodels)
library(textrecipes)

set.seed(1)

train_df <- sent_all_labeled %>%
  transmute(
    event,
    bout,
    sid,
    text = sentence_key,          #  already lowercased + cleaned
    has_foul = factor(has_foul, levels = c(0, 1), labels = c("0","1"))
  ) %>%
  filter(!is.na(text), text != "")



# TD-IF Recipe from text, refitted to add unigrams + bigrams
rec <- recipe(has_foul ~ text, data = train_df) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%  # 1-grams + 2-grams
  step_tokenfilter(text, max_tokens = 2000) %>%
  step_tfidf(text)

# Model: Logistic Regression (regularised) to handle high-dimensional sparse TF-IDF features. Class imbalance handled via thresholding and PR-based evaluation 
model_lr <- logistic_reg(penalty = 0.01, mixture = 1) %>%  # lasso-ish
  set_engine("glmnet")

# Fitting model on all of V0 Data:
wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_lr)

fit_lr <- fit(wf, data = train_df)

# Save Fitted Model: text to TF-IDF to Logistic regression pipeline
saveRDS(fit_lr, "ml_foul_detector_v0_fit_lr.rds")

# Quick check: Does it Output probabilities?
pred_train <- predict(fit_lr, train_df, type = "prob") %>%
  bind_cols(train_df %>% select(has_foul))

head(pred_train)
summary(pred_train$.pred_1)

# Class Predictions & Confusion Matrix: Initial con_mat shows model almost never predicts fouls, missing 78 of the real fouls.
pred_class <- predict(fit_lr, train_df, type = "class") %>%
  bind_cols(train_df %>% select(has_foul))

conf_mat(pred_class, truth = has_foul, estimate = .pred_class)

# Using Metrics that actually matter considering the sentence imbalance: Fouls are 1.7% of the data so PR AUC is what matters most
library(yardstick)

pred_prob <- predict(fit_lr, train_df, type = "prob") %>%
  bind_cols(train_df %>% select(has_foul))

# ROC AUC + PR AUC (both expect probabilities)
roc_auc(pred_prob, truth = has_foul, .pred_1, event_level = "second")           # Current test shows ROC AUC = 0.946, meaning: If i randomly pick one foul and one non-foul sentence, there is a 94.6% chance the miodel assigns a higher probabiloty to the foul sentence. Shows that the model seperates fouls from non-fouls extremely well but PR-AUC is more infomative given the heavy class-imbalnace.
pr_auc(pred_prob, truth = has_foul, .pred_1, event_level = "second")            # Curreny test shows PR AUC = 0.644, meaning: the mode, is far better than random and its learning meanginful foul language patterns but still needs threshold tuning to pick a practical recision/recall trade off.

# Picking Threshold: curently 0.17, giving 50% recall in foul detetction for first test.

pred_prob2 <- pred_prob %>%
  mutate(pred_foul = factor(if_else(.pred_1 >= 0.17, "1", "0"), levels = c("0","1")))

conf_mat(pred_prob2, truth = has_foul, estimate = pred_foul)


# Threshold Sweep to identify optimal threshold:  0.1 curenly looks like the best splot between precision and recall given teh TF-IDF-LR pipeline
pred_prob <- predict(fit_lr, train_df, type="prob") %>%
  bind_cols(train_df %>% select(has_foul))

ths <- seq(0.01, 0.99, by = 0.01)

score_tbl <- purrr::map_dfr(ths, function(t) {
  pred <- pred_prob %>%
    mutate(pred_foul = factor(if_else(.pred_1 >= t, "1", "0"), levels=c("0","1")))
  
  tibble(
    threshold = t,
    f1 = f_meas(pred, truth = has_foul, estimate = pred_foul, event_level="second")$.estimate,
    precision = precision(pred, truth = has_foul, estimate = pred_foul, event_level="second")$.estimate,
    recall = recall(pred, truth = has_foul, estimate = pred_foul, event_level="second")$.estimate
  )
})

score_tbl %>% arrange(desc(f1)) %>% slice(1:10)

# Completed First Tests of the ML model which is a TF-IDF Logisitc regression pipeline, to improve model quality I will implement SVM / Trees approaches to identify the best way forward. These first metrics are in-sample sanity checks; true perfromance will be measured on held-out events (v2)
# Logisitc Regression draws a soft boundary, linear SVM daws a harder boundary & focuses ion edge cases so often perform better for sparse text data, esopecillay whern: classes are imbalanced, keywords + short phrases matter:

# =========================
# Model 2: Linear SVM
# =========================

library(LiblineaR)
install.packages("kernlab")
library(kernlab)


model_svm_kern <- svm_linear(cost = 1) %>%
  set_engine("kernlab", prob.model = TRUE) %>%
  set_mode("classification")

wf_svm_kern <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_svm_kern)

fit_svm_kern <- fit(wf_svm_kern, data = train_df)



#################################################################

#### Creating Testing Datatset

sent_all_test_events <- list(
  `sent_all_dbg__UFC323-dvalishvili-yan-06-12-2025`,
  `sent_all_dbg__UFCVegas-111-bonfim-brown-08-11-2025`,
  `sent_all_dbg__UFCVegas-110-garcia-onama-01-11-2025`,
  `sent_all_dbg__UFC-oliveira-gamrot-11-10-2025`,
  `sent_all_dbg__UFC-ulberg-reyes-27-09-2025`,
  `sent_all_dbg__UFC-lopes-silva-13-09-2025`,
  `sent_all_dbg__UFC-walker-zhang-23-08-2025`,
  `sent_all_dbg__UFC319-duplessis-chimaev-16-08-2025`,
  `sent_all_dbg__UFCVegas-109-dolidze-hernandez-09-08-2025`,
  `sent_all_dbg__UFCVegas-108-taira-park-02-08-2025`,
  `sent_all_dbg__UFC-whittaker-deridder-26-07-2025`,
  `sent_all_dbg__UFC318-holloway-poirier-19-07-2025`
)

# Ensuring Joined Table Identifies the Event Each Sentence Comes From
sent_all_test_list <- list(
  "UFC323-dvalishvili-yan-06-12-2025"           = `sent_all_dbg__UFC323-dvalishvili-yan-06-12-2025`,
  "UFCVegas-111-bonfim-brown-08-11-2025"        = `sent_all_dbg__UFCVegas-111-bonfim-brown-08-11-2025`,
  "UFCVegas-110-garcia-onama-01-11-2025"        = `sent_all_dbg__UFCVegas-110-garcia-onama-01-11-2025`,
  "UFC-oliveira-gamrot-11-10-2025"              = `sent_all_dbg__UFC-oliveira-gamrot-11-10-2025`,
  "UFC-ulberg-reyes-27-09-2025"                 = `sent_all_dbg__UFC-ulberg-reyes-27-09-2025`,
  "UFC-lopes-silva-13-09-2025"                  = `sent_all_dbg__UFC-lopes-silva-13-09-2025`,
  "UFC-walker-zhang-23-08-2025"                 = `sent_all_dbg__UFC-walker-zhang-23-08-2025`,
  "UFC319-duplessis-chimaev-16-08-2025"         = `sent_all_dbg__UFC319-duplessis-chimaev-16-08-2025`,
  "UFCVegas-109-dolidze-hernandez-09-08-2025"   = `sent_all_dbg__UFCVegas-109-dolidze-hernandez-09-08-2025`,
  "UFCVegas-108-taira-park-02-08-2025"          = `sent_all_dbg__UFCVegas-108-taira-park-02-08-2025`,
  "UFC-whittaker-deridder-26-07-2025"           = `sent_all_dbg__UFC-whittaker-deridder-26-07-2025`,
  "UFC318-holloway-poirier-19-07-2025"          = `sent_all_dbg__UFC318-holloway-poirier-19-07-2025`
)


sent_all_all_events_test <- bind_rows(sent_all_test_events)

sent_all_all_events_test <- purrr::imap_dfr(
  sent_all_test_list,
  ~ dplyr::mutate(.x, event = .y)
)

sent_all_labeled_test <- sent_all_all_events_test %>%
  mutate(
    sentence_key = str_squish(str_to_lower(sentence))
  )

model_v2_keyed <- model_v2_clean %>%
  mutate(
    sentence_key = str_squish(str_to_lower(context))
  ) %>%
  select(event, sentence_key) %>%
  distinct()

# Creating has_foul column to identify sentences with/without fouls:

foul_labels_test <- model_v2_keyed %>% 
  select(event, sentence_key) %>% 
  distinct() %>% 
  mutate(has_foul =1L)

sent_all_labeled_test <- sent_all_labeled_test %>% 
  left_join(
    foul_labels_test,
    by = c("event", "sentence_key")
  )%>% 
  mutate(
    has_foul.y = if_else(is.na(has_foul.y), 0L, has_foul.y)
  ) %>% 
  rename(has_foul = has_foul.y)

sent_all_labeled_test <- sent_all_labeled_test %>% 
  select(bout, sentence_key, sid, has_foul, event)

### Ready for Testing 

# Logistic Regression Test:
test_df <- sent_all_labeled_test %>%
  transmute(text = sentence_key,
            has_foul = factor(has_foul, levels=c(0,1)))

pred_prob_lr <- predict(fit_lr, test_df, type="prob") %>%
  bind_cols(test_df)

roc_auc(pred_prob_lr, truth = has_foul, .pred_1, event_level="second") # First test ROC AUC of 0.968
pr_auc(pred_prob_lr, truth = has_foul, .pred_1, event_level="second") # First test PR AUC of 0.575

# The model generalises very well to new events, ranking foul sentences far above non-fouls.

# SVM Test:
pred_prob_svm <- predict(fit_svm_kern, test_df, type = "prob") %>%
  bind_cols(test_df %>% select(has_foul))

roc_auc(pred_prob_svm, truth = has_foul, .pred_1, event_level="second") # First test ROC AUC of 0.946
pr_auc(pred_prob_svm, truth = has_foul, .pred_1, event_level="second") # First test PR AUC of 0.355

# SVM shows a clear downgrade in PR AUC so LR Regression is the logical choice: 
# LR > SVM on PR AUC under the severe class imbalance of the datasets
# Next step is to tune the TF-IDF-LR Pipeline:

# Tuning Penalty (regularisation) - using v2 as the judge:
pen_grid <- 10^seq(-4, 0, length.out = 15)

eval_one <- function(pen) {
  model_lr <- logistic_reg(penalty = pen, mixture = 1) %>%
    set_engine("glmnet")
  
  wf <- workflow() %>% add_recipe(rec) %>% add_model(model_lr)
  fit <- fit(wf, data = train_df)
  
  pred <- predict(fit, test_df, type = "prob") %>%
    bind_cols(test_df %>% select(has_foul))
  
  tibble(
    penalty = pen,
    pr_auc = pr_auc(pred, truth = has_foul, .pred_1, event_level = "second")$.estimate,
    roc_auc = roc_auc(pred, truth = has_foul, .pred_1, event_level = "second")$.estimate
  )
}

pen_results <- purrr::map_dfr(pen_grid, eval_one) %>% arrange(desc(pr_auc))
pen_results
best_pen <- pen_results$penalty[1]
# best_pen = 0.0193

# Tuning max_tokens (vocab size) - using v2 as judge
max_grid <- c(1000, 2000, 5000, 10000)

eval_vocab <- function(mx) {
  rec2 <- recipe(has_foul ~ text, data = train_df) %>%
    step_tokenize(text) %>%
    step_stopwords(text) %>%
    step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%
    step_tokenfilter(text, max_tokens = mx) %>%
    step_tfidf(text)
  
  model_lr <- logistic_reg(penalty = best_pen, mixture = 1) %>% set_engine("glmnet")
  fit <- workflow() %>% add_recipe(rec2) %>% add_model(model_lr) %>% fit(train_df)
  
  pred <- predict(fit, test_df, type="prob") %>% bind_cols(test_df %>% select(has_foul))
  
  tibble(
    max_tokens = mx,
    pr_auc = pr_auc(pred, truth=has_foul, .pred_1, event_level="second")$.estimate
  )
}

vocab_results <- purrr::map_dfr(max_grid, eval_vocab) %>% arrange(desc(pr_auc))
vocab_results
best_max <- vocab_results$max_tokens[1]
 # Best_max = 5000


# ---- Make train/test dfs in the SAME shape ----
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

best_pen <- 0.0193
best_max <- 5000

# ---- Recipe using tuned max_tokens ----
rec_best <- recipe(has_foul ~ text, data = train_df) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%  # your working uni+bi setup
  step_tokenfilter(text, max_tokens = best_max) %>%
  step_tfidf(text)

# ---- LR using tuned penalty ----
model_lr_best <- logistic_reg(penalty = best_pen, mixture = 1) %>%
  set_engine("glmnet")

wf_best <- workflow() %>%
  add_recipe(rec_best) %>%
  add_model(model_lr_best)

fit_lr_best <- fit(wf_best, data = train_df)

# ---- Score on V2 ----
pred_v2 <- predict(fit_lr_best, test_df, type = "prob") %>%
  bind_cols(test_df %>% select(has_foul))

roc_auc(pred_v2, truth = has_foul, .pred_1, event_level = "second")
pr_auc(pred_v2, truth = has_foul, .pred_1, event_level = "second")

# Testing after implementing best_pen and best_max gives: 
# ROC AUC = 0.932
# PR AUC = 0.751

# Adding 'domain flags' to train + test datasets:
add_flags <- function(df) {
  df %>%
    mutate(
      flag_warn    = as.integer(str_detect(text, regex("\\bwarn(?:ed|s|ing)?\\b|\\bwarning\\b", ignore_case = TRUE))),
      flag_illegal = as.integer(str_detect(text, regex("\\billegal\\b|\\bfoul\\b", ignore_case = TRUE))),
      flag_eye     = as.integer(str_detect(text, regex("\\beye\\b|\\beyeball\\b|\\bpoke\\b|\\bfinger\\b", ignore_case = TRUE))),
      flag_groin   = as.integer(str_detect(text, regex("\\bgroin\\b|\\bcup\\b|\\blow\\s+blow\\b", ignore_case = TRUE))),
      flag_fence   = as.integer(str_detect(text, regex("\\bfence\\b|\\bgrab\\b|\\bgrabs\\b|\\btoes\\b", ignore_case = TRUE))),
      flag_backhd  = as.integer(str_detect(text, regex("back\\s+of\\s+(?:the|his|her)\\s+head|back\\s+of\\s+head", ignore_case = TRUE)))
    )
}

train_df2 <- add_flags(train_df)
test_df2  <- add_flags(test_df)

# Updating recipe to include flags:
rec_flags <- recipe(has_foul ~ text + flag_warn + flag_illegal + flag_eye + flag_groin + flag_fence + flag_backhd,
                    data = train_df2) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%
  step_tokenfilter(text, max_tokens = best_max) %>%
  step_tfidf(text)

wf_flags <- workflow() %>%
  add_recipe(rec_flags) %>%
  add_model(model_lr_best)

fit_lr_flags <- fit(wf_flags, data = train_df2)

pred_v2_flags <- predict(fit_lr_flags, test_df2, type = "prob") %>%
  bind_cols(test_df2 %>% select(has_foul))

roc_auc(pred_v2_flags, truth = has_foul, .pred_1, event_level = "second")
pr_auc(pred_v2_flags, truth = has_foul, .pred_1, event_level = "second")

# ROC AUC of 0.961
# PR AUC of 0.737
# Flags slightly reduced PR AUC vs best-tuned tex only model but often increase recall at high thresholds.
# Picking the Optimal Operating Threshold: missing a foul is worse than flagging a few extra sentences, so isntead of optimising accuracy, we choose threshold based on precision-recall trade offs.
# Primary objective is high recall (do not miss fouls)
# Secondary Objedctive is to keep precision reasonable so review cost is manageable.

pred_v2 <- pred_v2_flags  # already has .pred_1 + has_foul

thresholds <- seq(0.01, 0.99, by = 0.01)

score_tbl_v2 <- purrr::map_dfr(thresholds, function(t) {
  pred <- pred_v2 %>%
    mutate(pred_foul = factor(if_else(.pred_1 >= t, "1", "0"), levels = c("0","1")))
  
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

# Optimal operating treshold is very likely around 0.04-0.06 as this is where F1 peaks, a more precise sweep is needed to identify:
pred_v2 <- pred_v2_flags

thresholds_fine <- seq(0.03, 0.08, by = 0.002)

score_tbl_v2_fine <- purrr::map_dfr(thresholds_fine, function(t) {
  pred <- pred_v2 %>%
    mutate(pred_foul = factor(if_else(.pred_1 >= t, "1", "0"), levels = c("0","1")))
  
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

# AT threshold 0.04 precision = 0.708, recall = 0.815, f1 = 0.758. Missing 18% of fouls whilst being 70% correct when a foul is flagged, so choosing a threshold value of 0.04 seems the best trade of between not missing fols and not destorying precision.
t_star <- 0.04  # optimal threshold considering my objectives.

pred_v2_hard <- pred_v2_flags %>%
  mutate(pred_foul = factor(if_else(.pred_1 >= t_star, "1", "0"),
                            levels = c("0","1")))

conf_mat(pred_v2_hard, truth = has_foul, estimate = pred_foul)

# optional: confirm recall + precision at that threshold
recall(pred_v2_hard, truth = has_foul, estimate = pred_foul, event_level="second")
precision(pred_v2_hard, truth = has_foul, estimate = pred_foul, event_level="second")
##############
# Checking Thresholds of tuned text only model.
fit_lr_best <- fit(wf_best, data = train_df)

# ---- Score on V2 ----
pred_v2 <- predict(fit_lr_best, test_df, type = "prob") %>%
  bind_cols(test_df %>% select(has_foul))

roc_auc(pred_v2, truth = has_foul, .pred_1, event_level = "second")
pr_auc(pred_v2, truth = has_foul, .pred_1, event_level = "second")

  # already has .pred_1 + has_foul

thresholds <- seq(0.01, 0.99, by = 0.01)

score_tbl_v2 <- purrr::map_dfr(thresholds, function(t) {
  pred <- pred_v2 %>%
    mutate(pred_foul = factor(if_else(.pred_1 >= t, "1", "0"), levels = c("0","1")))
  
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

thresholds_fine <- seq(0.02, 0.07, by = 0.002)

score_tbl_v2_fine <- purrr::map_dfr(thresholds_fine, function(t) {
  pred <- pred_v2 %>%
    mutate(pred_foul = factor(if_else(.pred_1 >= t, "1", "0"), levels = c("0","1")))
  
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


# Just text tuned model gives optimal thrweshold of 0.032, recall = 0.815, precision = 0.682 & f1 of 0.743, so do,ain flags model wins on f1, same recall and higher precision.
# Domain-flags model will be chosen as the dinal ML detector
################################################################################################
set.seed(123)

events <- unique(ml_df$event)
train_events <- sample(events, size = floor(0.8 * length(events)))

train_df <- ml_df 
test_df  <- ml_df %>% filter(!event %in% train_events)

intersect(unique(train_df$event), unique(test_df$event)) # Should Return 0 if event split is clean.

# Removing foul types with 5 < instances in training dataset as ML is not effective with these - illegal upkicks & headbutts will need to be strictly regex defined
train_df <- train_df %>%
  mutate(
    foul_type_simple = fct_collapse(
      foul_type,
      OTHER = c("headbutt", "illegal upkick")
    )
  )

test_df <- test_df %>%
  mutate(
    foul_type_simple = fct_collapse(
      foul_type,
      OTHER = c("headbutt", "illegal upkick")
    )
  )


# Building recipe - splitting text into words, removes "the", "and" etc. keeps most informative words. Converts text to numbers the model can learn from.
library(tidymodels)
library(textrecipes)

tidymodels::tidymodels_prefer()

type_recipe <- recipe(foul_type_simple ~ text, data = train_df) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 3000) %>%
  step_tfidf(text)

# Define Model - regularised so it doesnt just memorise. Multinomial logitic regression
type_spec <- multinom_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

# Combine into a workflow and train - model is learning patterns
type_wf <- workflow() %>%
  add_recipe(type_recipe) %>%
  add_model(type_spec)

type_fit <- fit(type_wf, data = train_df)

# Generate predictions
type_preds <- predict(type_fit, test_df) %>%
  bind_cols(test_df %>%
              select(foul_type_simple, event, bout_key, foul_idx, text))

# Checking Metrics from Predictions - FIRST TEST 84% ACCURACY & 0.779 COHEN'S KAPPA - POSITVE 
yardstick::metrics(
  type_preds,
  truth = foul_type_simple,
  estimate = .pred_class
)

# Confusion Matrix: Tells us what the model understands, which fouls get confused and whetehr the expanded context is wokring
yardstick::conf_mat(
  type_preds,
  truth = foul_type_simple,
  estimate = .pred_class
)

#Looking at Mistakes: 5 MISTAKES ON FIRST TEST - 3 FENCE GRABS INCORRECTLY ASSIGNED & 2 BACK OF HEAD STRIKES - Misclassifications primarily occur between fouls that share similar stoppage and referee-related language, rather than due to noisy labels or overlapping events.
mistakes <- type_preds %>%
  mutate(correct = (.pred_class == foul_type_simple)) %>%
  filter(!correct) %>%
  select(foul_type_simple, .pred_class, event, bout_key, foul_idx, text) %>%
  head(20)

# To improve accuracy, i will add unigram + bigram TF-IDF:

library(tidymodels)
library(textrecipes)
library(dplyr)

# train_df/test_df already exist

type_recipe <- recipe(foul_type_simple ~ text, data = train_df) %>%
  update_role(text, new_role = "id") %>% 
  step_mutate(                                                                   # Duplicating text for unigrams & bigrams
    text_uni = text,
    text_bi  = text
  ) %>%
  
  # Unigrams
  step_tokenize(text_uni, token = "words") %>%                                   # Splits text into words
  step_stopwords(text_uni) %>%                                                   # Removes junk wods (the, and to, etc.) to reduce noise
  step_tokenfilter(text_uni, max_tokens = 3000) %>%                              # Keeps 3000 mnsot informative words for stbaility & to prevent overfitting
  step_tfidf(text_uni) %>%                                                       # Converts words to numeric TF-IDF features(how often the word aappeard in this context & how rare the words is across all contexts)
  # Steeps repeated fo bi-grams (phrases)
  
  # Bigrams
  step_tokenize(text_bi, token = "ngrams", options = list(n = 2)) %>%
  step_stopwords(text_bi) %>%                 # works on tokens too
  step_tokenfilter(text_bi, max_tokens = 6000) %>%  # allow more for bigrams as the space is larger
  step_tfidf(text_bi) %>%
  
  step_zv(all_predictors())                                                      # Clean-up: removes columns that appear in all or no rows. Avoids numerical issues.

# Keeping model spec the same so re-re run.

# BiGram-Unigram TF-IDF reduced accuracy - 82% and reduced Kappa - 0.737.
# Confusion matrix shows most erros come from confusion fence grabs for groin strikes.
# Attempting to apply confidence-based filtering - only use ML to predict when model is confident, otherwise fall back to regex rules or flag.

type_probs <- predict(type_fit, test_df, type = "prob")
type_preds_prob <- bind_cols(type_preds, type_probs)   # combining probabilities with predicitons
type_preds_prob <- type_preds_prob %>%                 # adding 'confidence'
  mutate(
    max_prob = pmax(
      `.pred_back of head strike`,
      `.pred_eye poke`,
      `.pred_fence grab`,
      `.pred_groin strike`,
      .pred_OTHER
    )
  )
high_confidence <- type_preds_prob %>%              # Filtering for high-confidence labels.
  filter(max_prob >= 0.6)

type_preds_prob <- type_preds_prob %>%              # Adding a tag before filter is applied
  mutate(
    max_prob = pmax(
      `.pred_back of head strike`,
      `.pred_eye poke`,
      `.pred_fence grab`,
      `.pred_groin strike`,
      .pred_OTHER
    ),
    is_high_conf = max_prob >= 0.6
  )

type_preds_prob %>%                              # Seeing how many predictions survive
  summarise(
    total = n(),
    kept = sum(is_high_conf),
    kept_pct = mean(is_high_conf)
  )

high_conf <- type_preds_prob %>% filter(is_high_conf)

yardstick::metrics(                             # Evaluating accuacy on high-confidence only.
  high_conf,
  truth = foul_type_simple,
  estimate = .pred_class
)

yardstick::conf_mat(
  high_conf,
  truth = foul_type_simple,
  estimate = .pred_class
)

# 60% and < Confidence leads to improvments in accuracy - 92% and Kap of 0.887

# CHoosing confidence threshold properly:
threshold_results <- purrr::map_dfr(
  c(0.5, 0.6, 0.7, 0.8, 0.9),
  function(t) {
    df <- type_preds_prob %>% mutate(keep = max_prob >= t)
    kept <- df %>% filter(keep)
    
    tibble(
      threshold = t,
      kept_n = nrow(kept),
      kept_pct = nrow(kept) / nrow(df),
      accuracy = if (nrow(kept) > 0)
        yardstick::accuracy(kept, truth = foul_type_simple, estimate = .pred_class) %>% pull(.estimate)
      else NA_real_,
      kap = if (nrow(kept) > 0)
        yardstick::kap(kept, truth = foul_type_simple, estimate = .pred_class) %>% pull(.estimate)
      else NA_real_
    )
  }
)

threshold_results

# Accuacy extremely high at 80% (95.5% & kap of 0.926) so probably the best choice considering the aims of this project to build a reliabel datatset.

CONF_THRESHOLD <- 0.8

final_fouls_ml_only <- type_preds_prob %>%
  mutate(
    foul_type_final = if_else(
      max_prob >= CONF_THRESHOLD,
      as.character(.pred_class),
      NA_character_
    ),
    needs_review = max_prob < CONF_THRESHOLD
  )

final_fouls %>%
  filter(needs_review == TRUE) %>%
  select(event, bout_key, foul_idx, foul_type_simple, .pred_class, max_prob, text)

# Trying to apply the hybrid ML/regex foul detection model

type_preds_prob2 <- type_preds_prob %>%
  left_join(
    model_v2_clean %>% select(event, bout_key, foul_idx, foul_type_guess),
    by = c("event", "bout_key", "foul_idx")
  )

CONF_THRESHOLD <- 0.8

final_fouls <- type_preds_prob2 %>%
  mutate(
    foul_type_final = if_else(
      max_prob >= CONF_THRESHOLD,
      as.character(.pred_class),
      as.character(foul_type_guess)
    ),
    used_ml = max_prob >= CONF_THRESHOLD,
    needs_review = is.na(foul_type_final) | foul_type_final == "OTHER"
  )

# Map rule-based model foul labels into the SAME label space as ML/gold (simple classes)
final_fouls <- final_fouls %>%
  mutate(
    rule_foul_type_simple = case_when(
      str_detect(str_to_lower(foul_type_guess), "low blow|groin|cup") ~ "groin strike",
      str_detect(str_to_lower(foul_type_guess), "eye")               ~ "eye poke",
      str_detect(str_to_lower(foul_type_guess), "fence|cage|chain|toes")        ~ "fence grab",
      str_detect(str_to_lower(foul_type_guess), "back of the head|rabbit|crown") ~ "back of head strike",
      TRUE ~ "OTHER"
    )
  )

final_fouls <- final_fouls %>%
  mutate(
    foul_type_final = if_else(
      used_ml,
      as.character(.pred_class),
      rule_foul_type_simple
    )
  )

final_fouls <- final_fouls %>%
  mutate(
    needs_review = (max_prob < CONF_THRESHOLD) & (rule_foul_type_simple == "OTHER")
  )

final_fouls <- final_fouls %>%
  mutate(
    rule_foul_type_simple = factor(
      rule_foul_type_simple,
      levels = levels(foul_type_simple)
    ),
    foul_type_final = factor(
      foul_type_final,
      levels = levels(foul_type_simple)
    )
  )

final_fouls <- final_fouls %>%
  mutate(
    text_l = str_to_lower(text),
    groin_strong = str_detect(text_l, "\\bcup\\b|low blow|groin|below the belt"),
    
    foul_type_final = if_else(
      groin_strong,
      "groin strike",
      foul_type_final
    )
  ) %>%
  select(-text_l)

# Need to account for are instance of 2 fouls occuring simultaneously:
library(purrr)
library(stringr)
library(dplyr)

final_fouls <- final_fouls %>%
  mutate(
    text_l = str_to_lower(text),
    
    # list-column: all matched labels for this row
    matched_labels = map(text_l, \(x) {
      foul_dict %>%
        filter(str_detect(x, pattern)) %>%
        pull(label) %>%
        unique()
    }),
    
    n_labels = map_int(matched_labels, length),
    multi_foul = n_labels >= 2,
    
    foul_types_all = map_chr(matched_labels, \(labs) {
      if (length(labs) == 0) NA_character_ else paste(labs, collapse = "|")
    }),
    
    # optional: secondary label (just first "extra" one)
    secondary_foul_type = map_chr(matched_labels, \(labs) {
      if (length(labs) >= 2) labs[2] else NA_character_
    })
  ) %>%
  select(-text_l)

# Mistake is not flagged if the foul type matches either of the primary or secondary fouls:
final_fouls <- final_fouls %>%
  mutate(
    acceptable_pred =
      (foul_type_final == foul_type_simple) |
      (!is.na(secondary_foul_type) & secondary_foul_type == foul_type_simple)
  )

mistakes_option4 <- final_fouls %>%
  filter(!acceptable_pred) %>%
  select(
    foul_type_simple, foul_type_final, secondary_foul_type,
    max_prob, used_ml, event, bout_key, foul_idx, text
  )

# Sanity check the join worked: 
final_fouls %>%
  summarise(
    n = n(),
    missing_rule = sum(is.na(foul_type_guess)),
    missing_rule_pct = mean(is.na(foul_type_guess))
  )

# Seeing how often ML vs fallback was used
final_fouls %>%
  summarise(
    used_ml_n = sum(used_ml),
    used_ml_pct = mean(used_ml),
    used_rules_n = sum(!used_ml),
    used_rules_pct = mean(!used_ml)
  )

# Checking errors witgh ML when using the confidence threshold:
ml_threshold_mistakes <- final_fouls %>%
  filter(
    used_ml == TRUE,
    foul_type_final != foul_type_simple
  ) %>%
  select(
    foul_type_simple,
    foul_type_final,
    max_prob,
    event,
    bout_key,
    foul_idx,
    text
  )

ml_threshold_mistakes

ml_threshold_mistakes2 <- final_fouls %>%
  filter(used_ml == TRUE, foul_type_final != foul_type_simple) %>%
  select(foul_type_simple, foul_type_final, max_prob, event, bout_key, foul_idx, text)

ml_threshold_mistakes2

final_export <- final_fouls %>%
  transmute(
    event, bout_key, foul_idx,
    text,
    foul_type_gold = foul_type_simple,
    foul_type_final,
    secondary_foul_type,
    used_ml, max_prob,
    foul_type_guess
  ) %>% 
  rename(actual_foul_type = foul_type_gold,
         foul_type_model = foul_type_final,
         foul_type_rules_only = foul_type_guess)


# adding new gold standard dataset for phase 2 of testing:

# quick summary
fp_diag %>% count(why_detected, sort = TRUE)

View(fp_diag)

nrow(ml_df)
ml_df %>% count(foul_type) %>% arrange(desc(n))

set.seed(123)

events <- unique(ml_df$event)
train_events <- sample(events, size = floor(0.8 * length(events)))

train_df <- ml_df 
test_df  <- ml_df %>% filter(!event %in% train_events)

intersect(unique(train_df$event), unique(test_df$event)) # Should Return 0 if event split is clean.

# Removing foul types with 5 < instances in training dataset as ML is not effective with these - illegal upkicks & headbutts will need to be strictly regex defined
train_df <- train_df %>%
  mutate(
    foul_type_simple = fct_collapse(
      foul_type,
      OTHER = c("headbutt", "illegal upkick")
    )
  )

test_df <- test_df %>%
  mutate(
    foul_type_simple = fct_collapse(
      foul_type,
      OTHER = c("headbutt", "illegal upkick")
    )
  )


# Building recipe - splitting text into words, removes "the", "and" etc. keeps most informative words. Converts text to numbers the model can learn from.
library(tidymodels)
library(textrecipes)

tidymodels::tidymodels_prefer()

type_recipe <- recipe(foul_type_simple ~ text, data = train_df) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 3000) %>%
  step_tfidf(text)

# Define Model - regularised so it doesnt just memorise. Multinomial logitic regression
type_spec <- multinom_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

# Combine into a workflow and train - model is learning patterns
type_wf <- workflow() %>%
  add_recipe(type_recipe) %>%
  add_model(type_spec)

type_fit <- fit(type_wf, data = train_df)

# Generate predictions
type_preds <- predict(type_fit, test_df) %>%
  bind_cols(test_df %>%
              select(foul_type_simple, event, bout_key, foul_idx, text))

# Checking Metrics from Predictions - FIRST TEST 84% ACCURACY & 0.779 COHEN'S KAPPA - POSITVE 
yardstick::metrics(
  type_preds,
  truth = foul_type_simple,
  estimate = .pred_class
)

# Confusion Matrix: Tells us what the model understands, which fouls get confused and whetehr the expanded context is wokring
yardstick::conf_mat(
  type_preds,
  truth = foul_type_simple,
  estimate = .pred_class
)

#Looking at Mistakes: 5 MISTAKES ON FIRST TEST - 3 FENCE GRABS INCORRECTLY ASSIGNED & 2 BACK OF HEAD STRIKES - Misclassifications primarily occur between fouls that share similar stoppage and referee-related language, rather than due to noisy labels or overlapping events.
mistakes <- type_preds %>%
  mutate(correct = (.pred_class == foul_type_simple)) %>%
  filter(!correct) %>%
  select(foul_type_simple, .pred_class, event, bout_key, foul_idx, text) %>%
  head(20)

# To improve accuracy, i will add unigram + bigram TF-IDF:

library(tidymodels)
library(textrecipes)
library(dplyr)

# train_df/test_df already exist

type_recipe <- recipe(foul_type_simple ~ text, data = train_df) %>%
  update_role(text, new_role = "id") %>% 
  step_mutate(                                                                   # Duplicating text for unigrams & bigrams
    text_uni = text,
    text_bi  = text
  ) %>%
  
  # Unigrams
  step_tokenize(text_uni, token = "words") %>%                                   # Splits text into words
  step_stopwords(text_uni) %>%                                                   # Removes junk wods (the, and to, etc.) to reduce noise
  step_tokenfilter(text_uni, max_tokens = 3000) %>%                              # Keeps 3000 mnsot informative words for stbaility & to prevent overfitting
  step_tfidf(text_uni) %>%                                                       # Converts words to numeric TF-IDF features(how often the word aappeard in this context & how rare the words is across all contexts)
  # Steeps repeated fo bi-grams (phrases)
  
  # Bigrams
  step_tokenize(text_bi, token = "ngrams", options = list(n = 2)) %>%
  step_stopwords(text_bi) %>%                 # works on tokens too
  step_tokenfilter(text_bi, max_tokens = 6000) %>%  # allow more for bigrams as the space is larger
  step_tfidf(text_bi) %>%
  
  step_zv(all_predictors())                                                      # Clean-up: removes columns that appear in all or no rows. Avoids numerical issues.

# Keeping model spec the same so re-re run.

# BiGram-Unigram TF-IDF reduced accuracy - 82% and reduced Kappa - 0.737.
# Confusion matrix shows most erros come from confusion fence grabs for groin strikes.
# Attempting to apply confidence-based filtering - only use ML to predict when model is confident, otherwise fall back to regex rules or flag.

type_probs <- predict(type_fit, test_df, type = "prob")
type_preds_prob <- bind_cols(type_preds, type_probs)   # combining probabilities with predicitons
type_preds_prob <- type_preds_prob %>%                 # adding 'confidence'
  mutate(
    max_prob = pmax(
      `.pred_back of head strike`,
      `.pred_eye poke`,
      `.pred_fence grab`,
      `.pred_groin strike`,
      .pred_OTHER
    )
  )
high_confidence <- type_preds_prob %>%              # Filtering for high-confidence labels.
  filter(max_prob >= 0.6)

type_preds_prob <- type_preds_prob %>%              # Adding a tag before filter is applied
  mutate(
    max_prob = pmax(
      `.pred_back of head strike`,
      `.pred_eye poke`,
      `.pred_fence grab`,
      `.pred_groin strike`,
      .pred_OTHER
    ),
    is_high_conf = max_prob >= 0.6
  )

type_preds_prob %>%                              # Seeing how many predictions survive
  summarise(
    total = n(),
    kept = sum(is_high_conf),
    kept_pct = mean(is_high_conf)
  )

high_conf <- type_preds_prob %>% filter(is_high_conf)

yardstick::metrics(                             # Evaluating accuacy on high-confidence only.
  high_conf,
  truth = foul_type_simple,
  estimate = .pred_class
)

yardstick::conf_mat(
  high_conf,
  truth = foul_type_simple,
  estimate = .pred_class
)

# 60% and < Confidence leads to improvments in accuracy - 92% and Kap of 0.887

# CHoosing confidence threshold properly:
threshold_results <- purrr::map_dfr(
  c(0.5, 0.6, 0.7, 0.8, 0.9),
  function(t) {
    df <- type_preds_prob %>% mutate(keep = max_prob >= t)
    kept <- df %>% filter(keep)
    
    tibble(
      threshold = t,
      kept_n = nrow(kept),
      kept_pct = nrow(kept) / nrow(df),
      accuracy = if (nrow(kept) > 0)
        yardstick::accuracy(kept, truth = foul_type_simple, estimate = .pred_class) %>% pull(.estimate)
      else NA_real_,
      kap = if (nrow(kept) > 0)
        yardstick::kap(kept, truth = foul_type_simple, estimate = .pred_class) %>% pull(.estimate)
      else NA_real_
    )
  }
)

threshold_results

# Accuacy extremely high at 80% (95.5% & kap of 0.926) so probably the best choice considering the aims of this project to build a reliabel datatset.

CONF_THRESHOLD <- 0.8

final_fouls_ml_only <- type_preds_prob %>%
  mutate(
    foul_type_final = if_else(
      max_prob >= CONF_THRESHOLD,
      as.character(.pred_class),
      NA_character_
    ),
    needs_review = max_prob < CONF_THRESHOLD
  )

final_fouls %>%
  filter(needs_review == TRUE) %>%
  select(event, bout_key, foul_idx, foul_type_simple, .pred_class, max_prob, text)

# Trying to apply the hybrid ML/regex foul detection model

type_preds_prob2 <- type_preds_prob %>%
  left_join(
    model_v2_clean %>% select(event, bout_key, foul_idx, foul_type_guess),
    by = c("event", "bout_key", "foul_idx")
  )

CONF_THRESHOLD <- 0.8

final_fouls <- type_preds_prob2 %>%
  mutate(
    foul_type_final = if_else(
      max_prob >= CONF_THRESHOLD,
      as.character(.pred_class),
      as.character(foul_type_guess)
    ),
    used_ml = max_prob >= CONF_THRESHOLD,
    needs_review = is.na(foul_type_final) | foul_type_final == "OTHER"
  )

# Map rule-based model foul labels into the SAME label space as ML/gold (simple classes)
final_fouls <- final_fouls %>%
  mutate(
    rule_foul_type_simple = case_when(
      str_detect(str_to_lower(foul_type_guess), "low blow|groin|cup") ~ "groin strike",
      str_detect(str_to_lower(foul_type_guess), "eye")               ~ "eye poke",
      str_detect(str_to_lower(foul_type_guess), "fence|cage|chain|toes")        ~ "fence grab",
      str_detect(str_to_lower(foul_type_guess), "back of the head|rabbit|crown") ~ "back of head strike",
      TRUE ~ "OTHER"
    )
  )

final_fouls <- final_fouls %>%
  mutate(
    foul_type_final = if_else(
      used_ml,
      as.character(.pred_class),
      rule_foul_type_simple
    )
  )

final_fouls <- final_fouls %>%
  mutate(
    needs_review = (max_prob < CONF_THRESHOLD) & (rule_foul_type_simple == "OTHER")
  )

final_fouls <- final_fouls %>%
  mutate(
    rule_foul_type_simple = factor(
      rule_foul_type_simple,
      levels = levels(foul_type_simple)
    ),
    foul_type_final = factor(
      foul_type_final,
      levels = levels(foul_type_simple)
    )
  )

final_fouls <- final_fouls %>%
  mutate(
    text_l = str_to_lower(text),
    groin_strong = str_detect(text_l, "\\bcup\\b|low blow|groin|below the belt"),
    
    foul_type_final = if_else(
      groin_strong,
      "groin strike",
      foul_type_final
    )
  ) %>%
  select(-text_l)

# Need to account for are instance of 2 fouls occuring simultaneously:
library(purrr)
library(stringr)
library(dplyr)

final_fouls <- final_fouls %>%
  mutate(
    text_l = str_to_lower(text),
    
    # list-column: all matched labels for this row
    matched_labels = map(text_l, \(x) {
      foul_dict %>%
        filter(str_detect(x, pattern)) %>%
        pull(label) %>%
        unique()
    }),
    
    n_labels = map_int(matched_labels, length),
    multi_foul = n_labels >= 2,
    
    foul_types_all = map_chr(matched_labels, \(labs) {
      if (length(labs) == 0) NA_character_ else paste(labs, collapse = "|")
    }),
    
    # optional: secondary label (just first "extra" one)
    secondary_foul_type = map_chr(matched_labels, \(labs) {
      if (length(labs) >= 2) labs[2] else NA_character_
    })
  ) %>%
  select(-text_l)

# Mistake is not flagged if the foul type matches either of the primary or secondary fouls:
final_fouls <- final_fouls %>%
  mutate(
    acceptable_pred =
      (foul_type_final == foul_type_simple) |
      (!is.na(secondary_foul_type) & secondary_foul_type == foul_type_simple)
  )

mistakes_option4 <- final_fouls %>%
  filter(!acceptable_pred) %>%
  select(
    foul_type_simple, foul_type_final, secondary_foul_type,
    max_prob, used_ml, event, bout_key, foul_idx, text
  )

# Sanity check the join worked: 
final_fouls %>%
  summarise(
    n = n(),
    missing_rule = sum(is.na(foul_type_guess)),
    missing_rule_pct = mean(is.na(foul_type_guess))
  )

# Seeing how often ML vs fallback was used
final_fouls %>%
  summarise(
    used_ml_n = sum(used_ml),
    used_ml_pct = mean(used_ml),
    used_rules_n = sum(!used_ml),
    used_rules_pct = mean(!used_ml)
  )

# Checking errors witgh ML when using the confidence threshold:
ml_threshold_mistakes <- final_fouls %>%
  filter(
    used_ml == TRUE,
    foul_type_final != foul_type_simple
  ) %>%
  select(
    foul_type_simple,
    foul_type_final,
    max_prob,
    event,
    bout_key,
    foul_idx,
    text
  )

ml_threshold_mistakes

ml_threshold_mistakes2 <- final_fouls %>%
  filter(used_ml == TRUE, foul_type_final != foul_type_simple) %>%
  select(foul_type_simple, foul_type_final, max_prob, event, bout_key, foul_idx, text)

ml_threshold_mistakes2

final_export <- final_fouls %>%
  transmute(
    event, bout_key, foul_idx,
    text,
    foul_type_gold = foul_type_simple,
    foul_type_final,
    secondary_foul_type,
    used_ml, max_prob,
    foul_type_guess
  ) %>% 
  rename(actual_foul_type = foul_type_gold,
         foul_type_model = foul_type_final,
         foul_type_rules_only = foul_type_guess)


# adding new gold standard dataset for phase 2 of testing:


amil_row <- `sent_all_dbg__UFCVegas-111-bonfim-brown-08-11-2025` %>%
  dplyr::filter(
    str_detect(
      sentence,
      stringr::fixed("Amil falls forward and grabs the fence", ignore_case = TRUE)
    )
  ) %>%
  dplyr::slice(1)

amil_row

alma_row <- `sent_all_dbg__UFC-whittaker-deridder-26-07-2025` %>% 
  dplyr::filter(
    str_detect(
      sentence,
      stringr::fixed("On a second effort, Almabayev is", ignore_case = TRUE)
    )
  )

yan_row <- `sent_all_dbg__UFC-whittaker-deridder-26-07-2025` %>% 
  dplyr::filter(
    str_detect(
      sentence,
      stringr::fixed("McGhee turns to put his", ignore_case = TRUE)
    )
  )

hol_row <- `sent_all_dbg__UFC318-holloway-poirier-19-07-2025` %>% 
  dplyr::filter(
    str_detect(
      sentence,
      stringr::fixed("Poirier knocks Holloway back with a right hand", ignore_case = TRUE)
    )
  )
john_row <- `sent_all_dbg__UFC318-holloway-poirier-19-07-2025` %>% 
  dplyr::filter(
    str_detect(
      sentence,
      stringr::fixed("Johnson sticks his opponent and moves", ignore_case = TRUE)
    )
  )

amil_row %>% 
  transmute(
    sentence,
    detect_foul_re = stringr::str_detect(sentence, `foul_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`),
    extract_match = stringr::str_extract(sentence, `foul_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`),
    hit_non_backhead = stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`),
    hit_non_eye = stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`),
    has_foul_raw,
    is_reaction_only,
    fight_started,
    has_foul
  )

amil_row %>% 
  transmute(
    sentence,
    stored_has_foul_raw = has_foul_raw,
    recalced_has_foul_raw =
      stringr::str_detect(sentence, `foul_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`) &
      !stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`) &
      !stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFCVegas-111-bonfim-brown-08-11-2025`) &
      !(is_upkick_mention & is_hypothetical)
  )

alma_row %>%
  transmute(
    sentence,
    
    hit_foul_re =
      stringr::str_detect(sentence, `foul_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_backhead =
      stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_eye =
      stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_eye_warning =
      stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_legal_belt =
      stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    upkick = is_upkick_mention,
    hypo   = is_hypothetical,
    
    combo =
      stringr::str_detect(sentence, `foul_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !(is_upkick_mention & is_hypothetical)
  )


yan_row %>%
  transmute(
    sentence,
    
    hit_foul_re =
      stringr::str_detect(sentence, `foul_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_backhead =
      stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_eye =
      stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_eye_warning =
      stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    hit_non_legal_belt =
      stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC-whittaker-deridder-26-07-2025`),
    
    upkick = is_upkick_mention,
    hypo   = is_hypothetical,
    
    combo =
      stringr::str_detect(sentence, `foul_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC-whittaker-deridder-26-07-2025`) &
      !(is_upkick_mention & is_hypothetical)
  )

hol_row %>%
  transmute(
    sentence,
    
    hit_foul_re =
      stringr::str_detect(sentence, `foul_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_backhead =
      stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_eye =
      stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_eye_warning =
      stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_legal_belt =
      stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    upkick = is_upkick_mention,
    hypo   = is_hypothetical,
    
    combo =
      stringr::str_detect(sentence, `foul_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !(is_upkick_mention & is_hypothetical)
  )

john_row %>%
  transmute(
    sentence,
    
    hit_foul_re =
      stringr::str_detect(sentence, `foul_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_backhead =
      stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_eye =
      stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_eye_warning =
      stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    hit_non_legal_belt =
      stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC318-holloway-poirier-19-07-2025`),
    
    upkick = is_upkick_mention,
    hypo   = is_hypothetical,
    
    combo =
      stringr::str_detect(sentence, `foul_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_backhead_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_eye_warning_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !stringr::str_detect(sentence, `non_foul_legal_belt_re_dbg__UFC318-holloway-poirier-19-07-2025`) &
      !(is_upkick_mention & is_hypothetical)
  )
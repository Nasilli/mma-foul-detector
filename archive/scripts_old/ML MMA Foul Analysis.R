# --------------------------------------------------
# 0. Packages
# --------------------------------------------------
library(dplyr)
library(readr)
library(tidymodels)
library(textrecipes)
library(glmnet)
library(forcats)
library(discrim)
library(naivebayes)

tidymodels::tidymodels_prefer()

# --------------------------------------------------
# 1. Load data
# --------------------------------------------------
acc <- read_csv("Baseline Foul Model.csv")

# Make factors once here
acc <- acc %>%
  mutate(
    foul_type  = factor(foul_type),
    fouler     = factor(fouler),
    pause      = factor(fight_paused, levels = c(FALSE, TRUE)),
    deduction  = factor(point_deduction, levels = c(FALSE, TRUE))
  )

# --------------------------------------------------
# 2. Build modelling tables
# --------------------------------------------------

# Foul type (this is the one we’re modelling first)
df_type <- acc %>%
  transmute(
    bout_key,
    text      = context,
    foul_type = foul_type
  ) %>%
  filter(!is.na(foul_type))

# Fouler
df_fouler <- acc %>%
  transmute(
    bout_key,
    text   = context,
    fouler = fouler
  ) %>%
  filter(!is.na(fouler))

# Pause
df_pause <- acc %>%
  transmute(
    bout_key,
    text  = context,
    pause = pause
  )

# Deduction
df_ded <- acc %>%
  transmute(
    bout_key,
    text      = context,
    deduction = deduction
  )

# --------------------------------------------------
# 3. Train / test split BY FIGHT (bout_key)
# --------------------------------------------------
set.seed(123)

all_bouts  <- unique(df_type$bout_key)
n_train    <- floor(0.8 * length(all_bouts))
train_bouts <- sample(all_bouts, n_train)

# foul-type splits
type_train <- df_type %>% filter(bout_key %in% train_bouts)
type_test  <- df_type %>% filter(!bout_key %in% train_bouts)

# (same bouts are reused for fouler / pause / deduction later)
fouler_train <- df_fouler %>% filter(bout_key %in% train_bouts)
fouler_test  <- df_fouler %>% filter(!bout_key %in% train_bouts)

pause_train  <- df_pause  %>% filter(bout_key %in% train_bouts)
pause_test   <- df_pause  %>% filter(!bout_key %in% train_bouts)

ded_train    <- df_ded    %>% filter(bout_key %in% train_bouts)
ded_test     <- df_ded    %>% filter(!bout_key %in% train_bouts)

# quick sanity: no fights appear in both train and test
intersect(type_train$bout_key, type_test$bout_key)
intersect(fouler_train$bout_key, fouler_test$bout_key)
intersect(pause_train$bout_key,  pause_test$bout_key)
intersect(ded_train$bout_key,    ded_test$bout_key)

# --------------------------------------------------
# 4. Handle rare foul types (simpler: DROP them)
# --------------------------------------------------

# Check class counts in the *training* set
type_train %>% count(foul_type)

# Any foul type with < 3 examples is too rare to model
freq_train  <- type_train %>% count(foul_type)
rare_types  <- freq_train %>% filter(n < 3) %>% pull(foul_type)

rare_types   # just to see which ones we’re dropping

# Filter them out of train AND test, then drop unused factor levels
type_train <- type_train %>%
  filter(!foul_type %in% rare_types) %>%
  mutate(foul_type = fct_drop(foul_type))

type_test <- type_test %>%
  filter(!foul_type %in% rare_types) %>%
  mutate(foul_type = fct_drop(foul_type))

# Sanity check – now every class has at least 3 examples
type_train %>% count(foul_type)

# --------------------------------------------------
# 5. Text recipe + multinomial model for foul_type
# --------------------------------------------------

type_recipe <- recipe(foul_type ~ text, data = type_train) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 3000) %>%
  step_tfidf(text)

foul_type_spec <- multinom_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

foul_type_wf <- workflow() %>%
  add_model(foul_type_spec) %>%
  add_recipe(type_recipe)

foul_type_fit <- foul_type_wf %>%
  fit(data = type_train)

# --------------------------------------------------
# 6. Evaluate foul_type model
# --------------------------------------------------

type_preds <- predict(foul_type_fit, type_test) %>%
  bind_cols(type_test %>% select(foul_type, bout_key, text))

metrics_type <- yardstick::metrics(
  type_preds,
  truth    = foul_type,
  estimate = .pred_class
)

metrics_type

conf_mat_type <- yardstick::conf_mat(
  type_preds,
  truth    = foul_type,
  estimate = .pred_class
)

conf_mat_type

#----------------------------------
# Recipe for Fouler Training:
#----------------------------------

# Text recipe
fouler_recipe <- recipe(fouler ~ text, data = fouler_train) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 4000) %>%
  step_tfidf(text) %>%
  step_zv(all_predictors())     # drop any all-zero / constant columns


fouler_spec <- naive_Bayes() %>%
  set_engine("naivebayes", usekernel = FALSE) %>%  # <- key change
  set_mode("classification")

# Workflow
fouler_wf <- workflow() %>%
  add_recipe(fouler_recipe) %>%
  add_model(fouler_spec)

# Fit
fouler_fit <- fouler_wf %>% fit(data = fouler_train)

# Predict
fouler_preds <- predict(fouler_fit, fouler_test) %>%
  bind_cols(fouler_test)

# Metrics
yardstick::metrics(fouler_preds, truth = fouler, estimate = .pred_class)


#-------------------------
# Build fouler dataset
#-------------------------
df_fouler <- acc %>%
  transmute(
    bout_key,
    text   = context,
    fouler = factor(fouler)
  ) %>%
  filter(!is.na(fouler), text != "")

# How many examples / fighters?
df_fouler %>% count(fouler) %>% arrange(desc(n))

# Same fight-level split as before
fouler_train <- df_fouler %>%
  filter(bout_key %in% train_bouts)

fouler_test <- df_fouler %>%
  filter(!bout_key %in% train_bouts)

nrow(fouler_train); nrow(fouler_test)
length(unique(fouler_train$fouler))
length(unique(fouler_test$fouler))

#-------------------------
# Recipe for fouler
#-------------------------
fouler_recipe <- recipe(fouler ~ text, data = fouler_train) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 4000) %>%
  step_tfidf(text) %>%
  step_zv(all_predictors())

# Multinomial glmnet again
fouler_spec <- multinom_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

fouler_wf <- workflow() %>%
  add_recipe(fouler_recipe) %>%
  add_model(fouler_spec)

# Fit
fouler_fit <- fouler_wf %>%
  fit(data = fouler_train)

# Predict
fouler_preds <- predict(fouler_fit, fouler_test) %>%
  bind_cols(fouler_test)

# Check for NAs
table(is.na(fouler_preds$.pred_class))

# Drop any NAs just in case (there usually won't be many / any)
fouler_preds_clean <- fouler_preds %>%
  filter(!is.na(.pred_class))

# Metrics
metrics_fouler <- yardstick::metrics(
  fouler_preds_clean,
  truth    = fouler,
  estimate = .pred_class
)
metrics_fouler

conf_mat_fouler <- yardstick::conf_mat(
  fouler_preds_clean,
  truth    = fouler,
  estimate = .pred_class
)
conf_mat_fouler
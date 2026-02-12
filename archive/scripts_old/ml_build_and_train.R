library(tidyverse)
library(tidymodels)
library(textrecipes)

tidymodels::tidymodels_prefer()

foul_dict <- tibble::tribble(
  ~label,                ~pattern,
  "groin strike",        "low blow|groin|\\bcup\\b|below the belt",
  "eye poke",            "eye poke|poke.*eye|finger.*eye|thumb.*eye|\\beye\\b",
  "fence grab",          "(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?).{0,25}\\b(fence|cage)\\b|\\b(fence|cage)\\b.{0,25}(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?)",
  "back of head strike", "back of (the )?head|rabbit punch|behind the ear|to the back of the head"
)
# Load regex output
model_v0_clean <- read_csv("model_v0_all_events.csv")

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


nrow(ml_df)
ml_df %>% count(foul_type) %>% arrange(desc(n))

set.seed(123)

events <- unique(ml_df$event)
train_events <- sample(events, size = floor(0.8 * length(events)))

train_df <- ml_df %>% filter(event %in% train_events)
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
    model_v0_clean %>% select(event, bout_key, foul_idx, foul_type_guess),
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
# 03_train_models.R

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

#Train ML foul detector (logistic regression + TF-IDF + engineered flags) and save fitted workflow

# Regex patterns used for feature flags (anchors + known non_foul contexts)
# Patterns sourced from process_event_v2

# Back-of-head anchors (allows possesive/name variation)
back_of_head_pat <- "back of\\s+(?:the\\s+|his\\s+|her\\s+|[A-Za-z]+(?:['’`]?\\s*)?)?head"

back_head_patterns <- c(
  paste0(
    "(?:elbow|elbows?|strike|strikes?|punch(?:es)?|kick(?:s)?|knee(?:s)?|hammerfist(?:s)?|shot|shots|blow|blows|club(?:s)?|bangs?|smacks?|hacks?|cracks?|lands?).{0,120}",
    back_of_head_pat
  ),
  paste0(
    back_of_head_pat,
    ".{0,60}(?:illegal|foul|shot|strike|elbow|punch|kick|knee|warning|warns?)"
  ),
  paste0(
    "warn(?:s|ed)?\\s+for\\s+strik(?:e|es|ing|ed|uck)?\\s+(?:to\\s+)?",
    back_of_head_pat
  )
)
back_head_patterns <- c(
  back_head_patterns,
  "warn(?:ed|s)?\\s+for\\s+strik\\w*\\s+(?:to\\s+)?the\\s+back\\s+of\\s+(?:his|her|the)\\s+head"
)

# High-signal foul anchors (broad catch for recall)
strong_foul_anchor_re <- regex(
  paste(
    c(
      "cup",
      "groin",
      "low blow",
      "south of the border",
      "below the belt",
      "eye poke",
      "eyeball",
      "\\bcup\\b", "\\bgroin\\b", "low blow", "below the belt",
      "\\beye(s)?\\b",
      "headbutt",
      "\\bupkick\\b.{0,50}\\b(downed|grounded)\\b",
      "\\b(fence|fencing|cage|chain links?)\\b",
      "toes?.{0,40}(?:fence|fencing|cage|chain links?)",
      "(?:hooks?|tugs?|pulling|tugging).{0,40}(?:fence|fencing|cage|chain links?)",
      "back of the head",
      back_of_head_pat,
      back_head_patterns,                 # keep the full patterns too
      "rabbit punch",
      "toes? in the cage",
      "toes? in the fence",
      "toes? in the chain links?",
      "toes? are hooked",
      "pulls? on the fence",
      "grabs? the fence",
      "fence grab",
      "grabs? the cage",
      "pulls? on the cage",
      "(?:fence|cage)",
      "knee to (?:a )?grounded opponent",
      "illegal knee",
      "illegal upkick",
      "illegal (?:kick|upkick) to (?:a )?grounded opponent",
      "crown of the head",
      "illegal strike",
      "chain links",
      "uses?\\s+(?:his|her|their)\\s+toes?.{0,40}foul",
      "push(?:es|ed|ing)?\\s+off\\s+\\w+(?:['’]\\w+)?\\s+face\\s+and\\s+pok(?:e|es|ed|ing)\\s+\\w+",
      "toes?.{0,40}which\\s+too\\s+is\\s+a\\s+foul"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# Non-foul: Referee warnings about eye pokes (not an incident)
non_foul_eye_warning_re <- regex(
  paste(
    c(
      "(?:ref(?:eree)?|herzog|goddard|dean|peterson|beltran|tognoni|miragliotta|smith|sataki)\\b.{0,40}\\b(?:tells?|warns?|advises?|instructs?)\\b.{0,40}\\b(?:watch\\s+out\\s+for|watch\\s+(?:your|his|her)\\s+fingers?|keep\\s+your\\s+fingers?)\\b.{0,40}\\beye\\s+pokes?\\b",
      
      "\\b(?:tells?|warns?|advises?|instructs?)\\b.{0,40}\\b(?:watch\\s+out\\s+for|watch\\s+(?:your|his|her)\\s+fingers?|keep\\s+your\\s+fingers?)\\b.{0,40}\\beye\\s+pokes?\\b"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# Non-foul: injury/strike to eye language (legal strikes, swelling, etc.)
neg_injury_eye_re <- regex(
  "cut|laceration|swelling|hematoma|bruise|bleeding|blood|closed\\s+up|black\\s+eye|above\\s+.*eye",
  ignore_case = TRUE
)


# Fixed Hyperparameters (selected earlier during model selection/tuning - (00_working_model_selection.R))
best_pen <- 0.005
best_max <- 5000


# Feature Engineering: add biabnry flags that boost recall or rare foul phrasing
add_flags <- function(df) {
  df %>%
    mutate(
      text_lc = stringr::str_to_lower(text),
      
      # Baseline keyword flags
      flag_warn    = as.integer(str_detect(text_lc, regex("\\bwarn(?:ed|s|ing)?\\b|\\bwarning\\b", ignore_case = TRUE))),
      flag_illegal = as.integer(str_detect(text_lc, regex("\\billegal\\b|\\bfoul\\b", ignore_case = TRUE))),
      flag_eye     = as.integer(str_detect(text_lc, regex("\\beye\\b|\\beyeball\\b|\\bpoke\\b|\\bfinger\\b", ignore_case = TRUE))),
      flag_groin   = as.integer(str_detect(text_lc, regex("\\bgroin\\b|\\bcup\\b|\\blow\\s+blow\\b", ignore_case = TRUE))),
      flag_fence   = as.integer(str_detect(text_lc, regex("\\bfence\\b|\\bgrab\\b|\\bgrabs\\b|\\btoes\\b", ignore_case = TRUE))),
      flag_backhd  = as.integer(str_detect(text_lc, regex("back\\s+of\\s+(?:the|his|her)\\s+head|back\\s+of\\s+head", ignore_case = TRUE))),
      
      # High-recall flags (anchor phrases + context cues)
      flag_strong_anchor = as.integer(str_detect(text_lc, strong_foul_anchor_re)),
      
      flag_headbutt = as.integer(str_detect(text_lc, regex("head\\s*butt|headbutt", ignore_case = TRUE))),
      
      flag_upkick = as.integer(str_detect(text_lc, regex("\\bupkick\\b|illegal\\s+upkick", ignore_case = TRUE))),
      flag_grounded_ctx = as.integer(str_detect(text_lc, regex("downed|grounded|on\\s+the\\s+ground|knee\\s+down", ignore_case = TRUE))),
      flag_upkick_downed = as.integer(flag_upkick == 1 & flag_grounded_ctx == 1),
      
      flag_illegal_knee_grounded = as.integer(
        str_detect(text_lc, regex("illegal\\s+knee|knee\\s+to\\s+(a\\s+)?grounded", ignore_case = TRUE)) |
          (str_detect(text_lc, regex("\\bknee\\b", ignore_case = TRUE)) & flag_grounded_ctx == 1)
      ),
      
      flag_eye_poke_explicit = as.integer(str_detect(text_lc, regex(
        "eye\\s*pok|eyepok|finger(s)?\\s+.*eye|thumb\\s+.*eye|eye\\s*socket|poked\\s+in\\s+the\\s+eye",
        ignore_case = TRUE
      ))),
      
      flag_fence_grab = as.integer(str_detect(text_lc, regex(
        "grab(bed|bing)?\\s+the\\s+(fence|cage)|fence\\s*grab|cage\\s*grab|chain\\s*links?",
        ignore_case = TRUE
      ))),
      
      flag_toes_in_fence = as.integer(str_detect(text_lc, regex(
        "hook(ing)?\\s+toes|toes?\\s+in\\s+the\\s+(fence|cage)",
        ignore_case = TRUE
      ))),
      
      flag_ref_context = as.integer(str_detect(text_lc, regex(
        "warns?|warning|admonish|timeout|calls?\\s+time|doctor|point\\s+deduction",
        ignore_case = TRUE
      ))),
      
      flag_pause_context = as.integer(str_detect(text_lc, regex(
        "pause|stoppage|replay|time\\s*out|break",
        ignore_case = TRUE
      ))),
      
      # Negative Context flags: help supress systematic false positives
      flag_eye_warning_only = as.integer(str_detect(text_lc, non_foul_eye_warning_re)),
      flag_injury_eye       = as.integer(str_detect(text_lc, neg_injury_eye_re))
    ) %>%
    select(-text_lc)
}

# Apply Feature Engineering to labelled sentence dataset
ml_data2 <- add_flags(ml_data)

# Text pre-processing + TF-IDF recipe (includes engineered flags as additional predictors)
rec_flags <- recipe(
  has_foul ~ text +
    flag_warn + flag_illegal + flag_eye + flag_groin + flag_fence + flag_backhd +
    flag_strong_anchor + flag_headbutt + flag_upkick_downed + flag_illegal_knee_grounded +
    flag_eye_poke_explicit + flag_fence_grab + flag_toes_in_fence +
    flag_ref_context + flag_pause_context +
    flag_eye_warning_only + flag_injury_eye,
  data = ml_data2
) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_ngram(text, min_num_tokens = 1L, num_tokens = 2L) %>%
  step_tokenfilter(text, max_tokens = best_max) %>%
  step_tfidf(text)

# Model Specification (L1 - regularised logistic regression via glmnet)
model_lr_best <- logistic_reg(penalty = best_pen, mixture = 1) %>%
  set_engine("glmnet")

# Combined recipe + model into a workflow
wf_flags <- workflow() %>%
  add_recipe(rec_flags) %>%
  add_model(model_lr_best)

# Fit final model on full labelled dataset

fit_lr_flags <- fit(wf_flags, data = ml_data2)

# Save fitted workflow for scoring in later scripts

saveRDS(fit_lr_flags, file.path(paths$output, "ml_foul_detector_lr_flags.rds"))

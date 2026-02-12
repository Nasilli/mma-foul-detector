library(reticulate)


# 1) Check that conda + mma_srl exist
conda_list()

# 2) Tell reticulate to use that env
use_condaenv("mma_srl", required = TRUE)

# 3) Install spaCy from conda-forge (no Rust / tokenizers needed)
reticulate::conda_install(
  envname = "mma_srl",
  packages = "spacy",
  channel  = "conda-forge"
)

# still in the same R session, with mma_srl active
py_run_string("
import spacy
import spacy.cli
spacy.cli.download('en_core_web_sm')
")

py_run_string("
import spacy
nlp = spacy.load('en_core_web_sm')
doc = nlp('Marcos lands a low blow on Silva.')
print([(t.text, t.dep_, t.head.text) for t in doc])
")

py_run_string("
import spacy

# load model once
try:
    nlp = spacy.load('en_core_web_sm')
except Exception as e:
    nlp = None

# verbs that often indicate a foul action
FOUL_VERBS = {
    'kick','kicks','kicked',
    'knee','knees','kneed',
    'hit','hits','hit',
    'land','lands','landed',
    'headbutt','headbutts','headbutted',
    'poke','pokes','poked','gouge','gouges','gouged',
    'grab','grabs','grabbed','hold','holds','held',
    'elbow','elbows','elbowed'
}

def detect_fouler_spacy(text, f1_last, f2_last):
    \"\"\"Return 'fighter1', 'fighter2', or None based on dependency parse.\"\"\"
    if nlp is None or text is None:
        return None

    doc = nlp(text)
    f1 = (f1_last or '').lower()
    f2 = (f2_last or '').lower()
    guess = None

    for tok in doc:
        if tok.pos_ == 'VERB' and tok.lemma_.lower() in FOUL_VERBS:
            # find subject of this verb
            subj = None
            for child in tok.children:
                if child.dep_ in ('nsubj', 'nsubjpass'):
                    subj = child.text.lower()
                    break

            if subj == f1:
                guess = 'fighter1'
            elif subj == f2:
                guess = 'fighter2'

    return guess
")

# in a fresh R session:


py_code <- "
import spacy

print('Loading spaCy model…')
try:
    nlp = spacy.load('en_core_web_sm')
    print('spaCy model loaded OK')
except Exception as e:
    print('ERROR loading spaCy model:', e)
    nlp = None

FOUL_VERBS = {
    'kick','kicks','kicked',
    'knee','knees','kneed',
    'hit','hits','hit',
    'land','lands','landed',
    'headbutt','headbutts','headbutted',
    'poke','pokes','poked','gouge','gouges','gouged',
    'grab','grabs','grabbed','hold','holds','held',
    'elbow','elbows','elbowed'
}

def detect_fouler_spacy(text, f1_last, f2_last):
    \"\"\"Return 'fighter1', 'fighter2', or None based on syntax.\"\"\"
    if text is None or nlp is None:
        return None

    raw = text
    txt = (text or '').lower()
    f1 = (f1_last or '').lower()
    f2 = (f2_last or '').lower()

    doc = nlp(raw)

    # 1. Is there any potential foul verb?
    has_foul_verb = any(
        (tok.lemma_.lower() in FOUL_VERBS) and tok.pos_ in ('VERB', 'AUX')
        for tok in doc
    )
    if not has_foul_verb:
        return None

    # 2. Which last names actually appear?
    present_f1 = bool(f1) and f1 in txt
    present_f2 = bool(f2) and f2 in txt

    # If exactly one fighter is mentioned, treat them as victim -> other fighter fouls
    if present_f1 and not present_f2 and f2:
        return 'fighter2'
    if present_f2 and not present_f1 and f1:
        return 'fighter1'

    # 3. Look for foul verbs and their subjects
    for tok in doc:
        if tok.lemma_.lower() in FOUL_VERBS and tok.pos_ in ('VERB', 'AUX'):
            subj = None
            subj_dep = None
            for child in tok.children:
                if child.dep_ in ('nsubj', 'nsubjpass'):
                    subj = child.text.lower()
                    subj_dep = child.dep_
                    break
            if subj is None:
                continue

            # Passive subject (nsubjpass) = victim -> other fighter is fouler
            if subj_dep == 'nsubjpass':
                if subj == f1 and f2:
                    return 'fighter2'
                if subj == f2 and f1:
                    return 'fighter1'
            else:
                # Active subject = fouler
                if subj == f1:
                    return 'fighter1'
                if subj == f2:
                    return 'fighter2'

    # 4. No clear signal – let R rules decide.
    return None
"
library(reticulate)
use_condaenv("mma_srl", required = TRUE)
py_run_string(py_code)

# source your big R script so detect_fouler_v5 uses spaCy
source("MMA Foul Analysis Test 12.R")

# rebuild model outputs & eval
model_v0_all <- purrr::map_df(html_files, process_event_v0)
model_v0_clean <- model_v0_all %>% 
  # (optionally select/arrange columns, but DON'T change bout_key)
  select(event, bout, bout_key, foul_idx, everything()) # same as before
model_v0_clean <- model_v0_clean %>% mutate(bout_key = normalize_bout(bout_key))

model_v0_clean <- model_v0_clean %>%
  mutate(
    predicted_foul_type = case_when(
      str_detect(foul_type_guess, "eyepoke|eye poke|south of the border|eye scrape|eyescrape|eye") ~ "eye poke",
      str_detect(foul_type_guess, "cup|groin|low blow|belt") ~ "groin strike",
      str_detect(foul_type_guess, "(back|crown) of (the |his |her )?head|rabbit") ~ "back of head strike",
      str_detect(foul_type_guess, "fence|cage|chain|toes") ~ "fence grab",
      str_detect(foul_type_guess, "headbutt|clash of heads") ~ "headbutt",
      str_detect(foul_type_guess, "illegal upkick") ~ "illegal upkick",
      TRUE ~ "OTHER"
    ),
    fouler_model = word(foul_committed_by, -1)
  )


eval_tble <- gold_clean %>% 
  full_join(
    model_v0_clean,
    by = c("event", "bout_key", "foul_idx"),
    suffix = c("_gold", "_model")
  )
# Evaluation Table with all fouls from the Gold Standard Fouls Table and all fouls from the model. This is for accuracy testing, identifying inaccuracies, grouping them by error thus allowing code to be refined sequentially rather than holistically
eval_new <- eval_tble %>% 
  arrange(event) %>% 
  select(-foul_committed_by)


accuracy_test <- eval_new %>%
  mutate(
    match_foul_type = foul_type == predicted_foul_type, # after normalisation
    match_fouler = fouler == fouler_model,
    match_pause = fight_paused == referee_paused,
    match_deduction = point_deduction == foul_resulted_in_deduction,
    match_all = match_foul_type & match_fouler & match_pause & match_deduction
  ) %>% 
  mutate(
    match_foul_type = replace_na(match_foul_type, FALSE),
    match_fouler = replace_na(match_fouler, FALSE),
    match_pause = replace_na(match_pause, FALSE),
    match_deduction = replace_na(match_deduction, FALSE),
    match_all = replace_na(match_all, FALSE)
  ) %>% 
  mutate(
    foul_acc = mean(match_foul_type),
    fouler_acc = mean(match_fouler),
    pause_acc = mean(match_pause),
    ded_acc = mean(match_deduction),
    all_acc = mean(match_all)
  )

library(dplyr)
library(stringr)

errors_fouler <- accuracy_test %>% filter(match_fouler == FALSE)

errors_fouler_spacy <- errors_fouler %>%
  rowwise() %>%
  mutate(
    # 1) bout_key looks like "kape vs silva"
    fighters = list(strsplit(bout_key, "\\s+vs\\s+")[[1]]),
    
    # 2) last names are just the two pieces of bout_key
    f1_last = if (length(fighters) >= 1) fighters[1] else NA_character_,
    f2_last = if (length(fighters) >= 2) fighters[2] else NA_character_,
    
    # 3) ask spaCy (coerce NULL -> NA_character_)
    spacy_side = {
      tmp <- py$detect_fouler_spacy(context, f1_last, f2_last)
      if (is.null(tmp)) NA_character_ else as.character(tmp)
    },
    
    # 4) map 'fighter1/2' back to surnames
    spacy_fouler = case_when(
      spacy_side == "fighter1" ~ f1_last,
      spacy_side == "fighter2" ~ f2_last,
      TRUE                     ~ NA_character_
    ),
    
    # 5) does spaCy agree with the gold fouler?
    spacy_match = !is.na(spacy_fouler) &
      str_to_lower(spacy_fouler) == str_to_lower(fouler)
  ) %>%
  ungroup()

table(errors_fouler_spacy$spacy_match, useNA = "ifany")

errors_fouler_spacy %>%
  select(event, bout_key, context,
         fouler, fouler_model,
         spacy_side, spacy_fouler, spacy_match) %>%
  View()

mean(accuracy_test$match_fouler)  # check fouler accuracy

errors_foultype = accuracy_test %>% filter(match_foul_type == FALSE)
errors_fouler = accuracy_test %>% filter(match_fouler == FALSE)
errors_pause = accuracy_test %>% filter(match_pause == FALSE)
errors_ded = accuracy_test %>% filter(match_deduction == FALSE)


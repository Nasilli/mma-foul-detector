# ---- packages ----
library(rvest)
library(readr)
library(here)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(tokenizers)
library(zoo)

# ---- load saved Sherdog PBP page ----
file_path <- here("Raw PBP Data", "UFC-deridder-allen-18-10-2025.html")
doc       <- read_html(file_path)

pbp       <- doc %>% html_element("#playbyplay")
events    <- pbp %>% html_elements("div.event")

pbp_tbl <- tibble(
  bout = events %>% html_element("h2") %>% html_text2(),
  text = events  %>% html_text2()
)

# ==================== DICTIONARIES ====================

# ---- groin / cup patterns ----
groin_patterns <- c(
  "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|pounds?).{0,20}(?:to|in) the groin",
  "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|smacks?|brushes?|ricochets?|crashes?|bangs?|pounds?).{0,40}(?:into|to|on|the |his |her |their )?cup",
  "\\b(?:foot|leg|knee).{0,40}(?:smacks?|hits?|brushes?|ricochets?|crashes?|bangs?|pounds?).{0,20}(?:into |to |on )?(?:the )?cup",
  "\\bricochet.{0,30}(?:into |to |on )?(?:the )?cup\\b",
  "\\blow blow\\b",
  "\\bcup shot\\b",
  # 🔧 tightened: only match explicit euphemisms, not “kicks low”
  "(?:kick|knee|strike|shot|blow).{0,40}(south of the border|below the belt|low blow)",
  "(south of the border|below the belt) blow"
)

# ---- eye patterns ----
eye_patterns <- c(
  "\\beye poke\\b|\\beyepoke\\b",
  "finger(s)?.{0,25}(?:to|into|in|slides?|scrapes?|rakes?|grazes?).{0,10}(?:the )?eye",
  "\\bpoke.{0,15}eye\\b",
  "\\bthumb.{0,20}(?:in|to) (?:the )?eye\\b",
  "\\b(tells?|warns?|advises?)\\s+(?:him|her|\\w+)\\s+to\\s+watch\\s+(?:his|her)\\s+fingers\\b"
)

foul_patterns <- c(
  groin_patterns,
  eye_patterns,
  "head ?butt|clash of heads|head clash",
  "(?:fence|cage) grab",
  "grab(?:bing|s)? the (?:fence|cage)",
  "(?:pulls?|grabs?|holds?|clutches?) on the (?:fence|cage)",
  "rolls? to grab the fence",
  "back of the head|rabbit punch(?:es)?",
  "knee to (?:a )?grounded opponent|illegal knee"
)

foul_re    <- regex(paste(foul_patterns, collapse = "|"), ignore_case = TRUE)
eye_any_re <- regex(paste(eye_patterns,  collapse = "|"), ignore_case = TRUE)

# ---- pause patterns (foul-related only) ----
pause_patterns <- c(
  "\\bref(?:eree)?\\b.{0,20}\\bcalls? time\\b",
  "\\btime (?:is )?(?:called|stopped)\\b",
  "\\btime-?out(?: is)? called\\b",
  "\\bdoctor(?:'|’)s? (?:check|exam|examines|called in)\\b",
  "\\bgives? (?:him|her|them) time to recover\\b",
  "\\ballowed to recover\\b",
  "take as much time as (?:he|she|they) needs?",
  "\\breplay officials\\b|\\breplay is checked\\b",
  "\\bthey resume\\b|\\baction resumes\\b|\\bback underway\\b|\\bresume(?:s|d)?\\b"
)
pause_re <- regex(paste(pause_patterns, collapse = "|"), ignore_case = TRUE)

# pauses clearly not due to fouls
non_foul_pause_re <- regex(
  paste(c(
    "mouth ?guard|mouth ?piece",
    "replac(?:es?|ing) the mouthpiece",
    "\\bcut\\b.{0,20}check",
    "spilled water|towel",
    "tape|glove issue|equipment"
  ), collapse="|"),
  ignore_case = TRUE
)

# explicit “no pause” language
no_pause_re <- regex(
  paste(c(
    "waves? (?:off|it off|the ref off)",
    "ignores? it",
    "(?:but|and) (?:they |Santos |Prepolec )?(?:keep|continue)(?:s)? (?:going|fighting)",
    "lets? them fight"
  ), collapse="|"),
  ignore_case = TRUE
)

# ---- negation ----
negation_re <- regex(
  paste(
    "\\b(?:no|not|wasn'?t|weren'?t|barely|almost|nearly)\\b.{0,30}\\b(?:foul|groin|eye poke|headbutt)\\b",
    "clutching (?:his|her)? groin|adjust(?:s|ing)? (?:his|her)? cup",
    sep="|"
  ),
  ignore_case = TRUE
)

# ---- point deduction patterns ----
point_deduction_re <- regex(
  paste(c(
    "\\bpoint deduction\\b",
    "\\belects\\s+to\\s+deduct\\s+(?:a|one)?\\s*point(?:\\s+from\\s+\\w+)?\\b",
    "\\bdeducts?\\s+(?:a|one)?\\s*point(?:\\s+from\\s+\\w+)?\\b",
    "\\btakes?\\s+(?:a|one)?\\s*point\\b",
    "\\bremoves?\\s+(?:a|one)?\\s*point\\b",
    "\\bsubtracts?\\s+(?:a|one)?\\s*point\\b"
  ), collapse="|"),
  ignore_case = TRUE
)

deduction_neg_re <- regex(
  paste(c(
    "warning",
    "no point",
    "without (?:a|any) point",
    "no\\s+points?\\s+deducted"
  ), collapse="|"),
  ignore_case = TRUE
)

# general “anchor” words used later when looking for who did the foul
foul_anchor_re <- regex(
  "cup|groin|eye|fence|cage|back of the head|grounded opponent|illegal knee|eye poke",
  ignore_case = TRUE
)

# ==================== SENTENCE SPLIT + FEATURES ====================

to_sentences <- function(x){
  x %>%
    str_replace_all("<br>", " ") %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_squish() %>%
    tokenizers::tokenize_sentences(strip_punct = FALSE) %>%
    .[[1]]
}

time_re  <- regex("\\b([0-5]?\\d:[0-5]\\d)\\b")
round_re <- regex("\\bRound\\s+(?:One|Two|Three|Four|Five|\\d)\\b", ignore_case = TRUE)

sent_all <- pbp_tbl %>%
  mutate(sentence = map(text, to_sentences)) %>%
  select(bout, sentence) %>%
  unnest(sentence) %>%
  group_by(bout) %>%
  mutate(
    sid               = row_number(),
    has_foul          = str_detect(sentence, foul_re),
    pause_token       = str_detect(sentence, pause_re),
    non_foul_pz       = str_detect(sentence, non_foul_pause_re),
    explicit_no_pause = str_detect(sentence, no_pause_re),
    is_neg            = str_detect(sentence, negation_re),
    time_str          = str_extract(sentence, time_re),
    round_str_raw     = str_extract(sentence, round_re),
    is_deduction_raw  = str_detect(sentence, point_deduction_re) &
      !str_detect(sentence, deduction_neg_re)
  ) %>%
  ungroup() %>%
  group_by(bout) %>%
  mutate(round_mentioned = zoo::na.locf(round_str_raw, na.rm = FALSE)) %>%
  ungroup()

# ==================== INCIDENTS (FOUL RUNS) ====================
sent_runs <- sent_all %>%
  group_by(bout) %>%
  mutate(
    start_foul = has_foul & !lag(has_foul, default = FALSE),
    run_id_raw = if_else(start_foul, cumsum(start_foul), NA_integer_),
    run_id     = zoo::na.locf(run_id_raw, na.rm = FALSE, maxgap = 0)
  ) %>%
  ungroup() %>%
  filter(!is.na(run_id))

sent_runs_clean <- sent_runs %>%
  group_by(bout, run_id) %>%
  mutate(
    incident_has_eye = any(str_detect(sentence, eye_any_re)),
    incident_all_neg = all(is_neg)
  ) %>%
  ungroup() %>%
  filter(!(incident_all_neg & !incident_has_eye))

run_bounds <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  summarise(
    min_sid = min(sid),
    max_sid = max(sid),
    has_explicit_no_pause = any(explicit_no_pause),
    .groups = "drop"
  )

# ----- pause detection around each incident -----
pause_window_k <- 3

pause_check <- sent_all %>%
  select(bout, sid, pause_token, non_foul_pz) %>%
  right_join(run_bounds, by = "bout", relationship = "many-to-many") %>%
  mutate(
    in_window          = sid >= (min_sid - pause_window_k) & sid <= (max_sid + pause_window_k),
    valid_pause_signal = pause_token & !non_foul_pz & in_window
  ) %>%
  group_by(bout, run_id) %>%
  summarise(
    pause_detected      = any(valid_pause_signal, na.rm = TRUE),
    has_no_pause_signal = first(has_explicit_no_pause),
    .groups = "drop"
  ) %>%
  mutate(referee_paused = pause_detected & !has_no_pause_signal)

# ----- point deduction mapping -----
deduct_sents <- sent_all %>%
  filter(is_deduction_raw) %>%
  select(bout, sid_d = sid)

deduction_window_k <- 12

if (nrow(deduct_sents) > 0) {
  cand <- run_bounds %>%
    inner_join(deduct_sents, by = "bout") %>%
    mutate(forward_dist = sid_d - max_sid) %>%
    filter(forward_dist >= 0, forward_dist <= deduction_window_k)
  
  nearest <- cand %>%
    group_by(bout, sid_d) %>%
    slice_min(order_by = forward_dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(foul_resulted_in_deduction = TRUE) %>%
    select(bout, run_id, foul_resulted_in_deduction)
  
  deduction_map <- nearest %>%
    distinct(bout, run_id, foul_resulted_in_deduction)
} else {
  deduction_map <- tibble(bout = character(), run_id = integer(), foul_resulted_in_deduction = logical())
}

# ==================== BASE FOUL TABLE (NO FIGHTER NAMES YET) ====================

eye_categorise_re <- regex(
  paste(
    "\\beye poke\\b|\\beyepoke\\b",
    "finger(s)?.{0,30}(?:to|into|in|slides?|scrapes?|rakes?|grazes?|touches?).{0,10}(?:the )?eye",
    "\\bpoke.{0,15}eye\\b",
    "\\bwatch\\s+his\\s+fingers\\b",
    sep = "|"
  ), ignore_case = TRUE
)

fouls_core <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  summarise(
    context         = paste(sentence, collapse = " "),
    foul_raw_match  = str_extract(context, foul_re),
    time_mentioned  = na.omit(unique(time_str)) |> paste(collapse = ", "),
    round_mentioned = na.omit(unique(round_mentioned)) |> paste(collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(!is.na(foul_raw_match)) %>%
  mutate(
    foul_type_guess = case_when(
      str_detect(context, eye_categorise_re) ~ "eye poke",
      TRUE ~ foul_raw_match
    )
  ) %>%
  left_join(pause_check %>% select(bout, run_id, referee_paused),
            by = c("bout","run_id")) %>%
  left_join(deduction_map, by = c("bout","run_id")) %>%
  mutate(
    foul_resulted_in_deduction = if_else(is.na(foul_resulted_in_deduction),
                                         FALSE, foul_resulted_in_deduction)
  )

# ==================== FIGHTER NAMES PER BOUT ====================

extract_fighters <- function(bout) {
  parts <- str_split(bout, " vs\\. ", n = 2)[[1]]
  if (length(parts) < 2) {
    return(tibble(fighter1 = NA_character_, fighter2 = NA_character_))
  }
  f1 <- str_trim(str_remove(parts[1], "\\s*\\(.*$"))
  f2 <- str_trim(str_remove(parts[2], "\\s*\\(.*$"))
  tibble(fighter1 = f1, fighter2 = f2)
}

fighters_tbl <- pbp_tbl %>%
  mutate(tmp = map(bout, extract_fighters)) %>%
  unnest(tmp)

# ==================== FOULER DETECTION (RULE-BASED + LOCAL NLP) ====================
# ==================== ELITE FOULER DETECTION v5 ====================
# Simplified, debugged subject attribution
# Key: Object detection + explicit subject preservation

detect_fouler_v5 <- function(context, fighter1, fighter2) {
  if (is.na(fighter1) || is.na(fighter2) || is.na(context)) return("UNKNOWN")
  
  # Extract last names
  f1_last <- stringr::str_to_lower(stringr::str_extract(fighter1, "[^ ]+$"))
  f2_last <- stringr::str_to_lower(stringr::str_extract(fighter2, "[^ ]+$"))
  
  # Normalize text
  txt <- str_replace_all(context, "[\u2018\u2019\u201c\u201d`']", "'")
  txt <- stringr::str_to_lower(txt)
  
  has <- function(p) stringr::str_detect(txt, stringr::regex(p, ignore_case = TRUE))
  
  # ========== RULE 0: COMPLAINING (victim logic) ==========
  complain_verbs <- "(complain(s|ed|ing)?|protest(s|ed|ing)?|appeal(s|ed|ing)?\\s+to\\s+the\\s+ref|argue(s|d)?\\s+to\\s+the\\s+ref)"
  foul_words <- "(eye poke|eye|low blow|cup|groin|back of the head|fence|cage)"
  
  pat_complain <- function(name) {
    paste0("\\b", name, "\\b.{0,40}", complain_verbs, ".{0,40}", foul_words)
  }
  
  c1 <- has(pat_complain(f1_last))
  c2 <- has(pat_complain(f2_last))
  if (c1 && !c2) return(fighter2)
  if (c2 && !c1) return(fighter1)
  
  # ========== RULE 1-3: ORIGINAL SCORING RULES ==========
  verbs_attack <- "(kick(s|ing|ed)?|knee(s|ing|ed)?|strike(s|ing)?|hit(s|ting)?|drill(s|ing|ed)?|land(s|ing)?|shoot(s|ing)?|connect(s|ing)?|blast(s|ing|ed)?|smash(es|ing|ed)?|smack(s|ing)?|brush(es|ing)?|ricochet(s|ing)?|crash(es|ing)?|bang(s|ing)?|poke(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|push(es|ing)?|pull(s|ing)?|grab(s|ing|bed)?|roll(s|ing)?|flatten(s|ing)?|punch(es|ing)?|bludgeon(s|ing|ed)?)"
  obj_sensitive <- "(cup|groin|eye|head|fence|cage|back of the head)"
  verbs_passive <- "(kick(ed|ing)?|knee(d|ing)?|drill(ed|ing)?|hit|struck|smash(ed|ing)?|blast(ed|ing)?|elbow(ed|ing)?)"
  
  pat_actor <- function(name) {
    paste0("\\b", name, "\\b.{0,70}", verbs_attack, ".{0,40}", obj_sensitive)
  }
  pat_actor_loose <- function(name) {
    paste0("\\b", name, "\\b.{0,80}", verbs_attack)
  }
  pat_fence <- function(name) {
    paste0("\\b", name, "\\b.{0,70}(grab(s|ing|bed)?|pull(s|ing)?|hold(s|ing)?|clutch(es|ing)?).{0,20}(fence|cage)")
  }
  pat_actor_poss <- function(name) {
    paste0("\\b", name, "['']?s\\s+(finger|hand|fist|knee|foot|leg).{0,50}(slide(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|poke(s|ing)?|drill(s|ing)?|smack(s|ing)?).{0,40}", obj_sensitive)
  }
  pat_victim <- function(name) {
    paste0("\\b", name, "['']?s\\s+(cup|groin|eye|head)")
  }
  pat_by <- function(name) {
    paste0(verbs_passive, ".{0,40}(by|from)\\s+", name, "\\b")
  }
  
  score1 <- 0
  score2 <- 0
  
  score1 <- score1 +
    3 * has(pat_actor(f1_last)) +
    2 * has(pat_actor_loose(f1_last)) +
    3 * has(pat_fence(f1_last)) +
    3 * has(pat_actor_poss(f1_last)) +
    3 * has(pat_by(f1_last)) -
    3 * has(pat_victim(f1_last))
  
  score2 <- score2 +
    3 * has(pat_actor(f2_last)) +
    2 * has(pat_actor_loose(f2_last)) +
    3 * has(pat_fence(f2_last)) +
    3 * has(pat_actor_poss(f2_last)) +
    3 * has(pat_by(f2_last)) -
    3 * has(pat_victim(f2_last))
  
  if (score1 > score2 && score1 > 0) return(fighter1)
  if (score2 > score1 && score2 > 0) return(fighter2)
  
  # ========== NEW RULE 4: INTELLIGENT SUBJECT TRACKING ==========
  # Core principle: Identify which fighter is performing the ACTION CHAIN that ends in the foul
  
  foul_anchor_words <- c("back of the head", "cup", "groin", "eye", "fence", "cage")
  foul_anchor_pattern <- paste(foul_anchor_words, collapse = "|")
  foul_match <- stringr::str_locate(txt, stringr::regex(foul_anchor_pattern, ignore_case = TRUE))
  
  if (!is.na(foul_match[1])) {
    foul_pos <- foul_match[1]
    txt_before_foul <- stringr::str_sub(txt, 1, foul_pos - 1)
    
    # STEP 1: Find first fighter mentioned (likely the initial subject)
    f1_first_pos <- stringr::str_locate(txt_before_foul, 
                                        stringr::regex(paste0("\\b", f1_last, "\\b"), 
                                                       ignore_case = TRUE))[1]
    f2_first_pos <- stringr::str_locate(txt_before_foul, 
                                        stringr::regex(paste0("\\b", f2_last, "\\b"), 
                                                       ignore_case = TRUE))[1]
    
    first_fighter_pos <- if (!is.na(f1_first_pos) && !is.na(f2_first_pos)) {
      if (f1_first_pos < f2_first_pos) f1_first_pos else f2_first_pos
    } else if (!is.na(f1_first_pos)) {
      f1_first_pos
    } else if (!is.na(f2_first_pos)) {
      f2_first_pos
    } else {
      NA
    }
    
    first_fighter <- if (!is.na(first_fighter_pos)) {
      if (!is.na(f1_first_pos) && f1_first_pos == first_fighter_pos) fighter1 else fighter2
    } else {
      NA_character_
    }
    
    # STEP 2: Check if first fighter is followed by action verbs and continuation
    if (!is.na(first_fighter)) {
      region_after_first <- stringr::str_sub(txt_before_foul, first_fighter_pos, nchar(txt_before_foul))
      
      # Look for pattern: first fighter mentioned → verb(s) → "and starts/continues" → foul anchor
      has_action_verbs <- stringr::str_detect(
        region_after_first,
        stringr::regex(verbs_attack, ignore_case = TRUE)
      )
      
      # Check for continuation patterns that indicate subject persistence
      has_continuation <- stringr::str_detect(
        region_after_first,
        stringr::regex("\\band\\s+(starts?|begins?|continues?|proceeds?|keeps?|bludgeon)",
                       ignore_case = TRUE)
      )
      
      # Check for pronouns persisting the subject
      has_pronoun <- stringr::str_detect(
        region_after_first,
        stringr::regex("\\b(him|her|them)\\b\\s+with\\s+(punches?|kicks?|strikes?|knees?|elbows?)",
                       ignore_case = TRUE)
      )
      
      # If first fighter is performing actions AND either continuation or pronoun pattern exists
      if (has_action_verbs && (has_continuation || has_pronoun)) {
        return(first_fighter)
      }
    }
  }
  
  # ========== FALLBACK: Original positional heuristic ==========
  foul_anchor_re <- regex("cup|groin|eye|fence|cage|back of the head|grounded opponent|illegal knee|eye poke",
                          ignore_case = TRUE)
  loc <- stringr::str_locate(txt, foul_anchor_re)
  pre <- if (is.na(loc[1])) txt else stringr::str_sub(txt, 1, loc[1] - 1)
  
  has1 <- stringr::str_detect(pre, stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE))
  has2 <- stringr::str_detect(pre, stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE))
  
  if (has1 && !has2) return(fighter1)
  if (has2 && !has1) return(fighter2)
  
  "UNKNOWN"
}

# ==================== DEBUG: Test on foul #12 ==========
test_context <- "De Ridder flattens Allen out for a second and stars bludgeoning him with punches to the side and back of the head, and Herzog is telling him to knock it off but little more."
test_f1 <- "Reinier de Ridder"
test_f2 <- "Brendan Allen"

result <- detect_fouler_v5(test_context, test_f1, test_f2)
cat("Test result for foul #12:", result, "\n")

# ==================== APPLY TO FULL DATASET ==========

fouls_df <- fouls_core %>%
  left_join(fighters_tbl, by = "bout") %>%
  rowwise() %>%
  mutate(
    foul_committed_by = detect_fouler_v5(context, fighter1, fighter2)
  ) %>%
  ungroup() %>%
  select(
    bout,
    foul_committed_by,
    context,
    foul_type_guess,
    foul_resulted_in_deduction,
    referee_paused,
    round_mentioned
  )

fouls_df
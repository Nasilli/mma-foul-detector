pat_fence <- function(name) {
  paste0("\\b", name, "\\b.{0,70}(grab(s|ing|bed)?|pull(s|ing)?|hold(s|ing)?|clutch(es|ing)?).{0,20}(fence|cage)")
}
# NEW: possessive fence grab "Sandhagen's toes are hooked in the fence"
pat_fence_poss <- function(name) {
  paste0("\\b", name, "['']?s\\s+(toes?|feet|legs?|hand|arm).{0,50}(hooked|caught|hung|stuck).{0,20}(in|on).{0,10}(?:the\\s+)?(fence|cage)")
}
pat_actor_poss <- function(name) {
  paste0("\\b", name, "['']?s\\s+(finger|hand|fist|knee|foot|leg).{0,50}(slide(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|poke(s|ing)?|drill(s|ing)?|smack(s|ing)?).{0,40}(cup|groin|eye|head|fence|cage|back of the head)")
}# ---- packages ----
library(rvest)
library(readr)
library(here)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(tokenizers)
library(zoo)

# ==================== LOAD NEW PBP EVENT #2 ====================
file_path <- here("Raw PBP Data", "UFC320-ankalaev-pereira-04-10-2025.html")
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
  "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?).{0,20}(?:to|in) the groin",
  "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|smacks?|brushes?|ricochets?|crashes?|bangs?).{0,40}(?:into|to|on|the |his |her |their )?cup",
  "\\b(?:foot|leg|knee).{0,40}(?:smacks?|hits?|brushes?|ricochets?|crashes?|bangs?).{0,20}(?:into |to |on )?(?:the )?cup",
  "\\bricochet.{0,30}(?:into |to |on )?(?:the )?cup\\b",
  "\\blow blow\\b",
  "\\bcup shot\\b"
)

# ---- eye patterns ----
eye_patterns <- c(
  "\\beye poke\\b|\\beyepoke\\b",
  "finger(s)?.{0,25}(?:to|into|in|slides?|scrapes?|rakes?|grazes?).{0,10}(?:the )?eye",
  "\\bpoke.{0,15}eye\\b",
  "\\bthumb.{0,20}(?:in|to) (?:the )?eye\\b",
  "\\b(tells?|warns?|advises?)\\s+(?:him|her|\\w+)\\s+to\\s+watch\\s+(?:his|her)\\s+fingers\\b"
)

# ---- FIX 1: ADD TOES FENCE HOOK ----
foul_patterns <- c(
  groin_patterns,
  eye_patterns,
  "head ?butt|clash of heads|head clash",
  "(?:fence|cage) grab",
  "grab(?:bing|s)? the (?:fence|cage)",
  "(?:pulls?|grabs?|holds?|clutches?) on the (?:fence|cage)",
  "rolls? to grab the fence",
  "toes?.{0,20}(?:hooked|caught|hung|stuck)",  # toes hooked/caught (simplified)
  "(?:hooked|caught|hung|stuck).{0,20}(?:in|on).{0,10}(?:the\\s+)?fence",  # hooked in fence (catches Sandhagen case)
  "back of the head|rabbit punch(?:es)?",
  "knee to (?:a )?grounded opponent|illegal knee"
)

foul_re    <- regex(paste(foul_patterns, collapse = "|"), ignore_case = TRUE)
eye_any_re <- regex(paste(eye_patterns,  collapse = "|"), ignore_case = TRUE)

# ---- pause patterns ----
pause_patterns <- c(
  "\\bref(?:eree)?\\b.{0,20}\\bcalls? time\\b",
  "\\btime (?:is )?(?:called|stopped)\\b",
  "\\btime-?out(?: is)? called\\b",
  "\\bdoctor(?:'|')s? (?:check|exam|examines|called in)\\b",
  "\\bgives? (?:him|her|them) time to recover\\b",
  "\\ballowed to recover\\b",
  "take as much time as (?:he|she|they) needs?",
  "\\breplay officials\\b|\\breplay is checked\\b",
  "\\bthey resume\\b|\\baction resumes\\b|\\bback underway\\b|\\bresume(?:s|d)?\\b"
)
pause_re <- regex(paste(pause_patterns, collapse = "|"), ignore_case = TRUE)

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

no_pause_re <- regex(
  paste(c(
    "waves? (?:off|it off|the ref off)",
    "ignores? it",
    "(?:but|and) (?:they |Santos |Prepolec )?(?:keep|continue)(?:s)? (?:going|fighting)",
    "lets? them fight"
  ), collapse="|"),
  ignore_case = TRUE
)

negation_re <- regex(
  paste(
    "\\b(?:no|not|wasn'?t|weren'?t|barely|almost|nearly)\\b.{0,30}\\b(?:foul|groin|eye poke|headbutt)\\b",
    "clutching (?:his|her)? groin|adjust(?:s|ing)? (?:his|her)? cup",
    sep="|"
  ),
  ignore_case = TRUE
)

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

# ==================== BASE FOUL TABLE ====================

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

# ==================== FOULER DETECTION v5 WITH FIXES ====================

detect_fouler_v5 <- function(context, fighter1, fighter2, bout = NA) {
  if (is.na(fighter1) || is.na(fighter2) || is.na(context)) return("UNKNOWN")
  
  # Extract last names
  f1_last <- stringr::str_to_lower(stringr::str_extract(fighter1, "[^ ]+$"))
  f2_last <- stringr::str_to_lower(stringr::str_extract(fighter2, "[^ ]+$"))
  
  # Normalize text
  txt <- str_replace_all(context, "[\u2018\u2019\u201c\u201d`']", "'")
  txt <- stringr::str_to_lower(txt)
  
  has <- function(p) stringr::str_detect(txt, stringr::regex(p, ignore_case = TRUE))
  
  # ========== DEFINE VERBS EARLY (needed for Rule 0.5) ==========
  verbs_attack <- "(kick(s|ing|ed)?|knee(s|ing|ed)?|strike(s|ing)?|hit(s|ting)?|drill(s|ing|ed)?|land(s|ing)?|shoot(s|ing)?|connect(s|ing)?|blast(s|ing|ed)?|smash(es|ing|ed)?|smack(s|ing)?|brush(es|ing)?|ricochet(s|ing)?|crash(es|ing)?|bang(s|ing)?|poke(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|push(es|ing)?|pull(s|ing)?|grab(s|ing|bed)?|roll(s|ing)?|flatten(s|ing)?|punch(es|ing)?|bludgeon(s|ing|ed)?|headbutt(s|ing|ed)?)"
  
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
  
  # ========== RULE 0.5: REFEREE WARNING-BASED ATTRIBUTION ========== 
  # NEW: When ref warns someone for a foul, that person is the fouler
  
  warning_verbs <- c("warns?", "tells?", "advises?")
  ref_keywords <- c("referee", "ref", "tognoni", "herzog", "goddard", "mazzagatti", "herb", "kim", "jon")
  
  for (ref_kw in ref_keywords) {
    for (warn_verb in warning_verbs) {
      # Pattern 1: Direct fighter name
      warning_pattern_f1 <- paste0("\\b", ref_kw, "\\b.{0,20}", warn_verb, ".{0,30}\\b", 
                                   f1_last, "\\b.{0,30}(?:", verbs_attack, "|back of|groin|eye|fence)")
      warning_pattern_f2 <- paste0("\\b", ref_kw, "\\b.{0,20}", warn_verb, ".{0,30}\\b", 
                                   f2_last, "\\b.{0,30}(?:", verbs_attack, "|back of|groin|eye|fence)")
      
      if (stringr::str_detect(txt, stringr::regex(warning_pattern_f1, ignore_case = TRUE))) {
        return(fighter1)
      }
      if (stringr::str_detect(txt, stringr::regex(warning_pattern_f2, ignore_case = TRUE))) {
        return(fighter2)
      }
      
      # Pattern 2: Descriptor warning (e.g., "warns the American")
      descriptor_warning_pattern <- paste0("\\b", ref_kw, "\\b.{0,20}", warn_verb, 
                                           ".{0,30}(he|she|they|the\\s+\\w+).{0,30}(?:", 
                                           verbs_attack, "|back of|groin|eye|fence)")
      
      if (stringr::str_detect(txt, stringr::regex(descriptor_warning_pattern, ignore_case = TRUE))) {
        warning_match_text <- stringr::str_extract(txt, 
                                                   stringr::regex(descriptor_warning_pattern, 
                                                                  ignore_case = TRUE))
        
        if (!is.na(warning_match_text)) {
          # Find position of warning
          warning_pos <- stringr::str_locate(txt, stringr::regex(warning_match_text, 
                                                                 ignore_case = TRUE))[1]
          
          # ===== PHASE 1: Look backwards for active subject before warning =====
          context_start <- max(1, warning_pos - 250)
          context_before_warning <- stringr::str_sub(txt, context_start, warning_pos - 1)
          
          f1_before_positions <- stringr::str_locate_all(context_before_warning, 
                                                         stringr::regex(paste0("\\b", f1_last, "\\b"), 
                                                                        ignore_case = TRUE))[[1]][, 1]
          f2_before_positions <- stringr::str_locate_all(context_before_warning, 
                                                         stringr::regex(paste0("\\b", f2_last, "\\b"), 
                                                                        ignore_case = TRUE))[[1]][, 1]
          
          f1_last_mention_before <- if (length(f1_before_positions) > 0) max(f1_before_positions) else -Inf
          f2_last_mention_before <- if (length(f2_before_positions) > 0) max(f2_before_positions) else -Inf
          
          active_subject_before <- if (f1_last_mention_before > f2_last_mention_before) {
            fighter1
          } else if (f2_last_mention_before > f1_last_mention_before) {
            fighter2
          } else {
            NA_character_
          }
          
          # ===== PHASE 2: Look forward for resolution =====
          warning_end <- stringr::str_locate(txt, stringr::regex(warning_match_text, 
                                                                 ignore_case = TRUE))[2]
          resolution_region <- stringr::str_sub(txt, warning_end, warning_end + 200)
          
          resolution_pattern <- "(?:so|and)\\s+(he|she|they)\\s+(?:stops?|ceases?|quits?|pulls?\\s+back)"
          resolution_match <- stringr::str_extract(resolution_region, 
                                                   stringr::regex(resolution_pattern, 
                                                                  ignore_case = TRUE))
          
          # ===== PHASE 3: If phases 1&2 fail, use pronoun resolution =====
          # "so she stops" → check which fighter is referred to as "she" in the immediate context
          if (is.na(active_subject_before)) {
            # Extract pronoun from resolution if it exists
            if (!is.na(resolution_match)) {
              pronoun <- stringr::str_extract(resolution_match, "he|she|they")
              # In mixed-gender fights, "she" definitively refers to the female fighter
              # This is a heuristic but typically reliable
              # For now, return the first fighter if no clear before-context
              return(fighter1)
            }
          }
          
          # ===== Return if we have confirmation =====
          if (!is.na(active_subject_before) && !is.na(resolution_match)) {
            return(active_subject_before)
          }
          
          if (!is.na(active_subject_before)) {
            return(active_subject_before)
          }
          
          # Final fallback: if we found a resolution pronoun but no before-context,
          # the warned fighter is the one who stops, so they're the fouler
          if (!is.na(resolution_match)) {
            # "so she stops" was warned → that fighter is the fouler
            # Default to fighter1 
            return(fighter1)
          }
        }
      }
    }
  }
  
  # ========== RULE 1-3: ORIGINAL SCORING RULES ==========
  # FIX 2: ADD HEADBUTT TO verbs_attack
  verbs_attack <- "(kick(s|ing|ed)?|knee(s|ing|ed)?|strike(s|ing)?|hit(s|ting)?|drill(s|ing|ed)?|land(s|ing)?|shoot(s|ing)?|connect(s|ing)?|blast(s|ing|ed)?|smash(es|ing|ed)?|smack(s|ing)?|brush(es|ing)?|ricochet(s|ing)?|crash(es|ing)?|bang(s|ing)?|poke(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|push(es|ing)?|pull(s|ing)?|grab(s|ing|bed)?|roll(s|ing)?|flatten(s|ing)?|punch(es|ing)?|bludgeon(s|ing|ed)?|headbutt(s|ing|ed)?)"
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
    3 * has(pat_fence_poss(f1_last)) +
    3 * has(pat_actor_poss(f1_last)) +
    3 * has(pat_by(f1_last)) -
    3 * has(pat_victim(f1_last))
  
  score2 <- score2 +
    3 * has(pat_actor(f2_last)) +
    2 * has(pat_actor_loose(f2_last)) +
    3 * has(pat_fence(f2_last)) +
    3 * has(pat_fence_poss(f2_last)) +
    3 * has(pat_actor_poss(f2_last)) +
    3 * has(pat_by(f2_last)) -
    3 * has(pat_victim(f2_last))
  
  if (score1 > score2 && score1 > 0) return(fighter1)
  if (score2 > score1 && score2 > 0) return(fighter2)
  
  # ========== RULE 4: INTELLIGENT SUBJECT TRACKING ==========
  foul_anchor_words <- c("back of the head", "cup", "groin", "eye", "fence", "cage")
  foul_anchor_pattern <- paste(foul_anchor_words, collapse = "|")
  foul_match <- stringr::str_locate(txt, stringr::regex(foul_anchor_pattern, ignore_case = TRUE))
  
  if (!is.na(foul_match[1])) {
    foul_pos <- foul_match[1]
    txt_before_foul <- stringr::str_sub(txt, 1, foul_pos - 1)
    
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
    
    if (!is.na(first_fighter)) {
      region_after_first <- stringr::str_sub(txt_before_foul, first_fighter_pos, nchar(txt_before_foul))
      
      has_action_verbs <- stringr::str_detect(
        region_after_first,
        stringr::regex(verbs_attack, ignore_case = TRUE)
      )
      
      has_continuation <- stringr::str_detect(
        region_after_first,
        stringr::regex("\\band\\s+(starts?|begins?|continues?|proceeds?|keeps?|bludgeon)",
                       ignore_case = TRUE)
      )
      
      has_pronoun <- stringr::str_detect(
        region_after_first,
        stringr::regex("\\b(him|her|them)\\b\\s+with\\s+(punches?|kicks?|strikes?|knees?|elbows?)",
                       ignore_case = TRUE)
      )
      
      if (has_action_verbs && (has_continuation || has_pronoun)) {
        return(first_fighter)
      }
    }
  }
  
  # ========== FALLBACK: Original positional heuristic ==========
  foul_anchor_re <- regex("cup|groin|eye|fence|cage|back of the head|grounded opponent|illegal knee|eye poke|toes.*hooked|hooked.*fence",
                          ignore_case = TRUE)
  loc <- stringr::str_locate(txt, foul_anchor_re)
  pre <- if (is.na(loc[1])) txt else stringr::str_sub(txt, 1, loc[1] - 1)
  
  has1 <- stringr::str_detect(pre, stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE))
  has2 <- stringr::str_detect(pre, stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE))
  
  if (has1 && !has2) return(fighter1)
  if (has2 && !has1) return(fighter2)
  
  "UNKNOWN"
}

# ==================== APPLY TO FULL DATASET ==========

# ==================== CREATE EXPANDED CONTEXT FOR FALLBACK ==========
# Build a lookup table: for each foul incident, also store expanded context
expanded_contexts <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  mutate(min_sid_incident = min(sid), max_sid_incident = max(sid)) %>%
  ungroup() %>%
  distinct(bout, run_id, min_sid_incident, max_sid_incident) %>%
  left_join(
    sent_all %>% select(bout, sid, sentence),
    by = "bout",
    relationship = "many-to-many"
  ) %>%
  filter(sid >= (min_sid_incident - 2) & sid <= (max_sid_incident + 2)) %>%
  group_by(bout, run_id) %>%
  summarise(
    expanded_context = paste(sentence, collapse = " "),
    .groups = "drop"
  )

# ==================== APPLY TO FULL DATASET ==========

fouls_df2 <- fouls_core %>%
  left_join(fighters_tbl, by = "bout") %>%
  left_join(expanded_contexts, by = c("bout", "run_id")) %>%
  rowwise() %>%
  mutate(
    # First pass: narrow context
    foul_committed_by = detect_fouler_v5(context, fighter1, fighter2, bout),
    # Second pass: if UNKNOWN, try with expanded context
    foul_committed_by = if_else(
      foul_committed_by == "UNKNOWN",
      detect_fouler_v5(expanded_context, fighter1, fighter2, bout),
      foul_committed_by
    )
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

# Export for manual validation
write_csv(fouls_df2, "foul_detection_output_event2.csv")

# View results
fouls_df2

cat("\n✅ Event 2 analysis complete!\n")
cat(sprintf("Total fouls detected: %d\n", nrow(fouls_df2)))
cat(sprintf("Unknown foulers: %d\n", sum(fouls_df2$foul_committed_by == "UNKNOWN")))
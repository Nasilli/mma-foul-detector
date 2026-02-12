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

# ==================== LOAD EVENT ====================
file_path <- here("Raw PBP Data", "UFC-tsarukyan-hooker-22-11-2025.html")
doc       <- read_html(file_path)
pbp       <- doc %>% html_element("#playbyplay")
events    <- pbp %>% html_elements("div.event")

pbp_tbl <- tibble(
  bout = events %>% html_element("h2") %>% html_text2(),
  text = events  %>% html_text2()
)

# ==================== DICTIONARIES ====================

groin_patterns <- c(
  # “kicks him in the groin”
  "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|pounds?).{0,20}(?:to|in) the groin",
  
  # verbs … [anything] … cup / Wood's cup / groin cup
  "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|smacks?|brushes?|ricochets?|crashes?|bangs?|pounds?|scrapes?|scraped?).{0,40}(?:\\w+'s\\s+cup|groin cup|cup)",
  
  # explicit “groin cup” phrasing
  "(?:scrapes?|scraped?).{0,40}groin cup",
  
  "\\blow blow\\b",
  "\\bcup shot\\b",
  "(?:kick|knee|strike|shot|blow).{0,40}(south of the border|below the belt|low blow)",
  "(south of the border|below the belt) blow",
  
  # victim phrasing – “takes one to the groin”
  "\\btakes?\\s+(?:one\\s+)?to\\s+the\\s+groin\\b"
)
# ---- eye patterns (EXPANDED for "jam into eye socket") ----
eye_patterns <- c(
  "\\beye poke\\b|\\beyepoke\\b|\\beye gouge\\b|\\beye goug(s|ing)?\\b",
  
  # fingers ... verbs ... eye
  "finger(s)?.{0,40}(?:to|into|in|slides?|scrapes?|rakes?|grazes?|jams?|jammed|digs?|dug|presses?|swipes?|swatted|swats?).{0,25}(?:the )?(eye|eyes|eye socket|eye sockets)",
  
  "\\bpoke.{0,20}eye\\b",
  "\\bthumb.{0,25}(?:in|to) (?:the )?eye\\b",
  "\\b(tells?|warns?|advises?)\\s+(?:him|her|\\w+)\\s+to\\s+watch\\s+(?:his|her)\\s+fingers\\b",
  "\\bpoked\\s+in\\s+the\\s+eye(s)?\\b",
  "\\beye scrape\\b",
  
  # generic “X jams / swipes / scrapes into the eyes”
  "(slides?|scrapes?|rakes?|grazes?|jabs?|jabbed|jabbing|stabs?|stabbing|jams?|jammed|jamming|swipes?|swatted|swats?).{0,60}(?:into|in|to).{0,20}(?:the\\s+)?(eye|eyes|eye socket|eye sockets)"
)
# general "back of head" anchor (still used later)
# allow "back of Perez' head" as well as "back of the/his/her head"

# AFTER
back_of_head_pat <- "back of\\s+(?:the\\s+|his\\s+|her\\s+|[A-Za-z]+(?:['’]\\s*)?)?head"

back_head_patterns <- c(
  paste0(
    "(?:elbow|elbows?|strike|strikes?|punch(?:es)?|kick(?:s)?|knee(?:s)?|hammerfist(?:s)?|shot|shots|blow|blows|club(?:s)?|bangs?|smacks?|hacks?|cracks?|lands?).{0,120}",
    back_of_head_pat
  ),
  paste0(
    back_of_head_pat,
    ".{0,60}(?:illegal|foul|shot|strike|elbow|punch|kick|knee|warning|warns?)"
  )
)


# ---- foul patterns (for initial has_foul detection) ----
foul_patterns <- c(
  groin_patterns,
  eye_patterns,
  "head ?butt",
  # 🔹 treat any “back of the head / back of NAME's head” as a foul anchor
  "back of the head",
  "(?:fence|cage) grab",
  "grab(?:bing|s)? the (?:fence|cage)",
  "(?:pulls?|grabs?|holds?|clutches?) on the (?:fence|cage)",
  "rolls? to grab the fence",
  # illegal elbow/strike to crown of head
  "(?:elbow|strike).{0,30}crown of the head",
  "crown of the head.{0,40}illegal",
  "\\billegal strike\\b",
  # NEW: fingers hooked / in / out of the fence or cage
  # fingers or toes in the fence / cage / chain links
  "finger(s)?.{0,40}(?:in|hooked in|stuck in|out of).{0,10}(?:the\\s+)?(?:fence|cage|chain links?)",
  "toes?.{0,30}(?:in|into|on|hooked in).{0,20}(?:the\\s+)?(?:fence|cage|chain links?)",
  # NEW: generic "toes in the fence / cage / chain links"
  "toes?.{0,20}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
  # NEW: verbs + toes + fence/chain
  "hooks?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
  "pulls?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
  # "uses her toes ... which too is a foul" style phrasing
  "toes?.{0,40}(?:which\\s+too\\s+is\\s+a\\s+foul|is\\s+also\\s+a\\s+foul|is\\s+a\\s+foul)",
  # 🔹 NEW: generic "uses toes, which is a foul" pattern
  "uses?\\s+(?:his|her|their)\\s+toes?.{0,40}which\\s+too\\s+is\\s+a\\s+foul",
  back_head_patterns,
  "(?:tugs?|pulls?|grabs?|holds?).{0,30}(?:the\\s+)?chain links?",
  "rabbit punch(?:es)?",
  "knee to (?:a )?grounded opponent|illegal knee",
  "illegal upkick",
  "illegal (?:kick|upkick) to (?:a )?grounded opponent"
)
foul_re    <- regex(paste(foul_patterns, collapse = "|"), ignore_case = TRUE)
eye_any_re <- regex(paste(eye_patterns,  collapse = "|"), ignore_case = TRUE)

pause_patterns <- c(
  # 🔹 generic “calls time” – catches “Bosacki calls time”, “Goddard calls time”, etc.
  "\\bcalls? time\\b",
  "\\bref(?:eree)?\\b.{0,20}\\bcalls? time\\b",
  "\\btime (?:is )?(?:called|stopped)\\b",
  "\\btime-?out(?: is)? called\\b",
  "\\bdoctor(?:'|')s? (?:check|exam|examines|called in)\\b",
  # e.g. "gives him time to recover", "gives Rock ample time to recover"
  "\\bgives?\\s+(?:\\w+\\s+){0,3}time\\s+to\\s+recover\\b",
  "\\bbrings?\\s+in\\s+the\\s+doctor\\b",
  # NEW: Ruffy-style phrasing: "Ruffy gets time to recover"
  "\\bgets?\\s+time\\s+to\\s+recover\\b",
  # 🔹 explicit “gives NAME ample time to recover”
  "\\bgives?\\s+\\w+\\s+ample\\s+time\\s+to\\s+recover\\b",
  # 🔹 and a short generic anchor
  "\\bample\\s+time\\s+to\\s+recover\\b",
  "\\b(takes?|taking|took|will\\s+take)\\s+(?:a\\s+)?(?:moment|some\\s+time|time)\\s+to\\s+recover\\b",
  "\\bis\\s+taking\\s+time\\s+to\\s+recover\\b",
  "\\ballowed to recover\\b",
  # refs explicitly "call time" – keep but make sure it includes Bosacki too
  "\\b(?:tognoni|herzog|goddard|mazzagatti|hatley|peterson|beltran|rosenthal|dean|herb\\s+dean|bosacki)\\b.{0,20}calls? time\\b",
  "\\btime\\s+\\w+\\s+needs?\\s+to\\s+recover\\b",
  "\\btakes?\\s+\\d+\\s*(seconds?|minutes?)\\s+to\\s+(clear\\s+his\\s+vision|clear\\s+her\\s+vision|recover)\\b",
  "take as much time as (?:he|she|they) needs?",
  "takes a minute",
  "\\bthe fight (restarts?|is restarted)\\b",
  "\\bwe'?re back (?:in action|underway)\\b",
  "\\bback in action\\b",
  "\\breplay officials\\b|\\breplay is checked\\b",
  "\\bthey resume\\b|\\baction resumes\\b|\\bback underway\\b|\\bresume(?:s|d)?\\b"
)

pause_re <- regex(paste(pause_patterns, collapse = "|"), ignore_case = TRUE)


# AFTER
non_foul_pause_re <- regex(
  paste(
    c(
      "mouth ?guard|mouth ?piece",
      "replac(?:es?|ing) the mouthpiece",
      "\\bcut\\b.{0,20}check",
      "spilled water",
      "tape|glove issue|equipment"
    ),
    collapse="|"
  ),
  ignore_case = TRUE
)

no_pause_re <- regex(paste(
  "waves? (?:off|it off|the ref off)",
  "ignores? it",
  # 🔹 generic “but they keep fighting / going”
  "(?:but|and)\\s+(?:they\\s+)?(?:keep|continue)(?:s)?\\s+(?:going|fighting)",
  "lets? them fight",
  sep = "|"
), ignore_case = TRUE)

negation_re <- regex(paste("\\b(?:no|not|wasn'?t|weren'?t|barely|almost|nearly)\\b.{0,30}\\b(?:foul|groin|eye poke|headbutt)\\b", "clutching (?:his|her)? groin|adjust(?:s|ing)? (?:his|her)? cup", sep="|"), ignore_case = TRUE)

reaction_only_patterns <- c(
  # fighter behaviour after the foul – no new offence
  "(can barely|get[s]? back)\\s+to\\s+his\\s+feet",
  "(can barely|get[s]? back)\\s+to\\s+her\\s+feet",
  "(clutch(es|ing)?|holding|grabbing)\\s+(the\\s+)?back of (his|her) head",
  "(clutch(es|ing)?|holding|grabbing)\\s+(his|her)\\s+(cup|groin|eye|face|head)",
  "still\\s+clutching\\s+(his|her)\\s+(head|cup|groin|eye)",
  # 🔹 NEW: recap of already-committed eye pokes (Garry / Muhammad case)
  "after\\s+landing\\s+multiple\\s+eye\\s+pokes?",
  "in\\s+trouble\\s+after\\s+getting\\s+nailed"
)

reaction_only_re <- regex(
  paste(reaction_only_patterns, collapse = "|"),
  ignore_case = TRUE
)

point_deduction_re <- regex(paste(c("\\bpoint deduction\\b", "\\belects\\s+to\\s+deduct\\s+(?:a|one)?\\s*point(?:\\s+from\\s+\\w+)?\\b", "\\bdeducts?\\s+(?:a|one)?\\s*point(?:\\s+from\\s+\\w+)?\\b", "\\btakes?\\s+(?:a|one)?\\s*point\\b", "\\bremoves?\\s+(?:a|one)?\\s*point\\b", "\\bsubtracts?\\s+(?:a|one)?\\s*point\\b"), collapse="|"), ignore_case = TRUE)

deduction_neg_re <- regex(paste(c("warning", "no point", "without (?:a|any) point", "no\\s+points?\\s+deducted"), collapse="|"), ignore_case = TRUE)

# ==================== SENTENCE PROCESSING ====================

to_sentences <- function(x){
  x %>%
    str_replace_all("<br>", " ") %>%
    # fix mojibake single / double quotes first
    str_replace_all("â€™|â€˜", "'") %>%          # broken single quotes
    str_replace_all("â€œ|â€\u009d", "\"") %>%   # broken double quotes
    # then normal Unicode curly quotes -> ASCII apostrophe
    str_replace_all("[\u2018\u2019\u201c\u201d`']", "'") %>%
    str_squish() %>%
    tokenizers::tokenize_sentences(strip_punct = FALSE) %>%
    .[[1]]
}

time_re  <- regex("\\b([0-5]?\\d:[0-5]\\d)\\b")
round_re <- regex("\\bRound\\s+(?:One|Two|Three|Four|Five|\\d)\\b", ignore_case = TRUE)
result_re <- regex(
  paste(
    "defeats",
    "defeat[s]?",
    "wins by",
    "winner by",
    "via disqualification",
    "via dq",
    "dq \\(eye poke\\)",
    "dq \\(low blow\\)",
    "ruled a no contest",
    "no contest \\(accidental eye poke\\)",
    "no contest \\(accidental low blow\\)",
    sep = "|"
  ),
  ignore_case = TRUE
)

ref_re <- regex("\\breferee\\b", ignore_case = TRUE)

sent_all <- pbp_tbl %>%
  # 1) split each fight text into sentences
  mutate(sentence = map(text, to_sentences)) %>%
  select(bout, sentence) %>%
  unnest(sentence) %>%              # now we have one row per sentence
  group_by(bout) %>%
  mutate(
    sid          = row_number(),
    # raw foul detection
    has_foul_raw = str_detect(sentence, foul_re),
    # reaction-only lines
    is_reaction_only = str_detect(sentence, reaction_only_re),
    
    # extract timing & round info BEFORE we decide if fight has started
    time_str      = str_extract(sentence, time_re),
    round_str_raw = str_extract(sentence, round_re),
    # 🔹 NEW: has the referee been mentioned yet?
    ref_mentioned = str_detect(sentence, ref_re),
    
    # 🔹 UPDATED: fight only “starts” after ref OR the first visible clock
    fight_started = cumany(!is.na(time_str) | ref_mentioned),
    
    has_foul = has_foul_raw & !is_reaction_only & fight_started,
    pause_token       = str_detect(sentence, pause_re),
    non_foul_pz       = str_detect(sentence, non_foul_pause_re),
    explicit_no_pause = str_detect(sentence, no_pause_re),
    is_neg            = str_detect(sentence, negation_re),
    is_deduction_raw  = str_detect(sentence, point_deduction_re) &
      !str_detect(sentence, deduction_neg_re),
    is_result         = str_detect(sentence, result_re)
  ) %>%
  ungroup() %>%
  group_by(bout) %>%
  mutate(
    round_mentioned = zoo::na.locf(round_str_raw, na.rm = FALSE)
  ) %>%
  ungroup()


# ==================== INCIDENT GROUPING ====================

# ==================== INCIDENT GROUPING (v2: one foul sentence = one incident) ====================

# Each sentence that matches foul_re is its OWN incident.
# ==================== INCIDENT GROUPING (one foul sentence = one incident) ====================

sent_runs <- sent_all %>%
  filter(has_foul & !is_result) %>%
  group_by(bout) %>%
  mutate(
    # each foul sentence in a bout gets its own run_id
    run_id           = row_number(),
    incident_has_eye = str_detect(sentence, eye_any_re),
    incident_all_neg = is_neg
  ) %>%
  ungroup()

# Drop “negated” incidents (no-foul language) unless it’s an eye poke
sent_runs_clean <- sent_runs %>%
  filter(!(incident_all_neg & !incident_has_eye))

run_bounds <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  summarise(min_sid = min(sid), max_sid = max(sid), has_explicit_no_pause = any(explicit_no_pause), .groups = "drop")

pause_window_k <- 8

pause_check <- sent_all %>%
  select(bout, sid, pause_token, non_foul_pz) %>%
  right_join(run_bounds, by = "bout", relationship = "many-to-many") %>%
  mutate(
    pause_token  = dplyr::coalesce(pause_token,  FALSE),
    non_foul_pz  = dplyr::coalesce(non_foul_pz,  FALSE),
    in_window    = sid >= max_sid & sid <= (max_sid + pause_window_k),
    valid_pause_signal = pause_token & !non_foul_pz & in_window
  ) %>%
  group_by(bout, run_id) %>%
  summarise(
    pause_detected     = any(valid_pause_signal),
    has_no_pause_signal = any(has_explicit_no_pause),
    .groups = "drop"
  ) %>%
  mutate(referee_paused = pause_detected & !has_no_pause_signal)

deduct_sents <- sent_all %>% filter(is_deduction_raw) %>% select(bout, sid_d = sid)
deduction_window_k <- 12

if (nrow(deduct_sents) > 0) {
  cand <- run_bounds %>% inner_join(deduct_sents, by = "bout") %>% mutate(forward_dist = sid_d - max_sid) %>% filter(forward_dist >= 0, forward_dist <= deduction_window_k)
  nearest <- cand %>% group_by(bout, sid_d) %>% slice_min(order_by = forward_dist, n = 1, with_ties = FALSE) %>% ungroup() %>% mutate(foul_resulted_in_deduction = TRUE) %>% select(bout, run_id, foul_resulted_in_deduction)
  deduction_map <- nearest %>% distinct(bout, run_id, foul_resulted_in_deduction)
} else {
  deduction_map <- tibble(bout = character(), run_id = integer(), foul_resulted_in_deduction = logical())
}

# ==================== BASE FOUL TABLE ====================

eye_categorise_re <- regex(
  paste(
    "\\beye poke\\b|\\beyepoke\\b",
    "finger(s)?.{0,30}(?:to|into|in|slides?|scrapes?|rakes?|grazes?|jams?|jammed|digs?|dug|presses?).{0,15}(?:the )?(eye|eyes|eye socket|eye sockets)",
    "\\bpoke.{0,15}eye\\b",
    "\\bwatch\\s+his\\s+fingers\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

strong_foul_anchor_re <- regex(
  paste(
    c(
      "cup",
      "groin",
      "low blow",
      "south of the border",
      "below the belt",
      "eye poke",
      "\\beye(s)?\\b",
      "headbutt",
      # 🔹 plain back-of-head anchors
      "back of the head",
      back_of_head_pat,
      back_head_patterns,                 # keep the full patterns too
      "rabbit punch",
      # toes + structure
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
      # grounded-opponent stuff
      "knee to (?:a )?grounded opponent",
      "illegal knee",
      "illegal upkick",
      "illegal (?:kick|upkick) to (?:a )?grounded opponent",
      # crown-of-head / generic illegal strike
      "crown of the head",
      "illegal strike",
      "chain links",
      "uses?\\s+(?:his|her|their)\\s+toes?.{0,40}foul",
      "toes?.{0,40}which\\s+too\\s+is\\s+a\\s+foul"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# ==================== BASE FOUL TABLE ====================

# Aggregate sentences to incident-level context first
incident_context <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  summarise(
    context         = paste(sentence, collapse = " "),
    time_mentioned  = na.omit(unique(time_str)) |> paste(collapse = ", "),
    round_mentioned = na.omit(unique(round_mentioned)) |> paste(collapse = ", "),
    .groups = "drop"
  )

# Now extract *all* foul matches per incident and unnest them
fouls_core <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  summarise(
    context         = paste(sentence, collapse = " "),
    foul_raw_match  = str_extract(context, foul_re),
    time_mentioned  = na.omit(unique(time_str)) |> paste(collapse = ", "),
    round_mentioned = na.omit(unique(round_mentioned)) |> paste(collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(foul_raw_match),
    str_detect(context, strong_foul_anchor_re)
  ) %>%
  mutate(
    foul_type_guess = case_when(
      str_detect(context, eye_categorise_re) ~ "eye poke",
      TRUE                                  ~ foul_raw_match
    )
  ) %>%
  left_join(pause_check %>% select(bout, run_id, referee_paused),
            by = c("bout","run_id")) %>%
  left_join(deduction_map, by = c("bout","run_id")) %>%
  mutate(
    foul_resulted_in_deduction = if_else(is.na(foul_resulted_in_deduction),
                                         FALSE, foul_resulted_in_deduction),
    referee_paused = if_else(is.na(referee_paused), FALSE, referee_paused),
    referee_paused = referee_paused | foul_resulted_in_deduction
  ) %>%
  ## 🔽 NEW: drop exact duplicates of the same foul description in the same bout
  group_by(bout, context) %>%
  slice(1) %>%
  ungroup()

# ==================== FIGHTER NAMES ====================

extract_fighters <- function(bout) {
  parts <- str_split(bout, " vs\\. ", n = 2)[[1]]
  if (length(parts) < 2) return(tibble(fighter1 = NA_character_, fighter2 = NA_character_))
  f1 <- str_trim(str_remove(parts[1], "\\s*\\(.*$"))
  f2 <- str_trim(str_remove(parts[2], "\\s*\\(.*$"))
  tibble(fighter1 = f1, fighter2 = f2)
}

fighters_tbl <- pbp_tbl %>%
  mutate(tmp = map(bout, extract_fighters)) %>%
  unnest(tmp)

# ==================== FOULER DETECTION v5 ====================

detect_fouler_v5 <- function(context, fighter1, fighter2, bout = NA) {
  if (is.na(fighter1) || is.na(fighter2) || is.na(context)) return("UNKNOWN")
  
  f1_last <- stringr::str_to_lower(stringr::str_extract(fighter1, "[^ ]+$"))
  f2_last <- stringr::str_to_lower(stringr::str_extract(fighter2, "[^ ]+$"))
  
  txt <- context %>%
    str_replace_all("<br>", " ") %>%
    str_replace_all("â€™|â€˜|â|´|`|’|‘", "'") %>%
    str_replace_all("â€œ|â€\u009d|“|”", "\"") %>%
    str_squish() %>%
    str_to_lower()
  
  has <- function(p) stringr::str_detect(txt, stringr::regex(p, ignore_case = TRUE))
  
  # helper for possessive name (handles mojibake around apostrophes)
  name_possessive <- function(name) {
    paste0(
      "\\b", name,
      "(?:['’]|[^[:alpha:]]){0,3}s\\s+"   # e.g. gane’s, gane´s, gane ' s
    )
  }
  
  ## =========================================================
  ## HARD RULE 0: "NAME's kick ... bumps into the cup of his opponent"
  ## (Delgado foul 11 type)
  ## =========================================================
  actor_possessive_bumps_cup <- function(name) {
    paste0(
      name_possessive(name),
      "(kick|kicks|knee|knees|leg|legs|foot|feet|shin|shins|strike|strikes|shot|shots)",
      ".{0,120}",
      "(bumps?|ricochets?|crashes?|glances?)",
      ".{0,40}",
      "cup\\s+of\\s+(his|her)\\s+opponent"
    )
  }
  if (has(actor_possessive_bumps_cup(f1_last)) && !has(actor_possessive_bumps_cup(f2_last))) {
    return(fighter1)
  }
  if (has(actor_possessive_bumps_cup(f2_last)) && !has(actor_possessive_bumps_cup(f1_last))) {
    return(fighter2)
  }
  
  ## =========================================================
  ## HARD RULE 1: generic "cup of his/her opponent"
  ## → fouler = LAST surname before that phrase
  ## (still keep this, but earlier)
  ## =========================================================
  opp_cup_re <- stringr::regex("cup\\s+of\\s+(his|her)\\s+opponent", ignore_case = TRUE)
  if (stringr::str_detect(txt, opp_cup_re)) {
    loc <- stringr::str_locate(txt, opp_cup_re)[1]
    pre <- stringr::str_sub(txt, 1, loc)
    
    f1_pos <- stringr::str_locate_all(
      pre,
      stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    f2_pos <- stringr::str_locate_all(
      pre,
      stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    
    f1_last_pos <- if (nrow(f1_pos) > 0) max(f1_pos[, 1]) else -Inf
    f2_last_pos <- if (nrow(f2_pos) > 0) max(f2_pos[, 1]) else -Inf
    
    if (f1_last_pos > f2_last_pos) return(fighter1)
    if (f2_last_pos > f1_last_pos) return(fighter2)
  }
  
  ## =========================================================
  ## HARD RULE 2: "NAME's fingers ... jam into both of OTHERNAME's eyes"
  ## (Gane foul 12 type)
  ## =========================================================
  both_eyes_pattern <- function(actor, victim) {
    paste0(
      name_possessive(actor),
      "fingers?\\b.{0,160}",
      "jam(s|med|ming)?\\s+into\\s+both\\s+of\\s+",
      victim,
      "(?:['’]|[^[:alpha:]]){0,3}s?\\s+eyes?"
    )
  }
  if (has(both_eyes_pattern(f1_last, f2_last)) && !has(both_eyes_pattern(f2_last, f1_last))) {
    return(fighter1)
  }
  if (has(both_eyes_pattern(f2_last, f1_last)) && !has(both_eyes_pattern(f1_last, f2_last))) {
    return(fighter2)
  }
  
  ## =========================================================
  ## HARD RULE 3: generic "jam into both of ... eyes" with nearest-name victim
  ## → victim = closest surname before "eyes", fouler = other fighter
  ## =========================================================
  both_eyes_re <- stringr::regex(
    "jam(s|med|ming)?\\s+into\\s+both\\s+of\\s+.*eyes?",
    ignore_case = TRUE
  )
  if (stringr::str_detect(txt, both_eyes_re)) {
    eye_loc <- stringr::str_locate(txt, stringr::regex("eyes?", ignore_case = TRUE))[1]
    pre_eye <- stringr::str_sub(txt, 1, eye_loc)
    
    f1_pos <- stringr::str_locate_all(
      pre_eye,
      stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    f2_pos <- stringr::str_locate_all(
      pre_eye,
      stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    
    f1_last_pos <- if (nrow(f1_pos) > 0) max(f1_pos[, 1]) else -Inf
    f2_last_pos <- if (nrow(f2_pos) > 0) max(f2_pos[, 1]) else -Inf
    
    if (f1_last_pos > f2_last_pos) return(fighter2)  # f1 is victim
    if (f2_last_pos > f1_last_pos) return(fighter1)  # f2 is victim
  }
  
  ## =========================================================
  ## HARD RULE 4: "X controls Y ... and he is warned for grabbing the fence"
  ## (Aliskerov / Park foul 5 type)
  ## =========================================================
  controls_fence_pattern <- function(controller, controlled) {
    paste0(
      "\\b", controller, "\\b.{0,80}",
      "(controls|pins|keeps|holds)\\s+",
      "\\b", controlled, "\\b",
      ".{0,120}",
      "he\\s+is\\s+warned\\s+for\\s+",
      "(grabbing|holding|tugging)\\s+the\\s+(fence|cage|chain\\s+links?)"
    )
  }
  
  if (has(controls_fence_pattern(f1_last, f2_last)) &&
      !has(controls_fence_pattern(f2_last, f1_last))) {
    return(fighter1)
  }
  if (has(controls_fence_pattern(f2_last, f1_last)) &&
      !has(controls_fence_pattern(f1_last, f2_last))) {
    return(fighter2)
  }
  
  ## ------------------------------------------------------------------
  ## FROM HERE ON: keep your existing rules exactly as you had them
  ## (warning patterns, low-blow victim, scoring, subject tracking, fallback)
  ## ------------------------------------------------------------------
  
  # ... paste everything from your original detect_fouler_v5
  # starting from your existing "## ---------- helper: possessive name with broken quotes ----------"
  # down to the final fallback and "UNKNOWN" return.
  
  # (for brevity here I’m not re-pasting that huge block, but in your script you
  #  should literally keep all the existing code after this comment unchanged.)
  
  # FALLBACK if none of the above early rules decided:
  # (keep your existing fallback here)
  ## ---------- helper: possessive name with broken quotes ----------
  ## matches: "delgado's", "delgadoâs", "delgado´s", etc.
  name_possessive <- function(name) {
    paste0(
      "\\b", name,
      "(?:['’]|[^[:alpha:]]){0,3}s\\s+"   # up to 3 non-letters between name and s
    )
  }
  
  ## ---------- SUPER PRIORITY: Delgado’s kick → cup of his opponent ----------
  actor_possessive_low_super <- function(name) {
    paste0(
      name_possessive(name),
      "(kick|kicks|knee|knees|leg|legs|foot|feet|shin|shins|strike|strikes|shot|shots)",
      ".{0,120}",
      "(?:into\\s+the\\s+cup\\s+of\\s+his\\s+opponent",
      "|into\\s+the\\s+cup\\s+of\\s+her\\s+opponent",
      "|into\\s+his\\s+opponent(?:['’]|[^[:alpha:]]){0,3}s\\s+cup",
      "|into\\s+her\\s+opponent(?:['’]|[^[:alpha:]]){0,3}s\\s+cup",
      "|into\\s+the\\s+groin\\s+cup",
      "|into\\s+the\\s+cup)"
    )
  }
  
  apl1_super <- has(actor_possessive_low_super(f1_last))
  apl2_super <- has(actor_possessive_low_super(f2_last))
  
  if (apl1_super && !apl2_super) return(fighter1)
  if (apl2_super && !apl1_super) return(fighter2)
  
  ## ---------- SUPER PRIORITY: “Gane’s fingers ... jam into both of Aspinall’s eyes” ----------
  both_eyes_pattern <- function(actor, victim) {
    paste0(
      name_possessive(actor),
      "fingers?\\b.{0,160}",
      "jam(s|med|ming)?\\s+into\\s+both\\s+of\\s+",
      victim,
      "(?:['’]|[^[:alpha:]]){0,3}s?\\s+eyes?"
    )
  }
  
  be1 <- has(both_eyes_pattern(f1_last, f2_last))
  be2 <- has(both_eyes_pattern(f2_last, f1_last))
  
  if (be1 && !be2) return(fighter1)
  if (be2 && !be1) return(fighter2)
  
  ## ---------- generic “NAME’s fingers ... (eye)” patterns ----------
  finger_to_eyes_pattern <- function(actor, victim) {
    paste0(
      name_possessive(actor),
      "fingers?\\b.{0,120}",
      "(jam(s|med|ming)?|poke(s|d|ing)?|gouge(s|d|ing)?|rake(s|d|ing)?|scrape(s|d|ing)?|stab(s|bed|bing)?|push(es)?\\s+off)",
      ".{0,80}\\b",
      victim,
      "(?:['’]|[^[:alpha:]]){0,3}s?\\s+eyes?\\b"
    )
  }
  
  finger_eyes_actor_only <- function(actor) {
    paste0(
      name_possessive(actor),
      "fingers?\\b.{0,160}",
      "(eye|eyes|eye socket|eye sockets)"
    )
  }
  
  fe1_strong <- has(finger_to_eyes_pattern(f1_last, f2_last))
  fe2_strong <- has(finger_to_eyes_pattern(f2_last, f1_last))
  if (fe1_strong && !fe2_strong) return(fighter1)
  if (fe2_strong && !fe1_strong) return(fighter2)
  
  fe1_weak <- has(finger_eyes_actor_only(f1_last))
  fe2_weak <- has(finger_eyes_actor_only(f2_last))
  if (fe1_weak && !fe2_weak) return(fighter1)
  if (fe2_weak && !fe1_weak) return(fighter2)
  
  ## ------------------------------------------------------------------
  ## from here down, keep all your existing rules EXACTLY as they were
  ## (warning patterns, low-blow victim, scoring, subject tracking, fallback)
  ## ------------------------------------------------------------------
  
  # --- PRIORITY: "for NAME to not grab the fence / cage / chain links" ---
  chain_not_grab <- function(name) {
    paste0(
      "for\\s+", name, "\\s+to\\s+not\\s+grab\\s+the\\s+",
      "(?:fence|cage|chain links?)"
    )
  }
  
  cng1 <- has(chain_not_grab(f1_last))
  cng2 <- has(chain_not_grab(f2_last))
  if (cng1 && !cng2) return(fighter1)
  if (cng2 && !cng1) return(fighter2)
  
  # ---------- HIGH PRIORITY: "X is warned for ..." (no explicit ref) ----------
  warned_for_name_pattern <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,40}",
      "is\\s+warned\\s+for\\s+",
      ".{0,80}(back of the head|groin|cup|low blow|eye|eye poke|fence|cage|chain links?|illegal (knee|kick|upkick))"
    )
  }
  
  wf1 <- has(warned_for_name_pattern(f1_last))
  wf2 <- has(warned_for_name_pattern(f2_last))
  
  if (wf1 && !wf2) return(fighter1)
  if (wf2 && !wf1) return(fighter2)
  
  # ---------- GENERIC PRONOUN VERSION: "... is warned for ..." ----------
  if (stringr::str_detect(txt, stringr::regex("is\\s+warned\\s+for", ignore_case = TRUE))) {
    loc_warn <- stringr::str_locate(
      txt,
      stringr::regex("is\\s+warned\\s+for", ignore_case = TRUE)
    )[1]
    pre_warn <- stringr::str_sub(txt, 1, loc_warn - 1)
    
    f1_pos <- stringr::str_locate_all(
      pre_warn,
      stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    f2_pos <- stringr::str_locate_all(
      pre_warn,
      stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    
    f1_last_pos <- if (nrow(f1_pos) > 0) max(f1_pos[,1]) else -Inf
    f2_last_pos <- if (nrow(f2_pos) > 0) max(f2_pos[,1]) else -Inf
    
    if (f1_last_pos > f2_last_pos) return(fighter1)
    if (f2_last_pos > f1_last_pos) return(fighter2)
  }
  # ---------- SUPER-HIGH PRIORITY: explicit actor → foul anchor ----------
  
  explicit_actor_foul <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,120}",
      "(grabs?|grabbed|grabbing|holds?|clutches?|tugs?|pulls?)",
      ".{0,40}(fence|cage|chain links?)"
    )
  }
  
  explicit_actor_back_head <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,120}",
      "(elbows?|elbowed|slams?|strikes?|punches?|hammers?|clubs?)",
      ".{0,40}back of (the |his |her )?head"
    )
  }
  
  explicit_actor_cup <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,120}",
      "(kicks?|knees?|heels?|smacks?|drills?|lands?)",
      ".{0,60}(cup|groin cup|the groin)"
    )
  }
  
  ea1 <- has(explicit_actor_foul(f1_last))      |
    has(explicit_actor_back_head(f1_last)) |
    has(explicit_actor_cup(f1_last))
  
  ea2 <- has(explicit_actor_foul(f2_last))      |
    has(explicit_actor_back_head(f2_last)) |
    has(explicit_actor_cup(f2_last))
  
  if (ea1 && !ea2) return(fighter1)
  if (ea2 && !ea1) return(fighter2)
  explicit_illegal_upkick <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,120}",
      "(boots?|kicks?|lands?).{0,40}illegal upkick"
    )
  }
  
  iu1 <- has(explicit_illegal_upkick(f1_last))
  iu2 <- has(explicit_illegal_upkick(f2_last))
  
  if (iu1 && !iu2) return(fighter1)
  if (iu2 && !iu1) return(fighter2)
  # ---------- SUPER-HIGH PRIORITY: explicit low-blow victim wording ----------
  
  # e.g. "A low blow hits Kape", "Another low blow hits Silva"
  victim_low_blow_hit <- function(name) {
    paste0(
      "(?:a\\s+|another\\s+|yet\\s+another\\s+)?",
      "(low\\s+blow|groin\\s+shot|cup\\s+shot)",
      ".{0,10}(hits|catches|rocks|staggers|drops|nails|lands\\s+on)\\s+\\b",
      name, "\\b"
    )
  }
  
  # e.g. "Kape gets kicked in the groin again"
  victim_gets_kicked_groin <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,20}",
      "(gets?|got|is|was)\\s+kicked\\s+in\\s+the\\s+groin"
    )
  }
  
  # e.g. "Yet another low blow as Kape goes down"
  victim_low_blow_goes_down <- function(name) {
    paste0(
      "(?:a\\s+|another\\s+|yet\\s+another\\s+)?low\\s+blow",
      ".{0,40}\\b", name, "\\b.{0,25}(goes\\s+down|drops?|falls?)"
    )
  }
  
  # e.g. "Borralho takes one to the groin and walks it off."
  victim_takes_one_groin <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,30}",
      "takes?\\s+one\\s+to\\s+the\\s+groin"
    )
  }
  
  # --- victim detection: all low-blow victim patterns bundled ---
  v1 <- has(victim_low_blow_hit(f1_last))  |
    has(victim_gets_kicked_groin(f1_last)) |
    has(victim_low_blow_goes_down(f1_last)) |
    has(victim_takes_one_groin(f1_last))
  
  v2 <- has(victim_low_blow_hit(f2_last))  |
    has(victim_gets_kicked_groin(f2_last)) |
    has(victim_low_blow_goes_down(f2_last)) |
    has(victim_takes_one_groin(f2_last))
  
  # If exactly one fighter matches these victim patterns,
  # that fighter is the victim → the OTHER fighter is the fouler.
  if (v1 && !v2) return(fighter2)
  if (v2 && !v1) return(fighter1)
  # If exactly one fighter matches these victim patterns,
  # that fighter is the victim → the OTHER fighter is the fouler.
  if (v1 && !v2) return(fighter2)
  if (v2 && !v1) return(fighter1)
  
  
  # ---------- RULE 0X: "low blow hits Kape" / "headbutt catches Silva" ----------
  victim_object_pattern <- function(name) {
    paste0(
      "(low blow|groin shot|cup shot|headbutt|illegal headbutt|kick|knee|illegal kick|illegal knee)",
      ".{0,20}(hits|catches|rocks|staggers|nails|lands on)\\s+",
      name, "\\b"
    )
  }
  
  vobj1 <- has(victim_object_pattern(f1_last))
  vobj2 <- has(victim_object_pattern(f2_last))
  
  # if one fighter is clearly the *object* of the foul shot, they are the victim,
  # so the OTHER fighter is the fouler
  if (vobj1 && !vobj2) return(fighter2)
  if (vobj2 && !vobj1) return(fighter1)
  
  # ---------- ACTOR-POSSESSIVE LOW BLOW: "Delgado's kick ... cup of his opponent" ----------
  actor_possessive_low <- function(name) {
    paste0(
      "\\b", name, "['’]s\\s+",
      "(kicks?|knees?|legs?|feet|shins?|strikes?|shots?)",
      ".{0,80}",
      "(opponent['’]s\\s+cup|cup of his opponent|cup of her opponent|groin cup|cup)"
    )
  }
  
  apl1 <- has(actor_possessive_low(f1_last))
  apl2 <- has(actor_possessive_low(f2_last))
  
  if (apl1 && !apl2) return(fighter1)
  if (apl2 && !apl1) return(fighter2)
  
  # ---------- RULE 0A: simple "NAME ... kick/knee ... to the groin" ----------
  low_blow_groin <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,80}",
      "(kick(s|ed|ing)?|knee(s|ed|ing)?)",
      ".{0,40}to the groin"
    )
  }
  
  lb1 <- has(low_blow_groin(f1_last))
  lb2 <- has(low_blow_groin(f2_last))
  if (lb1 && !lb2) return(fighter1)
  if (lb2 && !lb1) return(fighter2)
  # (if both hit we drop through to later rules)
  
  # ... existing direct_low_blow / direct_groin_blow code continues here ...
  direct_low_blow <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,80}",
      "(spinning\\s+back\\s+kick|head\\s+kick|body\\s+kick|kick)",
      ".{0,60}(into|to|in).{0,20}(the\\s+)?cup"
    )
  }
  
  # direct "NAME ... kick ... to the groin" pattern
  direct_groin_blow <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,80}",
      "(spinning\\s+back\\s+kick|head\\s+kick|body\\s+kick|kick)",
      ".{0,60}(to|into|in).{0,20}(the\\s+)?groin"
    )
  }
  
  hit_low_1 <- has(direct_low_blow(f1_last))  || has(direct_groin_blow(f1_last))
  hit_low_2 <- has(direct_low_blow(f2_last))  || has(direct_groin_blow(f2_last))
  
  if (hit_low_1 && !hit_low_2) return(fighter1)
  if (hit_low_2 && !hit_low_1) return(fighter2)
  
  # Only use this rule when it clearly points to ONE fighter.
  # If it matches both (like "Erosa ... and Costa ... kick ... cup"),
  # we defer to later rules (subject tracking near 'cup').
  if (hit_low_1 && !hit_low_2) return(fighter1)
  if (hit_low_2 && !hit_low_1) return(fighter2)
  verbs_attack <- "(kick(s|ing|ed)?|knee(s|ing|ed)?|strike(s|ing)?|hit(s|ting)?|drill(s|ing|ed)?|land(s|ing)?|shoot(s|ing)?|connect(s|ing)?|blast(s|ing|ed)?|smash(es|ing|ed)?|smack(s|ing)?|brush(es|ing)?|ricochet(s|ing)?|crash(es|ing)?|bang(s|ing)?|poke(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|push(es|ing)?|pull(s|ing)?|grab(s|ing|bed)?|roll(s|ing)?|flatten(s|ing)?|punch(es|ing)?|hack(s|ed|ing)?|bludgeon(s|ing|ed)?|headbutt(s|ing|ed)?|club(s|bed|bing)?|delivers?|stop(s|ed|ping)?|pounds?)"
  
  # ---------- EYE-POKE VICTIM: "jabs NAME in the eye" ----------
  victim_eye_jab <- function(name) {
    paste0(
      "(?:jabs?|jabbed|jabbing|pokes?|poked|stabs?|thumbs?|",
      "swipes?|swiped|swiping|swats?|swatted|swatting)",
      "\\s+\\b", name, "\\b.{0,40}(?:in|into|to)\\s+the\\s+eye"
    )
  }
  e1 <- has(victim_eye_jab(f1_last))
  e2 <- has(victim_eye_jab(f2_last))
  
  if (e1 && !e2) return(fighter2)  # NAME is victim → other is fouler
  if (e2 && !e1) return(fighter1)
  # ---------- RULE 0: COMPLAINING – FINAL DECISION ----------
  complain_verbs <- "(complain(s|ed|ing)?|protest(s|ed|ing)?|appeal(s|ed|ing)?\\s+to\\s+the\\s+ref|argue(s|d)?\\s+to\\s+the\\s+ref)"
  foul_words     <- "(eye poke|eye|low blow|cup|groin|back of the head|headbutt|clash of heads|fence|cage)"
  pat_complain <- function(name) {
    paste0("\\b", name, "\\b.{0,80}", complain_verbs, ".{0,80}", foul_words)
  }
  
  c1 <- has(pat_complain(f1_last))
  c2 <- has(pat_complain(f2_last))
  
  # If one fighter is clearly complaining to the ref about a foul,
  # treat that fighter as the VICTIM => the OTHER fighter is the fouler.
  if (c1 && !c2) return(fighter2)
  if (c2 && !c1) return(fighter1)
  
  # ---------- RULE 0A: "NAME is admonished for ..." ----------
  admonished_pattern <- function(name) {
    paste0(
      "\\b", name, "\\b",                  # Tokkos
      ".{0,120}?",                         # some text (includes 'and ')
      "\\bis\\s+admonished\\s+for\\s+",    # 'is admonished for'
      "(eye\\s+goug(?:e|ing)|eye\\s+poke|low\\s+blow|",
      "grabbing\\s+the\\s+fence|fence\\s+grab|back\\s+of\\s+the\\s+head|",
      "illegal\\s+knee|illegal\\s+upkick)"
    )
  }
  
  a1 <- has(admonished_pattern(f1_last))
  a2 <- has(admonished_pattern(f2_last))
  
  if (a1 && !a2) return(fighter1)
  if (a2 && !a1) return(fighter2)
  
  # ---------- extra victim pattern: clutching/adjusting groin/cup ----------
  victim_cup_adj <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,80}",
      "(grumbles?|grimaces?|reels?|clutches?|grabs?).{0,40}",
      "(his|her)\\s+(cup|groin supporter|groin)"
    )
  }
  
  victim_crumble <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,80}",
      "(crumbl(es|ed)?|crumples?|drops?|falls?|goes)\\s+to\\s+the\\s+canvas.{0,60}",
      "(as a result of the foul|from (that|the) shot|in pain)"
    )
  }
  
  if (has(victim_crumble(f1_last))) return(fighter2)
  if (has(victim_crumble(f2_last))) return(fighter1)
  
  if (has(victim_cup_adj(f1_last))) return(fighter2)
  if (has(victim_cup_adj(f2_last))) return(fighter1)
  # extra victim pattern: “NAME turns away from it …”
  victim_turns_away <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,60}",
      "turns\\s+away\\s+from\\s+it"
    )
  }
  
  if (has(victim_turns_away(f1_last))) return(fighter2)
  if (has(victim_turns_away(f2_last))) return(fighter1)
  
  # RULE 0.25: VICTIM PASSIVITY - MOVED TO PRIORITY POSITION
  # Check this FIRST before any scoring - if someone is clearly smacked/hit, they're victim
  passive_patterns <- c(
    "\\b(is|gets?|was|were)\\s+(?:smacked?|hit|kicked|punched|struck|elbowed|kneed)",
    "\\b(is|gets?|was|were)\\s+(?:hit|struck)\\s+(?:in|on|to)\\s+(?:the\\s+)?(?:back of the head|head|groin|eye|cup)"
  )
  
  fouls_pred <- fouls_df11 %>%
    group_by(bout) %>%
    mutate(foul_index = row_number()) %>%  # order as produced by your extractor
    ungroup()
  
  for (pattern in passive_patterns) {
    pat_vic_f1 <- paste0("\\b", f1_last, "\\b.{0,60}", pattern)
    pat_vic_f2 <- paste0("\\b", f2_last, "\\b.{0,60}", pattern)
    
    if (has(pat_vic_f1)) return(fighter2)
    if (has(pat_vic_f2)) return(fighter1)
  }
  
  # RULE 0.5: REFEREE WARNING
  # RULE 0.5: REFEREE WARNING (Test 2 logic, with 'hatley' added)
  warning_verbs <- c("warns?", "tells?", "advises?")
  ref_keywords <- c("referee", "ref", "tognoni", "herzog", "goddard",
                    "mazzagatti", "herb", "kim", "jon", "hatley")
  
  for (ref_kw in ref_keywords) {
    for (warn_verb in warning_verbs) {
      # Pattern 1: explicit fighter name in the warning
      warning_pattern_f1 <- paste0(
        "\\b", ref_kw, "\\b.{0,20}", warn_verb,
        ".{0,30}\\b", f1_last, "\\b.{0,30}(?:",
        verbs_attack, "|back of|groin|eye|fence)"
      )
      warning_pattern_f2 <- paste0(
        "\\b", ref_kw, "\\b.{0,20}", warn_verb,
        ".{0,30}\\b", f2_last, "\\b.{0,30}(?:",
        verbs_attack, "|back of|groin|eye|fence)"
      )
      
      if (stringr::str_detect(txt, stringr::regex(warning_pattern_f1, ignore_case = TRUE))) {
        return(fighter1)
      }
      if (stringr::str_detect(txt, stringr::regex(warning_pattern_f2, ignore_case = TRUE))) {
        return(fighter2)
      }
      
      # Pattern 2: descriptor instead of explicit name
      # e.g. "Tognoni warns the American for striking the back of the head"
      descriptor_warning_pattern <- paste0(
        "\\b", ref_kw, "\\b.{0,20}", warn_verb,
        ".{0,30}(he|she|they|the\\s+\\w+).{0,30}(?:",
        verbs_attack, "|back of|groin|eye|fence)"
      )
      
      if (stringr::str_detect(txt, stringr::regex(descriptor_warning_pattern, ignore_case = TRUE))) {
        warning_match_text <- stringr::str_extract(
          txt,
          stringr::regex(descriptor_warning_pattern, ignore_case = TRUE)
        )
        
        if (!is.na(warning_match_text)) {
          # --- PHASE 1: look backwards for last named fighter ---
          warning_loc <- stringr::str_locate(
            txt,
            stringr::regex(warning_match_text, ignore_case = TRUE)
          )
          warning_pos <- warning_loc[1]
          
          context_start <- max(1, warning_pos - 250)
          context_before_warning <- stringr::str_sub(txt, context_start, warning_pos - 1)
          
          f1_before_positions <- stringr::str_locate_all(
            context_before_warning,
            stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE)
          )[[1]][, 1]
          f2_before_positions <- stringr::str_locate_all(
            context_before_warning,
            stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE)
          )[[1]][, 1]
          
          f1_last_mention_before <- if (length(f1_before_positions) > 0) max(f1_before_positions) else -Inf
          f2_last_mention_before <- if (length(f2_before_positions) > 0) max(f2_before_positions) else -Inf
          
          active_subject_before <- dplyr::case_when(
            f1_last_mention_before > f2_last_mention_before ~ fighter1,
            f2_last_mention_before > f1_last_mention_before ~ fighter2,
            TRUE                                             ~ NA_character_
          )
          
          # --- PHASE 2: look forwards for resolution ("so she stops") ---
          warning_end <- warning_loc[2]
          resolution_region <- stringr::str_sub(txt, warning_end, warning_end + 200)
          
          resolution_pattern <- "(?:so|and)\\s+(he|she|they)\\s+(?:stops?|ceases?|quits?|pulls?\\s+back)"
          resolution_match <- stringr::str_extract(
            resolution_region,
            stringr::regex(resolution_pattern, ignore_case = TRUE)
          )
          
          # --- PHASE 3: decision rules (same as Test 2) ---
          if (!is.na(active_subject_before) && !is.na(resolution_match)) {
            # we know who was acting AND we see them stop → that actor is the fouler
            return(active_subject_before)
          }
          
          if (!is.na(active_subject_before)) {
            # we at least know who was acting immediately before the warning
            return(active_subject_before)
          }
          
          if (is.na(active_subject_before) && !is.na(resolution_match)) {
            # ambiguous subject before, but ref warns and "he/she stops" – fall back to fighter1
            # (this is what makes Chiasson the fouler in the “American” sentence)
            return(fighter1)
          }
        }
      }
    }
  }
  # SPECIAL CASE: "is admonished for eye gouging" – assign fouler by nearest name before phrase
  if (stringr::str_detect(txt, "is\\s+admonished\\s+for\\s+eye\\s+goug(?:e|ing)")) {
    loc <- stringr::str_locate(txt, "is\\s+admonished\\s+for\\s+eye\\s+goug(?:e|ing)")[1]
    pre <- stringr::str_sub(txt, 1, loc - 1)
    
    f1_pos <- stringr::str_locate_all(pre, stringr::regex(paste0("\\b", f1_last, "\\b"), TRUE))[[1]]
    f2_pos <- stringr::str_locate_all(pre, stringr::regex(paste0("\\b", f2_last, "\\b"), TRUE))[[1]]
    
    f1_last_pos <- if (nrow(f1_pos) > 0) max(f1_pos[,1]) else -Inf
    f2_last_pos <- if (nrow(f2_pos) > 0) max(f2_pos[,1]) else -Inf
    
    if (f1_last_pos > f2_last_pos) return(fighter1)
    if (f2_last_pos > f1_last_pos) return(fighter2)
  }
  # RULE 1-3: SCORING
  # RULE 1–3: scoring patterns
  obj_sensitive <- "(cup|groin|eye|head|fence|cage|back of the head)"
  verbs_passive <- "(kick(ed|ing)?|knee(d|ing)?|drill(ed|ing)?|hit|struck|smash(ed|ing)?|blast(ed|ing)?|elbow(ed|ing)?|clubbed?)"
  
  pat_actor <- function(name) paste0("\\b", name, "\\b.{0,150}", verbs_attack, ".{0,100}", obj_sensitive)
  pat_actor_that <- function(name) paste0("\\b", name, "\\b.{0,150}", verbs_attack, ".{0,50}that.{0,50}(?:pounds?|lands?|crashes?|smashes?).{0,40}", obj_sensitive)
  pat_actor_loose <- function(name) paste0("\\b", name, "\\b.{0,80}", verbs_attack)
  pat_fence <- function(name) paste0("\\b", name, "\\b.{0,70}(grab(s|ing|bed)?|pull(s|ing)?|hold(s|ing)?|clutch(es|ing)?).{0,20}(fence|cage)")
  pat_fence_poss <- function(name) paste0("\\b", name, "['']?s\\s+(toes?|feet|legs?|hand|arm).{0,50}(hooked|caught|hung|stuck).{0,20}(in|on).{0,10}(?:the\\s+)?(fence|cage)")
  pat_actor_poss <- function(name) paste0("\\b", name, "['']?s\\s+(finger|hand|fist|knee|foot|leg).{0,50}(slide(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|poke(s|ing)?|drill(s|ing)?|smack(s|ing)?).{0,40}(cup|groin|eye|head|fence|cage|back of the head)")
  pat_victim <- function(name) paste0("\\b", name, "['']?s\\s+(cup|groin|eye|head)")
  pat_by <- function(name) paste0(verbs_passive, ".{0,40}(by|from)\\s+", name, "\\b")
  
  victim_passive_any <- function(name) {
    paste0(
      "\\b", name, "\\b.{0,40}",
      "(is|gets?|got|was|were)\\s+",
      "(hit|struck|headbutted|kicked|kneed|poked)\\s+",
      "(in|on|to)\\s+",
      "(the\\s+)?(groin|cup|head|back of the head|eye)"
    )
  }
  
  # base scores
  base1 <- 3*has(pat_actor(f1_last)) + 3*has(pat_actor_that(f1_last)) +
    2*has(pat_actor_loose(f1_last)) + 3*has(pat_fence(f1_last)) +
    3*has(pat_fence_poss(f1_last)) + 3*has(pat_actor_poss(f1_last)) +
    3*has(pat_by(f1_last)) - 3*has(pat_victim(f1_last))
  
  base2 <- 3*has(pat_actor(f2_last)) + 3*has(pat_actor_that(f2_last)) +
    2*has(pat_actor_loose(f2_last)) + 3*has(pat_fence(f2_last)) +
    3*has(pat_fence_poss(f2_last)) + 3*has(pat_actor_poss(f2_last)) +
    3*has(pat_by(f2_last)) - 3*has(pat_victim(f2_last))
  
  # apply penalty if someone is clearly the passive victim
  score1 <- base1 - 4*has(victim_passive_any(f1_last))
  score2 <- base2 - 4*has(victim_passive_any(f2_last))
  
  if (score1 > score2 && score1 > 0) return(fighter1)
  if (score2 > score1 && score2 > 0) return(fighter2)
  
  # RULE 4: SUBJECT TRACKING
  # RULE 4: SUBJECT TRACKING (use LAST name before foul anchor, not first)
  # RULE 4: SUBJECT TRACKING (use LAST name before foul anchor, not first)
  foul_anchor_words <- c(
    "back of the head",
    "cup",
    "groin",
    "eye",
    "fence",
    "cage",
    "illegal upkick",
    "illegal kick"
  )
  foul_anchor_pattern <- paste(foul_anchor_words, collapse = "|")
  foul_match <- stringr::str_locate(txt, stringr::regex(foul_anchor_pattern, ignore_case = TRUE))
  
  if (!is.na(foul_match[1])) {
    foul_pos <- foul_match[1]
    txt_before_foul <- stringr::str_sub(txt, 1, foul_pos - 1)
    
    # all occurrences of each last name before the anchor
    f1_locs <- stringr::str_locate_all(
      txt_before_foul,
      stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    f2_locs <- stringr::str_locate_all(
      txt_before_foul,
      stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE)
    )[[1]]
    
    f1_last_pos <- if (nrow(f1_locs) > 0) max(f1_locs[, 1]) else NA_real_
    f2_last_pos <- if (nrow(f2_locs) > 0) max(f2_locs[, 1]) else NA_real_
    
    last_fighter_pos <- if (!is.na(f1_last_pos) && !is.na(f2_last_pos)) {
      if (f1_last_pos > f2_last_pos) f1_last_pos else f2_last_pos
    } else if (!is.na(f1_last_pos)) {
      f1_last_pos
    } else if (!is.na(f2_last_pos)) {
      f2_last_pos
    } else {
      NA_real_
    }
    
    last_fighter <- if (!is.na(last_fighter_pos)) {
      if (!is.na(f1_last_pos) && f1_last_pos == last_fighter_pos) fighter1 else fighter2
    } else {
      NA_character_
    }
    
    if (!is.na(last_fighter)) {
      # region from the last name mention up to the foul anchor
      region_after_last <- stringr::str_sub(
        txt_before_foul,
        last_fighter_pos,
        nchar(txt_before_foul)
      )
      has_action_verbs <- stringr::str_detect(
        region_after_last,
        stringr::regex(verbs_attack, ignore_case = TRUE)
      )
      
      # If the same fighter is the last one mentioned before the foul
      # *and* there is at least one attack verb in that span,
      # treat that fighter as the fouler.
      if (has_action_verbs) {
        return(last_fighter)
      }
    }
  }
  
  # FALLBACK
  # FALLBACK – now also catches illegal upkicks/kicks
  foul_anchor_re <- regex(
    "cup|groin|eye|fence|cage|back of the head|grounded opponent|illegal knee|eye poke|illegal upkick|illegal kick",
    ignore_case = TRUE
  )
  loc <- stringr::str_locate(txt, foul_anchor_re)
  pre <- if (is.na(loc[1])) txt else stringr::str_sub(txt, 1, loc[1] - 1)
  
  has1 <- stringr::str_detect(pre, stringr::regex(paste0("\\b", f1_last, "\\b"), ignore_case = TRUE))
  has2 <- stringr::str_detect(pre, stringr::regex(paste0("\\b", f2_last, "\\b"), ignore_case = TRUE))
  
  if (has1 && !has2) return(fighter1)
  if (has2 && !has1) return(fighter2)
  
  "UNKNOWN"
}

# ==================== EXPANDED CONTEXT ====================

expanded_contexts <- sent_runs_clean %>%
  group_by(bout, run_id) %>%
  mutate(min_sid_incident = min(sid), max_sid_incident = max(sid)) %>%
  ungroup() %>%
  distinct(bout, run_id, min_sid_incident, max_sid_incident) %>%
  left_join(sent_all %>% select(bout, sid, sentence), by = "bout", relationship = "many-to-many") %>%
  filter(sid >= (min_sid_incident - 2) & sid <= (max_sid_incident + 2)) %>%
  group_by(bout, run_id) %>%
  summarise(expanded_context = paste(sentence, collapse = " "), .groups = "drop")

# ==================== FINAL OUTPUT ====================

fouls_df11 <- fouls_core %>%
  left_join(fighters_tbl, by = "bout") %>%
  left_join(expanded_contexts, by = c("bout", "run_id")) %>%
  rowwise() %>%
  mutate(
    foul_committed_by = detect_fouler_v5(context, fighter1, fighter2, bout),
    foul_committed_by = if_else(foul_committed_by == "UNKNOWN", detect_fouler_v5(expanded_context, fighter1, fighter2, bout), foul_committed_by)
  ) %>%
  ungroup() %>%
  select(bout, run_id, foul_committed_by, context, foul_type_guess, foul_resulted_in_deduction, referee_paused, round_mentioned)

write_csv(fouls_df11, "foul_detection_output_event11.csv")

fouls_df11

cat("\n✅ Event 11 analysis complete!\n")
cat(sprintf("Total fouls detected: %d\n", nrow(fouls_df11)))
cat(sprintf("Unknown foulers: %d\n", sum(fouls_df11$foul_committed_by == "UNKNOWN")))

context11 <- fouls_df11 %>% 
  select(context)
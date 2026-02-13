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

setwd("/Users/lucanasillo/Documents/MMA Foul Analysis")

process_event_v0 <- function(html_path) {
  doc <- read_html(html_path)
  
  # ------------ clean event name from file name ------------
  event_filename_raw <- basename(html_path)
  
  event_name <- event_filename_raw %>%
    stringr::str_remove("\\.html$")               # remove .html

  
  pbp    <- doc %>% html_element("#playbyplay")
  events <- pbp %>% html_elements("div.event")
  
  pbp_tbl <- tibble(
    bout = events %>% html_element("h2") %>% html_text2(),
    text = events %>% html_text2()
  )
  
  # --- all your existing pipeline code here ---
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
  # add to groin_patterns
  groin_patterns <- c(
    groin_patterns,
    # generic mentions of low blow, regardless of syntax
    "\\blow\\s+blow(s)?\\b",            # fix typo: you currently have "\\blow blow\\b"
    "\\blow\\s+blow\\b",                # explicit singular
    "\\ba\\s+low\\s+blow\\b",           # "a low blow"
    "low\\s+blow\\s+lands",             # "A low blow lands for Grant"
    "low\\s+blow\\b"                    # catch-all "low blow"
  )
  # ---- eye patterns (EXPANDED for "jam into eye socket") ----
  eye_patterns <- c(
    "\\beye poke\\b|\\beyepoke\\b|\\beye gouge\\b|\\beye goug(s|ing)?\\b",
      "\\beye\\s+poke(s)?\\b",
    
    
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
  # BEFORE

  
  # allow "back of Perez' head" as well as "back of the/his/her head"
  back_of_head_pat <- "back of\\s+(?:the\\s+|his\\s+|her\\s+|[A-Za-z]+(?:['’`]?\\s*)?)?head"
  
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

    "back of the head",
    "(?:fence|cage) grab",
    "grab(?:bing|s)? the (?:fence|cage)",
    "(?:pulls?|grabs?|holds?|clutches?) on the (?:fence|cage)",
    "rolls? to grab the fence",
    # illegal elbow/strike to crown of head
    "(?:elbow|strike).{0,30}crown of the head",
    "crown of the head.{0,40}illegal",
    "\\billegal strike\\b",
    "finger(s)?.{0,40}(?:in|hooked in|stuck in|out of).{0,10}(?:the\\s+)?(?:fence|cage|chain links?)",
    "toes?.{0,30}(?:in|into|on|hooked in).{0,20}(?:the\\s+)?(?:fence|cage|chain links?)",
    # NEW: generic "toes in the fence / cage / chain links"
    "toes?.{0,20}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
    # NEW: verbs + toes + fence/chain
    "hooks?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
    "pulls?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
    # "uses her toes ... which too is a foul" style phrasing
    "toes?.{0,40}(?:which\\s+too\\s+is\\s+a\\s+foul|is\\s+also\\s+a\\s+foul|is\\s+a\\s+foul)",

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
  
    "\\bcalls? time\\b",
    "\\bref(?:eree)?\\b.{0,20}\\bcalls? time\\b",
    "\\btime (?:is )?(?:called|stopped)\\b",
    "\\btime-?out(?: is)? called\\b",
    "\\bdoctor(?:'|')s? (?:check|exam|examines|called in)\\b",
   
    "\\bgives?\\s+(?:\\w+\\s+){0,3}time\\s+to\\s+recover\\b",
    "\\bbrings?\\s+in\\s+the\\s+doctor\\b",

    "\\bgets?\\s+time\\s+to\\s+recover\\b",

    "\\bgives?\\s+\\w+\\s+ample\\s+time\\s+to\\s+recover\\b",

    "\\bample\\s+time\\s+to\\s+recover\\b",
    "\\b(takes?|taking|took|will\\s+take)\\s+(?:a\\s+)?(?:moment|some\\s+time|time)\\s+to\\s+recover\\b",
    "\\bis\\s+taking\\s+time\\s+to\\s+recover\\b",
    "\\ballowed to recover\\b",

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
  
  non_foul_backhead_re <- regex(
    paste(
      
      "wraps?\\s+(his|her|their)\\s+arm\\s+around\\s+the\\s+back\\s+of\\s+the\\s+head",
      "wraps?\\s+up\\s+the\\s+head",
      "collar\\s+tie",
      "thai\\s+clinch",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  
  non_foul_eye_re <- regex(
    paste(
      # legit striking / targeting a damaged eye, not a poke/gouge
      "targeting\\s+the\\s+(?:right|left)\\s+eye\\s+of\\s+\\w+\\s+that\\s+is\\s+already\\s+damaged",
      "targeting\\s+the\\s+(?:right|left)\\s+eye\\s+that\\s+is\\s+already\\s+damaged",
      "continues?\\s+to\\s+target\\s+the\\s+(?:right|left)\\s+eye",
      "working\\s+over\\s+the\\s+(?:right|left)\\s+eye",
      "(?:swollen|badly\\s+swollen|already\\s+swollen)\\s+(?:right|left)\\s+eye",
      "cut\\s+(?:over|under)\\s+the\\s+(?:right|left)\\s+eye",
      "swelling\\s+(?:above|around|under|beneath)\\s+\\w+['’`]?s?\\s+(?:left\\s+|right\\s+)?eye",
      "developing\\s+some\\s+swelling\\s+(?:above|around|under|beneath)\\s+\\w+['’`]?s?\\s+(?:left\\s+|right\\s+)?eye",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  

  
  no_pause_re <- regex(paste(
    "waves? (?:off|it off|the ref off)",
    "ignores? it",

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
 
    "after\\s+landing\\s+multiple\\s+eye\\s+pokes?",
    "in\\s+trouble\\s+after\\s+getting\\s+nailed"
  )
  
  reaction_only_re <- regex(
    paste(reaction_only_patterns, collapse = "|"),
    ignore_case = TRUE
  )
  
  point_deduction_re <- regex(paste(c("\\bpoint deduction\\b", "\\belects\\s+to\\s+deduct\\s+(?:a|one)?\\s*point(?:\\s+from\\s+\\w+)?\\b", "\\bdeducts?\\s+(?:a|one)?\\s*point(?:\\s+from\\s+\\w+)?\\b", "\\btakes?\\s+(?:a|one)?\\s*point\\b", "\\bremoves?\\s+(?:a|one)?\\s*point\\b", "\\bsubtracts?\\s+(?:a|one)?\\s*point\\b"), collapse="|"), ignore_case = TRUE)
  
  deduction_neg_re <- regex(paste(c("warning", "no point", "without (?:a|any) point", "no\\s+points?\\s+deducted"), collapse="|"), ignore_case = TRUE)
  

  
  to_sentences <- function(x){
    x %>%
      str_replace_all("<br>", " ") %>%
      
      # 1) FIX THE REAL MOJIBAKE SEQUENCES FIRST

      str_replace_all("\u00E2\u0080[\u0098\u0099]", "'") %>%   # ‘ and ’

      str_replace_all("\u00E2\u0080[\u009C\u009D]", "\"") %>%  # “ and ”
      

      str_replace_all("â€™|â€˜", "'") %>%          
      str_replace_all("â€œ|â€\u009d", "\"") %>%
      
      # 3) Normalise any *proper* Unicode curly quotes
      str_replace_all("[\u2018\u2019`']", "'") %>% 
      str_replace_all("[\u201C\u201D]", "\"") %>%
      
      # 4) clean up spacing and split
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
  
  ref_re <- regex(
    paste(
      "referee",
      "ref",
      "keith peterson",
      "herb dean",
      "marc goddard",
      "jason herzog",
      "mike beltran",
      "chris tognoni",
      "dan miragliotta",
      "mark smith",
      "kevin sataki",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  
  sent_all <- pbp_tbl %>%
    # 1) split each fight text into sentences
    mutate(sentence = map(text, to_sentences)) %>%
    select(bout, sentence) %>%
    unnest(sentence) %>%              # now we have one row per sentence
    group_by(bout) %>%
    mutate(
      sid          = row_number(),
      # raw foul detection
      has_foul_raw = str_detect(sentence, foul_re) &
        !str_detect(sentence, non_foul_backhead_re) &
        !str_detect(sentence, non_foul_eye_re),
      # reaction-only lines
      is_reaction_only = str_detect(sentence, reaction_only_re),
      
 
      time_str      = str_extract(sentence, time_re),
      round_str_raw = str_extract(sentence, round_re),

      ref_mentioned = str_detect(sentence, ref_re),
      

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
      
  assign(paste0("sent_all_dbg__", event_name), sent_all, envir = .GlobalEnv)
  

  
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

        "back of the head",
        back_of_head_pat,
        back_head_patterns,               
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
  neg_injury_eye_re <- regex(
    "cut|laceration|swelling|hematoma|bruise|bleeding|blood|closed\\s+up|black\\s+eye|above\\s+.*eye",
    ignore_case = TRUE
  )
  

  
  # helper: clamp
  clamp01 <- function(x) pmax(0, pmin(1, x))
  

  sigmoid <- function(z) 1 / (1 + exp(-z))
  
  # score each sentence based on evidence + exclusions
  sent_all <- sent_all %>%
    mutate(
      pos_strong_anchor = str_detect(sentence, strong_foul_anchor_re),
      pos_eye_clean     = str_detect(sentence, eye_categorise_re),
      pos_pause         = pause_token & !non_foul_pz,
      pos_deduction     = is_deduction_raw,
      # ALIGNMENT WITH has_foul_raw 
      pos_foul_re_hit = str_detect(sentence, foul_re),
      
      neg_block_backhead = str_detect(sentence, non_foul_backhead_re),
      neg_block_eye      = str_detect(sentence, non_foul_eye_re),
      
      # ALIGNMENT WITH has_foul (final gate)
      neg_gate_reaction = is_reaction_only,
      neg_gate_not_started = !fight_started,
      pos_foul_language = str_detect(sentence, regex(
        paste0(
          # Eye poke / fingers
          "eye\\s*pok|eyepok|poke(d)?\\s+.*eye|finger(s)?\\s+.*eye|thumb\\s+.*eye|eye\\s*socket|eyeball|",
          # Groin / cup / low blow
          "low\\s*blow|below\\s+the\\s+belt|groin|cup(\\b|\\s)|(knee|kick|shot)\\s+.*(groin|cup)|",
          # Fence / cage
          "fence\\s*grab|cage\\s*grab|grab(s|bed|bing)?\\s+the\\s+(fence|cage)|",
          "hook(s|ed|ing)?\\s+.*(fence|cage)|",
          "toes?\\s+in\\s+the\\s+(fence|cage)|",
          "fingers?\\s+in\\s+the\\s+(fence|cage)|",
          "pulls?\\s+on\\s+the\\s+(fence|cage)|",
          "pulls?\\s+the\\s+(fence|cage)|",
          "chain\\s*links?|",
          # Back of head / rabbit punch
          "rabbit\\s*punch|back\\s+of\\s+(the\\s+)?head|behind\\s+the\\s+ear|",
          # Illegal knee / upkick
          "illegal\\s+knee|knee\\s+to\\s+(a\\s+)?grounded|",
          "illegal\\s+upkick|upkick\\b.*(grounded|downed)|",
          # Headbutt / crown
          "head\\s*butt|headbutt|crown\\s+of\\s+the\\s+head"
        ),
        ignore_case = TRUE
      )),
      
      neg_injury_stoppage = str_detect(sentence, regex(
        "cut|laceration|swelling|hematoma|bruise|bleeding|blood|accidental|clash\\s+of\\s+heads",
        ignore_case = TRUE
      )),
      
      pos_ref_words   = str_detect(sentence, regex("warns?|warning|admonished|calls? time|timeout|doctor", TRUE)),
      pos_pause_foul  = pos_pause & pos_foul_language,
      pos_ref_foul    = pos_ref_words & pos_foul_language,
      
      # negatives...
      neg_nonfoul_eye   = str_detect(sentence, non_foul_eye_re),
      neg_nonfoul_back  = str_detect(sentence, non_foul_backhead_re),
      neg_no_pause      = explicit_no_pause,
      neg_negation      = is_neg,
      neg_injury_eye = str_detect(sentence, neg_injury_eye_re),
      neg_reaction_only = is_reaction_only,
      
      rule_score_raw =
        # Core detection: mirror has_foul_raw
        80 * as.integer(pos_foul_re_hit) -
        
        # Core exclusions: mirror has_foul_raw blocks
        80 * as.integer(neg_block_backhead) -
        80 * as.integer(neg_block_eye) -
        
        # Final gates: mirror has_foul gates (these shouldn’t “kill” as hard as core exclusions)
        40 * as.integer(neg_gate_reaction) -
        40 * as.integer(neg_gate_not_started) +
        
        # Extra evidence (your existing confidence boosters)
        20 * as.integer(pos_strong_anchor) +
        10 * as.integer(pos_foul_language) +
        15 * as.integer(pos_eye_clean) +
        15 * as.integer(pos_pause_foul) +
        20 * as.integer(pos_deduction) +
        10 * as.integer(pos_ref_foul) -
        
        # Your extra negatives (keep them if they helped your FP issue)
        40 * as.integer(neg_injury_eye) -
        40 * as.integer(neg_injury_stoppage) -
        40 * as.integer(neg_negation) -
        40 * as.integer(neg_reaction_only) -
        15 * as.integer(neg_no_pause),
      
      rule_score = pmax(0, pmin(100, rule_score_raw)),
      rule_conf_sigmoid = clamp01(sigmoid((rule_score - 50) / 10)),
      rule_conf = rule_conf_sigmoid,
      
      rule_reasons_pos = str_squish(str_replace_all(paste0(
        if_else(pos_strong_anchor, "strong_anchor|", ""),
        if_else(pos_foul_language, "foul_language|", ""),
        if_else(pos_eye_clean,     "eye_clean_phrase|", ""),
        if_else(pos_pause_foul,    "pause_plus_foul_language|", ""),
        if_else(pos_deduction,     "point_deduction|", ""),
        if_else(pos_foul_re_hit, "foul_re_hit|", ""),
        if_else(pos_ref_foul,      "ref_plus_foul_language|", "")
      ), "\\|$", "")),
      
      rule_reasons_neg = str_squish(str_replace_all(paste0(
        if_else(neg_nonfoul_eye,     "nonfoul_eye_context|", ""),
        if_else(neg_nonfoul_back,    "nonfoul_backhead_clinch|", ""),
        if_else(neg_no_pause,        "explicit_no_pause|", ""),
        if_else(neg_negation,        "negation|", ""),
        if_else(neg_injury_eye, "injury_eye_context|", ""),
        if_else(neg_reaction_only,   "reaction_only|", ""),
        if_else(neg_injury_stoppage, "injury_stoppage|", "")
      ), "\\|$", ""))
    )
  
  
  assign(paste0("sent_all_dbg__", event_name), sent_all, envir = .GlobalEnv)

  
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
  

  
  extract_fighters <- function(bout) {
    parts <- str_split(bout, " vs\\. ", n = 2)[[1]]
    if (length(parts) < 2) return(tibble(fighter1 = NA_character_, fighter2 = NA_character_))
    f1 <- str_trim(str_remove(parts[1], "\\s*\\(.*$"))
    f2 <- str_trim(str_remove(parts[2], "\\s*\\(.*$"))
    tibble(fighter1 = f1, fighter2 = f2)
  }
  
  fighters_tbl <- pbp_tbl %>%
    mutate(tmp = map(bout, extract_fighters)) %>%
    unnest(tmp) %>%
    mutate(
      bout_key = paste(
        stringr::str_to_lower(stringr::str_extract(fighter1, "[^ ]+$")),
        "vs",
        stringr::str_to_lower(stringr::str_extract(fighter2, "[^ ]+$"))
      )
    )
  
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
    
    
    last_pos <- function(name, upto_txt = txt) {
      locs <- stringr::str_locate_all(
        upto_txt,
        stringr::regex(paste0("\\b", name, "\\b"), ignore_case = TRUE)
      )[[1]]
      if (nrow(locs) == 0) return(NA_real_)
      max(locs[, 1])
    }
    
    first_pos <- function(name, in_txt = txt) {
      loc <- stringr::str_locate(
        in_txt,
        stringr::regex(paste0("\\b", name, "\\b"), ignore_case = TRUE)
      )
      if (is.na(loc[1])) return(NA_real_)
      loc[1]
    }
    

    victim_gets_kicked_groin <- function(name) {
      paste0(
        "\\b", name, "\\b.{0,20}",
        "(gets?|got|is|was)\\s+kicked\\s+in\\s+the\\s+groin"
      )
    }
    
    if (stringr::str_detect(
      txt,
      stringr::regex("gets\\s+kicked\\s+in\\s+the\\s+groin", ignore_case = TRUE)
    )) {
      
      v1 <- stringr::str_detect(
        txt, stringr::regex(victim_gets_kicked_groin(f1_last), ignore_case = TRUE)
      )
      v2 <- stringr::str_detect(
        txt, stringr::regex(victim_gets_kicked_groin(f2_last), ignore_case = TRUE)
      )
      
      if (v1 && !v2) return(fighter2)  # fighter1 is victim → fighter2 fouler
      if (v2 && !v1) return(fighter1)  # fighter2 is victim → fighter1 fouler
    }
    

    if (stringr::str_detect(
      txt,
      stringr::regex("she\\s+is\\s+warned\\s+for\\s+grabbing\\s+the\\s+(fence|cage|chain\\s+links?)",
                     ignore_case = TRUE)
    )) {
      
      # In the Alencar sentence the one **acting** is the first-named fighter.
      f1_first <- first_pos(f1_last)
      f2_first <- first_pos(f2_last)
      
      if (!is.na(f1_first) && (is.na(f2_first) || f1_first < f2_first)) return(fighter1)
      if (!is.na(f2_first) && (is.na(f1_first) || f2_first < f1_first)) return(fighter2)
    }
   
    if (stringr::str_detect(
      txt,
      stringr::regex("warns\\s+the\\s+american\\s+for\\s+striking\\s+the\\s+back\\s+of\\s+the\\s+head",
                     ignore_case = TRUE)
    )) {
      
      return(fighter1)
    }
   
    if (stringr::str_detect(
      txt,
      stringr::regex("flattens\\s+.*bludgeoning\\s+him\\s+with\\s+punches\\s+to\\s+the\\s+side\\s+and\\s+back\\s+of\\s+the\\s+head",
                     ignore_case = TRUE)
    )) {
      
 
      f1_first <- first_pos(f1_last)
      f2_first <- first_pos(f2_last)
      
      if (!is.na(f1_first) && (is.na(f2_first) || f1_first < f2_first)) return(fighter1)
      if (!is.na(f2_first) && (is.na(f1_first) || f2_first < f1_first)) return(fighter2)
    }


    name_possessive <- function(name) {
      paste0(
        "\\b", name,
        "(?:['’]|[^[:alpha:]]){0,3}s\\s+"
      )
    }
    

    both_eyes_pattern <- function(actor, victim) {
      paste0(
        name_possessive(actor),
        "fingers?\\b.{0,160}",
        "jam(s|med|ming)?\\s+into\\s+both\\s+of\\s+",
        victim,
        "(?:['’]|[^[:alpha:]]){0,3}s?\\s+eyes?"
      )
    }
    

    
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
    

    
    if (has(both_eyes_pattern(f1_last, f2_last)) && !has(both_eyes_pattern(f2_last, f1_last))) {
      return(fighter1)
    }
    if (has(both_eyes_pattern(f2_last, f1_last)) && !has(both_eyes_pattern(f1_last, f2_last))) {
      return(fighter2)
    }
    

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
    

    controls_fence_pattern <- function(controller, controlled) {
      paste0(
        "\\b", controller, "\\b.{0,80}",
        "(controls|pins|keeps|holds|uses)\\s+",
        ".*?\\b", controlled, "\\b",
        ".{0,120}",
        "(he|she)\\s+is\\s+warned\\s+for\\s+",
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
    
   
    
    be1 <- has(both_eyes_pattern(f1_last, f2_last))
    be2 <- has(both_eyes_pattern(f2_last, f1_last))
    
    if (be1 && !be2) return(fighter1)
    if (be2 && !be1) return(fighter2)
    
  
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
    explicit_actor_back_head_general <- function(actor, victim) {
      paste0(
        "\\b", actor, "\\b.{0,80}",
        "(flattens?|rocks?|drops?|hurts?|bludgeon(s|ing|ed)?|punch(es|ing)?|hammers?|clubs?|smashes?)",
        ".{0,120}",
        "(back\\s+of\\s+the\\s+head)"
      )
    }
    
    if (has(explicit_actor_back_head_general(f1_last, f2_last)) &&
        !has(explicit_actor_back_head_general(f2_last, f1_last))) {
      return(fighter1)
    }
    if (has(explicit_actor_back_head_general(f2_last, f1_last)) &&
        !has(explicit_actor_back_head_general(f1_last, f2_last))) {
      return(fighter2)
    }
    
    explicit_actor_headbutt <- function(actor, victim) {
      paste0(
        "\\b", actor, "\\b.{0,40}",
        "headbutt(s|ed|ing)?\\s+",
        "\\b", victim, "\\b"
      )
    }
    
    if (has(explicit_actor_headbutt(f1_last, f2_last)) &&
        !has(explicit_actor_headbutt(f2_last, f1_last))) {
      return(fighter1)
    }
    if (has(explicit_actor_headbutt(f2_last, f1_last)) &&
        !has(explicit_actor_headbutt(f1_last, f2_last))) {
      return(fighter2)
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


    victim_low_blow_hit <- function(name) {
      paste0(
        "(?:a\\s+|another\\s+|yet\\s+another\\s+)?",
        "(low\\s+blow|groin\\s+shot|cup\\s+shot)",
        ".{0,10}(hits|catches|rocks|staggers|drops|nails|lands\\s+on)\\s+\\b",
        name, "\\b"
      )
    }
    

    victim_gets_kicked_groin <- function(name) {
      paste0(
        "\\b",name,"\\b.{0,20}",
        "(gets?|got|is|was)\\s+kicked\\s+in\\s+the\\s+groin"
      )
    }
    

    vkg1 <- has(victim_gets_kicked_groin(f1_last))
    vkg2 <- has(victim_gets_kicked_groin(f2_last))
    
    if (vkg1 && !vkg2) return(fighter2)  
    if (vkg2 && !vkg1) return(fighter1)  
    
    victim_low_blow_goes_down <- function(name) {
      paste0(
        "(?:a\\s+|another\\s+|yet\\s+another\\s+)?low\\s+blow",
        ".{0,40}\\b", name, "\\b.{0,25}(goes\\s+down|drops?|falls?)"
      )
    }
    

    victim_takes_one_groin <- function(name) {
      paste0(
        "\\b", name, "\\b.{0,30}",
        "takes?\\s+one\\s+to\\s+the\\s+groin"
      )
    }
 
    v1 <- has(victim_low_blow_hit(f1_last))  |
      has(victim_gets_kicked_groin(f1_last)) |
      has(victim_low_blow_goes_down(f1_last)) |
      has(victim_takes_one_groin(f1_last))
    
    v2 <- has(victim_low_blow_hit(f2_last))  |
      has(victim_gets_kicked_groin(f2_last)) |
      has(victim_low_blow_goes_down(f2_last)) |
      has(victim_takes_one_groin(f2_last))
    

    if (v1 && !v2) return(fighter2)
    if (v2 && !v1) return(fighter1)

    if (v1 && !v2) return(fighter2)
    if (v2 && !v1) return(fighter1)
    
    

    victim_object_pattern <- function(name) {
      paste0(
        "(low blow|groin shot|cup shot|headbutt|illegal headbutt|kick|knee|illegal kick|illegal knee)",
        ".{0,20}(hits|catches|rocks|staggers|nails|lands on)\\s+",
        name, "\\b"
      )
    }
    
    vobj1 <- has(victim_object_pattern(f1_last))
    vobj2 <- has(victim_object_pattern(f2_last))
    

    if (vobj1 && !vobj2) return(fighter2)
    if (vobj2 && !vobj1) return(fighter1)
    

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

    direct_low_blow <- function(name) {
      paste0(
        "\\b", name, "\\b.{0,80}",
        "(spinning\\s+back\\s+kick|head\\s+kick|body\\s+kick|kick)",
        ".{0,60}(into|to|in).{0,20}(the\\s+)?cup"
      )
    }
    

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
    

.
    if (hit_low_1 && !hit_low_2) return(fighter1)
    if (hit_low_2 && !hit_low_1) return(fighter2)
    verbs_attack <- "(kick(s|ing|ed)?|knee(s|ing|ed)?|strike(s|ing)?|hit(s|ting)?|drill(s|ing|ed)?|land(s|ing)?|shoot(s|ing)?|connect(s|ing)?|blast(s|ing|ed)?|smash(es|ing|ed)?|smack(s|ing)?|brush(es|ing)?|ricochet(s|ing)?|crash(es|ing)?|bang(s|ing)?|poke(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|push(es|ing)?|pull(s|ing)?|grab(s|ing|bed)?|roll(s|ing)?|flatten(s|ing)?|punch(es|ing)?|hack(s|ed|ing)?|bludgeon(s|ing|ed)?|headbutt(s|ing|ed)?|club(s|bed|bing)?|delivers?|stop(s|ed|ping)?|pounds?)"
    

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

    complain_verbs <- "(complain(s|ed|ing)?|protest(s|ed|ing)?|appeal(s|ed|ing)?\\s+to\\s+the\\s+ref|argue(s|d)?\\s+to\\s+the\\s+ref)"
    foul_words     <- "(eye poke|eye|low blow|cup|groin|back of the head|headbutt|clash of heads|fence|cage)"
    pat_complain <- function(name) {
      paste0("\\b", name, "\\b.{0,80}", complain_verbs, ".{0,80}", foul_words)
    }
    
    c1 <- has(pat_complain(f1_last))
    c2 <- has(pat_complain(f2_last))
    

    if (c1 && !c2) return(fighter2)
    if (c2 && !c1) return(fighter1)
    

    admonished_pattern <- function(name) {
      paste0(
        "\\b", name, "\\b",                  
        ".{0,120}?",                         
        "\\bis\\s+admonished\\s+for\\s+",    
        "(eye\\s+goug(?:e|ing)|eye\\s+poke|low\\s+blow|",
        "grabbing\\s+the\\s+fence|fence\\s+grab|back\\s+of\\s+the\\s+head|",
        "illegal\\s+knee|illegal\\s+upkick)"
      )
    }
    
    a1 <- has(admonished_pattern(f1_last))
    a2 <- has(admonished_pattern(f2_last))
    
    if (a1 && !a2) return(fighter1)
    if (a2 && !a1) return(fighter2)
    

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

    victim_turns_away <- function(name) {
      paste0(
        "\\b", name, "\\b.{0,60}",
        "turns\\s+away\\s+from\\s+it"
      )
    }
    
    if (has(victim_turns_away(f1_last))) return(fighter2)
    if (has(victim_turns_away(f2_last))) return(fighter1)

    passive_patterns <- c(
      "\\b(is|gets?|was|were)\\s+(?:smacked?|hit|kicked|punched|struck|elbowed|kneed)",
      "\\b(is|gets?|was|were)\\s+(?:hit|struck)\\s+(?:in|on|to)\\s+(?:the\\s+)?(?:back of the head|head|groin|eye|cup)"
    )
    
    for (pattern in passive_patterns) {
      pat_vic_f1 <- paste0("\\b", f1_last, "\\b.{0,60}", pattern)
      pat_vic_f2 <- paste0("\\b", f2_last, "\\b.{0,60}", pattern)
      
      if (has(pat_vic_f1)) return(fighter2)
      if (has(pat_vic_f2)) return(fighter1)
    }
    

    warning_verbs <- c("warns?", "tells?", "advises?", "telling")
    ref_keywords <- c("referee", "ref", "tognoni", "herzog", "goddard",
                      "mazzagatti", "herb", "kim", "jon", "hatley", "dean")
    
    for (ref_kw in ref_keywords) {
      for (warn_verb in warning_verbs) {

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
            

            warning_end <- warning_loc[2]
            resolution_region <- stringr::str_sub(txt, warning_end, warning_end + 200)
            
            resolution_pattern <- "(?:so|and)\\s+(he|she|they)\\s+(?:stops?|ceases?|quits?|pulls?\\s+back)"
            resolution_match <- stringr::str_extract(
              resolution_region,
              stringr::regex(resolution_pattern, ignore_case = TRUE)
            )
            

            victim_after_pattern <- function(name) {
              paste0(
                "\\b", name, "(?:['’]s)?\\s+",
                "(head|heads|eye|eyes|cup|groin|neck)"
              )
            }
            
            vic1_after <- stringr::str_detect(
              resolution_region,
              stringr::regex(victim_after_pattern(f1_last), ignore_case = TRUE)
            )
            vic2_after <- stringr::str_detect(
              resolution_region,
              stringr::regex(victim_after_pattern(f2_last), ignore_case = TRUE)
            )

            if (vic1_after && !vic2_after) {
              return(fighter2)
            }
            if (vic2_after && !vic1_after) {
              return(fighter1)
            }
            victim_before_pattern <- function(name) {
              paste0(
                "\\b", name, "(?:['’]s)?\\s+",
                "(head|heads|eye|eyes|cup|groin|neck)"
              )
            }
            
            vic1_before <- stringr::str_detect(
              context_before_warning,
              stringr::regex(victim_before_pattern(f1_last), ignore_case = TRUE)
            )
            vic2_before <- stringr::str_detect(
              context_before_warning,
              stringr::regex(victim_before_pattern(f2_last), ignore_case = TRUE)
            )
            

            if (vic1_before && !vic2_before) {
              return(fighter2)
            }
            if (vic2_before && !vic1_before) {
              return(fighter1)
            }

            if (!is.na(active_subject_before) && !is.na(resolution_match)) {
              # we know who was acting AND we see them stop → that actor is the fouler
              return(active_subject_before)
            }
            
            if (!is.na(active_subject_before)) {

              return(active_subject_before)
            }
            
            if (is.na(active_subject_before) && !is.na(resolution_match)) {

              return(fighter1)
            }
          }
        }
      }
    }

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
        
        # figure out the last fighter's *last name*
        last_last <- if (identical(last_fighter, fighter1)) f1_last else f2_last

        passive_pattern <- paste0(
          "\\b", last_last, "\\b.{0,40}",
          "(gets?|is|was|were)\\s+",
          "(kicked|knee[ds]?|hit|struck|elbowed|poked|headbutted)"
        )
        
        passive_here <- stringr::str_detect(
          region_after_last,
          stringr::regex(passive_pattern, ignore_case = TRUE)
        )
        
        if (passive_here) {
          # last_fighter is the victim → other fighter is the fouler
          return(if (identical(last_fighter, fighter1)) fighter2 else fighter1)
        }
        

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
  

  
  expanded_contexts <- sent_runs_clean %>%
    group_by(bout, run_id) %>%
    mutate(min_sid_incident = min(sid), max_sid_incident = max(sid)) %>%
    ungroup() %>%
    distinct(bout, run_id, min_sid_incident, max_sid_incident) %>%
    left_join(sent_all %>% select(bout, sid, sentence), by = "bout", relationship = "many-to-many") %>%
    filter(sid >= (min_sid_incident - 2) & sid <= (max_sid_incident + 2)) %>%
    group_by(bout, run_id) %>%
    summarise(expanded_context = paste(sentence, collapse = " "), .groups = "drop")
  # build sent_all, sent_runs_clean, run_bounds, pause_check, deduction_map, etc.
  
  fouls_all <- fouls_core %>%
    left_join(fighters_tbl, by = "bout") %>%
    left_join(expanded_contexts, by = c("bout", "run_id")) %>%
    rowwise() %>%
    mutate(
      foul_committed_by = detect_fouler_v5(context, fighter1, fighter2, bout),
      foul_committed_by = if_else(
        foul_committed_by == "UNKNOWN",
        detect_fouler_v5(expanded_context, fighter1, fighter2, bout),
        foul_committed_by
      )
    ) %>%
    ungroup() %>%
    group_by(bout) %>%
    mutate(
      foul_idx = row_number(),
      event = event_name # <- aligns with gold
      ) %>%  
    ungroup() %>%
    select(
      event, bout, bout_key, foul_idx,
      foul_committed_by,
      context,
      foul_type_guess,
      foul_resulted_in_deduction,
      referee_paused,
      round_mentioned,
      expanded_context
    )
  sent_out <- sent_all %>%
    select(
      bout, sid, sentence,
      has_foul_raw, has_foul,
      rule_score, rule_conf,
      rule_reasons_pos, rule_reasons_neg,
      time_str, round_mentioned
    ) %>%
    mutate(event = event_name)
  
  write.csv(sent_out, file = paste0("sentence_conf_", event_name, ".csv"), row.names = FALSE)
  fouls_all
}


html_files <- c(
  "Raw PBP Data/UFC-deridder-allen-18-10-2025.html",
  "Raw PBP Data/UFC320-ankalaev-pereira-04-10-2025.html",
  "Raw PBP Data/UFCvegas-106-burns-morales-17-05-2025.html",
  "Raw PBP Data/UFCvegas-107-blanchfield-barber-31-05-2025.html",
  "Raw PBP Data/UFC322-dellamaddalena-makhachev-15-11-2025.html",
  "Raw PBP Data/UFC-lewis-teixeira-12-06-2025.html",
  "Raw PBP Data/UFC-covington-buckley-14-12-2024.html",
  "Raw PBP Data/UFCvegas-105-emmett-murphy-05-04-2025.html",
  "Raw PBP Data/UFC-imavov-borralho-06-09-2025.html",
  "Raw PBP Data/UFC321-aspinall-gane-25-10-2025.html",
  "Raw PBP Data/UFC-tsarukyan-hooker-22-11-2025.html"
)

model_v0_all <- purrr::map_df(html_files, process_event_v0)
readr::write_csv(model_v0_all, "model_v0_all_events.csv")





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
  
# create a bout_key used ONLY for joining
model_v0_clean <- model_v0_all %>% 
  # (optionally select/arrange columns, but DON'T change bout_key)
  select(event, bout, bout_key, foul_idx, everything())



normalize_bout <- function(x) {
  x %>% 
    str_to_lower() %>% 
    str_replace_all("[^a-z\\s]", "") %>%  # remove weights, punctuation
    str_squish()
}

gold_clean <- gold_clean %>% mutate(bout_key = normalize_bout(bout))
model_v0_clean <- model_v0_clean %>% mutate(bout_key = normalize_bout(bout_key))

model_v0_clean <- model_v0_clean %>%
  mutate(
    predicted_foul_type = case_when(
      str_detect(foul_type_guess, "eyepoke|eye poke|eye scrape|eyescrape|eye") ~ "eye poke",
      str_detect(foul_type_guess, "cup|groin|south of the border|low blow|belt") ~ "groin strike",
      str_detect(foul_type_guess, "(back|crown) of (the |his |her )?head|rabbit|back of\\s+(?:the\\s+|his\\s+|her\\s+|[A-Za-z]+(?:['’`]?\\s*)?)?head") ~ "back of head strike",
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

errors_foultype = accuracy_test %>% filter(match_foul_type == FALSE)
errors_fouler = accuracy_test %>% filter(match_fouler == FALSE)
errors_pause = accuracy_test %>% filter(match_pause == FALSE)
errors_ded = accuracy_test %>% filter(match_deduction == FALSE)

write.csv(errors_foultype, "MMA-Foultype_acc.csv", row.names = FALSE)
write.csv(errors_fouler, "MMA-Fouler_acc2.csv", row.names = FALSE)
write.csv(errors_pause, "MMA-Pause_acc.csv", row.names = FALSE)
write.csv(errors_ded, "MMA-Ded_acc.csv", row.names = FALSE)

gold_clean %>% 
  group_by(event, bout_key) %>% 
  summarise(
    gold_idx = paste(foul_idx, collapse = ", "),
    .groups = "drop"
  )

model_v0_clean %>% 
  group_by(event, bout_key) %>% 
  summarise(
    model_idx = paste(foul_idx, collapse = ", "),
    .groups = "drop"
  )



idx_compare <- gold_clean %>% 
  group_by(event, bout_key) %>% 
  summarise(
    gold_idx  = list(sort(unique(foul_idx))),
    .groups = "drop"
  ) %>%
  full_join(
    model_v0_clean %>% 
      group_by(event, bout_key) %>% 
      summarise(
        model_idx = list(sort(unique(foul_idx))),
        .groups = "drop"
      ),
    by = c("event", "bout_key")
  ) %>%
  mutate(
  
    match = map2_lgl(gold_idx, model_idx, ~ identical(.x, .y))
  )


g_test <- gold_clean %>% 
  group_by(event, bout_key) %>% 
  summarise(gold_idx = paste(sort(unique(foul_idx)), collapse = ", "))

m_test <- model_v0_clean %>% 
  group_by(event, bout_key) %>% 
  summarise(model_idx = paste(sort(unique(foul_idx)), collapse = ", "))


context_all <- model_v0_clean %>% 
  select(context)

detect_fouler_v5("De Ridder flattens Allen out for a second and stars bludgeoning him with punches to the side and back of the head, and Herzog is telling him to knock it off but little more.",
                 "Reiner de Ridder",
                 "Brendan Allen")

detect_fouler_v5("Tognoni warns the American for striking the back of the head, so she stops hitting and isolates a rear-naked choke.",
                 "Macy Chiasson",
                 "Yana Santos")

detect_fouler_v5("Alencar uses her left arm to pressure behind Demopoulos's neck, and she is warned for grabbing the fence when imposing her weight on her opponent.",
                 "Talita Alencar",
                 "Vanessa Demopoulos")

detect_fouler_v5("Kape gets kicked in the groin again.",
                 "Manel Kape",
                 "Bruno Silva")

library(stringr)

txt <- "Kape gets kicked in the groin again." %>%
  str_squish() %>%
  str_to_lower()

f1_last <- str_to_lower(str_extract("Manel Kape", "[^ ]+$"))
f2_last <- str_to_lower(str_extract("Opponent Name", "[^ ]+$"))

victim_gets_kicked_groin <- function(name) {
  paste0(
    "\\b", name, "\\b.{0,20}",
    "(gets?|got|is|was)\\s+kicked\\s+in\\s+the\\s+groin"
  )
}

pattern1 <- victim_gets_kicked_groin(f1_last)
pattern2 <- victim_gets_kicked_groin(f2_last)

pattern1
str_detect(txt, regex(pattern1, ignore_case = TRUE))
pattern2
str_detect(txt, regex(pattern2, ignore_case = TRUE))
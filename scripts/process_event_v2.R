source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

# Packages

library(dplyr)
library(stringr)
library(rvest)
library(purrr)
library(tidyr)
library(zoo)

# Note: no setwd() in the Github version - handled in 00_paths.R: setwd("/Users/lucanasillo/Documents/MMA Foul Analysis")

process_event_v2 <- function(html_path) {
  # parse event HTML and extract play-by-play section
  doc <- read_html(html_path)
  
  # Derive event_name from the HTML file name
  event_filename_raw <- basename(html_path)
  
  event_name <- event_filename_raw %>%
    stringr::str_remove("\\.html$")               # remove .html
  
  # Build raw bout-level text table
  pbp    <- doc %>% html_element("#playbyplay")
  events <- pbp %>% html_elements("div.event")
  
  pbp_tbl <- tibble(
    bout = events %>% html_element("h2") %>% html_text2(),
    text = events %>% html_text2()
  )
  
  
  # ==================== DICTIONARIES ====================
  
  # Foul Dictionaries: Groin / Cup Patterns:
  
  groin_patterns <- c(
    "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|pounds?).{0,20}(?:to|in) the groin",
    "(?:kicks?|knees?|strikes?|hits?|drills?|lands?|shoots?|connects?|blasts?|smashes?|smacks?|brushes?|ricochets?|crashes?|bangs?|pounds?|scrapes?|scraped?).{0,40}(?:\\b\\w+['’]s\\s+cup\\b|\\bgroin\\s+cup\\b|\\bcup\\b)",
    "(?:scrapes?|scraped?).{0,40}groin cup",
    "\\blow blow\\b",
    "\\bcup shot\\b",
    "(?:kick|knee|strike|shot|blow).{0,40}(south of the border|below the belt|low blow)",
    "(south of the border|below the belt) blow",
    "(?:slams?|jam(s|med)?|glanc(es|ed)?|graz(es|ed)?|clips?|catches?).{0,40}(?:in|into|to).{0,10}(?:the\\s+)?(groin|cup)",
    "(?:glancing\\s+blow|glancing\\s+shot).{0,20}(?:to|in|into)\\s+the\\s+(cup|groin)",
    "(?:foot|knee|shin|kick).{0,40}(?:graz(es|ed)?|glanc(es|ed)?|clips?|catches?).{0,20}(?:the\\s+)?(cup|groin)",
    "\\bbumps?\\b.{0,20}(?:his|her|the)\\s+(cup|groin)",
    # victim phrasing – “takes one to the groin”
    "\\btakes?\\s+(?:one\\s+)?to\\s+the\\s+groin\\b"
  )
  # add to groin_patterns
  groin_patterns <- c(
    groin_patterns,
    # generic mentions of low blow, regardless of syntax
    "\\blow\\s+blow(s)?\\b",            
    "\\blow\\s+blow\\b",               
    "\\ba\\s+low\\s+blow\\b",           
    "low\\s+blow\\s+lands",             
    "low\\s+blow\\b"                    
  )
  
  # Foul Dictionaries: Eye Poke / Gouge Patterns:
  
  eye_patterns <- c(
    "\\beye poke\\b|\\beyepoke\\b|\\beye gouge\\b|\\beye goug(s|ing)?\\b",
    "\\beye\\s+poke(s)?\\b",
    "pokes?\\s+(?:his|her|their)?\\s*(?:opponent|him|her)?\\s*in\\s+the\\s+eye",
    "swipes?\\s+the\\s+eyeball",
    "(?:push(?:es|ing)?\\s+off).{0,60}(?:poke(s|d|ing)?).{0,60}(eye|eyeball|eye socket|eyes)",
    "finger(s)?.{0,40}(?:to|into|in|slides?|scrapes?|rakes?|grazes?|jams?|jammed|digs?|dug|presses?|swipes?|swatted|swats?).{0,25}(?:the )?(eye|eyes|eye socket|eye sockets)",
    "\\bpoke.{0,20}eye\\b",
    "\\bthumb.{0,25}(?:in|to) (?:the )?eye\\b",
    "\\b(tells?|warns?|advises?)\\s+(?:him|her|\\w+)\\s+to\\s+watch\\s+(?:his|her)\\s+fingers\\b",
    "\\bpoked\\s+in\\s+the\\s+eye(s)?\\b",
    "\\beye scrape\\b",
    "(slides?|scrapes?|rakes?|grazes?|jabs?|jabbed|jabbing|stabs?|stabbing|jams?|jammed|jamming|swipes?|swatted|swats?).{0,60}(?:into|in|to).{0,20}(?:the\\s+)?(eye|eyes|eye socket|eye sockets)"
  )
  
  eye_patterns <- c(
    eye_patterns,
    # eye poke described without saying "eye"
    "push(?:es|ed|ing)?\\s+off\\s+\\w+(?:['’]\\w+)?\\s+face\\s+and\\s+pok(?:e|es|ed|ing)\\s+\\w+"
  )

  # Foul Dictionaries: Back-of-head patterns (allow "back of NAME's head")
  
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

  
  # Master Foul Regex: union of all foul patterns (used for inital detection)

  foul_patterns <- c(
    groin_patterns,
    eye_patterns,
    "head ?butt",
    "back of the head",
    "(?:fence|cage) grab",
    "grab(?:bing|s)? the (?:fence|cage)",
    "(?:pulls?|grabs?|holds?|clutches?) on the (?:fence|cage)",
    "rolls? to grab the fence",
    "(?:elbow|strike).{0,30}crown of the head",
    "crown of the head.{0,40}illegal",
    "\\billegal strike\\b",
    "\\bgrabs?\\s+the\\s+fence\\b",
    "\\bgrabs?\\s+fence\\b",
    "finger(s)?.{0,40}(?:in|hooked in|stuck in|out of).{0,10}(?:the\\s+)?(?:fence|cage|chain links?)",
    "toes?.{0,30}(?:in|into|on|hooked in).{0,20}(?:the\\s+)?(?:fence|cage|chain links?)",
    "toes?.{0,20}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
    "hooks?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
    "pulls?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|inside)\\s+(?:the\\s+)?(?:fence|cage|chain links?)",
    "toes?.{0,40}(?:which\\s+too\\s+is\\s+a\\s+foul|is\\s+also\\s+a\\s+foul|is\\s+a\\s+foul)",
    "uses?\\s+(?:his|her|their)\\s+toes?.{0,40}which\\s+too\\s+is\\s+a\\s+foul",
    "hooks?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|on)\\s+(?:the\\s+)?(?:fence|fencing|cage|chain links?)",
    "tugs?\\s+(?:his|her|their)\\s+toes?.{0,30}(?:in|into|on)\\s+(?:the\\s+)?(?:fence|fencing|cage|chain links?)",
    "toes?.{0,40}(?:out\\s+of|in|into|on)\\s+(?:the\\s+)?(?:fence|fencing|cage|chain links?)",
    "(?:pulling|tugging)\\s+on\\s+(?:the\\s+)?(?:fence|fencing|cage|chain links?)",
    back_head_patterns,
    "(?:tugs?|pulls?|grabs?|holds?).{0,30}(?:the\\s+)?chain links?",
    "rabbit punch(?:es)?",
    "knee to (?:a )?grounded opponent|illegal knee",
    "illegal upkick",
    "(?:lands?|connects?|scores?|nails?|drills?|kicks?)\\s+an?\\s+upkick\\b.{0,50}\\b(downed|grounded)\\b",
    "illegal (?:kick|upkick) to (?:a )?grounded opponent"
  )
  foul_re    <- regex(paste(foul_patterns, collapse = "|"), ignore_case = TRUE)
  eye_any_re <- regex(paste(eye_patterns,  collapse = "|"), ignore_case = TRUE)
  
  # Pause/ Referee Stoppage Patterns (time call, doctor check, restart cues)
  
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
  
  
 # False-Positive Blockers (hypothetical language, legal belt line, warnings, injuries, negations)
  
  hypothetical_re <- regex(
    paste(
      c(
        "\\bwhat\\s+would\\s+be\\b",      
        "\\bwould\\s+be\\b",
        "\\bwould\\s+have\\s+been\\b",
        "\\bcould\\s+be\\b",
        "\\bmight\\s+be\\b",
        "\\bpotentially\\b",
        "\\bif\\s+.*\\b(?:illegal\\s+upkick|upkick)\\b"  # optional extra
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  )
  
  non_foul_legal_belt_re <- regex(
    paste(
      c(
        "\\bit was on the belt\\b",
        "\\bon the belt\\b",
        "\\blow but legal\\b",
        "\\blegal shot\\b",
        "\\bno foul\\b",
        "\\bnot a foul\\b",
        "\\bwasn'?t a foul\\b",
        "\\bkeep fighting\\b",
        "\\bcontinue\\b",
        "\\bfight on\\b",
        "\\bwaves? it off\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  )
  
  # referee instruction / warning about eye pokes (NOT an actual foul)
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

  assign(paste0("non_foul_backhead_re_dbg__", event_name), non_foul_backhead_re, envir = .GlobalEnv)
  


  
  non_foul_eye_re <- regex(
    paste(
      "targeting\\s+the\\s+(?:right|left)\\s+eye\\s+of\\s+\\w+\\s+that\\s+is\\s+already\\s+damaged",
      "targeting\\s+the\\s+(?:right|left)\\s+eye\\s+that\\s+is\\s+already\\s+damaged",
      "continues?\\s+to\\s+target\\s+the\\s+(?:right|left)\\s+eye",
      "working\\s+over\\s+the\\s+(?:right|left)\\s+eye",
      "(?:swollen|badly\\s+swollen|already\\s+swollen)\\s+(?:right|left)\\s+eye",
      "cut\\s+(?:over|under)\\s+the\\s+(?:right|left)\\s+eye",
      "swelling\\s+(?:above|around|under|beneath)\\s+\\w+['’`]?s?\\s+(?:left\\s+|right\\s+)?eye",
      "developing\\s+some\\s+swelling\\s+(?:above|around|under|beneath)\\s+\\w+['’`]?s?\\s+(?:left\\s+|right\\s+)?eye",
      "\\bjabs?\\s+(?:\\S+\\s+){0,4}in\\s+the\\s+(?:eye|eyeball|eye\\s*socket)\\b",
      "\\bpunch(?:es|ed|ing)?\\s+(?:\\S+\\s+){0,4}in\\s+the\\s+(?:eye|eyeball|eye\\s*socket)\\b",
      "\\bstrikes?\\s+(?:\\S+\\s+){0,4}in\\s+the\\s+(?:eye|eyeball|eye\\s*socket)\\b",
      "\\bhits?\\s+(?:\\S+\\s+){0,4}in\\s+the\\s+(?:eye|eyeball|eye\\s*socket)\\b",
      "\\bconnects?\\s+(?:\\S+\\s+){0,4}in\\s+the\\s+(?:eye|eyeball|eye\\s*socket)\\b",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  non_foul_eye_strike_re <- regex(
    paste(
      c(
        # explicit closed-fist strikes
        "\\bjabs?\\s+\\w+\\s+in\\s+the\\s+eye\\b",
        "\\bpunch(?:es|ed|ing)?\\s+\\w+\\s+in\\s+the\\s+eye\\b",
        "\\bstrikes?\\s+\\w+\\s+in\\s+the\\s+eye\\b",
        "\\bconnects?\\s+\\w+\\s+in\\s+the\\s+eye\\b",
        "\\bhits?\\s+\\w+\\s+in\\s+the\\s+eye\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  )

  assign(paste0("non_foul_eye_re_dbg__", event_name), non_foul_eye_re, envir = .GlobalEnv)

  non_foul_toes_fence_re <- regex(
    paste(
      c(
        "\\bnot\\s+hook(?:ing|ed)?\\s+(?:his|her|their)?\\s*toes?\\s+(?:in|into|on)\\s+the\\s+(?:fence|fencing|cage|chain\\s+links?)\\b",
        "\\bwithout\\s+hook(?:ing|ed)?\\s+(?:his|her|their)?\\s*toes?\\s+(?:in|into|on)\\s+the\\s+(?:fence|fencing|cage|chain\\s+links?)\\b",
        "\\bnot\\s+put(?:ting|s|t)?\\s+(?:his|her|their)?\\s*(?:feet|foot|toes?)\\s+on\\s+the\\s+(?:fence|fencing|cage)\\b"
      ),
      collapse = "|"
    ),
    ignore_case = TRUE
  )
  
  assign(paste0("non_foul_toes_fence_re_dbg__", event_name), non_foul_toes_fence_re, envir = .GlobalEnv)
  
  uncalled_foul_re <- regex(
    "(?:an\\s+)?uncalled\\s+foul\\s+(?:is|was)?\\s*(?:apolog(?:y|ised|ized|izing)|acknowledged|admitted)",
    ignore_case = TRUE
  )
  
  assign(paste0("uncalled_foul_re_dbg__", event_name), uncalled_foul_re, envir = .GlobalEnv)
  
  no_pause_re <- regex(paste(
    "waves? (?:off|it off|the ref off)",
    "ignores? it",
    "(?:but|and)\\s+(?:they\\s+)?(?:keep|continue)(?:s)?\\s+(?:going|fighting)",
    "lets? them fight",
    sep = "|"
  ), ignore_case = TRUE)
  
  negation_re <- regex(paste("\\b(?:no|not|wasn'?t|weren'?t|barely|almost|nearly)\\b.{0,30}\\b(?:foul|groin|eye poke|headbutt)\\b", "clutching (?:his|her)? groin|adjust(?:s|ing)? (?:his|her)? cup", sep="|"), ignore_case = TRUE)
  
  reaction_only_patterns <- c(
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

# Sentence Splitting + Text Normalisation (HTML breaks, quote mojibake fixes)  
  
  to_sentences <- function(x){
    x %>%
      str_replace_all("<br>", " ") %>%
      
      str_replace_all("\u00E2\u0080[\u0098\u0099]", "'") %>%   # ‘ and ’
      str_replace_all("\u00E2\u0080[\u009C\u009D]", "\"") %>%  # “ and ”
      
      str_replace_all("â€™|â€˜", "'") %>%          
      str_replace_all("â€œ|â€\u009d", "\"") %>%
      
      str_replace_all("[\u2018\u2019`']", "'") %>% 
      str_replace_all("[\u201C\u201D]", "\"") %>%
      
      str_squish() %>%
      tokenizers::tokenize_sentences(strip_punct = FALSE) %>%
      .[[1]]
  }

# Metadata Extraction Regexes: time, round, bout, result, referee mention, fight-start cue
  
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
  
  ready_re <- regex(
    "\\bready\\s+for\\s+(?:combat|action)\\b",
    ignore_case = TRUE
  )
  
  
# Build Sentence-level Dataset: one row per sentence with foul flags, pause flags, and fight-start gating
  
  
  sent_all <- pbp_tbl %>%
    # 1) split each fight text into sentences
    mutate(sentence = map(text, to_sentences)) %>%
    select(bout, sentence) %>%
    unnest(sentence) %>%              
    group_by(bout) %>%
    mutate(
      sid          = row_number(),
      is_upkick_mention = str_detect(sentence, regex("illegal upkick|illegal upkicks?|\\bupkick(s)?\\b", ignore_case = TRUE)),
      is_hypothetical   = str_detect(sentence, hypothetical_re),
      is_groin_candidate = str_detect(sentence, regex("groin|cup|low\\s+blow", ignore_case = TRUE)),
      # Raw foul detection
      has_foul_raw = (
        str_detect(sentence, foul_re) |
          str_detect(sentence, uncalled_foul_re)
      ) &
        !str_detect(sentence, non_foul_backhead_re) &
        !str_detect(sentence, non_foul_eye_re) &
        !(is_upkick_mention & is_hypothetical) &
        !str_detect(sentence, non_foul_eye_warning_re) &
        !str_detect(sentence, non_foul_toes_fence_re) &
        !(is_groin_candidate & str_detect(sentence, non_foul_legal_belt_re)),
      is_reaction_only = str_detect(sentence, reaction_only_re),
      time_str      = str_extract(sentence, time_re),
      round_str_raw = str_extract(sentence, round_re),
      # Has the ref been mentioned yet?
      ref_mentioned = str_detect(sentence, ref_re),
      ready_mentioned = str_detect(sentence, ready_re),
      
      # Fight only “starts” after ref OR the first visible clock
      fight_started = cumany(!is.na(time_str) | ref_mentioned | ready_mentioned),
      
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
  
 # Debug exports (regex objects)
  
  assign(paste0("foul_re_dbg__", event_name), foul_re, envir = .GlobalEnv)
  assign(paste0("hypothetical_re_dbg__", event_name), hypothetical_re, envir = .GlobalEnv)
  assign(paste0("non_foul_eye_warning_re_dbg__", event_name), non_foul_eye_warning_re, envir = .GlobalEnv)
  assign(paste0("non_foul_legal_belt_re_dbg__", event_name), non_foul_legal_belt_re, envir = .GlobalEnv)
  
  
  
# Incident Creation: each foul sentence becomes its own incident (run_id): This was primarily used when the model was strictly regex based. The hybrid model implementation assigns a foul confidence for each sentence.
  
  sent_runs <- sent_all %>%
    filter(has_foul & !is_result) %>%
    group_by(bout) %>%
    mutate(
      run_id           = row_number(),
      incident_has_eye = str_detect(sentence, eye_any_re),
      incident_all_neg = is_neg
    ) %>%
    ungroup()
  
  # Remove negated "no foul" incidents (except eye poke cases)
  sent_runs_clean <- sent_runs %>%
    filter(!(incident_all_neg & !incident_has_eye))
  
  
  # Compute Incident Bounds (sid window) for Pause / Deduction Lookup
  
  run_bounds <- sent_runs_clean %>%
    group_by(bout, run_id) %>%
    summarise(min_sid = min(sid), max_sid = max(sid), has_explicit_no_pause = any(explicit_no_pause), .groups = "drop")
  
  pause_window_k <- 8

  # Identify whether referee paused within a forward window after each incident
    
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
  
  
  # Map Point Deductions to nearest prior incident within a forward window
  
  deduct_sents <- sent_all %>% filter(is_deduction_raw) %>% select(bout, sid_d = sid)
  deduction_window_k <- 12
  
  if (nrow(deduct_sents) > 0) {
    cand <- run_bounds %>% inner_join(deduct_sents, by = "bout") %>% mutate(forward_dist = sid_d - max_sid) %>% filter(forward_dist >= 0, forward_dist <= deduction_window_k)
    nearest <- cand %>% group_by(bout, sid_d) %>% slice_min(order_by = forward_dist, n = 1, with_ties = FALSE) %>% ungroup() %>% mutate(foul_resulted_in_deduction = TRUE) %>% select(bout, run_id, foul_resulted_in_deduction)
    deduction_map <- nearest %>% distinct(bout, run_id, foul_resulted_in_deduction)
  } else {
    deduction_map <- tibble(bout = character(), run_id = integer(), foul_resulted_in_deduction = logical())
  }
  
  
  
  # Foul-Type Classification Helpers: eye categorisation, strong anchors
  
  eye_categorise_re <- regex(
    paste(
      "\\beye poke\\b|\\beyepoke\\b",
      "finger(s)?.{0,30}(?:to|into|in|slides?|scrapes?|rakes?|grazes?|jams?|jammed|digs?|dug|presses?).{0,15}(?:the )?(eye|eyes|eye socket|eye sockets)",
      "\\bpoke.{0,15}eye\\b",
      "push(?:es|ed|ing)?\\s+off\\s+\\w+(?:['’]\\w+)?\\s+face\\s+and\\s+pok(?:e|es|ed|ing)\\s+\\w+",
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
        back_head_patterns,                 
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
        # grounded-opponent stuff
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
  neg_injury_eye_re <- regex(
    "cut|laceration|swelling|hematoma|bruise|bleeding|blood|closed\\s+up|black\\s+eye|above\\s+.*eye",
    ignore_case = TRUE
  )
  
  
  
  # Rule-Based Confidence Scoring - Crucial for Hybrid Model: sentence-level features + rule_conf
  
  # helper: clamp
  clamp01 <- function(x) pmax(0, pmin(1, x))
  
  # helper: sigmoid (optional)
  sigmoid <- function(z) 1 / (1 + exp(-z))
  
  # Compute rule_score, rule_conf, and positive/negative reason flags per sentence:
  
  sent_all <- sent_all %>%
    mutate(
      pos_strong_anchor = str_detect(sentence, strong_foul_anchor_re),
      pos_eye_clean     = str_detect(sentence, eye_categorise_re),
      pos_pause         = pause_token & !non_foul_pz,
      pos_deduction     = is_deduction_raw,
      pos_foul_re_hit = str_detect(sentence, foul_re),
      
      neg_block_backhead = str_detect(sentence, non_foul_backhead_re),
      neg_block_eye      = str_detect(sentence, non_foul_eye_re),
      neg_block_upkick_hypo = is_upkick_mention & is_hypothetical,
      neg_block_eye_warning = str_detect(sentence, non_foul_eye_warning_re),
      neg_block_toes_fence  = str_detect(sentence, non_foul_toes_fence_re),
      neg_block_legal_belt  = is_groin_candidate & str_detect(sentence, non_foul_legal_belt_re),
      
      # Alignment with has_foul (final gate) 
      neg_gate_reaction = is_reaction_only,
      neg_gate_not_started = !fight_started,
      pos_foul_language = str_detect(sentence, regex(
        paste0(
          "eye\\s*pok|eyepok|poke(d)?\\s+.*eye|finger(s)?\\s+.*eye|thumb\\s+.*eye|eye\\s*socket|eyeball|",
          "low\\s*blow|below\\s+the\\s+belt|groin|cup(\\b|\\s)|(knee|kick|shot)\\s+.*(groin|cup)|",
          "fence\\s*grab|cage\\s*grab|grab(s|bed|bing)?\\s+the\\s+(fence|cage)|",
          "hook(s|ed|ing)?\\s+.*(fence|cage)|",
          "toes?\\s+in\\s+the\\s+(fence|cage)|",
          "fingers?\\s+in\\s+the\\s+(fence|cage)|",
          "pulls?\\s+on\\s+the\\s+(fence|cage)|",
          "pulls?\\s+the\\s+(fence|cage)|",
          "chain\\s*links?|",
          "rabbit\\s*punch|back\\s+of\\s+(the\\s+)?head|behind\\s+the\\s+ear|",
          "illegal\\s+knee|knee\\s+to\\s+(a\\s+)?grounded|",
          "illegal\\s+upkick|upkick\\b.*(grounded|downed)|",
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
      pos_uncalled_foul = str_detect(sentence, uncalled_foul_re),
      
      # Negatives
      neg_hypo_upkick   = is_upkick_mention & is_hypothetical,
      neg_nonfoul_eye   = str_detect(sentence, non_foul_eye_re),
      neg_eye_warning   = str_detect(sentence, non_foul_eye_warning_re),
      neg_nonfoul_back  = str_detect(sentence, non_foul_backhead_re),
      neg_toes_negated  = str_detect(sentence, non_foul_toes_fence_re),
      neg_legal_belt    = is_groin_candidate & str_detect(sentence, non_foul_legal_belt_re),
      neg_no_pause      = explicit_no_pause,
      neg_negation      = is_neg,
      neg_injury_eye = str_detect(sentence, neg_injury_eye_re),
      neg_reaction_only = is_reaction_only,
      
      rule_score_raw =
        # Core detection: mirror has_foul_raw
        100 * as.integer(pos_uncalled_foul) +
        80 * as.integer(pos_foul_re_hit) -
        
        # Core exclusions: mirror has_foul_raw blocks
        80 * as.integer(neg_block_backhead) -
        80 * as.integer(neg_block_eye) -
        60 * as.integer(neg_block_upkick_hypo) -
        60 * as.integer(neg_block_eye_warning) -
        60 * as.integer(neg_block_toes_fence) -
        60 * as.integer(neg_block_legal_belt) -
        
        # Final gates: mirror has_foul gates
        40 * as.integer(neg_gate_reaction) -
        40 * as.integer(neg_gate_not_started) +
        
        # Extra evidence 
        20 * as.integer(pos_strong_anchor) +
        10 * as.integer(pos_foul_language) +
        15 * as.integer(pos_eye_clean) +
        15 * as.integer(pos_pause_foul) +
        20 * as.integer(pos_deduction) +
        10 * as.integer(pos_ref_foul) -
        
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
        if_else(pos_uncalled_foul, "explicit_uncalled_foul_acknowledged|", ""),
        if_else(pos_eye_clean,     "eye_clean_phrase|", ""),
        if_else(pos_pause_foul,    "pause_plus_foul_language|", ""),
        if_else(pos_deduction,     "point_deduction|", ""),
        if_else(pos_foul_re_hit, "foul_re_hit|", ""),
        if_else(pos_ref_foul,      "ref_plus_foul_language|", "")
      ), "\\|$", "")),
      
      rule_reasons_neg = str_squish(str_replace_all(paste0(
        if_else(neg_nonfoul_eye,     "nonfoul_eye_context|", ""),
        if_else(neg_eye_warning,     "ref_eye_warning_only|", ""),
        if_else(neg_nonfoul_back,    "nonfoul_backhead_clinch|", ""),
        if_else(neg_toes_negated,    "toes_negated|", ""),
        if_else(neg_legal_belt,      "legal_belt|", ""),
        if_else(neg_hypo_upkick,     "hypothetical_upkick|", ""),
        if_else(neg_no_pause,        "explicit_no_pause|", ""),
        if_else(neg_negation,        "negation|", ""),
        if_else(neg_injury_eye, "injury_eye_context|", ""),
        if_else(neg_reaction_only,   "reaction_only|", ""),
        if_else(neg_block_backhead, "blocked_backhead|", ""),
        if_else(neg_block_eye, "blocked_eye|", ""),
        if_else(neg_block_upkick_hypo, "blocked_upkick_hypo|", ""),
        if_else(neg_block_eye_warning, "blocked_eye_warning|", ""),
        if_else(neg_block_toes_fence, "blocked_toes_fence|", ""),
        if_else(neg_block_legal_belt, "blocked_legal_belt|", ""),
        if_else(neg_gate_reaction, "gate_reaction_only|", ""),
        if_else(neg_gate_not_started, "gate_not_started|", ""),
        if_else(neg_injury_stoppage, "injury_stoppage|", "")
      ), "\\|$", ""))
    )
  
 # Extracts all sentences from each html file: Crucial for ML and implementation of Rule Confidences
  
  write.csv(
    sent_all,
    file = file.path(paths$sentences, paste0("sent_all_dbg__", event_name, ".csv")),
    row.names = FALSE
  )
  
  
  
  # Aggregate sentences to incident-level context first
  incident_context <- sent_runs_clean %>%
    group_by(bout, run_id) %>%
    summarise(
      context         = paste(sentence, collapse = " "),
      time_mentioned  = na.omit(unique(time_str)) |> paste(collapse = ", "),
      round_mentioned = na.omit(unique(round_mentioned)) |> paste(collapse = ", "),
      .groups = "drop"
    )
  
  # Build incident-level foul table (context, foul_type_guess, pause/deduction flags)
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
  !is.na(foul_raw_match) | str_detect(context, uncalled_foul_re),
  str_detect(context, strong_foul_anchor_re) | str_detect(context, uncalled_foul_re)
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
    group_by(bout, context) %>%
    slice(1) %>%
    ungroup()
  
  # Parse Fighter names from bout title + build bout_key
  
  extract_fighters <- function(bout) {
    parts <- str_split(bout, " vs\\. ", n = 2)[[1]]
    if (length(parts) < 2) return(tibble(fighter1 = NA_character_, fighter2 = NA_character_))
    f1 <- str_trim(str_remove(parts[1], "\\s*\\(.*$"))
    f2 <- str_trim(str_remove(parts[2], "\\s*\\(.*$"))
    tibble(fighter1 = f1, fighter2 = f2)
  }
  
  last_name_clean <- function(x) {
    x %>%
      stringr::str_to_lower() %>%
      stringr::str_remove("\\(.*\\)$") %>%          
      stringr::str_replace_all("-", " ") %>%        
      stringr::str_replace_all("[^a-z ]", " ") %>%  
      stringr::str_squish() %>%
      stringr::str_extract("[^ ]+$")                
  }
  
  fighters_tbl <- pbp_tbl %>%
    mutate(tmp = map(bout, extract_fighters)) %>%
    unnest(tmp) %>%
    mutate(
      bout_key = paste(last_name_clean(fighter1), "vs", last_name_clean(fighter2))
      )
    
  
  # Fouler Detection (v5): infer which fighter committed the foul from context text
  
  detect_fouler_v5 <- function(context, fighter1, fighter2, bout = NA) {
    # make inputs safe (length-1, character) 
    fighter1 <- if (length(fighter1) >= 1) as.character(fighter1[[1]]) else NA_character_
    fighter2 <- if (length(fighter2) >= 1) as.character(fighter2[[1]]) else NA_character_
    context  <- if (length(context)  >= 1) as.character(context[[1]])  else NA_character_
    
    # safe guard: Never returns NA inside if() ---
    if (
      isTRUE(is.na(fighter1)) || isTRUE(is.na(fighter2)) || isTRUE(is.na(context)) ||
      !nzchar(fighter1) || !nzchar(fighter2) || !nzchar(context)
    ) {
      return("UNKNOWN")
    }
    
    f1_last <- stringr::str_to_lower(stringr::str_extract(fighter1, "[^ ]+$"))
    f2_last <- stringr::str_to_lower(stringr::str_extract(fighter2, "[^ ]+$"))
    
    txt <- context %>%
      str_replace_all("<br>", " ") %>%
      str_replace_all("â€™|â€˜|â|´|`|’|‘", "'") %>%
      str_replace_all("â€œ|â€\u009d|“|”", "\"") %>%
      str_squish() %>%
      str_to_lower()
    
    has <- function(p) stringr::str_detect(txt, stringr::regex(p, ignore_case = TRUE))
    

    # SPECIAL CASE PATCHES FOR KNOWN TRICKY FOULS Special Case Patches for Known Tricky Fouls
    
    # Helper: last name first/last positions 
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
    
    # PATCH A: "NAME gets kicked in the groin" 
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
    
    # PATCH B: "… and she is warned for grabbing the fence" 
    if (stringr::str_detect(
      txt,
      stringr::regex("she\\s+is\\s+warned\\s+for\\s+grabbing\\s+the\\s+(fence|cage|chain\\s+links?)",
                     ignore_case = TRUE)
    )) {
      
      # In the example sentence the one *acting* is the first-named fighter.
      f1_first <- first_pos(f1_last)
      f2_first <- first_pos(f2_last)
      
      if (!is.na(f1_first) && (is.na(f2_first) || f1_first < f2_first)) return(fighter1)
      if (!is.na(f2_first) && (is.na(f1_first) || f2_first < f1_first)) return(fighter2)
    }
    
    # PATCH C: "Tognoni warns the American ..." very specific fix for a persistent problem
    if (stringr::str_detect(
      txt,
      stringr::regex("warns\\s+the\\s+american\\s+for\\s+striking\\s+the\\s+back\\s+of\\s+the\\s+head",
                     ignore_case = TRUE)
    )) {
      return(fighter1)
    }
    
    # PATCH D: "flattens Allen ... bludgeoning him ... back of the head" 
    if (stringr::str_detect(
      txt,
      stringr::regex("flattens\\s+.*bludgeoning\\s+him\\s+with\\s+punches\\s+to\\s+the\\s+side\\s+and\\s+back\\s+of\\s+the\\s+head",
                     ignore_case = TRUE)
    )) {
      
      # For this pattern, the *actor* is the first name in the sentence
      f1_first <- first_pos(f1_last)
      f2_first <- first_pos(f2_last)
      
      if (!is.na(f1_first) && (is.na(f2_first) || f1_first < f2_first)) return(fighter1)
      if (!is.na(f2_first) && (is.na(f1_first) || f2_first < f1_first)) return(fighter2)
    }
    
    # helper: possessive name with broken quotes 
    
    name_possessive <- function(name) {
      paste0(
        "\\b", name,
        "(?:['’]|[^[:alpha:]]){0,3}s\\s+"
      )
    }
    
    # helper: both_eyes_pattern, used in HARD RULE 2 and later
    both_eyes_pattern <- function(actor, victim) {
      paste0(
        name_possessive(actor),
        "fingers?\\b.{0,160}",
        "jam(s|med|ming)?\\s+into\\s+both\\s+of\\s+",
        victim,
        "(?:['’]|[^[:alpha:]]){0,3}s?\\s+eyes?"
      )
    }
    

    
    
    # HARD RULE 0: "NAME's kick ... bumps into the cup of his opponent"
    
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
    

    # HARD RULE 1: generic "cup of his/her opponent"
    
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
    

    # HARD RULE 2: "NAME's fingers ... jam into both of OTHERNAME's eyes"
    
    if (has(both_eyes_pattern(f1_last, f2_last)) && !has(both_eyes_pattern(f2_last, f1_last))) {
      return(fighter1)
    }
    if (has(both_eyes_pattern(f2_last, f1_last)) && !has(both_eyes_pattern(f1_last, f2_last))) {
      return(fighter2)
    }
    
    
    # HARD RULE 3: generic "jam into both of ... eyes" with nearest-name victim
    # victim = closest surname before "eyes", fouler = other fighter

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
    

    ## HARD RULE 4: "X controls Y ... and he is warned for grabbing the fence"

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
    

    
    # FALLBACK if none of the above early rules decided:
    
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
    

    
    # "for NAME to not grab the fence / cage / chain links"
    
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
    
    # "X is warned for ..." (no explicit ref)
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
    
    # "... is warned for ..." 
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
    # explicit actor → foul anchor 
    
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
    
    
    # explicit low-blow victim wording 
    
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
    
    # "NAME gets kicked in the groin" = NAME is victim 
    vkg1 <- has(victim_gets_kicked_groin(f1_last))
    vkg2 <- has(victim_gets_kicked_groin(f2_last))
    
    if (vkg1 && !vkg2) return(fighter2)  # fighter1 = victim → other is fouler
    if (vkg2 && !vkg1) return(fighter1)  # fighter2 = victim → other is fouler

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
    
    # all low-blow victim patterns bundled
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
    
    # simple "NAME ... kick/knee ... to the groin" 
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
    # If it matches both, defer to later rules
    
    if (hit_low_1 && !hit_low_2) return(fighter1)
    if (hit_low_2 && !hit_low_1) return(fighter2)
    verbs_attack <- "(kick(s|ing|ed)?|knee(s|ing|ed)?|strike(s|ing)?|hit(s|ting)?|drill(s|ing|ed)?|land(s|ing)?|shoot(s|ing)?|connect(s|ing)?|blast(s|ing|ed)?|smash(es|ing|ed)?|smack(s|ing)?|brush(es|ing)?|ricochet(s|ing)?|crash(es|ing)?|bang(s|ing)?|poke(s|ing)?|scrape(s|ing)?|rake(s|ing)?|graze(s|ing)?|push(es|ing)?|pull(s|ing)?|grab(s|ing|bed)?|roll(s|ing)?|flatten(s|ing)?|punch(es|ing)?|hack(s|ed|ing)?|bludgeon(s|ing|ed)?|headbutt(s|ing|ed)?|club(s|bed|bing)?|delivers?|stop(s|ed|ping)?|pounds?)"
    
    # Eye-Poke Victim:
    
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
    
    # If one fighter is clearly complaining to the ref about a foul,
    # treat that fighter as the VICTIM and the OTHER fighter is the fouler.
    if (c1 && !c2) return(fighter2)
    if (c2 && !c1) return(fighter1)
    
    # "NAME is admonished for ..."
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
    
    # extra victim pattern: clutching/adjusting groin/cup ----------
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
    

    # Check this FIRST before any scoring - if someone is clearly smacked/hit, they're victim
    
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
            # look backwards for last named fighter 
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
            
            # look forwards for resolution ("so she stops")
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
            
            # if exactly one fighter is clearly the victim after the warning, treat the OTHER fighter as the fouler
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
            
            # if exactly one fighter is clearly the victim before the warning, treat the OTHER fighter as fouler
            if (vic1_before && !vic2_before) {
              return(fighter2)
            }
            if (vic2_before && !vic1_before) {
              return(fighter1)
            }
           
            if (!is.na(active_subject_before) && !is.na(resolution_match)) {
              
              # know who was acting AND see them stop = that actor is the fouler
              return(active_subject_before)
            }
            
            if (!is.na(active_subject_before)) {
              # at least know who was acting immediately before the warning
              return(active_subject_before)
            }
            
            if (is.na(active_subject_before) && !is.na(resolution_match)) {
              # ambiguous subject before, but ref warns and "he/she stops" – fall back to fighter1
              return(fighter1)
            }
          }
        }
      }
    }
    # Special Case: "is admonished for eye gouging" – assign fouler by nearest name before phrase
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
  
    # Rule 1–3: scoring patterns
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
    
    # Rule 4: Subject Tracking
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
        region_after_last <- stringr::str_sub(
          txt_before_foul,
          last_fighter_pos,
          nchar(txt_before_foul)
        )
        
        # figure out the last fighter's last name
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
        
        # If the same fighter is the last one mentioned before the foul and there is at least one attack verb in that span, treat that fighter as the fouler.
        if (has_action_verbs) {
          return(last_fighter)
        }
      }
    }
    

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
  
  # Build Expanded Context (±2 sentences around each incident) for improved fouler detection:
  
  expanded_contexts <- sent_runs_clean %>%
    group_by(bout, run_id) %>%
    mutate(min_sid_incident = min(sid), max_sid_incident = max(sid)) %>%
    ungroup() %>%
    distinct(bout, run_id, min_sid_incident, max_sid_incident) %>%
    left_join(sent_all %>% select(bout, sid, sentence), by = "bout", relationship = "many-to-many") %>%
    filter(sid >= (min_sid_incident - 2) & sid <= (max_sid_incident + 2)) %>%
    group_by(bout, run_id) %>%
    summarise(expanded_context = paste(sentence, collapse = " "), .groups = "drop")
  
 # Assemble final foul-level output table:
  
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
  # Export sentence-level confidence table (sentence_conf<event>.csv)
  sent_out <- sent_all %>%
    select(
      bout, sid, sentence,
      has_foul_raw, has_foul,
      rule_score, rule_conf,
      rule_reasons_pos, rule_reasons_neg,
      time_str, round_mentioned
    ) %>%
    mutate(event = event_name)
  
  # Write sentence_conf CSV for downstream hybrid / diagnostics scripts

  write.csv(
    sent_out,
    file = file.path(paths$confidence_build, paste0("sentence_conf_", event_name, ".csv")),
    row.names = FALSE
  )
  fouls_all
}



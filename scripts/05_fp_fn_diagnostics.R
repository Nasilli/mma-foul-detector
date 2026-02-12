# 05_fp_fn_diagnostics

source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))

# Diagnostics to identify and solve issues with model output mismatching with gold standard table - accuracy needed to build the ML training dataset which needs the contexts from the Model and the 100% accurcate fouler information in the Gold Standard Table

# Simple regex dictionary to label likely foul category triggers (diagnostics only)
# NOTE: labels are coarse; "eye" is intentionally broad for triage
foul_dict <- tibble::tribble(
  ~label,                ~pattern,
  "groin strike",        "low blow|groin|\\bcup\\b|below the belt",
  "eye poke",            "eye poke|poke.*eye|finger.*eye|thumb.*eye|\\beye\\b",
  "fence grab",          "(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?).{0,25}\\b(fence|cage)\\b|\\b(fence|cage)\\b.{0,25}(grab(bed|s|bing)?|hold(ing)?|clutch(ing)?|hook(ing)?|pull(ing)?)",
  "back of head strike", "back of (the )?head|rabbit punch|behind the ear|to the back of the head"
)

# What Model Fouls Don't Exist in Gold? False Positives: model incidents with no matching event, bout_key, foul idx
model_unmatched <- model_v2_clean %>%
  anti_join(
    gold_clean_2 %>% select(event, bout_key, foul_idx),
    by = c("event", "bout_key", "foul_idx")
  )

readr::write_csv(model_unmatched, file.path(paths$interim, "model_unmatched_fouls.csv"))

# What Gold Fouls Don't Exist in the Model? False Negatives: gold incidents with no matching event, bout_key, foul idx
gold_unmatched <- gold_clean_2 %>%
  anti_join(
    model_v2_clean %>% select(event, bout_key, foul_idx),
    by = c("event", "bout_key", "foul_idx")
  )


readr::write_csv(gold_unmatched,  file.path(paths$interim, "Model_False_Negatives.csv"))


# FP diagnostics: label each unmatched model foul with the likely regex family that fired
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

# Summarise most common FP trigger families
fp_diag %>% count(why_detected, sort = TRUE)

View(fp_diag)
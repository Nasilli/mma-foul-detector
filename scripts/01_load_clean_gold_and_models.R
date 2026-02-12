# 01_load_clean_gold_and_models.R
# Purpose: load model outputs + gold standard tables, standardise fields, and build incident-level labelled dataset for future modelling (fouler/type/pause/deduction). Not required for the current foul-detection hybrid.
# Gold standard tables are joined with the model output by event, bout and fight id. 
#
# Outputs: gold_clean, gold_clean_2, model_v2_clean (loaded), ml_df, ml_df2 (model_v0_clean was created in an earlier iteration of process_event but is only used for it's foul contexts to train the ML model.)



# Load libs + paths 
source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))


source(file.path(paths$scripts, "run_model_v2.R"))

normalize_bout <- function(x) {
  x %>% 
    str_to_lower() %>% 
    str_replace_all("[^a-z\\s]", "") %>%  # remove weights, punctuation
    str_squish()
}

# Load model output table: incident-level predictions + context (model_v0 created from previous iteration of process_event, the output is 100% accurate and is used to join the correctly formatted contexts to the gold standard table)
model_v0_all <- readr::read_csv(
  file.path(paths$output, "model_v0_all_events.csv"),
  show_col_types = FALSE
)

model_v0_clean <- model_v0_all %>% 
  select(event, bout, bout_key, foul_idx, everything())

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


model_v2_clean <- readr::read_csv(
  file.path(paths$output, "model_v2_clean.csv"),
  show_col_types = FALSE
)


# Load gold standard v1 - original manually made confirmed fouls list. Useful for training the ML model as we use this to identify confirmed fouls - not relying on rule based model's predictions.

gold <- read_csv(
  file = paths$gold1,
  name_repair = "minimal",
  skip = 1,
  col_names = TRUE,
  col_select = -1
)

# Clean/standardise gold v1 columns and create bout_key for joining to model outputs
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
    # bout_key: normalised bout string used for cross-table joining
    bout_key = bout %>%
      str_to_lower() %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish()
  )

# Load gold standard v2 (new events) - originally used to test the original iteration of the ML model for accuracy but now is joined with original training dataset to give the ML model more data to train from.
gold_v2 <- read_csv(
  file = paths$gold2,
  name_repair = "minimal",
  col_names = TRUE,
  col_select = -1
)

# Clean/standardise gold v2 
gold_clean_2 <- gold_v2 %>% 
  rename(
    event = Event,
    bout = Bout,
    fouler = Fouler,
    foul_type = Foul,
    fight_paused = `Pause?`,
    point_deduction = `Deduction?`,
    foul_idx = `Foul Index`
  ) %>% 
  mutate(
    fight_paused = str_to_lower(fight_paused) == "yes",
    point_deduction = str_to_lower(point_deduction) == "yes",
    bout_key = bout %>%
      str_to_lower() %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish() %>%
      str_split(" vs ") %>%
      map_chr(~ {
        if (length(.x) != 2) return(NA_character_)
        paste(
          word(.x[1], -1),  # last word of fighter 1
          "vs",
          word(.x[2], -1)   # last word of fighter 2
        )
      })
  )

# Build ML dataset from gold v1 by joining to model contexts (text = expanded_context)
# NOTE: this join expects model_v0_clean to exist (legacy). If not used, remove ml_df or switch to model_v2_clean.
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

# Build ML dataset from gold v1 by joining to model_v3_clean contexts
ml_df2 <- gold_clean_2 %>%
  inner_join(
    model_v2_clean %>%
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


# Result: ml_df and ml_df2 are ML-ready datasets with labels + text field
# Current state of the model has no use for ml_df and ml_df2 as these were built for hybrid in fouler, pause and deduction detection.
# This was originally made before I decided to take a step back and focus on implementing hybrid foul detection before anything else, so when I advance past foul detection, this script may come useful.
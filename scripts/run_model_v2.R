# DATA GENERATION SCRIPT FOR DOWNSTREAM USAGE: Outputs from this script are in the working directory folders and are imported into the project due to copyright reasons - I can't upload the raw html files to GitHub, so am extacting the neccessary output needed for the project

# Packages

library(dplyr)
library(purrr)
library(readr)
library(stringr)

# Load the rule-based extractor function (process_event_v2.R)
source(file.path("scripts", "00_setup.R"))
source(file.path("scripts", "Paths.R"))
source(file.path("scripts", "process_event_v2.R"))

# Input List of raw play-by-play HTML files to process

html_files2 <- file.path(
  paths$pbp_raw,
  c(
    "UFC323-dvalishvili-yan-06-12-2025.html",
    "UFCVegas-111-bonfim-brown-08-11-2025.html",
    "UFCVegas-110-garcia-onama-01-11-2025.html",
    "UFC-oliveira-gamrot-11-10-2025.html",
    "UFC-ulberg-reyes-27-09-2025.html",
    "UFC-lopes-silva-13-09-2025.html",
    "UFC-walker-zhang-23-08-2025.html",
    "UFC319-duplessis-chimaev-16-08-2025.html",
    "UFCVegas-109-dolidze-hernandez-09-08-2025.html",
    "UFCVegas-108-taira-park-02-08-2025.html",
    "UFC-whittaker-deridder-26-07-2025.html",
    "UFC318-holloway-poirier-19-07-2025.html"
  )
)

html_files <- file.path(
  paths$pbp_raw,
  c(
    "UFC-deridder-allen-18-10-2025.html",
    "UFC320-ankalaev-pereira-04-10-2025.html",
    "UFCvegas-106-burns-morales-17-05-2025.html",
    "UFCvegas-107-blanchfield-barber-31-05-2025.html",
    "UFC322-dellamaddalena-makhachev-15-11-2025.html",
    "UFC-lewis-teixeira-12-06-2025.html",
    "UFC-covington-buckley-14-12-2024.html",
    "UFCvegas-105-emmett-murphy-05-04-2025.html",
    "UFC-imavov-borralho-06-09-2025.html",
    "UFC321-aspinall-gane-25-10-2025.html",
    "UFC-tsarukyan-hooker-22-11-2025.html"
  )
)

# Run v0 HTMLs just to create sent_all_dbg__... + sentence_conf files needed downstream
stopifnot(all(file.exists(html_files)))
invisible(purrr::walk(html_files, process_event_v2))



# Run Extractor over all HTML files and combine results
# Output is foul-level (one-row per detected incident)
model_v2_all <- purrr::map_df(html_files2, process_event_v2)


# Standardise output columns (preserve bout_key for downstream joins)
model_v2_clean <- model_v2_all %>% 
  select(event, bout, bout_key, foul_idx, everything())

# Post-process: Normalise foul text and map to a simplified predicted_foul_type

model_v2_clean <- model_v2_clean %>%
  mutate(
    foul_type_guess = stringr::str_to_lower(foul_type_guess),
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

# Write clean foul-level output table for downstream ML / Hybrid Scripts: This is mainly used for testing and tuning the later model
# PATHS NOTE: write to paths once 00_paths.R is implemented
readr::write_csv(model_v2_clean, file.path(paths$output, "model_v2_clean.csv"))


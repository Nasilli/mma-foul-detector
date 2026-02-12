# run_all.R
# One-click full pipeline runner (from project root / .Rproj)
# Always run from project root - open the .Rproj file, then run this script

# Core setup + Paths
source(file.path("scripts", "Paths.R"))
source(file.path("scripts", "00_setup.R"))

# Core data / gold / model outputs used downstream
source(file.path("scripts", "01_load_clean_gold_and_models.R"))

# Sentence-level datasets
source(file.path("scripts", "1.5_add_flags.R"))
source(file.path("scripts", "02_build_sentence_datasets.R"))

# ML Training
source(file.path("scripts", "03_train_models.R"))

# Evaluation + Thresholds
source(file.path("scripts", "04_evaluate_and_threshold.R"))

# Optional Diagnostics
# source(file.path("scripts", "05_fp_fn_diagnostics.R"))

# Create rule_confidence sentence-level predictions
# source(file.path("scripts", "run_model_v2.R"))  only run if the necessary files have been downloaded and placed in Raw PBP Data (not inclded in upload due to copyright restictions of uploading the raw HTML files.)
source(file.path("scripts", "Rule Confidence.R"))

# Hybrid Model build + tuning
source(file.path("scripts", "Building Hybrid Model.R"))
source(file.path("scripts", "Tuning Hybrid Model.R"))

# Hybrid Model Test & Final incident-level Output Table:
source(file.path("scripts", "Hybrid Model Test.R"))
source(file.path("scripts", "Final Output Table.R"))

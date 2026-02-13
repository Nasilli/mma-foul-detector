# MMA Foul Detector

## Overview
This MMA Foul Detector is a hybrid regex rules-based / ML NLP system that reads Sherdog play-by-play HTML commentary and produces a structured table of detected foul incidents; detailing event, bout, foul type, fouler and referee action.
Built entirely in R using tidyverse, glmnet, and a modular script pipeline

## Motivation
This project was inspired by recurring controversy surrounding fouls in professional MMA, particularly high-profile incidents such as fight-ending eye pokes (Tom Aspinall vs Cyril Gane). Despite frequent debate around referee decisions and foul frequency, to my knowledge, there is currently no publicly available structured dataset of MMA fouls.
To analyse fouls at scale, a detection system was required.
The project began as a purely regex-based rule engine evaluated against manually constructed gold standard tables. After repeatedly refining rules across multiple events, it became clear that rule-only approaches were reactive to variation in commentary phrasing and risked over-fitting.
This led to a pivot toward a hybrid NLP architecture combining:
- Domain-specific regex anchors  
- Engineered contextual features  
- TF-IDF representations  
- Regularised logistic regression  

The goal then shifted from perfect rule matching to scalable, high-recall foul detection under extreme class imbalance.
The structured extraction layer (foul type, fouler, referee action) is built on top of a detection system designed first for recall at scale.

## Model Performance:
Current model statistics from the blind test:
- Recall: 94.4%
- Precision: 79.2%
- 89 fouls tested across approximately 6200 sentences
The blind test was compared against a manually constructed gold standard table. I manually read through all relevant event commentaries and built a table detailing every foul incident. The fouls detected by the hybrid model are compared against this table to determine accuracy. Given the extreme class imbalance (≈1.4% positive rate), PR AUC was prioritised over ROC AUC when evaluating model performance.

## Technical Architecture:
The foul detector follows a structured NLP pipeline:
### HTML Parsing:
- Play-by-play commentary pages downloaded from Sherdog are parsed using a custom function (process_event_v2). This extracts event metadata, bout names and every commentary sentence with a sentence ID (sid), producing a sentence-level dataset for each event.
  
### Sentence Extraction & Normalisation:
- Each commentary sentence is cleaned and normalised (lowercasing, whitespace trimming, punctuation handling) to create a stable join key (sentence_key) used across rule-based detection, ML scoring and evaluation against the gold standard dataset.
  
### Rule-Based Foul Detection:
- A regex dictionary detects explicit foul phrases (e.g. eye pokes, groin strikes, fence grabs, back-of-the-head strikes).
This includes:
 - Strong foul anchors
 - Contextual cues (e.g. referee warnings, grounded opponent language)
 - Suppression rules for known false positives (e.g. injury mentions like “cut above the eye”)
   
### ML Probability Scoring (TF-IDF + Engineered Flags + Logistic Regression:
- A sentence-level classifier is trained using:
   - TF-IDF features (unigrams + bigrams)
   - Manually engineered binary flags derived from domain-specific regex signals
- An L1-regularised logistic regression model (glmnet) performs feature selection and outputs a probability (ml_prob) that a sentence describes a foul. Logistic regression was compared against SVM and selected based on performance (particularly PR AUC).

### Threshold Tuning:
- Precision, recall and F1 are evaluated across a threshold sweep on a held-out split. The operating threshold is selected to prioritise recall while keeping precision within an acceptable range. Similar tuning methods were used to determine optimal hyperparameters.

### Hybrid Decision Rule:
- Final foul classification uses hybrid logic:
  - Accept sentence as foul if ML probability ≥ tuned threshold (0.32)
  - Rescue ML misses if rule confidence ≥ 0.7
  - Override for extremely high-confidence rule hits (≥ 0.9)
- This balances statistical generalisation (ML) with domain certainty (rules).

### Final Structured Extraction:
- Sentences classified as fouls are passed into a structured rules layer, which extracts:
  - Event
  - Bout
  - Fouler
  - Foul Type
  - Referee action (pause/point deduction)
- This produces the final incident-level output table.


## Data Pipeline
The project follows a modular, script-based pipeline structured around reproducible data stages. The system is designed to separate raw ingestion, feature generation, model training, hybrid inference, and final structured output.

### 1. Raw Data Ingestion
Sherdog play-by-play HTML files are stored locally under: ```data/raw```
Two key subfolders are used:
- `Raw PBP Data/` – historical events used for model development  
- `Hybrid Model Test Events/` – blind test events used for evaluation  
Raw HTML files are parsed but not stored in this repository due to copyright constraints.

### 2. Sentence-Level Dataset Creation
The script:
```scripts/process_event_v2.R```
parses each event HTML file and extracts:
- Event metadata  
- Bout information  
- Sentence-level commentary  
- Sentence ID (`sid`)  
This produces structured sentence-level datasets saved under:
```data/interim/training_sentences```
These datasets serve as the foundation for both rule-based detection and ML feature generation.

### 3. Feature Engineering & Model Training
Training and feature construction are handled through scripts:
```scripts/02_build_sentence_dataset.R```
```scripts/03_train_models.R```
```scripts/04_evaluate_and_threshold.R```
These scripts perform:
- TF-IDF feature construction (unigrams + bigrams)
- Regex-derived binary feature flag generation
- L1-regularised logistic regression training (`glmnet`)
- Threshold tuning using Precision-Recall metrics
Model artefacts and intermediate objects are stored in:
```data/interim```
  
### 4. Hybrid Detection Execution
The master orchestration script:
```run_all.R```
executes the full pipeline:
1. Parse new event HTML files  
2. Apply regex-based foul detection  
3. Apply ML probability scoring  
4. Apply threshold + hybrid decision logic  
5. Pass accepted sentences into structured extraction  
This ensures reproducible end-to-end execution from raw commentary to structured foul output.

### 5. Final Output Generation

The final incident-level foul table:
```fouls_all_hybrid```
is exported as:
```data/output/Final Output Table Hybrid Model.csv```
This table contains one row per detected foul incident, including event, bout, foul type, fouler, and referee action.


## Design Philosophy (Recall Prioritisation):
Recall was prioritised because false negatives are more damaging than false positives.
I can filter out non-fouls that the model incorrectly detects, but I cannot add fouls that were missed without manually reading every commentary. When scaling to multiple years of events, manually checking for missed fouls becomes impossible; recall is the primary optimisation objective.
The project also faces extreme class imbalance (e.g., 89 fouls across ∼6200 sentences). For this reason, PR AUC was prioritised over ROC AUC when comparing model performance and tuning hyperparameters.


### Current Limitations
- The final structured output table is not fully validated and currently requires manual cleaning.
- The accuracy of foul type detection, fouler detection, and referee action extraction has not yet been formally evaluated.
- Detected fouls currently require manual validation of structured fields before downstream analysis.
- Regex components remain somewhat reactive due to variation in commentary language and phrasing.
- Separating referee warnings from actual fouls remains technically challenging.
- Context expansion (±2 sentences) is necessary because key foul information is often not contained in a single sentence.
Foul detection itself is the priority; structured detail extraction will be improved once detection performance is sufficiently strong

## Reproducibility:
Due to copyright constraints, raw Sherdog HTML files are not included in this repository.
For the data used to build and train the model, I have extracted the necessary sentence-level outputs and imported them from saved files in the project directory instead of uploading full HTML files.

## How to Run:
Download the HTMLs of the events you want to detect fouls for (If testing for accuracy, use the same events listed)

Save HTMLs in:
```raw/Hybrid Model Test Events```
Follow the file-naming conventions:
- UFC320-ankalaev-pereira-04-10-2025.html - for a numbered event
- UFCvegas-107-blanchfield-barber-31-05-2025.html - for a Vegas event
- UFC-ulberg-reyes-27-09-2025.html - for a fight night event

Open the project through the ```.Rproj``` file

Run:
```run_all.R```

The final output table:
```fouls_all_hybrid```
is saved as:
```Final Output Table Hybrid Model.csv```

For accuracy analysis (if using the same events) visit the end of:
```Hybrid Model Test.R```
and look for “accuracy check”

## Events I used for the test:
- UFC 317
- UFC Fight Night - Garry vs Prates
- UFC Fight Night - Hill vs Rountree
- UFC Fight Night - Sandhagen vs Figueiredo
- UFC Fight Night - Usman vs Buckley
- UFC 314
- UFC 315
- UFC 316
- UFC 324
- UFC 325
- UFC Vegas 112 - Royval vs Kape

## Future Works
This is an evolving research project focused on maximising scalable foul detection performance.
My aim is to:
- Achieve ≥98% recall on a larger blind test set
- Maintain acceptable precision so filtering false positives is not time-intensive
- Fully validate structured extraction (fouler, foul type, referee action)
- Scale the system to generate a structured foul dataset across multiple UFC seasons (e.g. 2020–2025)
- Conduct deeper foul impact analysis (e.g. foul types most correlated with losses, relationship between ranking and foul frequency, etc.)
  
Only once foul detection is sufficiently reliable at scale will I consider the detection stage complete and move fully to structured extraction refinement and downstream analysis.

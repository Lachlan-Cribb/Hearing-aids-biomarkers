library(dotenv)
library(targets)
library(tarchetypes)
library(qs2)
library(here)
library(quarto)
library(knitr)
#options(clustermq.scheduler="multiprocess")
#library(clustermq)

# env variables
load_dot_env()
Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "data.table",
    "here",
    "readxl",
    "psych",
    "haven",
    "SuperLearner",
    "earth",
    "xgboost",
    "dbarts",
    "glmnet",
    "mgcv",
    "gtools",
    "mice",
    "tmle",
    "future.apply",
    "rms",
    "nnet",
    "quantreg",
    "gtsummary",
    "bayesplot",
    "patchwork",
    "earth",
    "caret",
    "rpart"
  ),
  format = "qs",
  seed = 1234,
  error = "continue"
)

# Source the R scripts in the analysis folder
tar_source()
source(here("..", "..", "utils", "SF12_scoring.R"))

# Source target modules
source("risk_model_targets.R")
source("imputation_targets.R")
source("eligibility_targets.R")
source("descriptives_targets.R")
source("primary_analysis_targets.R")
source("sensitivity_neg_treatment_control_targets.R")
source("sensitivity_hearing_eligibility_targets.R")
source("sensitivity_delta_adj_targets.R")
source("sensitivity_mars_targets.R")

## Pipeline
list(
  ## Create dataset
  tar_target(df, prepare_data()),
  ## Estimate biomarker-dementia risk model
  risk_model_targets,
  ## Multiple imputation per-bootstrap sample
  imputation_targets,
  ## Eligibility criteria
  eligibility_targets,
  ## Descriptive summaries
  descriptives_targets,
  ## Primary analysis
  primary_analysis_targets,
  ## Negative treatment control
  sensitivity_neg_treatment_control_targets,
  ## Objective hearing impairment eligibility sensitivity
  sensitivity_hearing_eligibility_targets,
  ## Sensitivity with delta adjustment
  sensitivity_delta_adj_targets,
  ## Sensitivity with MARS for tmle treatment/outcome models
  sensitivity_mars_targets,
  ## Quarto document with results
  tar_quarto(report, path = "quarto_report.qmd")
)

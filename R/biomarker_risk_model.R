## Add risk estimate to main dataset

add_biomarker_risk <- function(
  data,
  dem_risk_model,
  death_risk_model,
  final_visit = 10,
  interval_duration = 2
) {
  x <- data[, .(
    Safehaven,
    S1_pTau181,
    S1_Abeta40,
    S1_Abeta42,
    S1_GFAP,
    S1_NFlight
  )] |>
    na.omit()

  # Expand to person - period format

  x <- x[rep(1:nrow(x), each = final_visit), ]

  x <- x[, Time := rep(1:final_visit, nrow(x) / final_visit)]

  # exclude intermediate time points as per interval length

  visits <- seq(
    from = interval_duration,
    to = final_visit,
    by = interval_duration
  )

  x <- x[Time %in% visits, ]

  # set variable order
  x <- x[, .(
    Safehaven,
    Time,
    S1_pTau181,
    S1_Abeta40,
    S1_Abeta42,
    S1_GFAP,
    S1_NFlight
  )]

  # add ratio
  x$ratio <- x$S1_Abeta42 / x$S1_Abeta40

  # standardise
  x[,
    (names(x[, !"Safehaven"])) := lapply(.SD, scale),
    .SDcols = names(x[, !"Safehaven"])
  ]

  # predicted hazard from fitted models
  x[,
    haz_dem := predict(
      dem_risk_model,
      newdata = x[, !"Safehaven"],
      onlySL = TRUE
    )$pred
  ]
  x[,
    haz_death := predict(
      death_risk_model,
      newdata = x[, !"Safehaven"],
      onlySL = TRUE
    )$pred
  ]

  # calculate risk
  x[,
    risk_score := cumsum(
      haz_dem * cumprod((1 - shift(haz_dem, fill = 0)) * (1 - haz_death))
    ),
    by = "Safehaven"
  ]

  # filter to final year then add risk score to main data
  data <- merge(
    data,
    x[Time == max(Time), .(Safehaven, risk_score)],
    by = "Safehaven",
    all.x = TRUE
  )

  return(data)
}

## Dementia risk model based on baseline biomarkers

get_dem_risk_model <- function(final_visit = 10, V = 10) {
  data <- prepare_external_data()
  # Apply eligibility criteria (so data is external to main dataset)
  data <- external_eligibility(data)
  # long format
  data <- to_long(data, final_visit)
  data <- remove_postevent(data)
  # fit model for discrete time hazards
  dem_mod <- fit_risk_model(data, "Dem", V = V)
  return(dem_mod)
}

## Death risk model based on baseline biomarkers

get_death_risk_model <- function(final_visit = 10, V = 10) {
  data <- prepare_external_data()
  # Apply eligibility criteria (so data is external to main dataset)
  data <- external_eligibility(data)
  # long format
  data <- to_long(data, final_visit)
  data <- remove_postevent(data)
  # fit model for discrete time hazards
  death_mod <- fit_risk_model(data, "Death", V = V)
  return(death_mod)
}

## Model for hazard

fit_risk_model <- function(data, outcome, V) {
  learners <- c(
    "SL.glm",
    "SL.glmnet",
    "SL.mgcv",
    "SL.earth",
    "tmle.SL.dbarts2",
    "SL.xgboost.1"
  )

  # data for model
  vars <- c(
    "Safehaven",
    "Time",
    "S1_pTau181",
    "S1_Abeta40",
    "S1_Abeta42",
    "S1_GFAP",
    "S1_NFlight",
    outcome
  )

  # exclude missing obs
  data <- data[, ..vars] |> na.omit()
  print(paste("Observations:", length(unique(data$Safehaven))))

  # outcome
  Y <- data[[outcome]]
  print(paste(outcome, "cases:", sum(Y)))

  # predictors
  non_preds <- c(outcome, "Safehaven")
  X <- data[, !..non_preds]
  # ab42 to 40 ratio
  X$ratio <- X$S1_Abeta42 / X$S1_Abeta40
  # standardise
  X[, (names(X)) := lapply(.SD, scale), .SDcols = names(X)]

  # fit dementia model
  model <- SuperLearner(
    Y = Y,
    X = X,
    SL.library = learners,
    family = binomial(),
    method = "method.NNloglik",
    id = data$Safehaven,
    verbose = TRUE,
    cvControl = list(V = V)
  )

  return(model)
}


## Prepare external data

prepare_external_data <- function() {
  ## read biomarker data
  biomarkers <- fread(here(
    "..",
    "..",
    "Data",
    "Dementia Plasma Biomarkers data set",
    "Dementia Plasma Biomarkers data set v3_Section A.csv"
  ))

  ## read hearing data
  alsop_bl <-
    fread(
      here(
        "..",
        "..",
        "Data",
        "ALSOP Baseline Data Set",
        "ALSOP Baseline Medical",
        "ALSOP_MedicalBaseline_DataSet_SafehavenID_V3.csv"
      )
    )

  setnames(alsop_bl, "SAFEID", "Safehaven")

  ## Outcome data

  demo <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionA1_GenDemo_XT06_v1_SafeHavenID.csv"
    )
  )

  death <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionF3_SecondaryEndpoints_XT06_v1.csv"
    )
  )

  # Add Live variables to check if visit was completed
  death <- merge(
    death,
    demo[, .SD, .SDcols = patterns("Safehaven|Live")],
    by = "Safehaven",
    all = TRUE
  )

  # function for death by visit k

  death_function <- function(data, visit) {
    death_out <- ifelse(
      data$Death == 1 &
        data$Death_DSR < (365 * visit) &
        data[[paste0("C_AV", visit, "_Live")]] %in% c(25, 26),
      1,
      ifelse(
        data$Death == 0 &
          data$Death_DSR < (365 * visit) &
          data[[paste0("C_AV", visit, "_Live")]] %in% c(25, 26),
        NA,
        0
      )
    )

    if (visit > 1) {
      death_out <-
        ifelse(data[[paste0("AV", visit - 1, "_Death")]] == 1, 1, death_out)
    }

    return(death_out)
  }

  # add death

  for (i in 1:10) {
    death[[paste0("AV", i, "_Death")]] <- death_function(death, i)
  }

  death_vars <- death[, .SD, .SDcols = patterns("Safehaven|^AV.*_Death$")]

  # Incident dementia

  dem_function <- function(data, visit) {
    ifelse(
      data$Dementia == 1 & data$Dementia_DSR < (365 * visit),
      1,
      ifelse(data$Dementia == 0 & data$Dementia_DSR < (365 * visit), NA, 0)
    )
  }

  # add dementia

  for (i in 1:10) {
    death[[paste0("AV", i, "_Dem")]] <- dem_function(death, i)
  }

  dem_vars <- death[, .SD, .SDcols = patterns("Safehaven|^AV.*Dem$")]

  # merge

  data <- merge(
    biomarkers,
    alsop_bl[, .(Safehaven, HearingProbs, HearingAid)],
    by = "Safehaven",
    all.x = TRUE
  )

  data <- merge(data, dem_vars, by = "Safehaven", all.x = TRUE)
  data <- merge(data, death_vars, by = "Safehaven", all.x = TRUE)

  ## set biomarker measurements with reported tech error to missing
  data[,
    S1_pTau181 := ifelse(
      S1_pTau181_TechErr %in% c(1, 2),
      NA,
      S1_pTau181
    )
  ]

  data[,
    S1_Abeta40 := ifelse(
      S1_Abeta40_TechErr %in% c(1, 2),
      NA,
      S1_Abeta40
    )
  ]

  data[,
    S1_Abeta42 := ifelse(
      S1_Abeta42_TechErr %in% c(1, 2),
      NA,
      S1_Abeta42
    )
  ]

  data[,
    S1_GFAP := ifelse(
      S1_GFAP_TechErr %in% c(1, 2),
      NA,
      S1_GFAP
    )
  ]

  data[,
    S1_NFlight := ifelse(
      S1_NFlight_TechErr %in% c(1, 2),
      NA,
      S1_NFlight
    )
  ]

  return(data)
}

## Data to long format

to_long <- function(data, final_visit, interval_duration = 2) {
  # create indicators for censoring

  for (i in 1:final_visit) {
    data[[paste0("AV", i, "_Cens")]] <-
      if_else(
        is.na(data[[paste0("AV", i, "_Dem")]]) &
          (is.na(data[[paste0("AV", i, "_Death")]]) |
            data[[paste0("AV", i, "_Death")]] == 0),
        1,
        0
      )
    if (i > 1) {
      data[[paste0("AV", i, "_Cens")]] <-
        if_else(
          data[[paste0("AV", i - 1, "_Cens")]] == 1,
          1,
          data[[paste0("AV", i, "_Cens")]]
        )
    }
  }

  # recode death to NA if event has already occurred
  for (i in 1:final_visit) {
    data[[paste0("AV", i, "_Death")]] <-
      if_else(
        data[[paste0("AV", i, "_Dem")]] == 1 &
          !is.na(data[[paste0("AV", i, "_Dem")]]),
        NA,
        data[[paste0("AV", i, "_Death")]]
      )
  }

  # long format

  keep <- c(
    "Safehaven",
    "S1_pTau181",
    "S1_Abeta40",
    "S1_Abeta42",
    "S1_GFAP",
    "S1_NFlight"
  )

  long <- melt(
    data,
    id.vars = keep,
    measure.vars = patterns("_Cens$", "_Dem$", "_Death$"),
    value.name = c("Cens", "Dem", "Death"),
    variable.name = "Time"
  )

  # set max time

  long[, Time := as.numeric(Time)]

  long <- long[Time <= final_visit, ]

  # exclude intermediate time points as per interval length

  visits <- seq(
    from = interval_duration,
    to = final_visit,
    by = interval_duration
  )

  long <- long[Time %in% visits, ]

  # If censoring present, set death to NA
  long[, Death := if_else(Cens == 1, NA, Death)]

  return(long)
}

### Remove rows after event/death/censoring occurs

remove_postevent <- function(data) {
  data <- data[order(Safehaven, Time)]

  data[, sumDeath := cumsum(Death), by = "Safehaven"]
  data <- data[sumDeath < 2 | is.na(sumDeath), ]

  data[, sumCens := cumsum(Cens), by = "Safehaven"]
  data <- data[sumCens < 2 | is.na(sumCens), ]

  data[, sumDem := cumsum(Dem), by = "Safehaven"]
  data <- data[sumDem < 2 | is.na(sumDem), ]

  return(data)
}

##randomly fill missing in variables with <10 missing obs
single_imputation <- function(data) {
  data[is.na(Edu), "Edu"] <-
    sample(data$Edu, 1, replace = TRUE)

  data[is.na(BL_AlcWk), "BL_AlcWk"] <-
    sample(data$BL_AlcWk, 1, replace = TRUE)

  data[is.na(BL_CesdOverall), "BL_CesdOverall"] <-
    sample(data$BL_CesdOverall, 1, replace = TRUE)

  data[is.na(BL_MCS), "BL_MCS"] <-
    sample(data$BL_MCS, 1, replace = TRUE)

  data[is.na(BL_PCS), "BL_PCS"] <-
    sample(data$BL_PCS, 1, replace = TRUE)

  data[is.na(Racial), "Racial"] <-
    sample(data$Racial, 1, replace = TRUE)

  return(data)
}

## Multiple imputation for bootstrap samples
fit_mi <- function(data, m = 2, maxit = 10, mean_sd) {
  # Variables to be ignored in imputation
  to_ignore <- c(
    "Safehaven",
    "Y3M_ALSOP_DSR",
    "S1_Abeta42",
    "S1_Abeta42_lower",
    "S1_Abeta40",
    "S3_Abeta42",
    "S3_Abeta42_lower",
    "S3_Abeta40",
    "AV4_Dem",
    "AV5_Dem",
    "AV7_Dem",
    "AV8_Dem",
    "AV9_Dem",
    "AV11_Dem",
    "tar_batch",
    "tar_rep",
    "tar_seed",
    paste0("AV", 4:12, "_Death")
  )

  ## Auxiliary variables

  aux_vars <- c(
    "Y3M_HearingProbs",
    "Y3M_HearingProbs_1",
    "Y3M_Buzz",
    "Y3M_QuietRm_1",
    "Y3M_QuietRm_2",
    "Y3M_CrowdedRm",
    "Y3MToneAvg_Better",
    "AV6_Polypharmacy",
    "AV9_Polypharmacy",
    "AV6_CesdOverall",
    "AV9_CesdOverall",
    "AV6_3MS_OverallScore_C",
    "AV9_3MS_OverallScore_C",
    "AV6_HVLT4",
    "AV9_HVLT4",
    "AV6_PCS",
    "AV6_MCS",
    "AV9_PCS",
    "AV9_MCS",
    "AV6_Dem",
    "AV10_Dem",
    "AV6_Cancer",
    "AV9_Cancer",
    "AV6_CVD",
    "AV9_CVD",
    "AV6_Frailty_DAI50",
    "AV9_Frailty_DAI50"
  )

  ## First add derived variables to data (interactions and squared terms)
  # quadratic terms
  non_binary <- apply(
    data[, !..to_ignore],
    2,
    function(x) length(unique(x[!is.na(x)])) > 2
  )
  quad_terms <- which(non_binary) |> names()
  # remove outcomes from quad terms
  quad_terms <- quad_terms[-grep("S3_", quad_terms)]

  # interaction terms
  interaction_vars <- c(
    "AgeAtRand",
    "Gender",
    "Edu",
    "S1_pTau181",
    "S1_Abeta42_40",
    "S1_GFAP",
    "S1_NFlight",
    "S3_pTau181",
    "S3_Abeta42_40",
    "S3_GFAP",
    "S3_NFlight",
    "risk_score",
    "BLToneAvg_Better",
    "apoe_e4",
    "BL_3MS_OverallScore_C",
    "BL_Frailty_DAI50",
    "Y3M_HearingAid",
    "Y3M_HearingAidUse"
  )

  interaction_terms <-
    combinations(
      n = length(interaction_vars),
      r = 2,
      v = interaction_vars,
      repeats.allowed = FALSE
    ) |>
    as.data.table()

  interaction_vars2 <- c(
    "Y3M_HearingAid",
    "Y3M_HearingAidUse",
    "AgeAtRand",
    "Edu",
    "Gender",
    "AV6_3MS_OverallScore_C",
    "AV9_3MS_OverallScore_C"
  )

  interaction_terms2 <-
    combinations(
      n = length(interaction_vars2),
      r = 2,
      v = interaction_vars2,
      repeats.allowed = FALSE
    ) |>
    as.data.table()

  interaction_vars3 <- c(
    "Y3M_HearingAid",
    "Y3M_HearingAidUse",
    "AV10_Dem"
  )

  interaction_terms3 <-
    combinations(
      n = length(interaction_vars3),
      r = 2,
      v = interaction_vars3,
      repeats.allowed = FALSE
    ) |>
    as.data.table()

  interaction_vars4 <- c(
    "Y3M_HearingAid",
    "Y3M_HearingAidUse",
    "S3_Time"
  )

  interaction_terms4 <-
    combinations(
      n = length(interaction_vars4),
      r = 2,
      v = interaction_vars4,
      repeats.allowed = FALSE
    ) |>
    as.data.table()

  interaction_terms <- rbind(
    interaction_terms,
    interaction_terms2,
    interaction_terms3,
    interaction_terms4
  )

  interaction_terms <- unique(interaction_terms)

  ## Exclude unidentified interactions

  interaction_terms <-
    interaction_terms[!(grepl("HearingAid", V1) & grepl("HearingAid", V2)), ]

  interaction_terms <-
    interaction_terms[!(grepl("Dem", V1) & grepl("Dem", V2)), ]

  interaction_terms <-
    interaction_terms[!(grepl("S1_", V1) & grepl("risk_score", V2)), ]

  interaction_terms <-
    interaction_terms[!(grepl("risk_score", V1) & grepl("S1_", V2)), ]

  interaction_terms <-
    interaction_terms[!(grepl("S3_", V1) & grepl("S3_", V2)), ]

  ## Final interaction terms

  interaction_terms <- paste(
    interaction_terms$V1,
    interaction_terms$V2,
    sep = ":"
  )

  # Add derived terms to data
  derived <-
    add_derived(
      quad_terms = quad_terms,
      interaction_terms = interaction_terms,
      data = data
    )

  # remove derived variables with no observations

  derived$model_df <-
    derived$model_df[,
      !which(colSums(is.na(derived$model_df)) == nrow(derived$model_df)),
      with = FALSE
    ]

  data <- cbind(data, derived$model_df)

  interaction_terms <- derived$int_terms

  ### Construct predictor matrix
  predmat <- make.predictorMatrix(data)

  ## Exclude some variables from being used as predictors
  pnames <- colnames(predmat)
  predmat[, to_ignore] <- 0
  predmat[to_ignore, ] <- 0
  predmat[grep("HearingAid", pnames), grep("HearingAid", pnames)] <- 0
  predmat["Y3M_HearingAidUse", "Y3M_HearingAid"] <- 0
  predmat["BLToneAvg_Better", grep("AV9|AV10", colnames(predmat))] <- 0
  predmat["Y3MToneAvg_Better", grep("AV9|AV10", colnames(predmat))] <- 0
  predmat[grep("AV9|AV10", colnames(predmat)), "BLToneAvg_Better"] <- 0
  predmat[grep("AV9|AV10", colnames(predmat)), "Y3MToneAvg_Better"] <- 0
  predmat[grep("CVD", pnames), grep("CVD", pnames)] <- 0
  predmat[grep("Dem", pnames), grep("Dem", pnames)] <- 0
  predmat[grep("Cancer", pnames), grep("Cancer", pnames)] <- 0
  # Biomarkers are missing together so don't use for each other
  predmat[grep("S3_", pnames), grep("S3_", pnames)] <- 0
  predmat[grep("S1_", pnames), grep("S1_", pnames)] <- 0

  ## Don't use derived(x), like quadratic terms, to predict x

  for (i in rownames(predmat)) {
    predmat[i, grep(i, pnames)] <- 0
  }

  ### Vector of imputation methods

  methods <- make.method(data)

  miss_vec <- methods[!methods == ""]

  ## Passive imputation
  # quadratic terms

  quadratic_terms <- quad_terms[quad_terms %in% names(miss_vec)]

  quad_methods <-
    paste("~I(", quadratic_terms, " * ", quadratic_terms, ")", sep = "")

  methods[paste(quadratic_terms, "2", sep = "")] <- quad_methods

  # interaction terms

  int_terms <- interaction_terms[interaction_terms %in% names(miss_vec)]

  interaction_methods <- str_split_fixed(int_terms, "_by_", 2)
  interaction_methods <- paste(
    "~I(",
    interaction_methods[, 1],
    " * ",
    interaction_methods[, 2],
    ")",
    sep = ""
  )

  methods[int_terms] <- interaction_methods

  # set method to LASSO PMM or PMM for auxiliary variables (faster)

  methods[methods == "pmm"] <- "lasso.pmm"

  methods[names(methods) %in% aux_vars] <- "pmm"

  methods[to_ignore] <- ""

  ## Don't use derived to predict auxiliary variables
  predmat[aux_vars, interaction_terms] <- 0
  predmat[aux_vars, paste(quadratic_terms, "2", sep = "")] <- 0

  ## imputation

  imp_out <-
    mice(
      data,
      m = m,
      method = methods,
      predictorMatrix = predmat,
      maxit = maxit
    )

  print(imp_out$loggedEvents)

  ## Extract complete datasets from MICE
  imp <- complete(imp_out, "all")
  imp <- lapply(imp, as.data.table)
  # remove derived
  imp <- lapply(imp, remove_derived_cols)
  # undo standardise
  imp <- lapply(imp, undo_standardise, mean_sd = mean_sd)

  return(imp)
}

## derived variables for imputation
add_derived <-
  function(quad_terms, interaction_terms, data) {
    int_formula <-
      paste(interaction_terms, collapse = " + ")

    quad_formula <-
      paste(paste0("I(", quad_terms, "^2)"), collapse = " + ")

    model_formula <-
      paste("~", paste(int_formula, quad_formula, sep = " + ")) |>
      as.formula()

    model_df <-
      model.matrix(
        model_formula,
        model.frame(
          model_formula,
          data = data,
          na.action = NULL
        )
      )[, -1] |>
      as.data.table()

    names(model_df) <- gsub("I\\(", "", names(model_df))
    names(model_df) <- gsub("\\^2", "", names(model_df))
    names(model_df) <- gsub("\\)", "2", names(model_df))
    names(model_df) <- gsub("\\:", "_by_", names(model_df))

    # save quad and interaction terms for later
    quad_terms <- paste(quad_terms, "2", sep = "")
    interaction_terms <- model_df[, !..quad_terms] |> names()

    return(list(
      model_df = model_df,
      quad_terms = quad_terms,
      int_terms = interaction_terms
    ))
  }


# Remove interactions and non-linear terms from dataset
remove_derived_cols <- function(data) {
  data <- data[, .SD, .SDcols = !patterns("_by_")]
  quad_vars <- paste0(names(data), "2")
  data <- data[, setdiff(names(data), quad_vars), with = FALSE]
}

## CART imputation sensitivity
fit_mi_cart <- function(data, m = 2, maxit = 10, mean_sd) {
  # Variables to be ignored in imputation
  to_ignore <- c(
    "Safehaven",
    "Y3M_ALSOP_DSR",
    "S1_Abeta42",
    "S1_Abeta42_lower",
    "S1_Abeta40",
    "S3_Abeta42",
    "S3_Abeta42_lower",
    "S3_Abeta40",
    "AV4_Dem",
    "AV5_Dem",
    "AV7_Dem",
    "AV8_Dem",
    "AV9_Dem",
    "AV11_Dem",
    "tar_batch",
    "tar_rep",
    "tar_seed",
    paste0("AV", 4:12, "_Death")
  )

  ## Auxiliary variables

  aux_vars <- c(
    "Y3M_HearingProbs",
    "Y3M_HearingProbs_1",
    "Y3M_Buzz",
    "Y3M_QuietRm_1",
    "Y3M_QuietRm_2",
    "Y3M_CrowdedRm",
    "Y3MToneAvg_Better",
    "AV6_Polypharmacy",
    "AV9_Polypharmacy",
    "AV6_CesdOverall",
    "AV9_CesdOverall",
    "AV6_3MS_OverallScore_C",
    "AV9_3MS_OverallScore_C",
    "AV6_HVLT4",
    "AV9_HVLT4",
    "AV6_PCS",
    "AV6_MCS",
    "AV9_PCS",
    "AV9_MCS",
    "AV6_Dem",
    "AV10_Dem",
    "AV6_Cancer",
    "AV9_Cancer",
    "AV6_CVD",
    "AV9_CVD",
    "AV6_Frailty_DAI50",
    "AV9_Frailty_DAI50"
  )

  ### Construct predictor matrix
  predmat <- make.predictorMatrix(data)

  ## Exclude some variables from being used as predictors
  pnames <- colnames(predmat)
  predmat[, to_ignore] <- 0
  predmat[to_ignore, ] <- 0
  predmat[grep("HearingAid", pnames), grep("HearingAid", pnames)] <- 0
  predmat["Y3M_HearingAidUse", "Y3M_HearingAid"] <- 0
  predmat["BLToneAvg_Better", grep("AV9|AV10", colnames(predmat))] <- 0
  predmat["Y3MToneAvg_Better", grep("AV9|AV10", colnames(predmat))] <- 0
  predmat[grep("AV9|AV10", colnames(predmat)), "BLToneAvg_Better"] <- 0
  predmat[grep("AV9|AV10", colnames(predmat)), "Y3MToneAvg_Better"] <- 0
  predmat[grep("CVD", pnames), grep("CVD", pnames)] <- 0
  predmat[grep("Dem", pnames), grep("Dem", pnames)] <- 0
  predmat[grep("Cancer", pnames), grep("Cancer", pnames)] <- 0
  # Biomarkers are missing together so don't use for each other
  predmat[grep("S3_", pnames), grep("S3_", pnames)] <- 0
  predmat[grep("S1_", pnames), grep("S1_", pnames)] <- 0

  ## imputation
  imp_out <-
    mice(data, m = m, method = "cart", predictorMatrix = predmat, maxit = maxit)

  print(imp_out$loggedEvents)

  ## Extract complete datasets from MICE
  imp <- complete(imp_out, "all")
  imp <- lapply(imp, as.data.table)
  # undo standardise
  imp <- lapply(imp, undo_standardise, mean_sd = mean_sd)

  return(imp)
}

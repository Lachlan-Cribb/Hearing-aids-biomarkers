## Missing data summary

get_missing <- function(unimputed_data, eligible_data){

  variables <- c(
    "HearingAid",
    "HearingProbs",
    "Y3M_HearingAid",
    "Y3M_HearingAidUse",
    "AV3_Skn_Ph",
    "S3_pTau181",
    "S1_pTau181",
    "AgeAtRand",
    "Gender",
    "Racial",
    "Edu",
    "Buzz",
    "QuietRm",
    "CrowdedRm",
    "HrsSleep",
    "PresPhysAct",
    "SocialEngagement",
    "Income",
    "BLToneAvg_Better",
    "BL_SBP_Mean",
    "BL_BMI",
    "Pt_Cr",
    "DAB",
    "BL_AHP",
    "BL_Polypharmacy",
    "BL_eGFR_CKD",
    "LFT_PC1",
    "BL_CesdOverall",
    "BL_SmHis3",
    "BL_AlcWk",
    "BL_PCS",
    "BL_MCS",
    "Pt_scoreIRSAD",
    "CKD",
    "apoe_e4",
    "BL_3MS_OverallScore_C",
    "BL_HVLT4",
    "BL_Frailty_DAI50",
    "EyesRate"
  )

  eligible_pts <- lapply(eligible_data, \(.x) .x$Safehaven)

  unimputed_data <- lapply(eligible_pts, \(.x) unimputed_data[Safehaven %in% .x,])

  get_missing <- function(data, var){
    num <- sum(is.na(data[[var]]))
    prop <- num / nrow(data) * 100
    data.table(Variable = var, num = num, prop = prop)
  }

  table <- rbindlist(
    lapply(
    unimputed_data,
    \(.x) rbindlist(lapply(variables, get_missing, data = .x))
  ))
  table
}

get_missing_summary <- function(missing_data){

  nice_names <- c(
    "Prevalent hearing aid prescription at cohort entry",
    "Self-reported hearing impairment",
    "New hearing aid prescription (ASPREE year 3)",
    "Hearing aid use (ASPREE year 3)",
    "New skin cancer screening",
    "Follow-up biomarkers of ADRD",
    "Baseline biomarkers of ADRD",
    "Age",
    "Gender",
    "Race",
    "Education",
    "Tinnitus",
    "Difficulty hearing in quiet room",
    "Difficulty hearing in crowded room",
    "Typical sleep duration",
    "Physical activity",
    "Social engagement",
    "Income",
    "4-frequency pure tone average",
    "Systolic blood pressure",
    "Body mass index",
    "History of cancer",
    "History of diabetes",
    "Antihypertensive use",
    "Polypharmacy",
    "eGFR",
    "Liver function",
    "CES-D Overall score",
    "Smoking status",
    "Alcohol consumption",
    "SF-12 Physical component score",
    "SF-12 Mental component score",
    "Socioeconomic status",
    "Chronic kidney disease",
    "APOE-e4",
    "3MS Overall score",
    "HVLT Delayed recall",
    "Frailty",
    "Visual function"
  )

  missing_data <- missing_data[, .(num = mean(num), prop = mean(prop)), by = Variable]
  missing_data[, Variable := nice_names]
  setorder(missing_data, -num)
  missing_data[, prop := round(ifelse(prop < 1 & prop > 0, -1, prop))]
  missing_data[, prop := ifelse(prop == -1, "<1", prop)]
  missing_data[, prop := paste0(prop, "%")]
  missing_data[, Missing := paste0(num, " ", "(", prop, ")")]
  missing_data[, list(Variable, Missing)]
}

## Sample size across imputed datasets
get_sample_size <- function(data){
  total_n <- unlist(lapply(data, nrow))
  treated_n <- unlist(lapply(data, \(x) nrow(x[Y3M_HearingAid == 1,])))
  untreated_n <- unlist(lapply(data, \(x) nrow(x[Y3M_HearingAid == 0,])))
  data.table(total = total_n, treated = treated_n, untreated = untreated_n)
}

sample_size_summarise <- function(data){
  data[, .(
    mean_n = mean(total),
    median_n = median(total),
    lower_n = quantile(total, 0.25),
    upper_n = quantile(total, 0.75),
    mean_treated = mean(treated),
    median_treated = median(treated),
    lower_treated = quantile(treated, 0.25),
    upper_treated = quantile(treated, 0.75),
    mean_untreated = mean(untreated),
    median_untreated = median(untreated),
    lower_untreated = quantile(untreated, 0.25),
    upper_untreated = quantile(untreated, 0.75)
  )]
}

## Average over imputations for baseline characteristics table

baseline_table_summary <- function(data){
  # extract and combine list elements
  numeric_data <- rbindlist(data[str_detect(names(data), "_num")], idcol = "b")
  cat_data <- rbindlist(data[str_detect(names(data), "_cat")], idcol = "b")

  # average over imputations and make presentable
  numeric_data <-
    numeric_data |>
    group_by(Variable) |>
    summarise(across(-c(b), mean)) |>
    mutate(across(-Variable, ~ my_round(., 2))) |>
    mutate(across(-Variable, ~ gsub(" ", "", .))) |>
    mutate(Yes = paste0(Yes_median, " (", Yes_lower, ", ", Yes_upper, ")"),
           No = paste0(No_median, " (", No_lower, ", ", No_upper, ")")) |>
    select(Variable, Yes, No)

  cat_data <-
    cat_data |>
    group_by(Variable) |>
    summarise(across(-c(b), mean)) |>
    mutate(across(-Variable, ~ my_round(., 2))) |>
    mutate(across(-Variable, ~ gsub(" ", "", .))) |>
    mutate(Yes = paste0(Yes_count, " (", Yes_perc,"%)"),
           No = paste0(No_count, " (", No_perc,"%)")) |>
    select(Variable, Yes, No)

  rbindlist(list(cat_data, numeric_data))

}

## Baseline characteristic table

get_baseline_table <- function(data){
  data <- rbindlist(data, idcol = "m")
  data$m <- as.numeric(as.factor(data$m))

  data$Smoking <- ifelse(
    data$BL_SmHis2 == 1,
    "Former",
    ifelse(
      data$BL_SmHis3 == 1,
      "Never",
      "Current")
    )

  data$apoe_e4 <- ifelse(data$apoe_e4 >= 1, 1, 0)

  data$S1_Abeta42_40 <- data$S1_Abeta42_40 * 1000

  data$Gender <- ifelse(data$Gender == 1, "Woman", "Man")

  data$Racial <- ifelse(data$Racial == 1, "Non-White", "White")

  data$Edu <- as.factor(data$Edu)
  levels(data$Edu) <- c("< 9", "9-11", "12", "13-15", "16", "17-21")

  bl_variables <- c(
    "m",
    "Y3M_HearingAid",
    "AgeAtRand",
    "Edu",
    "Gender",
    "Racial",
    "BL_BMI",
    "BL_SBP_Mean",
    "BL_AHP",
    "DAB",
    "Smoking",
    "apoe_e4",
    "BL_3MS_OverallScore_C",
    "BLToneAvg_Better",
    "risk_score",
    "S1_pTau181",
    "S1_Abeta42_40",
    "S1_GFAP",
    "S1_NFlight"
  )

  nice_names <- c(
    "m",
    "Y3M_HearingAid",
    "Age",
    "Education",
    "Gender",
    "Race",
    "BMI",
    "Systolic blood pressure",
    "Antihypertensive",
    "History of diabetes",
    "Smoking status",
    "APOE e4 positivity",
    "3MS Overall score",
    "4-frequency pure tone average",
    "Risk score",
    "pTau181",
    "Abeta-42 to Abeta-40 ratio",
    "GFAP",
    "NfL"
  )

  # all categorical variables to binary indicators
  data <- data[, ..bl_variables]
  setnames(data, bl_variables, nice_names)

  data <- as.data.table(predict(dummyVars(~ ., data = data, fullRank = FALSE), newdata = data))

  # split numeric and categorical data
  num_data <- select(data, m, Y3M_HearingAid, where( ~ n_distinct(.) > 2))
  cat_data <- select(data, where( ~ n_distinct(.) == 2))

  # summarise continuous data
  num_out <- map(c("median", "lower", "upper"), ~ my_summarise(num_data, .x))
  num_out <- Reduce(function(x,y) full_join(x, y, by = "Variable"), num_out)

  # summarise categorical data
  cat_out <- map(c("count", "perc"), ~ my_summarise(cat_data, .x))
  cat_out <- Reduce(function(x,y) full_join(x, y, by = "Variable"), cat_out)

  list(cat = cat_out, num = num_out)
}

get_surv_summary <- function(data_all, data_survivors){
  surv_summary <- map2(data_all, data_survivors, function(.x, .y) {
    n_deaths <- nrow(.x) - nrow(.y)
    prop_deaths <- (n_deaths / nrow(.x)) * 100
    n_deaths_trt <- nrow(.x[Y3M_HearingAid == 1, ]) -
      nrow(.y[Y3M_HearingAid == 1, ])
    prop_deaths_trt <- (n_deaths_trt / nrow(.x[Y3M_HearingAid == 1, ])) * 100
    n_deaths_untrt <- nrow(.x[Y3M_HearingAid == 0, ]) -
      nrow(.y[Y3M_HearingAid == 0, ])
    prop_deaths_untrt <- (n_deaths_untrt / nrow(.x[Y3M_HearingAid == 0, ])) * 100
    data.table(
      n_deaths,
      prop_deaths,
      n_deaths_trt,
      prop_deaths_trt,
      n_deaths_untrt,
      prop_deaths_untrt
    )
  })

  rbindlist(surv_summary)
}

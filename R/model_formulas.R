#### Outcome models

get_outcome_formula <- function() {
  outcome_covars <- c(
    "as.factor(A)",
    "rcs(AgeAtRand, c(70.8, 73.3, 80.1))",
    "Gender",
    "Racial",
    "Edu + I(Edu^2)",
    "Buzz + I(Buzz^2)",
    "QuietRm + I(QuietRm^2)",
    "CrowdedRm + I(CrowdedRm^2)",
    "HrsSleep + I(HrsSleep^2)",
    "PresPhysAct + I(PresPhysAct^2)",
    "rcs(SocialEngagement, c(18, 26, 32))",
    "Income + I(Income^2)",
    "rcs(S1_pTau181, c(19, 32, 61))",
    "rcs(S1_Abeta42_40, c(0.046, 0.061, 0.074))",
    "rcs(S1_GFAP, c(66, 117, 210))",
    "rcs(S1_NFlight, c(12, 19, 31))",
    "rcs(risk_score, c(0.02, 0.05, 0.15))",
    "rcs(S3_Time, c(2164, 2663, 3164))",
    "rcs(BLToneAvg_Better, c(15, 29, 44))",
    "rcs(BL_SBP_Mean, c(118, 139, 161))",
    "rcs(BL_BMI, c(22, 27, 33))",
    "Pt_Cr",
    "DAB",
    "BL_AHP",
    "rcs(BL_Polypharmacy, c(0,3,6))",
    "rcs(BL_eGFR_CKD, c(56, 75, 89))",
    "rcs(LFT_PC1, c(-0.77, -0.19, 0.78))",
    "rcs(LFT_PC2, c(-1, 0, 0.87))",
    "rcs(BL_CesdOverall, c(0, 2, 8))",
    "BL_SmHis2",
    "BL_SmHis3",
    "rcs(BL_AlcWk, c(0, 3, 8))",
    "rcs(BL_PCS, c(37, 51, 57))",
    "rcs(BL_MCS, c(45, 58, 63))",
    "rcs(Pt_scoreIRSAD, c(911, 1010, 1100))",
    "CKD",
    "apoe_e4",
    "rcs(BL_3MS_OverallScore_C, c(88, 95, 99))",
    "rcs(BL_HVLT4, c(4, 8, 12))",
    "rcs(BL_Frailty_DAI50, c(0.04, 0.09, 0.18))",
    "EyesRate + I(EyesRate^2)"
  )

  outcome_model_interactions <- c(
    "as.factor(A)",
    "AgeAtRand",
    "Gender",
    "Edu",
    "S1_pTau181",
    "S1_Abeta42_40",
    "S1_GFAP",
    "S1_NFlight",
    "risk_score",
    "S3_Time",
    "BLToneAvg_Better",
    "apoe_e4",
    "BL_3MS_OverallScore_C",
    "BL_Frailty_DAI50"
  )

  outcome_model_interactions <-
    combinations(
      n = length(outcome_model_interactions),
      r = 2,
      v = outcome_model_interactions,
      repeats.allowed = FALSE
    ) |>
    as.data.table()

  outcome_model_interactions <- paste(
    outcome_model_interactions$V1,
    outcome_model_interactions$V2,
    sep = ":"
  )

  outcome_model_interactions <- c(
    outcome_model_interactions,
    c("as.factor(A):CrowdedRm", "as.factor(A):BL_HVLT4")
  )

  outcome_form <- as.formula(
    paste(
      "Y ~ 1",
      paste(outcome_covars, collapse = " + "),
      paste(outcome_model_interactions, collapse = " + "),
      sep = " + "
    )
  )

  return(outcome_form)
}

## Treatment models

get_treatment_formula <- function() {
  treatment_covars <- c(
    "rcs(AgeAtRand, c(70.8, 73.3, 80.1))",
    "Gender",
    "Racial",
    "Edu + I(Edu^2)",
    "Buzz + I(Buzz^2)",
    "QuietRm + I(QuietRm^2)",
    "CrowdedRm + I(CrowdedRm^2)",
    "HrsSleep + I(HrsSleep^2)",
    "PresPhysAct + I(PresPhysAct^2)",
    "rcs(SocialEngagement, c(18, 26, 32))",
    "Income + I(Income^2)",
    "rcs(S1_pTau181, c(19, 32, 61))",
    "rcs(S1_Abeta42_40, c(0.046, 0.061, 0.074))",
    "rcs(S1_GFAP, c(66, 117, 210))",
    "rcs(S1_NFlight, c(12, 19, 31))",
    "rcs(risk_score, c(0.02, 0.05, 0.15))",
    "rcs(BLToneAvg_Better, c(15, 29, 44))",
    "rcs(BL_SBP_Mean, c(118, 139, 161))",
    "rcs(BL_BMI, c(22, 27, 33))",
    "Pt_Cr",
    "DAB",
    "BL_AHP",
    "rcs(BL_Polypharmacy, c(0,3,6))",
    "rcs(BL_eGFR_CKD, c(56, 75, 89))",
    "rcs(LFT_PC1, c(-0.77, -0.19, 0.78))",
    "rcs(LFT_PC2, c(-1, 0, 0.87))",
    "rcs(BL_CesdOverall, c(0, 2, 8))",
    "BL_SmHis2",
    "BL_SmHis3",
    "rcs(BL_AlcWk, c(0, 3, 8))",
    "rcs(BL_PCS, c(37, 51, 57))",
    "rcs(BL_MCS, c(45, 58, 63))",
    "rcs(Pt_scoreIRSAD, c(911, 1010, 1100))",
    "CKD",
    "apoe_e4",
    "rcs(BL_3MS_OverallScore_C, c(88, 95, 99))",
    "rcs(BL_HVLT4, c(4, 8, 12))",
    "rcs(BL_Frailty_DAI50, c(0.04, 0.09, 0.18))",
    "EyesRate + I(EyesRate^2)"
  )

  # trt_model_interactions <- c(
  #   "AgeAtRand",
  #   "Gender",
  #   "Edu",
  #   "BLToneAvg_Better",
  #   "CrowdedRm",
  #   "Buzz",
  #   "QuietRm",
  #   "BL_3MS_OverallScore_C",
  #   "BL_Frailty_DAI50"
  # )

  # trt_model_interactions <-
  #   combinations(
  #     n = length(trt_model_interactions),
  #     r = 2,
  #     v = trt_model_interactions,
  #     repeats.allowed = FALSE
  #   ) |>
  #   as.data.table()
  #
  # trt_model_interactions <- paste(
  #   trt_model_interactions$V1,
  #   trt_model_interactions$V2,
  #   sep = ":")

  trt_form <- as.formula(
    paste(
      "A ~ 1",
      paste(treatment_covars, collapse = " + "),
      # paste(trt_model_interactions, collapse = " + "),
      sep = " + "
    )
  )

  return(trt_form)
}

## MARS formulas

get_outcome_formula_mars <- function() {
  Y ~
    A +
      AgeAtRand +
      Gender +
      Racial +
      Edu +
      Buzz +
      QuietRm +
      CrowdedRm +
      HrsSleep +
      PresPhysAct +
      SocialEngagement +
      Income +
      S1_pTau181 +
      S1_Abeta42_40 +
      S1_GFAP +
      S1_NFlight +
      risk_score +
      S3_Time +
      BLToneAvg_Better +
      BL_SBP_Mean +
      BL_BMI +
      Pt_Cr +
      DAB +
      BL_AHP +
      BL_Polypharmacy +
      BL_eGFR_CKD +
      LFT_PC1 +
      LFT_PC2 +
      BL_CesdOverall +
      BL_SmHis2 +
      BL_SmHis3 +
      BL_AlcWk +
      BL_PCS +
      BL_MCS +
      Pt_scoreIRSAD +
      CKD +
      apoe_e4 +
      BL_3MS_OverallScore_C +
      BL_HVLT4 +
      BL_Frailty_DAI50 +
      EyesRate
}

get_treatment_formula_mars <- function() {
  A ~
    AgeAtRand +
      Gender +
      Racial +
      Edu +
      Buzz +
      QuietRm +
      CrowdedRm +
      HrsSleep +
      PresPhysAct +
      SocialEngagement +
      Income +
      S1_pTau181 +
      S1_Abeta42_40 +
      S1_GFAP +
      S1_NFlight +
      risk_score +
      S3_Time +
      BLToneAvg_Better +
      BL_SBP_Mean +
      BL_BMI +
      Pt_Cr +
      DAB +
      BL_AHP +
      BL_Polypharmacy +
      BL_eGFR_CKD +
      LFT_PC1 +
      LFT_PC2 +
      BL_CesdOverall +
      BL_SmHis2 +
      BL_SmHis3 +
      BL_AlcWk +
      BL_PCS +
      BL_MCS +
      Pt_scoreIRSAD +
      CKD +
      apoe_e4 +
      BL_3MS_OverallScore_C +
      BL_HVLT4 +
      BL_Frailty_DAI50 +
      EyesRate
}

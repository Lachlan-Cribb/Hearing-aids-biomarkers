### mice LASSO PMM function ###

mice.impute.lasso.pmm <- function(y, ry, x, wy = NULL, nfolds = 5, ...) {
  if (is.null(wy)) {
    wy <- !ry
  }
  x_glmnet <- cbind(1, x)
  xobs <- x_glmnet[ry, , drop = FALSE]
  xmis <- x[wy, ]
  yobs <- y[ry]
  cv_lasso <- glmnet::cv.glmnet(
    x = xobs,
    y = yobs,
    family = "gaussian",
    nfolds = nfolds,
    alpha = 1
  )
  glmnet_coefs <- as.matrix(coef(cv_lasso, s = "lambda.min"))[, 1]
  AS <- which((glmnet_coefs != 0)[-1])
  forced_vars <- c(
    "Y3M_HearingAid",
    "Y3M_HearingAidUse",
    "AgeAtRand",
    "Gender",
    "Racial",
    "Edu",
    "Buzz",
    "QuietRm",
    "CrowdedRm",
    "SupplementUse",
    "HrsSleep",
    "PresPhysAct",
    "SocialEngagement",
    "Income",
    "S1_pTau181",
    "S1_Abeta42_40",
    "S1_GFAP",
    "S1_NFlight",
    "risk_score",
    "S3_Time",
    "S3_pTau181",
    "S3_Abeta42_40",
    "S3_GFAP",
    "S3_NFlight",
    "BLToneAvg_Better",
    "BL_SBP_Mean",
    "BL_BMI",
    "Pt_Cr",
    "DAB",
    "BL_AHP",
    "BL_Polypharmacy",
    "BL_eGFR_CKD",
    "LFT_PC1",
    "LFT_PC2",
    "BL_CesdOverall",
    "BL_SmHis2",
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
    "EyesRate",
    "Y3M_HearingAid_by_S1_pTau181",
    "Y3M_HearingAid_by_S1_Abeta42_40",
    "Y3M_HearingAid_by_S1_GFAP",
    "Y3M_HearingAid_by_S1_NFlight",
    "Y3M_HearingAidUse_by_S1_pTau181",
    "Y3M_HearingAidUse_by_S1_Abeta42_40",
    "Y3M_HearingAidUse_by_S1_GFAP",
    "Y3M_HearingAidUse_by_S1_NFlight",
    "Y3M_HearingAid_by_AgeAtRand",
    "Y3M_HearingAidUse_by_AgeAtRand",
    "Y3M_HearingAid_by_Gender",
    "Y3M_HearingAidUse_by_Gender",
    "Y3M_HearingAid_by_BLToneAvg_Better",
    "Y3M_HearingAidUse_by_BLToneAvg_Better",
    "Y3M_HearingAid_by_apoe_e4",
    "Y3M_HearingAidUse_by_apoe_e4",
    "Y3M_HearingAid_by_BL_3MS_OverallScore_C",
    "Y3M_HearingAidUse_by_BL_3MS_OverallScore_C",
    "Y3M_HearingAid_by_risk_score",
    "Y3M_HearingAidUse_by_risk_score"
  )
  forced <- which(names(glmnet_coefs)[-1] %in% forced_vars)
  AS <- unique(c(AS, forced))
  xas <- x_glmnet[, AS, drop = FALSE]
  vec <- mice.impute.pmm(y = y, ry = ry, x = xas, wy = wy, ...)
  vec
}

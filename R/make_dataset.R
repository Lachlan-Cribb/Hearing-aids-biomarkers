## Function to prepare data

prepare_data <- function() {
  # read ALSOP medical baseline data

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

  # read ALSOP medical 3 year data

  alsop_3y <-
    read_excel(
      here(
        "..",
        "..",
        "Data",
        "ALSOP Year 3",
        "ticket_5272_CC_ALSOP Medical Year 3 Dataset_22.04.2020_SafeHavenID.xlsx"
      )
    )

  # Read baseline ALSOP social

  alsop_social <-
    fread(
      here(
        "..",
        "..",
        "Data",
        "ALSOP Baseline Data Set",
        "ALSOP Baseline Social",
        "CSV",
        "ALSOP_SocialBaseline_CompleteDataSet_SafehavenID.csv"
      )
    )

  ## ALSOP Days since randomisation

  dsr <- fread(here("..", "..", "Data", "ALSOP_DSR.csv"))

  # merge datasets

  alsop <- merge(alsop_bl, alsop_3y, by = "Safehaven", all = TRUE)

  alsop <- merge(alsop, alsop_social, by = "Safehaven", all = TRUE)

  alsop <- merge(alsop, dsr, by = "Safehaven", all = TRUE)

  ### Create new variables

  # Sum score for social engagement

  alsop$SocialEngagement <-
    rowSums(
      alsop |>
        select(RelHearFrm:FrndHelp) |>
        select(-starts_with("C_"))
    )

  # Supplement use

  alsop$SupplementUse <- rowSums(alsop |> select(FishOil:OtherHerb))

  ## Hearing aid use - make 1 (never) if never prescribed

  alsop[,
    HearingAidUse := ifelse(
      HearingAid == 0 & (HearingAidUse < 2 | is.na(HearingAidUse)),
      1,
      HearingAidUse
    )
  ]

  alsop[,
    Y3M_HearingAidUse := ifelse(
      Y3M_HearingAid == 0 & (Y3M_HearingAidUse < 2 | is.na(Y3M_HearingAidUse)),
      1,
      Y3M_HearingAidUse
    )
  ]

  ## Hearing aids - make 1 (prescribed) if missing & hearingaiduse > never
  ## Note: very rare

  alsop[, HearingAid := ifelse(alsop$HearingAidUse > 1, 1, alsop$HearingAid)]

  alsop[,
    Y3M_HearingAid := ifelse(
      alsop$Y3M_HearingAidUse > 1,
      1,
      alsop$Y3M_HearingAid
    )
  ]

  ## Cochlear implant - collapse one ear and both ears

  alsop[,
    CochlearImplnt := ifelse(alsop$CochlearImplnt == 2, 1, alsop$CochlearImplnt)
  ]

  ## Select required ALSOP variables

  alsop_vars <-
    alsop[, .(
      Safehaven,
      Y3M_ALSOP_DSR,
      CochlearImplnt,
      HearingAid,
      HearingAidUse,
      EyesRate,
      HearingProbs,
      HearingProbs_2,
      Buzz,
      QuietRm,
      CrowdedRm,
      SupplementUse,
      HrsSleep,
      PresPhysAct,
      SocialEngagement,
      Income,
      Y3M_HearingProbs,
      Y3M_HearingProbs_1,
      Y3M_HearingAid,
      Y3M_HearingAidUse,
      Y3M_Buzz,
      Y3M_QuietRm_1,
      Y3M_QuietRm_2,
      Y3M_CrowdedRm
    )]

  #### IRSAD and CKD ####

  derived <- fread(
    here(
      "..",
      "..",
      "shared",
      "Resource-XT04",
      "CSV Version",
      "SectionI1_DerivedBaselineVariables_XT04_v1_SafeHavenID.csv"
    )
  )

  derived_vars <- derived[, .(Safehaven, Pt_scoreIRSAD, CKD)]

  #### Hearing substudy data ####

  # Read hearing data

  hear <- read_excel(
    here(
      "..",
      "..",
      "Data",
      "Baseline_Hearingv2",
      "Safehaven",
      "Baseline_Hearing_SectionA_v2.xlsx"
    )
  )

  hear2 <- fread(
    here(
      "..",
      "..",
      "Data",
      "Longitudinal_Hearing",
      "Safehaven",
      "SectionAA Dataset_Safehaven.csv"
    )
  )

  hear <- merge(hear, hear2, by = "Safehaven", all = TRUE)

  setDT(hear)

  ## select hearing vars ##

  hear[,
    BLToneAvg_Better := ifelse(
      BLToneAvg_Left < BLToneAvg_Right,
      BLToneAvg_Left,
      BLToneAvg_Right
    )
  ]

  hear[,
    Y3MToneAvg_Better := ifelse(
      HV3ToneAvg_Left < HV3ToneAvg_Right,
      HV3ToneAvg_Left,
      HV3ToneAvg_Right
    )
  ]

  hear_vars <- hear[, list(Safehaven, BLToneAvg_Better, Y3MToneAvg_Better)]

  #### ASPREE data ####

  ### Gen demo

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

  # Collapse racial categories

  demo[, Racial := ifelse(demo$Racial == 1, 0, 1)]

  demo_vars <- demo[, .(Safehaven, AgeAtRand, Gender, Racial, Edu)]

  ### Alcohol and smoking

  alc <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionA2_AlcSmHis_XT06_v1_SafeHavenID.csv"
    )
  )

  # create typical weekly alcohol variable

  alc[, BL_AlcWk := ifelse(BL_Alc %in% c(1, 2), BL_AlcDays * BL_AlcNo, 0)]

  alc_vars <- alc[, .(Safehaven, C_BL_SmHis, BL_SmHis, BL_AlcWk)]

  ### Baseline medical history

  history <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionB1_MedicalHistory_XT06_v1_SafeHavenID.csv"
    )
  )

  # cancer, diabetes

  history[, DAB := ifelse(history$DAB == 1, 1, 0)]

  history[, Pt_Cr := ifelse(history$Pt_Cr == 1, 1, 0)]

  history_vars <- history[, .(Safehaven, Pt_Cr, DAB)]

  ### Longitudinal physical assessments

  # BMI and BP

  phys <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionB2_PhysicalExam_XT06_v1_SafeHavenID.csv"
    )
  )

  # phys vars

  phys_vars <-
    phys[, .SD, .SDcols = patterns("Safehaven|^BL_BMI|^BL_SBP_Mean")]

  ### Liver and kidney function

  # eGFR

  path <- fread(here(
    "..",
    "..",
    "Shared",
    "Resource-XT04",
    "CSV Version",
    "SectionB4_LongitudinalPathology_XT04_v1_SafeHavenID.csv"
  ))

  path <- path[, .(Safehaven, BL_eGFR_CKD)]

  ## Liver function tests

  ck <- read_excel(here(
    "..",
    "..",
    "Data",
    "Clinical Measures (Alfred Clinical Chemistry) data set",
    "Clinical_Measures_Baseline_Data_Set_v2.xlsx"
  ))

  setDT(ck)

  ck[, GGT0 := as.numeric(GGT0)]

  # first two PCs of LFTs

  pc <- principal(ck[, .(GGT0, ALT0, Bilirubin0, AST0, ALP0)], nfactors = 2)

  ck$LFT_PC1 <- pc$scores[, 1]

  ck$LFT_PC2 <- pc$scores[, 2]

  ck <- ck[, .(Safehaven, LFT_PC1, LFT_PC2)]

  ### antihypertensive use

  conmeds <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT04",
      "CSV Version",
      "SectionB5_ConMeds_XT04_v1_SafeHavenID.csv"
    )
  )

  ahp_ids <- conmeds[grepl("^C02", ATC) & BL_MedPres == 1, .(Safehaven)]

  ahp <- demo[, .(Safehaven)]
  ahp <- ahp[, BL_AHP := ifelse(Safehaven %in% ahp_ids$Safehaven, 1, 0)]

  ### Polypharmacy

  get_polypharm <- function(vars, visit, data) {
    out <- data |>
      filter(if_any(vars, ~ . == 1)) |>
      group_by(Safehaven) |>
      tally(name = paste(visit, "Polypharmacy", sep = "_"))

    out <- merge(polypharm, out, by = "Safehaven", all.x = TRUE)

    return(out)
  }

  polypharm <- demo[, .(Safehaven)]

  polypharm <- get_polypharm(c("Baseline_MedPres"), "BL", conmeds)

  polypharm[,
    BL_Polypharmacy := ifelse(is.na(BL_Polypharmacy), 0, BL_Polypharmacy)
  ]

  polypharm <-
    get_polypharm(c("AV6_MedOn"), "AV6", conmeds)

  polypharm <-
    get_polypharm(c("AV9_MedOn"), "AV9", conmeds)

  # med vars

  conmed_vars <- merge(ahp, polypharm, by = "Safehaven", all = TRUE)

  #### Cognitive outcomes ####

  ### 3MS

  ms <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionC1_3MS_XT06_v1_SafeHavenID.csv"
    )
  )

  ## transform phone 3MS to be on same scale with
  ## regular 3MS, and then amalgamate

  transform_ms <- function(visit, data) {
    data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]] <-
      data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]] / 0.74

    out <-
      ifelse(
        is.na(data[[paste(visit, "3MS_OverallScore", sep = "_")]]) &
          !is.na(data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]]),
        data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]],
        data[[paste(visit, "3MS_OverallScore", sep = "_")]]
      )
    return(out)
  }

  ms_transformed <-
    lapply(
      c("BL", paste("AV", c(3, 6, 9), sep = "")),
      transform_ms,
      data = ms
    ) |>
    as.data.table()

  names(ms_transformed) <-
    paste(
      c("BL", paste("AV", c(3, 6, 9), sep = "")),
      "3MS_OverallScore_C",
      sep = "_"
    )

  ms <- cbind(ms, ms_transformed)

  ms_vars <-
    ms[, .SD, .SDcols = patterns("Safehaven|OverallScore_C")]

  ### Other cognitive tests

  cog <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionC2_OtherCogs_XT06_v1_SafeHavenID.csv"
    )
  )

  ## HVLT

  cog_vars <- cog[, .SD, .SDcols = patterns("Safehaven|BL_HVLT4|^AV.*_HVLT4$")]
  cog_vars <- cog_vars[, .SD, .SDcols = patterns("Safehaven|BL|AV6|AV9")]

  #### Other outcomes ####

  ### Death

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

  for (i in 1:11) {
    death[[paste0("AV", i, "_Death")]] <- death_function(death, i)
  }

  death$AV12_Death <- ifelse(death$Death == 1, 1, death$AV11_Death)

  death_vars <- death[, .SD, .SDcols = patterns("Safehaven|^AV.*_Death$")]

  ### Incident dementia

  dem_function <- function(data, visit) {
    ifelse(
      data$Dementia == 1 & data$Dementia_DSR < (365 * visit),
      1,
      ifelse(data$Dementia == 0 & data$Dementia_DSR < (365 * visit), NA, 0)
    )
  }

  # add dementia

  for (i in 1:11) {
    death[[paste0("AV", i, "_Dem")]] <- dem_function(death, i)
  }

  death$AV11_Dem <- ifelse(death$Dementia == 1, 1, death$AV11_Dem)

  dem_vars <- death[, .SD, .SDcols = patterns("Safehaven|^AV.*Dem$")]

  ### Cancer

  death[, Cancer_DSR := ifelse(Cancer == 1, Cancer_DSR, Death_DSR)]

  cancer_function <- function(data, visit) {
    ifelse(
      data$Cancer == 1 & data$Cancer_DSR < (365 * visit),
      1,
      ifelse(data$Cancer == 0 & data$Cancer_DSR < (365 * visit), NA, 0)
    )
  }

  # add cancer

  for (i in c(3, 6, 9)) {
    death[[paste0("AV", i, "_Cancer")]] <- cancer_function(death, i)
  }

  cancer_vars <- death[, .SD, .SDcols = patterns("Safehaven|^AV.*Cancer")]

  #### CVD ####

  cvd <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionF5_DerivedEndpoints_XT06_v1.csv"
    )
  )

  ## Create CVD variable for each visit

  cvd_function <- function(data, visit) {
    if (visit == 1) {
      data[["AV1_CVD"]] <-
        ifelse(
          data$CVD_protocol == 1 &
            data$CVD_protocol_DSR < 365,
          1,
          0
        )
    } else {
      data[[paste0("AV", visit, "_CVD")]] <-
        ifelse(
          data$CVD_protocol == 1 &
            data$CVD_protocol_DSR < (365 * visit),
          1,
          0
        )
      data[[paste0("AV", visit, "_CVD")]] <-
        ifelse(
          data$CVD_protocol == 0 &
            data$CVD_protocol_DSR < (365 * visit),
          NA,
          data[[paste0("AV", visit, "_CVD")]]
        )
    }
    return(data[[paste0("AV", visit, "_CVD")]])
  }

  # add CVD to data

  for (i in c(3, 6, 9)) {
    cvd[[paste0("AV", i, "_CVD")]] <- cvd_function(cvd, i)
  }

  cvd_vars <- cvd[, .SD, .SDcols = patterns("Safehaven|AV.*_CVD")]

  ### CESD

  cesd <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionE2_CESD_XT06_v1_SafeHavenID.csv"
    )
  )

  cesd_vars <- cesd[, .SD, .SDcols = patterns("Safehaven|CesdOverall")]
  cesd_vars <- cesd_vars[, .SD, .SDcols = !patterns("Reassess")]
  cesd_vars <- cesd_vars[, .SD, .SDcols = patterns("Safehaven|BL|AV6|AV9")]

  ### SF-12

  sf <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionE3_SF12_XT06_v1_SafeHavenID.csv"
    )
  )

  sf <- as_tibble(sf)

  ## Calculate mental and physical component

  visits <- c("BL", paste0("AV", 1:11))

  for (i in visits) {
    sf <- sf12(i, sf)
  }

  setDT(sf)

  sf_vars <- sf[, .SD, .SDcols = patterns("Safehaven|MCS|PCS")]
  sf_vars <- sf_vars[, .SD, .SDcols = patterns("Safehaven|BL|AV6|AV9")]

  ### Frailty

  frailty <- read_dta(
    here(
      "..",
      "..",
      "Data",
      "frailty",
      "3. Final_Aung_Edited_FrailtyVariables_Only_Longitudinal_XT04_June_20240611.dta"
    )
  )

  setDT(frailty)

  frailty_vars <- frailty[, .SD, .SDcols = patterns("Safehaven|DAI50$")]
  frailty_vars <- frailty_vars[,
    .SD,
    .SDcols = patterns("Safehaven|BL|AV6|AV9")
  ]

  ### APOE

  apoe <- read_excel(
    here(
      "..",
      "..",
      "shared",
      "P552",
      "ticket_12797_SNP_TPS_APOE_no_Popul_final_dataset_withEthincity_n19114_msid_SafehavenID.xlsx"
    )
  )

  apoe$apoe_e4 <-
    case_when(
      apoe$APOE_geno == "e1/e3:e2/e4" ~ 1,
      apoe$APOE_geno == "e1/e4" ~ 1,
      apoe$APOE_geno == "e2/e2" ~ 0,
      apoe$APOE_geno == "e2/e3" ~ 0,
      apoe$APOE_geno == "e3/e3" ~ 0,
      apoe$APOE_geno == "e3/e4" ~ 1,
      apoe$APOE_geno == "e4/e4" ~ 2,
      TRUE ~ NA
    )

  setDT(apoe)

  apoe <- apoe[, .(Safehaven, apoe_e4)]

  ### Plasma biomarkers

  bl_biomarkers <- fread(here(
    "..",
    "..",
    "Data",
    "Dementia Plasma Biomarkers data set",
    "Dementia Plasma Biomarkers data set v3_Section A.csv"
  ))

  # set measurements with reported tech error to missing

  bl_biomarkers[,
    S1_pTau181 := ifelse(
      S1_pTau181_TechErr %in% c(1, 2),
      NA,
      S1_pTau181
    )
  ]

  bl_biomarkers[,
    S1_Abeta40 := ifelse(
      S1_Abeta40_TechErr %in% c(1, 2),
      NA,
      S1_Abeta40
    )
  ]

  bl_biomarkers[,
    S1_Abeta42 := ifelse(
      S1_Abeta42_TechErr %in% c(1, 2),
      NA,
      S1_Abeta42
    )
  ]

  bl_biomarkers[,
    S1_GFAP := ifelse(
      S1_GFAP_TechErr %in% c(1, 2),
      NA,
      S1_GFAP
    )
  ]

  bl_biomarkers[,
    S1_NFlight := ifelse(
      S1_NFlight_TechErr %in% c(1, 2),
      NA,
      S1_NFlight
    )
  ]

  bl_biomarkers[, S1_Abeta42_lower := ifelse(S1_Abeta42_TechErr == 3, 1, 0)]

  bl_biomarkers[, S1_Abeta42_40 := S1_Abeta42 / S1_Abeta40]

  bl_biomarkers <- bl_biomarkers[, .(
    Safehaven,
    S1_pTau181,
    S1_Abeta40,
    S1_Abeta42,
    S1_Abeta42_40,
    S1_Abeta42_lower,
    S1_GFAP,
    S1_NFlight
  )]

  ## Follow-up biomarkers

  fu_biomarkers <- fread(here(
    "..",
    "..",
    "Shared",
    "P552",
    "Dementia Plasma Biomarkers data set interim_v1_Section C.csv"
  ))

  # set measurements with reported tech error to missing

  fu_biomarkers[,
    S3_pTau181 := ifelse(
      S3_pTau181_TechErr %in% c(1, 2),
      NA,
      S3_pTau181
    )
  ]

  fu_biomarkers[,
    S3_Abeta40 := ifelse(
      S3_Abeta40_TechErr %in% c(1, 2),
      NA,
      S3_Abeta40
    )
  ]

  fu_biomarkers[,
    S3_Abeta42 := ifelse(
      S3_Abeta42_TechErr %in% c(1, 2),
      NA,
      S3_Abeta42
    )
  ]

  fu_biomarkers[,
    S3_GFAP := ifelse(
      S3_GFAP_TechErr %in% c(1, 2),
      NA,
      S3_GFAP
    )
  ]

  fu_biomarkers[,
    S3_NFlight := ifelse(
      S3_NFlight_TechErr %in% c(1, 2),
      NA,
      S3_NFlight
    )
  ]

  fu_biomarkers[, S3_Abeta42_lower := ifelse(S3_Abeta42_TechErr == 3, 1, 0)]

  fu_biomarkers[, S3_Abeta42_40 := S3_Abeta42 / S3_Abeta40]

  fu_biomarkers <- fu_biomarkers[, .(
    Safehaven,
    S3_pTau181,
    S3_Abeta40,
    S3_Abeta42,
    S3_Abeta42_40,
    S3_Abeta42_lower,
    S3_GFAP,
    S3_NFlight
  )]

  ## FOllow-up biomarkers days since randomisation

  fu_dsr <- fread(here(
    "..",
    "..",
    "Shared",
    "P552",
    "ARDL_S3_PlasmaCollection_DSR_interim_v1.csv"
  ))

  fu_dsr <- merge(
    fu_dsr,
    alsop[, list(Safehaven, Y3M_ALSOP_DSR)],
    by = "Safehaven",
    all.x = TRUE
  )

  fu_dsr$S3_Time <- fu_dsr$S3_Plasma_Collection_DSR - fu_dsr$Y3M_ALSOP_DSR
  fu_dsr <- fu_dsr[, list(Safehaven, S3_Time)]

  ## Skin cancer screening negative treatment control ##

  cancer_screen <- fread(here(
    "..",
    "..",
    "Shared",
    "Resource-XT06",
    "CSV Version",
    "SectionB3_CancerScreening_XT06_v1_SafeHavenID.csv"
  ))

  cancer_screen <- cancer_screen[, list(Safehaven, AV3_Skn_Phy)]

  #### Create final dataframe ####

  ## Join created df's

  df_out <-
    merge(demo_vars, alsop_vars, by = "Safehaven", all.y = TRUE) |>
    merge(bl_biomarkers, by = "Safehaven", all.x = TRUE) |>
    merge(fu_dsr, by = "Safehaven", all.x = TRUE) |>
    merge(fu_biomarkers, by = "Safehaven", all.x = TRUE) |>
    merge(hear_vars, by = "Safehaven", all.x = TRUE) |>
    merge(phys_vars, by = "Safehaven", all.x = TRUE) |>
    merge(history_vars, by = "Safehaven", all.x = TRUE) |>
    merge(conmed_vars, by = "Safehaven", all.x = TRUE) |>
    merge(path, by = "Safehaven", all.x = TRUE) |>
    merge(ck, by = "Safehaven", all.x = TRUE) |>
    merge(cesd_vars, by = "Safehaven", all.x = TRUE) |>
    merge(alc_vars, by = "Safehaven", all.x = TRUE) |>
    merge(sf_vars, by = "Safehaven", all.x = TRUE) |>
    merge(derived_vars, by = "Safehaven", all.x = TRUE) |>
    merge(apoe, by = "Safehaven", all.x = TRUE) |>
    merge(ms_vars, by = "Safehaven", all.x = TRUE) |>
    merge(cog_vars, by = "Safehaven", all.x = TRUE) |>
    merge(dem_vars, by = "Safehaven", all.x = TRUE) |>
    merge(cvd_vars, by = "Safehaven", all.x = TRUE) |>
    merge(cancer_vars, by = "Safehaven", all.x = TRUE) |>
    merge(death_vars, by = "Safehaven", all.x = TRUE) |>
    merge(frailty_vars, by = "Safehaven", all.x = TRUE) |>
    merge(cancer_screen, by = "Safehaven", all.x = TRUE)

  ## Remove any unneeded variables

  df_out <- df_out |>
    select(
      -contains("DeathSub"),
      -contains("DeathSubType"),
      -contains("_Detail"),
      -contains("EventID"),
      -contains("Subclass"),
      -contains("MetType"),
      -contains("Stage"),
      -contains("Antihyp"),
      -starts_with("C_"),
      -starts_with("AV13")
    )

  ## Make 'don't know' item responses missing data

  df_out[, CochlearImplnt := ifelse(CochlearImplnt == 9, NA, CochlearImplnt)]

  df_out[, HearingProbs := ifelse(HearingProbs == 9, NA, HearingProbs)]

  df_out[, HearingProbs_2 := ifelse(HearingProbs_2 == 9, NA, HearingProbs_2)]

  df_out[,
    Y3M_HearingProbs := ifelse(
      Y3M_HearingProbs == 9,
      NA,
      Y3M_HearingProbs
    )
  ]

  df_out[,
    Y3M_HearingProbs_1 := ifelse(
      Y3M_HearingProbs_1 == 9,
      NA,
      Y3M_HearingProbs_1
    )
  ]

  ## set variable types

  df_out[, BL_SmHis := as.factor(BL_SmHis)]

  df_out[, Gender := Gender - 1]

  return(df_out)
}

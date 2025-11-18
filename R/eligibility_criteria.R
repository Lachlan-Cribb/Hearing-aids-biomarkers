## apply eligiblity criteria

# xt04 survival data 

get_xt04_survival <- function(data){
  xt04_survival <- fread(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT04",
      "CSV Version",
      "SectionF3_SecondaryEndpoints_XT04_v2_SafeHavenID.csv"
    ))
    
  xt04_survival <- xt04_survival[, list(Safehaven, Death)]
  setnames(xt04_survival, c("Safehaven", "Death_xt04"))
}

# Condition on survival until time of biomarker measurement 

condition_survival <- function(data, xt04_data) {
  lapply(data, function(.x) {
    merged <- merge(.x, xt04_data, by = "Safehaven", all.x = TRUE)
    merged[!is.na(S3_Abeta42_lower) |
             (is.na(S3_Abeta42_lower) & Death_xt04 == 0), ]
  })
}

# remove prevalent users

remove_prevalent_users <- function(data){
  data <- data[(HearingAid == 0 | is.na(HearingAid)) & 
         (CochlearImplnt == 0 | is.na(CochlearImplnt)),]
  data[, `:=`(HearingAid = NULL, HearingAidUse = NULL, CochlearImplnt=NULL)]
  return(data)
}

# Remove death or dementia before start of follow-up

remove_death_dem <- function(data) {
  data <- data[AV3_Death == 0  & AV3_Dem == 0, ]
  data <- data[
    , 
    .SD, 
    .SDcols = !patterns("AV1_Dem|AV2_Dem|AV3_Dem|AV1_Death|AV2_Death|AV3_Death")
  ]
  return(data)
}

# eligibility criteria for analysis sample
  
main_eligibility <- function(data, selfrated_hearing = TRUE){
  if(isTRUE(selfrated_hearing)){
    
    data <- lapply(data, function(.x) .x[HearingProbs == 1,])
    
    return(data)
    
  } else {
    
    data <- lapply(data, function(.x) { 
          .x[BLToneAvg_Better >= 30 & BLToneAvg_Better < 70,]
          }
    )
    
    return(data)
  }
}

# eligibility criteria for external sample (for risk model)

external_eligibility <- function(data) {
  data <- data[HearingProbs == 0 | HearingAid == 1,]
  return(data)
}


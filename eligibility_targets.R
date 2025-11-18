eligibility_targets <- list(
  tar_target(xt04_survival, get_xt04_survival(xt04_data)),
  # primary analysis eligibility criteria
  tar_target(
    df_elig,
    main_eligibility(imp_list, selfrated_hearing = TRUE),
    pattern = map(imp_list)
  ),
  # sensitivity analysis objective hearing impairment eligibility criteria
  tar_target(
    df_elig_sens ,
    main_eligibility(imp_list, selfrated_hearing = FALSE),
    pattern = map(imp_list)
  ),
  # condition on survival until approximate time of biomarker measurement
  tar_target(
    df_main, 
    condition_survival(df_elig, xt04_survival), 
    pattern = map(df_elig)),
  
  tar_target(
    df_hear, 
    condition_survival(df_elig_sens, xt04_survival), 
    pattern = map(df_elig_sens))
)
risk_model_targets <- list(
  # estimate parameters of dementia risk model
  tar_target(dem_risk_model, get_dem_risk_model(final_visit = 10, V = 10)),

  # estimate parameters of death risk model
  tar_target(death_risk_model, get_death_risk_model(final_visit = 10, V = 10)),

  # Add estimated risk score to main dataset
  tar_target(df2, add_biomarker_risk(df, dem_risk_model, death_risk_model))
)

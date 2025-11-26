sensitivity_hearing_eligibility_targets <- list(
  #### TMLE ####
  # TMLE for binary exposure (HA prescription)
  tar_target(
    tmle_binary_hear,
    get_tmle_bin(df_hear, "Y3M_HearingAid", outcomes),
    pattern = cross(df_hear, outcomes),
    iteration = "list"
  ),

  # Get confidence intervals for tmle estimates
  tar_target(
    tmle_binary_summary_hear,
    get_bootstrap_intervals_tmle(tmle_binary_hear, outcomes),
    pattern = map(outcomes)
  ),

  # TMLE for ordinal exposure (HA use)
  tar_target(
    tmle_ordinal_hear,
    get_tmle_cat(df_hear, "Y3M_HearingAidUse", outcomes),
    pattern = cross(df_hear, outcomes),
    iteration = "list"
  ),

  # Get confidence intervals for tmle estimates
  tar_target(
    tmle_ordinal_summary_hear,
    get_bootstrap_intervals_tmle(tmle_ordinal_hear, outcomes),
    pattern = map(outcomes)
  ),

  #### QUANTILE EFFECTS ####
  # quantile effects for binary exposure
  tar_target(
    quantile_binary_hear,
    get_quantile_bin(df_hear, "Y3M_HearingAid", outcomes, quantile = 0.9),
    pattern = cross(df_hear, outcomes)
  ),

  # Get confidence intervals for quantile estimates
  tar_target(
    quantile_binary_summary_hear,
    get_bootstrap_intervals_quantiles(quantile_binary_hear, outcomes),
    pattern = map(outcomes)
  ),

  # quantile effects for binary exposure
  tar_target(
    quantile_ordinal_hear,
    get_quantile_cat(df_hear, "Y3M_HearingAidUse", outcomes, quantile = 0.9),
    pattern = cross(df_hear, outcomes)
  ),

  # Get confidence intervals for quantile estimates
  tar_target(
    quantile_ordinal_summary_hear,
    get_bootstrap_intervals_quantiles(quantile_ordinal_hear, outcomes),
    pattern = map(outcomes)
  )
)

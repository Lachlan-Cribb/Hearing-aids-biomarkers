sensitivity_mars_targets <- list(
  # TMLE for binary exposure (HA prescription)
  tar_target(
    tmle_binary_mars,
    get_tmle_bin_mars(df_main, "Y3M_HearingAid", outcomes),
    pattern = cross(df_main, outcomes)
  ),

  # Get confidence intervals for tmle estimates
  tar_target(
    tmle_binary_summary_mars,
    get_bootstrap_intervals_mars(tmle_binary_mars, outcomes),
    pattern = map(outcomes)
  ),

  # TMLE for ordinal exposure (HA use)
  tar_target(
    tmle_ordinal_mars,
    get_tmle_cat_mars(df_main, "Y3M_HearingAidUse", outcomes),
    pattern = cross(df_main, outcomes)
  ),

  # Get confidence intervals for tmle estimates
  tar_target(
    tmle_ordinal_summary_mars,
    get_bootstrap_intervals_mars(tmle_ordinal_mars, outcomes),
    pattern = map(outcomes)
  )
)

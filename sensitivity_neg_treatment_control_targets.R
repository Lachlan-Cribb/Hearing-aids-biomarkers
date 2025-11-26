sensitivity_neg_treatment_control_targets <- list(
  tar_target(
    tmle_binary_ntc,
    get_tmle_bin(df_main, "AV3_Skn_Phy", outcomes),
    pattern = cross(df_main, outcomes),
    iteration = "list"
  ),

  tar_target(
    tmle_binary_summary_ntc,
    get_bootstrap_intervals_tmle(tmle_binary_ntc, outcomes),
    pattern = map(outcomes)
  )
)

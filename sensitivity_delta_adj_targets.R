sensitivity_delta_adj_targets <- list(
  tar_target(data_ltfu, add_ltfu(df_main), pattern = map(df_main)),
  tar_map(
    values = tibble(
      delta_name = c("05", "25"),
      delta_value = c(-0.05, -0.25)
    ),
    names = delta_name,
    tar_target(
      df_delta,
      add_delta(data_ltfu, delta_value),
      pattern = map(data_ltfu)
    ),
    tar_target(
      tmle_binary_delta,
      get_tmle_bin(df_delta, "Y3M_HearingAid", outcomes),
      pattern = cross(df_delta, outcomes),
      iteration = "list"
    ), # Get confidence intervals for tmle estimates
    tar_target(
      tmle_binary_summary_delta,
      get_bootstrap_intervals_tmle(tmle_binary_delta, outcomes),
      pattern = map(outcomes)
    ), # TMLE for ordinal exposure (HA use)
    tar_target(
      tmle_ordinal_delta,
      get_tmle_cat(df_delta, "Y3M_HearingAidUse", outcomes),
      pattern = cross(df_delta, outcomes),
      iteration = "list"
    ), # Get confidence intervals for tmle estimates
    tar_target(
      tmle_ordinal_summary_delta,
      get_bootstrap_intervals_tmle(tmle_ordinal_delta, outcomes),
      pattern = map(outcomes)
    )
  )
)

primary_analysis_targets <- list(
  # outcomes
  tar_target(
    outcomes,
    c("S3_pTau181", "S3_Abeta42_40", "S3_GFAP", "S3_NFlight")
  ),

  #### TMLE ####
  # TMLE for binary exposure (HA prescription)
  tar_target(
    tmle_binary,
    get_tmle_bin(df_main, "Y3M_HearingAid", outcomes),
    pattern = cross(df_main, outcomes),
    iteration = "list"
  ),

  # Get confidence intervals for tmle estimates
  tar_target(
    tmle_binary_summary,
    get_bootstrap_intervals_tmle(tmle_binary, outcomes),
    pattern = map(outcomes)
  ),

  # TMLE for ordinal exposure (HA use)
  tar_target(
    tmle_ordinal,
    get_tmle_cat(df_main, "Y3M_HearingAidUse", outcomes),
    pattern = cross(df_main, outcomes),
    iteration = "list"
  ),

  # Get confidence intervals for tmle estimates
  tar_target(
    tmle_ordinal_summary,
    get_bootstrap_intervals_tmle(tmle_ordinal, outcomes),
    pattern = map(outcomes)
  ),

  #### QUANTILE EFFECTS ####
  # quantile effects for binary exposure
  tar_target(
    quantile_binary,
    get_quantile_bin(df_main, "Y3M_HearingAid", outcomes, quantile = 0.9),
    pattern = cross(df_main, outcomes)
  ),

  # Get confidence intervals for quantile estimates
  tar_target(
    quantile_binary_summary,
    get_bootstrap_intervals_quantiles(quantile_binary, outcomes),
    pattern = map(outcomes)
  ),

  # quantile effects for binary exposure
  tar_target(
    quantile_ordinal,
    get_quantile_cat(df_main, "Y3M_HearingAidUse", outcomes, quantile = 0.9),
    pattern = cross(df_main, outcomes)
  ),

  # Get confidence intervals for quantile estimates
  tar_target(
    quantile_ordinal_summary,
    get_bootstrap_intervals_quantiles(quantile_ordinal, outcomes),
    pattern = map(outcomes)
  ),

  #### EFFECT MODIFICATION ####
  tar_target(
    effect_modifiers,
    c(
      "risk_score",
      "S1_pTau181",
      "S1_NFlight",
      "S1_GFAP",
      "S1_Abeta42_40",
      "apoe_e4",
      "BLToneAvg_Better",
      "BL_3MS_OverallScore_C"
    )
  ),

  tar_target(effect_modifier_levels, get_em_levels(df5)),

  tar_target(
    tmle_msm_binary,
    get_tmle_msm_bin(
      df_main,
      "Y3M_HearingAid",
      outcomes,
      effect_modifiers,
      effect_modifier_levels
    ),
    pattern = cross(df_main, outcomes, effect_modifiers)
  ),

  tar_target(
    tmle_msm_summary,
    get_bootstrap_intervals_msm(
      tmle_msm_binary,
      outcomes,
      effect_modifiers,
      effect_modifier_levels
    ),
    pattern = cross(outcomes, effect_modifiers)
  ),

  tar_map(
    plot_params,
    names = outcomes,
    tar_target(
      msm_plots,
      get_msm_plot(
        tmle_msm_summary,
        outcomes,
        effect_mods,
        ylims,
        ncol = 2,
        ybreaks
      )
    )
  ),

  tar_map(
    plot_params,
    names = outcomes,
    tar_target(
      msm_plots_wide,
      get_msm_plot(
        tmle_msm_summary,
        outcomes,
        effect_mods,
        ylims,
        ncol = 3,
        ybreaks
      )
    )
  )
)

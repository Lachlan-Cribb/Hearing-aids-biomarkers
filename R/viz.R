plot_params <- tibble::tibble(
  outcomes = c("S3_pTau181", "S3_Abeta42_40", "S3_GFAP", "S3_NFlight"),
  effect_mods = list(
    c("risk_score", 
      "S1_pTau181", 
      "apoe_e4", 
      "BLToneAvg_Better", 
      "BL_3MS_OverallScore_C"),
    c("risk_score", 
      "S1_Abeta42_40", 
      "apoe_e4", 
      "BLToneAvg_Better", 
      "BL_3MS_OverallScore_C"),
    c("risk_score", 
      "S1_GFAP", 
      "apoe_e4", 
      "BLToneAvg_Better", 
      "BL_3MS_OverallScore_C"),
    c("risk_score", 
      "S1_NFlight", 
      "apoe_e4", 
      "BLToneAvg_Better", 
      "BL_3MS_OverallScore_C")),
  ylims = list(
    c(17.5, 62.5),
    c(0.04, 0.09),
    c(75, 300),
    c(10, 60)),
  ybreaks = list(
    c(seq(20, 60, length.out = 5)),
    c(seq(0.04, 0.09, length.out = 6)),
    c(seq(75, 300, length.out = 6)),
    c(seq(10, 60, length.out = 6)))
)

get_msm_plot <- function(
    data, 
    outcomes, 
    effect_mods,
    ylims, 
    ncol,
    ybreaks){
  
  plots <- lapply(
    effect_mods, 
    msm_plot, 
    data = data, 
    outcomes = outcomes,
    ylims = ylims, 
    ybreaks = ybreaks,
    gcomp = FALSE)
  
  wrap_plots(plots, 
             ncol = ncol,
             guides = "collect",
             axes = "collect", 
             axes_titles = "collect") &
    theme(legend.position = "bottom")
}

msm_plot <- function(
    data, 
    outcomes, 
    effect_mod, 
    ylims, 
    ybreaks, 
    gcomp){
  
  # relabel effect modifiers
  data[, new_effect_modifier := fcase(
    effect_modifier == "risk_score", "Baseline dementia risk score",
    effect_modifier == "S1_pTau181", "Baseline pTau181 (pg/mL)",
    effect_modifier == "S1_NFlight", "Baseline NfL (pg/mL)",
    effect_modifier == "S1_GFAP", "Baseline GFAP (pg/mL)",
    effect_modifier == "S1_Abeta42_40", "Baseline Abeta42:Abeta40",
    effect_modifier == "apoe_e4",  "APOE e4 positivity",
    effect_modifier == "BLToneAvg_Better",  "Baseline heaing loss (db HL)",
    effect_modifier == "BL_3MS_OverallScore_C",  "Baseline 3MS overall score"
  )]
  
  # relabel outcomes
  data[, new_outcome := fcase(
    outcome == "S3_pTau181", "pTau181 (pg/mL)",
    outcome == "S3_Abeta42_40", "Abeta42:Abeta40",
    outcome == "S3_GFAP", "GFAP (pg/mL)",
    outcome == "S3_NFlight", "NfL (pg/mL)"
  )]
  
  data <- data[outcome == outcomes & effect_modifier == effect_mod, ]
  
  # exclude extreme ends of effect modifier
  if(!effect_mod == "apoe_e4"){
    data <- data[
      !effect_modifier_levels == max(effect_modifier_levels) & 
        !effect_modifier_levels == min(effect_modifier_levels),]
  }
  
  # G-computation or TMLE 
  if (!gcomp) {
    data <- data[!str_detect(parameter, "gcomp"), ]
    if ("Y2" %in% data$parameter) {
      data[, Treatment := fcase(
        parameter == "Y0", "Never use hearing aids",
        parameter == "Y1", "Rarely/sometimes use hearing aids",
        parameter == "Y2", "Often/always use hearing aids"
      )]
    } else {
      data[, Treatment := fcase(
        parameter == "Y0", "No hearing aid prescription",
        parameter == "Y1", "Hearing aid prescription")]
    }
  } else {
    data <- data[str_detect(parameter, "gcomp"), ]
    if ("Y2" %in% data$parameter) {
      data[, Treatment := fcase(
        parameter == "Y0_gcomp", "Never use hearing aids",
        parameter == "Y1_gcomp", "Rarely/sometimes use hearing aids",
        parameter == "Y2_gcomp", "Often/always use hearing aids"
      )]
    } else {
      data[, Treatment := fcase(
        parameter == "Y0_gcomp", "No hearing aid prescription",
        parameter == "Y1_gcomp", "Hearing aid prescription"
      )]
    }
  }
  
  colours <- if("Y2" %in% data$parameter){
    c("black", "#e66101", "#1f78b4")
  } else {
    c("#e66101", "#1f78b4")
  }
  
  ylab <- paste("Estimated mean", unique(data$new_outcome))
  
  if(!effect_mod == "apoe_e4"){
    plot <- ggplot(data, 
           aes(
             x = effect_modifier_levels, 
             y = estimate, 
             group = Treatment)) +
      geom_line(aes(colour = Treatment), linewidth = 1.5) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = Treatment), 
                  alpha = 0.15,
                  show.legend = FALSE) +
      labs(x = unique(data$new_effect_modifier), 
           y = ylab,
           group = "", 
           colour = "") +
      theme_default(base_size=14) +
      theme(
        legend.position = "bottom",
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5))
      ) +
      scale_colour_manual(values = colours) +
      scale_fill_manual(values = colours) +
      scale_y_continuous(limits = ylims, breaks = ybreaks)
  } else {
    data$effect_modifier_levels <- as.factor(data$effect_modifier_levels)
    levels(data$effect_modifier_levels) <- c("No", "Yes")
    plot <- ggplot(data, 
                   aes(
                     x = effect_modifier_levels, 
                     y = estimate, 
                     group = Treatment)) +
      geom_point(aes(colour = Treatment),
                 position = position_dodge(0.2),
                 show.legend = FALSE) + 
      geom_errorbar(aes(ymin = lower, ymax = upper, colour = Treatment), 
                    position = position_dodge(0.2),
                    width = 0.1,
                    linetype = "dashed",
                    show.legend = FALSE) +
      labs(x = unique(data$new_effect_modifier), 
           y = ylab,
           group = "",
           colour = "") +
      theme_default(base_size=14) +
      theme(
        legend.position = "bottom",
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5))
      ) +
      scale_colour_manual(values = colours) +
      scale_y_continuous(limits = ylims, breaks = ybreaks)
  }
  
  plot
}
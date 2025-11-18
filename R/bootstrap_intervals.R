## Bootstrap confidence intervals 

# Main analysis

get_bootstrap_intervals_tmle <- function(data, outcomes) {
  result <- rbindlist(lapply(data, function(.x) .x$results))
  
  result <- result[outcome == outcomes, ]

  params <- setdiff(names(result), c("B", "outcome", "exposure"))
  
  rbindlist(lapply(params, bootstrap_intervals, data = result))
}

# Quantile effects

get_bootstrap_intervals_quantiles <- function(data, outcomes) {

  params <- setdiff(names(data), c("B", "outcome", "exposure", "quantile"))
  
  data <- data[outcome == outcomes, ]
  
  rbindlist(lapply(params, bootstrap_intervals, data = data))
}

# MSM estimates

get_bootstrap_intervals_msm <- function(
    data, 
    outcomes, 
    effect_modifiers, 
    effect_modifier_levels){
  
  em_levels <- effect_modifier_levels[[effect_modifiers]]
  
  data <- data[outcome == outcomes & effect_modifier == effect_modifiers,]
  
  params <- c("Y0", "Y1", "Y0_gcomp", "Y1_gcomp")
  
  out <- list()
  
  for(i in seq_len(length(em_levels))){
    out[[i]] <- rbindlist(
      lapply(
        params, 
        bootstrap_intervals, 
        data = data[effect_modifier_levels == em_levels[i],]))
  }
  
  rbindlist(out)
}

# TMLE MARS analysis

get_bootstrap_intervals_mars <- function(data, outcomes) {
  data <- data[outcome == outcomes, ]
  
  params <- setdiff(names(data), c("B", "outcome", "exposure"))
  
  rbindlist(lapply(params, bootstrap_intervals, data = data))
}

## Bootstrap standard errors

bootstrap_intervals <- function(data, parameter){
  data$Y <- data[[parameter]]
  B <- max(data$B)
  # estimate between and within mean sum of squares
  model <- aov(Y ~ as.factor(B), data = data)
  MSB <- summary(model)[[1]]$`Mean Sq`[1]
  MSW <- summary(model)[[1]]$`Mean Sq`[2]
  sigma1 <- (MSB - MSW) / 2
  sigma1 <- ifelse(sigma1 < 0, 0, sigma1)
  sigma2 <- ifelse(sigma1 < 0, var(data$Y), MSW)
  Var <- ((1 + (1/B))*sigma1) + ((1/(B*2))*sigma2)
  part1 <- ((((B+1)/(B*2))^2) * (MSB^2))/(B-1)
  part2 <- MSW^2 / (4*B)
  df <- ((((B + 1) / (2*B))*MSB - (MSW / 2)) ^ 2) / 
    (((((B + 1) / (2*B))^2 * MSB^2) / (B - 1)) + (MSW^2 / (4*B)))
  estimate <- mean(data$Y)
  se <- sqrt(Var)
  lower <- estimate - qt(0.975, df)*se
  upper <- estimate + qt(0.975, df)*se
  
  if(!is.null(data$quantile)){
    return(data.table(
      exposure = unique(data$exposure),
      outcome = unique(data$outcome),
      parameter = parameter,
      quantile = unique(data$quantile),
      estimate = estimate,
      se = se,
      lower = lower,
      upper = upper,
      df = df
    )) 
  } else if(!is.null(data$effect_modifier)){
    return(data.table(
      exposure = unique(data$exposure),
      outcome = unique(data$outcome),
      effect_modifier = unique(data$effect_modifier),
      effect_modifier_levels = unique(data$effect_modifier_levels),
      parameter = parameter,
      estimate = estimate,
      se = se,
      lower = lower,
      upper = upper,
      df = df
    ))
  } else {
    return(data.table(
      exposure = unique(data$exposure),
      outcome = unique(data$outcome),
      parameter = parameter,
      estimate = estimate,
      se = se,
      lower = lower,
      upper = upper,
      df = df
    ))
  }
}

## Effect modifier levels for plotting
get_em_levels <- function(data){
  quantiles <- function(x){
    quantile(x, seq(0.025, 0.975, length.out = 20), na.rm=T)
  }
  
  list(
    apoe_e4 = c(0, 1),
    risk_score = quantiles(data$risk_score),
    S1_pTau181 = quantiles(data$S1_pTau181),
    S1_NFlight = quantiles(data$S1_NFlight),
    S1_GFAP = quantiles(data$S1_GFAP),
    S1_Abeta42_40 = quantiles(data$S1_Abeta42_40),
    BLToneAvg_Better = quantiles(data$BLToneAvg_Better),
    BL_3MS_OverallScore_C = quantiles(data$BL_3MS_OverallScore_C)
  )
}


## Main TMLE function for effect estimates

get_tmle_msm_bin <- function(
    data, 
    exposure,
    outcome, 
    effect_modifier, 
    effect_modifier_levels) {
  
  em_levels <- effect_modifier_levels[[effect_modifier]]
  
  out <- lapply(
    data, 
    tmle_msm_bin, 
    exposure = exposure, 
    outcome = outcome, 
    effect_modifier = effect_modifier, 
    effect_modifier_levels = em_levels)
  
  rbindlist(out)
}

get_tmle_msm_cat <- function(
    data, 
    exposure,
    outcome, 
    effect_modifier, 
    effect_modifier_levels){
  
  em_levels <- effect_modifier_levels[[effect_modifier]]
  
  out <- lapply(
    data, 
    tmle_msm_cat, 
    exposure = exposure, 
    outcome = outcome, 
    effect_modifier = effect_modifier, 
    effect_modifier_levels = em_levels)
  
  rbindlist(out)
}

## Functions for estimating MSM parameters

## Binary exposure

tmle_msm_bin <- function(
    data, 
    exposure, 
    outcome, 
    effect_modifier,
    effect_modifier_levels){
  data$Y <- data[[outcome]]
  data$A <- data[[exposure]]
  if(!effect_modifier == "apoe_e4"){
    data$effect_modifier <- data[[effect_modifier]]
    em_knots <- quantile(data$effect_modifier, c(0.1,0.5,0.9))
    em_levels <- effect_modifier_levels
  } else {
    data$effect_modifier <- ifelse(data$apoe_e4 == 0, 0, 1)
    em_levels <- unique(data$effect_modifier)
  }
  
  Qform <- get_outcome_formula()
  gform <- get_treatment_formula()
  
  ## GCOMP
  Qmod <- lm(Qform, data = data)
  data[, Q0_gcomp := predict(Qmod, newdata = mutate(data, A = 0))]
  data[, Q1_gcomp := predict(Qmod, newdata = mutate(data, A = 1))]
  
  # long dataset
  long_gcomp <- data.table(
    Q_gcomp = c(data$Q0_gcomp, data$Q1_gcomp),
    A = c(rep(0, nrow(data)), rep(1, nrow(data))),
    effect_modifier = rep(data$effect_modifier, 2)
  )
  if(!effect_modifier == "apoe_e4"){
    msm_mod <- lm(
      Q_gcomp ~ A*rcs(effect_modifier, em_knots), 
      data = long_gcomp)
  } else {
    msm_mod <- lm(
      Q_gcomp ~ A*effect_modifier, 
      data = long_gcomp)
  }
  newdata <- tibble(A = 0, effect_modifier = em_levels)
  Y0_gcomp <- predict(msm_mod, newdata = newdata)
  Y1_gcomp <- predict(msm_mod, newdata = mutate(newdata, A = 1))
  
  ## TMLE
  # set lower bound on IP weights
  lb <- 5 / sqrt(nrow(data)) / log(nrow(data))
  # Map Y to (0, 1)
  a <- min(data$Y)
  b <- max(data$Y)
  data$Y <- (data$Y - a) / (b-a)
  # outcome model
  Qmod <- glm(Qform, data = data, family = quasibinomial())
  # Initial Q on logit scale
  data[, QA := predict(Qmod)]
  data[, Q0 := predict(Qmod, newdata = mutate(data, A = 0))]
  data[, Q1 := predict(Qmod, newdata = mutate(data, A = 1))]
  # numerator for stabilised weights
  if(!effect_modifier == "apoe_e4"){
    hmod <- glm(
      A ~ rcs(effect_modifier, em_knots), 
      data = data, 
      family = binomial())
  } else {
    hmod <- glm(
      A ~ effect_modifier, 
      data = data, 
      family = binomial())
  }
  data$hscore <- predict(hmod, type = "response")
  # treatment model
  gmod <- glm(gform, data = data, family = binomial)
  # propensity scores
  data[, g := predict(gmod, type = "response")]
  # MSM model matrix 
  if(!effect_modifier == "apoe_e4"){
    MSM_form <- Y ~ A * rcs(effect_modifier, em_knots)
  } else {
    MSM_form <- Y ~ A * effect_modifier
  }
  
  MSM_mat <- model.matrix(MSM_form, data = data)
  MSM_mat0 <- model.matrix(MSM_form, data = mutate(data, A = 0))
  MSM_mat1 <- model.matrix(MSM_form, data = mutate(data, A = 1))
  C1 <- 1 / bounds(data$g, lb, 1) * MSM_mat
  # fit updating model
  epsilon <- coef(glm(
    Y ~ -1 + C1, 
    offset = QA, 
    data = data,
    family = quasibinomial())
  )
  # update Q and return to (0,1) scale
  data[, Q0star := plogis(
    Q0 + 
      bounds(1 - hscore, lb, 1) /  bounds(1 - g, lb, 1) * 
      MSM_mat0 %*% 
      epsilon)]
  data[, Q1star := plogis(
    Q1 + 
      bounds(hscore, lb, 1) /  bounds(g, lb, 1) * 
      MSM_mat1 %*% 
      epsilon)]
  # rescale to original units
  data[, Q0star := Q0star * (b - a) + a]
  data[, Q1star := Q1star * (b - a) + a]
  
  # long dataset for fitting MSM
  long <- data.table(
    Q_star = c(data$Q0star, data$Q1star),
    A = c(rep(0, nrow(data)), rep(1, nrow(data))),
    effect_modifier = rep(data$effect_modifier, 2),
    wts = c(1 - data$hscore, data$hscore)
  )
  
  if(!effect_modifier == "apoe_e4"){
    msm_mod <- lm(
      Q_star ~ A*rcs(effect_modifier, em_knots), 
      data = long, 
      weights = wts)
  } else {
    msm_mod <- lm(
      Q_star ~ A*effect_modifier, 
      data = long, 
      weights = wts)
  }
  
  Y0 <- predict(msm_mod, newdata = mutate(newdata, A = 0))
  Y1 <- predict(msm_mod, newdata = mutate(newdata, A = 1))
  
  
  result <- data.table(
    exposure = exposure, 
    outcome = outcome,
    effect_modifier = effect_modifier,
    effect_modifier_levels = rep(em_levels, 2),
    B = unique(data$tar_batch),
    Y0,
    Y1,
    Y0_gcomp,
    Y1_gcomp
  )
  
  result
}

## categorical exposure

tmle_msm_cat <- function(data, exposure, outcome){
  data$Y <- data[[outcome]]
  data$A <- data[[exposure]] - 1
  data$A <- ifelse(data$A == 0, 0, ifelse(data$A %in% c(1,2), 1, 2))
  Qform <- get_outcome_formula()
  gform <- get_treatment_formula()
  # set lower bound on IP weights
  lb <- 5/sqrt(nrow(data))/log(nrow(data))
  # Map Y to (0, 1)
  a <- min(data$Y)
  b <- max(data$Y)
  data$Y <- (data$Y - a) / (b-a)
  # outcome model
  Qmod <- glm(Qform, data = data, family = quasibinomial())
  # Initial Q on logit scale
  data[, QA := predict(Qmod)]
  data[, Q0 := predict(Qmod, newdata = mutate(data, A = "0"))]
  data[, Q1 := predict(Qmod, newdata = mutate(data, A = "1"))]
  data[, Q2 := predict(Qmod, newdata = mutate(data, A = "2"))]
  # treatment model
  gmod <- nnet::multinom(gform, data = data, maxit = 1000)
  # propensity scores
  pscore <- predict(gmod, type = "probs")
  pscore <- as.data.frame(apply(pscore, 2, bounds, lower = lb, upper = 1))
  setnames(pscore, c("g0", "g1", "g2"))
  data <- cbind(data, pscore)
  # construct clever covariates 
  data[, h0 := as.integer(A==0) / g0]
  data[, h1 := as.integer(A==1) / g1]
  data[, h2 := as.integer(A==2) / g2]
  # fit updating model
  epsilon <- coef(glm(
    Y ~ -1 + h0 + h1 + h2, 
    offset = QA, 
    data = data,
    family = quasibinomial())
  )
  # update Q and return to (0,1) scale
  data[, Q0star := plogis(Q0 + epsilon[1] / g0)]
  data[, Q1star := plogis(Q1 + epsilon[2] / g1)]
  data[, Q2star := plogis(Q2 + epsilon[3] / g2)]
  # rescale to original units
  data[, Q0star := Q0star * (b - a) + a]
  data[, Q1star := Q1star * (b - a) + a]
  data[, Q2star := Q2star * (b - a) + a]
  return(data.table(
    B = mean(data$tar_batch),
    exposure = exposure,
    outcome = outcome,
    EY0 = mean(data$Q0star),
    EY1 = mean(data$Q1star),
    EY2 = mean(data$Q2star),
    ATE = mean(data$Q2star) - mean(data$Q0star)
  ))
}

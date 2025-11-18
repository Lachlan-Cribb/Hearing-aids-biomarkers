## Mapping over outcomes

get_tmle_bin_mars <- function(data, exposure, outcome) {
  
  out <- lapply(data, tmle_bin_mars, exposure = exposure, outcome = outcome)
  
  rbindlist(out)
}

get_tmle_cat_mars <- function(data, exposure, outcome){
  
  out <- lapply(data, tmle_cat_mars, exposure = exposure, outcome = outcome)
  
  rbindlist(out)
}

## Functions for estimating TMLE

## Binary exposure

tmle_bin_mars <- function(data, exposure, outcome){
  data$Y <- data[[outcome]]
  data$A <- data[[exposure]]
  Qform <- get_outcome_formula_mars()
  gform <- get_treatment_formula_mars()
  ## TMLE
  # set lower bound on IP weights
  lb <- 5 / sqrt(nrow(data)) / log(nrow(data))
  # Map Y to (0, 1)
  a <- min(data$Y)
  b <- max(data$Y)
  data$Y <- (data$Y - a) / (b-a)
  # outcome model
  Qmod <- earth(
    Qform, 
    data = data, 
    glm = list(family = quasibinomial),
    degree = 2, 
    nk = 150)
  # Initial Q on logit scale
  data[, QA := predict(Qmod)]
  data[, Q0 := predict(Qmod, newdata = mutate(data, A = 0))]
  data[, Q1 := predict(Qmod, newdata = mutate(data, A = 1))]
  # treatment model
  gmod <- earth(
    gform, 
    data = data, 
    glm = list(family = binomial),
    degree = 2, 
    nk = 150)
  # propensity scores
  data[, g := predict(gmod, type = "response")]
  # construct clever covariates 
  data[, h0 := (1 - A) / bounds(1 - g, lb, 1)]
  data[, h1 := A / bounds(g, lb, 1)]
  # fit updating model
  epsilon <- coef(glm(
    Y ~ -1 + h0 + h1, 
    offset = QA, 
    data = data,
    family = quasibinomial())
  )
  # update Q and return to (0,1) scale
  data[, Q0star := plogis(Q0 + epsilon[1] / bounds(1 - g, lb, 1))]
  data[, Q1star := plogis(Q1 + epsilon[2] / bounds(g, lb, 1))]
  # rescale to original units
  data[, Q0star := Q0star * (b - a) + a]
  data[, Q1star := Q1star * (b - a) + a]
  
  data.table(
    B = unique(data$tar_batch),
    outcome = outcome,
    exposure = exposure,
    EY0 = mean(data$Q0star),
    EY1 = mean(data$Q1star),
    EY1_EY0 = mean(data$Q1star) - mean(data$Q0star)
  )
}

## categorical exposure

tmle_cat_mars <- function(data, exposure, outcome){
  data$Y <- data[[outcome]]
  data$A <- data[[exposure]] - 1
  data$A <- ifelse(data$A == 0, 0, ifelse(data$A %in% c(1,2), 1, 2))
  data$A <- as.factor(data$A)
  Qform <- get_outcome_formula_mars()
  gform <- get_treatment_formula_mars()
  ## TMLE
  # set lower bound on IP weights
  lb <- 5/sqrt(nrow(data))/log(nrow(data))
  # Map Y to (0, 1)
  a <- min(data$Y)
  b <- max(data$Y)
  data$Y <- (data$Y - a) / (b-a)
  # outcome model
  Qmod <- earth(
    Qform, 
    data = data, 
    glm = list(family = quasibinomial),
    degree = 2, 
    nk = 150)
  # Initial Q on logit scale
  data[, QA := predict(Qmod)]
  data[, Q0 := predict(Qmod, newdata = mutate(data, A = "0"))]
  data[, Q1 := predict(Qmod, newdata = mutate(data, A = "1"))]
  data[, Q2 := predict(Qmod, newdata = mutate(data, A = "2"))]
  # treatment model
  gmod <- earth(
    gform, 
    data = data, 
    glm = list(family = binomial),
    degree = 2, 
    nk = 150)
  # propensity scores
  pscore <- predict(gmod, type = "response") |> as.data.table()
  setnames(pscore, c("g0", "g1", "g2"))
  pscore_bounded <- as.data.frame(
    apply(pscore, 2, bounds, lower = lb, upper = 1))
  setnames(pscore_bounded, c("g0_bounded", "g1_bounded", "g2_bounded"))
  data <- cbind(data, pscore_bounded)
  # construct clever covariates 
  data[, h0 := as.integer(A==0) / g0_bounded]
  data[, h1 := as.integer(A==1) / g1_bounded]
  data[, h2 := as.integer(A==2) / g2_bounded]
  # fit updating model
  epsilon <- coef(glm(
    Y ~ -1 + h0 + h1 + h2, 
    offset = QA, 
    data = data,
    family = quasibinomial())
  )
  # update Q and return to (0,1) scale
  data[, Q0star := plogis(Q0 + epsilon[1] / g0_bounded)]
  data[, Q1star := plogis(Q1 + epsilon[2] / g1_bounded)]
  data[, Q2star := plogis(Q2 + epsilon[3] / g2_bounded)]
  # rescale to original units
  data[, Q0star := Q0star * (b - a) + a]
  data[, Q1star := Q1star * (b - a) + a]
  data[, Q2star := Q2star * (b - a) + a]
  
  data.table(
    B = unique(data$tar_batch),
    exposure = exposure,
    outcome = outcome,
    EY0 = mean(data$Q0star),
    EY1 = mean(data$Q1star),
    EY2 = mean(data$Q2star),
    EY1_EY0 = mean(data$Q1star) - mean(data$Q0star),
    EY2_EY0 = mean(data$Q2star) - mean(data$Q0star)
  )
}
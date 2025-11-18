## IPW quantile estimator

get_quantile_bin <- function(data, exposure, outcome, quantile = 0.9) {
  
  out <- lapply(
    data, 
    est_quantile_bin, 
    exposure = exposure, 
    outcome = outcome,
    quantile = quantile)
  
  rbindlist(out)
}

get_quantile_cat <- function(data, exposure, outcome, quantile = 0.9){
  
  out <- lapply(
    data, 
    est_quantile_cat, 
    exposure = exposure, 
    outcome = outcome,
    quantile = quantile)
  
  rbindlist(out)
}


## Binary exposure
est_quantile_bin <- function(data, exposure, outcome, quantile){
  data$Y <- data[[outcome]]
  data$A <- data[[exposure]]
  if(outcome == "S3_Abeta42_40") { quantile <- 1 - quantile }
  # lower bound on propensity score
  lb <- 5/sqrt(nrow(data))/log(nrow(data))
  # estimate weights
  gform <- get_treatment_formula()
  gmod <- glm(gform, data = data, family = binomial())
  data[, g := bounds(predict(gmod, type = "response"), lb, 1)]
  data[, w := ifelse(A==0, 1 / (1 - g), 1 / g)]
  # normalise weights
  data[, w := w / sum(w), by = A]
  # quantile regression
  qmod <- rq(Y ~ A, tau = quantile, weights = w, data = data)
  return(
    data.table(
      B = mean(data$tar_batch),
      exposure = exposure,
      outcome = outcome,
      quantile = quantile,
      Q0 = coef(qmod)[1],
      Q1 = coef(qmod)[1] + coef(qmod)[2],
      Q1_Q0 = coef(qmod)[2]
    )
  )
}

## Ordinal exposure
est_quantile_cat <- function(data, exposure, outcome, quantile){
  data$Y <- data[[outcome]]
  data$A <- data[[exposure]] - 1
  data$A <- ifelse(data$A == 0, 0, ifelse(data$A %in% c(1,2), 1, 2))
  if(outcome == "S3_Abeta42_40") { quantile <- 1 - quantile }
  # lower bound on propensity score
  lb <- 5/sqrt(nrow(data))/log(nrow(data))
  # estimate weights
  gform <- get_treatment_formula()
  gmod <- nnet::multinom(gform, data = data, maxit = 1000)
  # construct weights
  pscore <- predict(gmod, type = "probs")
  pscore <- as.data.frame(apply(pscore, 2, bounds, lower = lb, upper = 1))
  setnames(pscore, c("g0", "g1", "g2"))
  data <- cbind(data, pscore)
  data[, w := ifelse(A==0, 1 / g0, ifelse(A==1, 1 / g1, 1 / g2))]
  # normalise weights
  data[, w := w / sum(w), by = A]
  # quantile regression
  qmod <- rq(Y ~ as.factor(A), tau = quantile, weights = w, data = data)
  return(
    data.table(
      B = mean(data$tar_batch),
      exposure = exposure,
      outcome = outcome,
      quantile = quantile,
      Q0 = coef(qmod)[1],
      Q1 = coef(qmod)[1] + coef(qmod)[2],
      Q2 = coef(qmod)[1] + coef(qmod)[3],
      Q1_Q0 = coef(qmod)[2],
      Q2_Q0 = coef(qmod)[3]
    )
  )
}


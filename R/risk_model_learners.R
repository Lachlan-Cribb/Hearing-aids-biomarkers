### Generalised additive model
SL.mgcv <- function (Y,
                     X,
                     newX,
                     family,
                     obsWeights,
                     ...)
{
  gam.model <- as.formula(paste("Y~", paste(
    paste("s(", colnames(X), ", k = ", 4, ")", sep = ""), collapse = "+"
  )))
  fit.gam <- mgcv::gam(gam.model,
                       data = X,
                       family = family,
                       weights = obsWeights)
  pred <- predict(fit.gam, newdata = newX, type = "response")
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mgcv")
  return(out)
}

predict.SL.mgcv <- function (object, newdata, ...)
{
  pred <- predict(object = object$object,
                  newdata = newdata,
                  type = "response")
  return(pred)
}

### xgboost 

SL.xgboost.1 <- function(..., minobspernode = 25, max_depth = 3) 
  SL.xgboost(..., minobspernode = minobspernode, max_depth = max_depth)



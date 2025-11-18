## Bounds on propensity score
bounds <- function(x, lower, upper){
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

quantile_bounds <- function(x, quantile){
  x[x < quantile(x, (1 - quantile))] <- quantile(x, (1 - quantile))
  x[x > quantile(x, quantile)] <- quantile(x, quantile)
  return(x)
}

## Take bootstrap sample
get_boot_samples <- function(data){
  rows <- sample(1:nrow(data), nrow(data), replace = TRUE)
  data[rows,]
}

## Save mean and SD to undo standardisation

save_mean_sd <- function(df) {
  ID <- df[, "Safehaven"]
  ## Convert factor variables to dummies
  num_data <- model.matrix(
    ~ ., model.frame(
      ~ ., 
      data = df[, -1], 
      na.action = NULL))[, -1] |>
    as.data.table()
  
  data <- cbind(ID, num_data)
  
  ## standardise non-binary variables
  non_binary <- apply(data, 2, function(x)
    length(unique(x[!is.na(x)])) > 2)
  non_binary <- data[, ..non_binary] |> names()
  non_binary <- non_binary[!non_binary == "Safehaven"]
  
  mean_vec <- apply(data[, ..non_binary], 2, mean, na.rm = T)
  
  sd_vec <- apply(data[, ..non_binary], 2, sd, na.rm = T)
  
  saved_mean_sd <- data.table(var_name = names(data[, ..non_binary]),
                              mean = mean_vec,
                              sd = sd_vec)
  
  return(saved_mean_sd)
}

## standardise variables to mean 0 SD 1

standardise <- function(df){
  ID <- df[, "Safehaven"]
  ## Convert factor variables to dummies
  num_data <- model.matrix(
    ~ ., model.frame(
      ~ ., 
      data = df[, -1], 
      na.action = NULL))[, -1] |>
    as.data.table()
  
  data <- cbind(ID, num_data)
  
  ## standardise non-binary variables
  non_binary <- apply(data, 2, function(x) length(unique(x[!is.na(x)])) > 2)
  non_binary <- data[, ..non_binary] |> names()
  non_binary <- non_binary[!non_binary == "Safehaven"]
  
  scale <- function(x){(x - mean(x, na.rm=T)) / sd(x, na.rm=T)}

  data[, (non_binary) := lapply(.SD, scale), .SDcols = non_binary]
  
  return(data)
}

## return to original units 

undo_standardise <- function(df, mean_sd){
  
  mean_sd <- mean_sd[var_name %in% names(df),]
  
  for (i in mean_sd$var_name){
    df[[i]] <- 
      (df[[i]] * as.numeric(mean_sd[var_name == i, "sd"])) +
      as.numeric(mean_sd[var_name == i, "mean"])
  }
  return(df)
}

## Round function for table output

my_round <- function(x, digits){
  formatC(x, digits = digits, format = "fg", drop0trailing = FALSE, flag = "#")
}

## Tidy function for table output

tidy_table <- function(x) {
  cols <- c("estimate", "lower", "upper")
  gap <- ifelse(any(grep("EY2", x$parameter)), 2, 1)
  
  out <- x[!grep("_gcomp", parameter), list(outcome, parameter, estimate, lower, upper)]
  
  out[, (cols) := lapply(.SD, \(.x) ifelse(outcome == "S3_Abeta42_40", .x * 1000, .x)), 
      .SDcols = cols]
  
  out[, (cols) := lapply(.SD, \(.x) my_round(.x, 2)), .SDcols = cols]
  
  out[, estimate := paste0(estimate, " (", lower, ", ", upper, ")")]
  
  out <- out[, list(outcome, parameter, estimate)]
  
  out[, mean_dif := shift(estimate, gap, type = "lead")]
  out[, mean_dif := ifelse(parameter == "EY0", "", mean_dif)]
  out <- out[!grep("_", parameter),]
  out
}

get_stat <- function(x, stat){
  switch(stat,
         mean = mean(x, na.rm=T),
         lower = quantile(x, 0.25, na.rm=T),
         upper = quantile(x, 0.75, na.rm=T),
         count = sum(x, na.rm=T),
         perc = sum(x, na.rm=T) / length(!is.na(x)) * 100)
}

my_summarise <- function(data, stat){
  data |> 
    group_by(m, Y3M_HearingAid) |> 
    summarise(across(everything(), ~ get_stat(., stat))) |> 
    group_by(Y3M_HearingAid) |> summarise(across(-m, mean)) |> 
    pivot_longer(-Y3M_HearingAid, names_to = "var") |> 
    pivot_wider(names_from = Y3M_HearingAid) |> 
    select(var, `1`, `0`) |> 
    set_names(c("Variable", paste0("Yes_", stat), paste0("No_", stat)))
}
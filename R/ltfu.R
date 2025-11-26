# Delta adjustment
add_delta <- function(data, delta) {
  lapply(data, function(.x) {
    .x[, `:=`(
      S3_pTau181 = ifelse(
        ltfu,
        S3_pTau181 - delta * sd(S3_pTau181, na.rm = T),
        S3_pTau181
      ),
      S3_GFAP = ifelse(ltfu, S3_GFAP - delta * sd(S3_GFAP, na.rm = T), S3_GFAP),
      S3_NFlight = ifelse(
        ltfu,
        S3_NFlight - delta * sd(S3_NFlight, na.rm = T),
        S3_NFlight
      ),
      S3_Abeta42_40 = ifelse(
        ltfu,
        S3_Abeta42_40 + delta * sd(S3_Abeta42_40, na.rm = T),
        S3_Abeta42_40
      )
    )]
  })
}

# Add LTFU indicator
add_ltfu <- function(data) {
  out <- lapply(data, function(.x) {
    .x[, ltfu := is.na(S3_pTau181)]
  })
  out
}

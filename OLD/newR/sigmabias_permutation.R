prep_data <- function(n) {
  df <- ritwals::WALS
  df <- df[df$feature_ID == "44A", ]
  families <- unique(df$family)
  chosen <- sample(families, size=n, replace=FALSE)
  df$value_ID <- ifelse(df$family %in% chosen, 1, 6)
  df
}

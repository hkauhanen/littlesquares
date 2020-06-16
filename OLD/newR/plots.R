plot.main_result <- function(df,
                             neighbourhood_size = 10) {
  df <- df[df$neighbourhood_size == neighbourhood_size, ]

  plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 0.5))

  for (feat in unique(df$feature_ID)) {
    thisdf <- df[df$feature_ID==feat, ]
    rhos <- quantile(thisdf$

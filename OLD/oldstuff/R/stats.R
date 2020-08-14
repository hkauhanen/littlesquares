compute_tau_correlations_by_rep <- function(data) {
  out <- data.frame(rep=unique(data$rep), spearman=NA, p.value=NA)
  for (i in 1:nrow(out)) {
    df <- data[data$rep == out[i,]$rep, ]
    result <- cor.test(df$temperature.x, df$temperature.y, method="spearman")
    out[i,]$spearman <- result$estimate
    out[i,]$p.value <- result$p.value
  }
  out
}


compute_median_tau_correlation_from_xy <- function(data,
                                                   method = "spearman",
                                                   FUN = mean) {
  out <- data.frame(feature=unique(data$feature_ID), temperature.x=NA, temperature.y=NA)
  for (i in 1:nrow(out)) {
    df <- data[data$feature_ID==out[i,]$feature, ]
    out[i,]$temperature.x <- FUN(df$temperature.x, na.rm=TRUE)
    out[i,]$temperature.y <- FUN(df$temperature.y, na.rm=TRUE)
  }
  result <- cor.test(out$temperature.x, out$temperature.y, method=method)
  list(correlation=result$estimate, p.value=result$p.value)
}


filter_gen_units <- function(data,
                             wals = ritwals::WALS_languages,
                             column = 1,
                             lim) {
  numbers <- table(wals[[names(data)[column]]])
  data$N <- NA
  for (i in 1:nrow(data)) {
    data[i,]$N <- numbers[[data[i, column]]]
  }
  data[data$N >= lim, ]
}


scale_correlations_sequential <- function(res = final_results$correlations_scale) {
  scales <- sort(unique(res$neighbourhood_size))
  out <- data.frame(eta=scales, spearman=NA)
  for (i in 2:length(scales)) {
    df1 <- res[res$neighbourhood_size == scales[i], ]
    df2 <- res[res$neighbourhood_size == scales[i-1], ]
    df1new <- data.frame(feature_ID=unique(df1$feature_ID), mediantau=NA)
    df2new <- data.frame(feature_ID=unique(df2$feature_ID), mediantau=NA)
    for (j in 1:nrow(df1new)) {
      df1new[j,]$mediantau <- median(df1[df1$feature_ID==df1new[j,]$feature_ID, ]$temperature)
    }
    for (j in 1:nrow(df2new)) {
      df2new[j,]$mediantau <- median(df2[df2$feature_ID==df2new[j,]$feature_ID, ]$temperature)
    }
    tes <- cor.test(df1new$mediantau, df2new$mediantau)
    out[out$eta==scales[i], ]$spearman <- tes$estimate
  }
  print(ggplot(out, aes(x=eta, y=spearman)) + geom_point() + geom_line() + ylim(-0.05,1.05))
  out
}


scale_correlations_matrix <- function(df) {
  scales <- unique(df$neighbourhood_size)
  out <- expand.grid(x=scales, y=scales, z=NA)
  for (i in 1:nrow(out)) {
    print(paste(i, "out of", nrow(out)))
    df1 <- df[df$neighbourhood_size == out[i,]$x, ]
    df2 <- df[df$neighbourhood_size == out[i,]$y, ]
    df1 <- df1[, c("feature_ID", "temperature")]
    df2 <- df2[, c("feature_ID", "temperature")]
    df1 <- aggregate(temperature~feature_ID, df1, median)
    df2 <- aggregate(temperature~feature_ID, df2, median)
    cort <- cor.test(df1$temperature, df2$temperature, method="spearman")
    if (out[i,]$x < out[i,]$y) {
      out[i,]$z <- cort$estimate
    } else if (out[i,]$x > out[i,]$y) {
      #out[i,]$z <- cort$p.value
    }
  }
  out
}

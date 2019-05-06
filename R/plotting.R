require(ggplot2)
require(ggrepel)


# Plot all the figures for the manuscript, including those that go in the
# appendices.
#
plot.all_for_paper <- function(folder) {
  # Main text

  # Appendices
}


# Plot "geo-scale correlations", i.e. correlations of tau when computed
# with neighbourhood sizes i and j. The "optimal" neighbourhood size
# i_opt is figured out first by taking that size j that maximizes the
# sum of Spearman correlation coefficients across all i. The correlation
# of temperatures between i and i_opt is then plotted.
#
plot.correlations_scale <- function(df,
                                    testee = NA) {
  scales <- sort(unique(df$neighbourhood_size))

  if (is.na(testee)) {
  # find optimal scale (in the sense that the sum of the Spearman
  # correlation coefficient across scales is maximized)
  testdf <- data.frame(testee=scales, cumulative_coefficient=NA)
  for (testee in scales) {
    out <- expand.grid(scale=scales, coefficient=NA, p=NA)
    for (i in 1:nrow(out)) {
      df1 <- df[df$neighbourhood_size==testee, ]
      df2 <- df[df$neighbourhood_size==out[i,]$scale, ]

      # get temperature means
      df1 <- aggregate(df1[, 2], list(df1$feature_ID), mean)
      df2 <- aggregate(df2[, 2], list(df2$feature_ID), mean)
      names(df1) <- c("feature_ID", "temperature")
      names(df2) <- c("feature_ID", "temperature")

      # correlate
      cortest <- cor.test(df1$temperature, df2$temperature, method="spearman")
      out[i,]$coefficient <- cortest$estimate
      out[i,]$p <- cortest$p.value
    }
    testdf[testdf$testee==testee, ]$cumulative_coefficient <- sum(out$coefficient)
  }
  testdf <- testdf[order(testdf$cumulative_coefficient, decreasing=TRUE), ]
  best_testee <- testdf[1,]$testee
  } else {
    best_testee <- testee
  }

  # get correlations against best testee
    out <- expand.grid(scale=scales, coefficient=NA, p=NA)
    for (i in 1:nrow(out)) {
      df1 <- df[df$neighbourhood_size==best_testee, ]
      df2 <- df[df$neighbourhood_size==out[i,]$scale, ]

      # get temperature means
      df1 <- aggregate(df1[, 2], list(df1$feature_ID), mean)
      df2 <- aggregate(df2[, 2], list(df2$feature_ID), mean)
      names(df1) <- c("feature_ID", "temperature")
      names(df2) <- c("feature_ID", "temperature")

      # correlate
      cortest <- cor.test(df1$temperature, df2$temperature, method="spearman")
      out[i,]$coefficient <- cortest$estimate
      out[i,]$p <- cortest$p.value
    }

  # plot
  ggplot(out, aes(x=scale, y=coefficient)) + geom_point() + ylim(0.5, 1.0) + geom_smooth() 
}


# Plot median temperature against neighbourhood size.
#
plot.tau_against_scale <- function(df) {
  g <- ggplot(df, aes(x=neighbourhood_size, y=temperature)) + stat_summary(fun.y=median, geom="point")
  g <- g + scale_y_log10() + scale_x_log10() + annotation_logticks()
  g <- g + theme_bw()
  g <- g + xlab("neighbourhood size") + ylab("median temperature")
  g
}

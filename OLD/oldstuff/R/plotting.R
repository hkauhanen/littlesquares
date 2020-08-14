require(ggplot2)
require(ggrepel)


# Plot all the figures for the manuscript, including those that go in the
# appendices.
#
plot.all_for_paper <- function(folder) {
  # Main text

  # Appendices
}


plot.correlations_scale_subsequent <- function(df) {
  scales <- sort(unique(df$neighbourhood_size))
    out <- expand.grid(scale=scales, coefficient=NA, p=NA)
    for (i in 2:nrow(out)) {
      print(out[i-1,]$scale)
      print(out[i,]$scale)
      df1 <- df[df$neighbourhood_size==out[i-1,]$scale, ]
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



plot.correlations_scale_matrix <- function(df) {
  scales <- sort(unique(df$neighbourhood_size))
  out <- expand.grid(ns1=scales, ns2=scales, coefficient=NA, p=NA)
  
  for (i in 1:length(scales)) {
    for (j in i:length(scales)) {
      this1 <- scales[i]
      this2 <- scales[j]
    df1 <- df[df$neighbourhood_size==this1, ]
    df2 <- df[df$neighbourhood_size==this2, ]

    # temperature means
    df1 <- aggregate(df1[, 2], list(df1$feature_ID), mean)
    df2 <- aggregate(df2[, 2], list(df2$feature_ID), mean)
    names(df1) <- c("feature_ID", "temperature")
    names(df2) <- c("feature_ID", "temperature")

      # correlate
      cortest <- cor.test(df1$temperature, df2$temperature, method="spearman")
      out[out$ns1==this1 & out$ns2==this2, ]$coefficient <- cortest$estimate
      out[out$ns1==this1 & out$ns2==this2, ]$p <- cortest$p.value
    }
  }

  g <- ggplot(out, aes(x=ns1, y=ns2, fill=coefficient)) + geom_raster()
  print(g)
}




# Plot "geo-scale correlations", i.e. correlations of tau when computed
# with neighbourhood sizes i and j. The "optimal" neighbourhood size
# i_opt is figured out first by taking that size j that maximizes the
# sum of Spearman correlation coefficients across all i. The correlation
# of temperatures between i and i_opt is then plotted.
#
plot.correlations_scale <- function(df,
                                    testee = NA,
                                    smoothspan = 0.75) {
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

      # get temperature medians
      df1 <- aggregate(df1[, 2], list(df1$feature_ID), median)
      df2 <- aggregate(df2[, 2], list(df2$feature_ID), median)
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
  ggplot(out, aes(x=scale, y=coefficient)) + geom_point() + ylim(0.5, 1.0) + geom_smooth(span=smoothspan, se=F) 
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

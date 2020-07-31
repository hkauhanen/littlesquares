# Merge our dataset with Dediu's
merge_with_dediu <- function(df = read.csv("../data/main-analysis.csv"),
                             dediufile = "../data/dediu-2010/binary.csv") {
  dediu <- read.csv(dediufile)
  names(dediu)[1] <- "feature_ID"

  df <- aggregate(df[, 2:4], list(df$feature_ID), median, na.rm=TRUE)
  names(df)[1] <- "feature_ID"

  df <- merge(df, dediu, by="feature_ID")

  df
}


# Correlate medians of temperatures for the OW and NW subsets
correlate_OWNW <- function(df = read.csv("../data/OWNW.csv")) {
  df <- aggregate(df[, 2:4], list(df$feature_ID, df$hemisphere), median, na.rm=TRUE)
  names(df)[1:2] <- c("feature_ID", "hemisphere")

  dfOW <- df[df$hemisphere=="OW", ]
  dfNW <- df[df$hemisphere=="NW", ]

  cor.test(dfOW$temperature, dfNW$temperature, method="spearman")
}


correlate_spatially_subsequent <- function(df,
                                           method = "spearman") {
  sizes <- sort(unique(df$neighbourhood_size))
  out <- data.frame(neighbourhood_size=sizes, correlation=NA, p.value=NA)
  for (s in sizes[-1]) {
    df1 <- df[df$neighbourhood_size==s, ]
    df2 <- df[df$neighbourhood_size==s-1, ]
    df1 <- aggregate(df1[, 2:4], list(df1$feature_ID), median, na.rm=TRUE)
    df2 <- aggregate(df2[, 2:4], list(df2$feature_ID), median, na.rm=TRUE)
    res <- cor.test(df1$temperature, df2$temperature, method=method)
    out[out$neighbourhood_size==s, ]$correlation <- res$estimate
    out[out$neighbourhood_size==s, ]$p.value <- res$p.value
  }
  out
}


# Correlate temperature against Dediu's PC1
correlate_with_dediu <- function(method = "spearman",
                                 no_of_knockouts = 4) {
  df <- merge_with_dediu()

  if (no_of_knockouts > 0) {
    knockouts <- hipster::knockout_lm(df, PC1~temperature, id.var="feature_ID")
    knockouts <- knockouts[1:no_of_knockouts, ]
    df <- df[!(df$feature_ID %in% knockouts$knockee), ]
    print(knockouts)
  }

  print(cor.test(df$temperature, df$PC1, method=method))

  knockouts
}


correlate_over_neighbourhood_size <- function(df = read.csv("../data/neighbourhoods.csv"),
                                              reference = 1,
                                              method = "spearman") {
  sizes <- unique(df$neighbourhood_size)
  out <- data.frame(neighbourhood_size=as.numeric(sizes), correlation=NA, p.value=NA)
  for (s in sizes) {
    df1 <- df[df$neighbourhood_size==reference, ]
    df1 <- aggregate(df1[, 2:4], list(df1$feature_ID), median, na.rm=TRUE)
    df2 <- df[df$neighbourhood_size==s, ]
    df2 <- aggregate(df2[, 2:4], list(df2$feature_ID), median, na.rm=TRUE)
    here <- cor.test(log(df1$temperature), log(df2$temperature), method=method)
    out[out$neighbourhood_size==s, ]$correlation <- here$estimate
    out[out$neighbourhood_size==s, ]$p.value <- here$p.value
  }
  out
}


figure_out_best_neighbourhood_size <- function(df,
                                               method = "spearman") {
  sizes <- unique(df$neighbourhood_size)
  bestcorsum <- -2
  best_size <- NA
  for (s in sizes) {
    here <- correlate_over_neighbourhood_size(df=df, reference=s, method=method)
    corsum <- sum(here$correlation)
    if (corsum > bestcorsum) {
      bestcorsum <- corsum
      best_size <- s
    }
  }
  best_size
}


correlate_between_bootstraps <- function(df,
                                         neighbourhood_size,
                                         howmany,
                                         method) {
  df <- df[df$neighbourhood_size==neighbourhood_size, ]
  maxrep <- max(df$rep)
  out <- data.frame(comparison=1:howmany, correlation=NA, p.value=NA)
  for (h in 1:howmany) {
    reps <- sample(1:maxrep, size=2, replace=FALSE)
    res <- cor.test(df[df$rep==reps[1], ]$temperature, df[df$rep==reps[2], ]$temperature, method=method)
    out[h,]$correlation <- res$estimate
    out[h,]$p.value <- res$p.value
  }
  out
}


correlate_cpp <- function(df,
                          iteration = 12250000,
                          method = "spearman") {
  df <- df[df$iteration==iteration, ]
  df <- df[df$tau < 1000, ] # boundary of tau hash
  cor.test(log(df$tau), log(df$tau_actual), method=method)
}

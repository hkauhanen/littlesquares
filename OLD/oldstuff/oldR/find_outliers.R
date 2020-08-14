# Finds outliers in a linear regression by recursively removing data
# points which contribute most to the residual sum of squares.
#
find_outliers <- function(df,
                          variable,
                          formula) {
  findone <- function(data) {
    model <- lm(formula, data)
    SSR <- sum(model$residuals^2)
    redSSR <- rep(NA, nrow(data)-1)
    for (i in 1:nrow(data)) {
      reddata <- data[-i,]
      model <- lm(formula, reddata)
      redSSR[i] <- sum(model$residuals^2)
    }
    SSRdiff <- SSR - redSSR
    outlier <- which.max(SSRdiff)
    outliername <- as.character(data[outlier,][[variable]])
    list(data=data[-outlier,], outlier=outliername, improvement=max(SSRdiff))
  }

  no_iterations <- nrow(df)-1
  results <- data.frame(matrix(NA, nrow=no_iterations, ncol=3))
  names(results) <- c("iteration", "removed", "improvement")
  for (r in 1:no_iterations) {
    oneresult <- findone(df)
    results[r,] <- c(r, oneresult$outlier, oneresult$improvement)
    df <- oneresult$data
  }

  results$iteration <- as.numeric(results$iteration)
  results$improvement <- as.numeric(results$improvement)

  results
}



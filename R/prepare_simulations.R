# Prepare parameter files for use with C++ simulations


# Sample each of pI, pE, pI', pE' and q at random
prepare_simulations_random <- function(file,
                                       n_features = 100) {
  df <- data.frame(matrix(runif(n=n_features*4, min=0, max=0.25), ncol=4))
  df <- cbind(df, runif(n=n_features, min=0.5, max=0.95))
  write.table(df, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}


# Sample pI, pE and q at random but assuming pI' = pE' = lambda
prepare_simulations_random_symmetric_lambda <- function(file,
                                                        n_features) {
  df <- data.frame(matrix(runif(n=n_features*3, min=0, max=0.5), ncol=3))
  df <- cbind(df, df[,3])
  df <- cbind(df, runif(n=n_features, min=0, max=0.5))
  write.table(df, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}


# Sample pI, pE and q at random but assuming pI' = pE' = 0
prepare_simulations_random_zero_lambda <- function(file,
                                                   n_features) {
  df <- data.frame(matrix(runif(n=n_features*2, min=0, max=0.5), ncol=2))
  df <- cbind(df, 0)
  df <- cbind(df, 0)
  df <- cbind(df, runif(n=n_features, min=0, max=0.5))
  write.table(df, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}


# Systematically vary pI, pE and q for a desired value of tau, assuming pI' = pE' = 0
prepare_simulations_for_figure <- function(file,
                                           resolution = 8,
                                           taus = c(1, 0.1, 0.01)) {
  out <- NULL
  for (tau in taus) {
    pIs <- seq(from=0.05, to=0.95, length.out=resolution)
    pEs <- seq(from=0.05, to=0.95, length.out=resolution)
    df <- expand.grid(pI=pIs, pE=pEs, pIprime=0, pEprime=0, q=NA)
    df$q <- (df$pI + df$pE)/(tau + df$pI + df$pE)
    out <- rbind(out, df)
  }
  write.table(out, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}

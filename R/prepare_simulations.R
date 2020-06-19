# Prepare parameter files for use with C++ simulations


# Sample each of pI, pE, pI' and pE' at random
prepare_simulations_random <- function(file,
                                       n_features) {
  df <- data.frame(matrix(runif(n=n_features*4, min=0, max=0.5), ncol=4))
  write.table(df, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}


# Sample the p* at random but assuming pI' = pE' = lambda
prepare_simulations_random_symmetric_lambda <- function(file,
                                                        n_features) {
  df <- data.frame(matrix(runif(n=n_features*3, min=0, max=0.5), ncol=3))
  df <- cbind(df, df[,3])
  write.table(df, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}


# Sample the p* at random but assuming pI' = pE' = 0
prepare_simulations_random_zero_lambda <- function(file,
                                                        n_features) {
  df <- data.frame(matrix(runif(n=n_features*2, min=0, max=0.5), ncol=2))
  df <- cbind(df, 0)
  df <- cbind(df, 0)
  write.table(df, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}


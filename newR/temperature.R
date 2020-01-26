temperature <- function(id,
                        up,
                        down,
                        resample_size = 0,
                        bootstrap = FALSE,
                        neighbourhood_size = 1,
                        NN_hash,
                        data = ritwals::WALS) {
  NN_hash <- NN_hash[NN_hash$rank <= neighbourhood_size, ]

  # Check that up and down are subsets of feature's possible values
  feature_codes <- ritwals::WALS_features[ritwals::WALS_features$feature_ID == id, ]$value_ID
  if (sum(!(up %in% feature_codes)) > 0) {
    stop("At least one value in up is not a valid value of this feature")
  }
  if (sum(!(down %in% feature_codes)) > 0) {
    stop("At least one value in down is not a valid value of this feature")
  }

  # Check that up and down do not overlap
  if (sum(up %in% down) > 0) {
    stop("up and down intersect!")
  }

  # Pare down WALS database to languages for which the feature is attested
  df <- data[data$feature_ID == id, ]

  # Binarize
  df <- df[df$value_ID %in% c(up, down), ]
  df$value_ID <- ifelse(df$value_ID %in% up, -1, -2)

  sigmadf <- df

  # Check that requested resample and neighbourhood sizes are not too large
  if (resample_size > nrow(df)) {
    stop("Trying to take a larger resample than there are languages; make resample_size smaller")
  }
  if (neighbourhood_size > nrow(df)) {
    stop("Trying to consider neighbourhoods that are larger than the feature sample size; make neighbourhood_size smaller")
  }

  resampled <- FALSE

  # Resample if required
  if (resample_size > 0) {
    resample <- sample(1:nrow(df), size=resample_size, replace=FALSE)
    df <- df[resample, ]
    resampled <- TRUE
  }

  # Bootstrap if required
  if (bootstrap) {
    resample <- sample(1:nrow(df), size=nrow(df), replace=TRUE)
    df <- df[resample, ]
    resampled <- TRUE
  }

  # Calculate feature frequency (rho)
  cat("Calculating feature frequency...\n")
  upspins <- sum(df$value_ID == -1)
  total <- nrow(df)
  rho <- upspins/total

  # Calculate isogloss density (sigma)
  if (neighbourhood_size == 1) {
    cat(paste0("Calculating isogloss density based on ", length(unique(df$language_ID)), " languages and ", neighbourhood_size, " neighbour, please be patient...\n"))
  } else {
    cat(paste0("Calculating isogloss density based on ", length(unique(df$language_ID)), " languages and ", neighbourhood_size, " neighbours, please be patient...\n"))
  }
  #interfaces <- length(unique(df$language_ID))*neighbourhood_size
  interfaces <- 0
  isoglosses <- 0
  pb <- txtProgressBar(min=0, max=length(unique(df$language_ID)), initial=0, style=3)
  ctr <- 0
  for (lang in unique(df$language_ID)) {
    ctr <- ctr + 1
    setTxtProgressBar(pb, ctr)
    neighbours <- NN_hash[NN_hash$language_ID==lang, ]
    neighbours <- neighbours[1:neighbourhood_size, ]
    selfspin <- sigmadf[sigmadf$language_ID==lang, ]$value_ID
    neighbour_spins <- sigmadf[sigmadf$language_ID %in% neighbours$neighbour_ID, ]$value_ID
    isoglosses <- isoglosses + sum(selfspin != neighbour_spins)
    interfaces <- interfaces + length(neighbour_spins)
  }
  close(pb)
  sigma <- isoglosses/interfaces

  # Estimate temperature (tau)
  cat("Calculating temperature...\n")
  Htau <- sigma/(2*rho*(1-rho))
  if (rho == 0 || rho == 1) {
    tau <- NA
  } else {
    tau <- invert_H(Htau)
  }

  up <- paste(up, collapse=":")
  down <- paste(down, collapse=":")

  cat("Done!\n")
  data.frame(feature_ID=id, temperature=tau, feature_frequency=rho, isogloss_density=sigma, value_ID_up=up, value_ID_down=down, resampled=resampled, sample_size=length(unique(df$language_ID)), neighbourhood_size=neighbourhood_size)
}

invert_H <- function(Htau) {
  if (Htau < 0) {
    warning("Htau less than 0 (theoretically impossible)")
  } else if (Htau > 1) {
    warning("Htau greater than 1 (theoretically impossible)")
  }
  index_of_closest_Htau <- index_of_closest_in_vector(vec=temperature_hash$Htau, x=Htau)
  temp <- temperature_hash[index_of_closest_Htau, ]$tau
  if (temp == max(temperature_hash$tau)) {
    temp <- NA
  }
  temp
}

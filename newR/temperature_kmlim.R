#' Temperature of Binary Feature
#'
#' Computes the temperature of a binary reinterpretation of a given WALS feature.
#'
#' 'Temperature' quantifies the propensity of a binary typological feature to flip its value over repeated interactions of language dynamics (in either 'vertical' inheritance or 'horizontal' contact). See References below for its definition. Runtime increases polynomially with the number of languages; future implementations may wish to consider rewriting the code in a compiled language.
#' @param id Feature's WALS ID
#' @param up Vector of feature values considered to represent presence of binary feature
#' @param down Vector of feature values considered to represent absence of binary feature
#' @param resample_size Size of resample (number of languages) taken before computation of feature; if non-positive, no resample is taken, and entire dataset is used instead
#' @param bootstrap Whether to take a bootstrap sample of the data
#' @param neighbourhood_size Size of each language's neighbourhood
#' @param data Optionally, a data frame (such as a subset of WALS)
#' @references Kauhanen, H., Gopal, D., Galla, T. & Bermúdez-Otero, R. (2018). Geospatial distributions reflect the rates of evolution of features of language. arXiv preprint. \url{https://arxiv.org/abs/1801.09637}.
#' @return A data frame of 9 variables:
#' \describe{
#' \item{\code{feature_ID}}{Feature's WALS ID}
#' \item{\code{temperature}}{Estimated temperature}
#' \item{\code{feature_frequency}}{Feature frequency}
#' \item{\code{isogloss_density}}{Isogloss density}
#' \item{\code{value_ID_up}}{WALS value IDs corresponding to feature's up (on) state}
#' \item{\code{value_ID_down}}{WALS value IDs corresponding to feature's down (off) state}
#' \item{\code{resampled}}{Boolean; whether a resample of the data was taken}
#' \item{\code{sample_size}}{Sample size (number of languages) used}
#' \item{\code{neighbourhood_size}}{Neighbourhood size used}
#' }
#' @import ritwals
#' @import utils
#' @export
temperature_kmlim <- function(id,
                        up,
                        down,
                        resample_size = 0,
                        bootstrap = FALSE,
                        km_limit,
                        consider_all_for_sigma = TRUE,
                        NN_hash,
                        data = ritwals::WALS) {
  NN_hash <- NN_hash[NN_hash$distance <= km_limit, ]

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

  if (1 == 0) {
  # Check that requested resample and neighbourhood sizes are not too large
  if (resample_size > nrow(df)) {
    stop("Trying to take a larger resample than there are languages; make resample_size smaller")
  }
  if (neighbourhood_size > nrow(df)) {
    stop("Trying to consider neighbourhoods that are larger than the feature sample size; make neighbourhood_size smaller")
  }
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

  if (!consider_all_for_sigma) {
    sigmadf <- df
  }

  # Calculate feature frequency (rho)
  #cat("Calculating feature frequency...\n")
  upspins <- sum(df$value_ID == -1)
  total <- nrow(df)
  rho <- upspins/total

  # Calculate isogloss density (sigma)
  if (1==0) {
  if (neighbourhood_size == 1) {
    cat(paste0("Calculating isogloss density based on ", length(unique(df$language_ID)), " languages and ", neighbourhood_size, " neighbour, please be patient...\n"))
  } else {
    cat(paste0("Calculating isogloss density based on ", length(unique(df$language_ID)), " languages and ", neighbourhood_size, " neighbours, please be patient...\n"))
  }
  }
  #interfaces <- length(unique(df$language_ID))*neighbourhood_size
  interfaces <- 0
  isoglosses <- 0
  #pb <- txtProgressBar(min=0, max=length(unique(df$language_ID)), initial=0, style=3)
  ctr <- 0
  for (lang in unique(df$language_ID)) {
    ctr <- ctr + 1
    #setTxtProgressBar(pb, ctr)
    neighbours <- NN_hash[NN_hash$language_ID==lang, ]
    #neighbours <- neighbours[1:neighbourhood_size, ]
    selfspin <- sigmadf[sigmadf$language_ID==lang, ]$value_ID
    neighbour_spins <- sigmadf[sigmadf$language_ID %in% neighbours$neighbour_ID, ]$value_ID
    isoglosses <- isoglosses + sum(selfspin != neighbour_spins)
    interfaces <- interfaces + length(neighbour_spins)
  }
  #close(pb)
  sigma <- isoglosses/interfaces

  # Estimate temperature (tau)
  #cat("Calculating temperature...\n")
  Htau <- sigma/(2*rho*(1-rho))
  if (rho == 0 || rho == 1) {
    tau <- NA
  } else {
    tau <- invert_H(Htau)
  }

  up <- paste(up, collapse=":")
  down <- paste(down, collapse=":")

  #cat("Done!\n")
  data.frame(feature_ID=id, temperature=tau, feature_frequency=rho, isogloss_density=sigma, value_ID_up=up, value_ID_down=down, resampled=resampled, sample_size=length(unique(df$language_ID)), km_limit=km_limit)
}


#' Invert H
#'
#' Performs a numerical inversion of the H function that relates temperature
#' to feature frequency and isogloss density.
#'
#' Inversion is via a static look-up table (hash) prepared pre-build and
#' shipped with the package as the object \code{temperature_hash}. This
#' hash provides reasonable cutoff points for tau as well as providing
#' reasonable resolution between successive values of tau and fast
#' execution times. Notice that the value of tau produced is an approximation.
#'
#' @param Htau Value of H(tau) to be inverted; must lie between 0 and 1
#' @return Numeric; the value of tau corresponding to given H(tau)
#' @references Kauhanen, H., Gopal, D., Galla, T. & Bermúdez-Otero, R. (2018). Geospatial distributions reflect the rates of evolution of features of language. arXiv preprint. \url{https://arxiv.org/abs/1801.09637}.
#' @export
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

###########
# analyse.R
#
# WALS data analysis.
#
# Henri Kauhanen 2016-2017
##########################



# wals_analysis()
#
# Perform the rho-sigma estimation for WALS data, bootstrapping if
# desired. Various filters (such as a sample size lower bound) may be
# specified.
#
# featuresize:      language sample size lower bound
# exactsample:      if positive, take samples of this exact size for each
#                   feature
# regions:          character vector of WALS regions to analyse. If NULL,
#                   whole world analysed
# bootsize:         size of bootstrap sample. Set to 1 for no bootstrap
# confval:          desired confidence intervals for the bootstrap
#
wals_analysis <- function(folder = "../data/wals-20170718/features/",
                          featurefile = "../data/unsilly-features.csv",
                          infofile = "../data/wals-20170718/languoid.tab",
                          nnfile = "../data/wals-20170718/adjacencies.csv",
                          glob = ".tab.pruned.csv",
                          featurelim = NULL,
                          featuresize = 1,
                          exactsample = -1,
                          regions = NULL,
                          bootsize = 1,
                          confval = 0.025,
                          alpha_min = 0.000001,
                          alpha_max = 100,
                          alpha_res = 10000,
                          verbose = TRUE) {
  # Set up alpha hash, for estimating empirical values of alpha with help
  # of theory
  alphahash <- make_table_of_halpha(alpha=exp(seq(from=log(alpha_min), to=log(alpha_max), length.out=alpha_res)))

  # Read in features dataframe and nearest neighbours table
  features <- read.csv(featurefile)
  nns <- read.csv(nnfile, stringsAsFactors=FALSE)

  # Limit analysis to subset of features, if desired (this functionality is
  # needed by analyse_wals_condor())
  if (!is.null(featurelim)) {
    features <- features[features$feature %in% featurelim, ]
  }

  # Results will be collected in a dataframe which is "rbound" feature by
  # feature (we know, this is computationally ineffective, shhh...)
  res <- NULL

  # Loop through features
  valid_features_ctr <- 0
  for (i in 1:nrow(features)) {
    # Print status for the impatient
    if (verbose) {
      print(paste("Working on feature ", i, " of ", nrow(features), sep=""))
    }

    # We only take feature into account if it has at least 'featuresize'
    # languages in its language sample (to get rid of data that's too
    # noisy).
    if (as.numeric(features[i,]$sample.size) >= featuresize) {
      valid_features_ctr <- valid_features_ctr + 1
      thisfeat <- features[i,]$feature

      # Get nearest neighbours data for this particular feature
      thisnns <- nns[nns$feature == thisfeat, ]

      # Determine feature's domain
      area <- features[i,]$area
      hyperdomain <- features[i,]$our_area
      ourname <- paste(valid_features_ctr, hyperdomain, sep="")
      if (hyperdomain == "P") {
        hyperdomain <- "phonology"
        domno <- 1
      }
      else if (hyperdomain == "M") {
        hyperdomain <- "morphology"
        domno <- 2
      }
      else if (hyperdomain == "S") {
        hyperdomain <- "syntax"
        domno <- 3
      }
      else if (hyperdomain == "L") {
        hyperdomain <- "lexicon"
        domno <- 4
      }
      else {
        hyperdomain <- "other"
        domno <- 5
      }

      # Calculate rho and sigma for feature, bootstrapping
      bsdf <- matrix(NA, nrow=bootsize, ncol=4)
      for (bs in 1:bootsize) {
        bsdf[bs,] <- wals_sigma_onefeature(folder=folder, feature=thisfeat, down=features[i,]$down, up=features[i,]$up, hash=thisnns, resample=TRUE, glob=glob, regions=regions, infofile=infofile, exactsample=exactsample, alphahash=alphahash)
      }
      rho <- median(bsdf[,1])
      sigma <- median(bsdf[,2])
      alpha <- median(bsdf[,4])
      rho_bot <- rho - quantile(rho - bsdf[,1], prob=c(confval, 1-confval))[2]
      rho_top <- rho - quantile(rho - bsdf[,1], prob=c(confval, 1-confval))[1]
      sigma_bot <- sigma - quantile(sigma - bsdf[,2], prob=c(confval, 1-confval))[2]
      sigma_top <- sigma - quantile(sigma - bsdf[,2], prob=c(confval, 1-confval))[1]
      alpha_bot <- alpha - quantile(alpha - bsdf[,4], prob=c(confval, 1-confval))[2]
      alpha_top <- alpha - quantile(alpha - bsdf[,4], prob=c(confval, 1-confval))[1]

      thisfeatrow <- data.frame(ourname, thisfeat, as.numeric(features[i,]$sample.size), domno, hyperdomain, area, as.numeric(rho_bot), as.numeric(rho), as.numeric(rho_top), as.numeric(sigma_bot), as.numeric(sigma), as.numeric(sigma_top), as.numeric(alpha_bot), as.numeric(alpha), as.numeric(alpha_top))
      names(thisfeatrow) <- c("feature", "WALS_feature", "sample_size", "domain_no", "domain", "WALS_domain", "rho_bot", "rho", "rho_top", "sigma_bot", "sigma", "sigma_top", "alpha_bot", "alpha", "alpha_top")

      # Push to results dataframe
      res <- rbind(res, thisfeatrow)
    }
  }
  res
}


# wals_sigma_onefeature()
#
# Helper routine, called by wals_analysis().
#
wals_sigma_onefeature <- function(folder,
                                  feature,
                                  down,
                                  up,
                                  hash,
                                  alphahash,
                                  resample,
                                  exactsample,
                                  regions,
                                  infofile,
                                  glob) {
  # Read in feature and infodf
  file <- paste(folder, "/", feature, glob, sep="")
  infodf <- read.csv(infofile, sep="\t", stringsAsFactors=FALSE)
  df <- read.csv(file, sep="\t")

  # Down and up spin specifications into vectors
  down <- as.numeric(strsplit(as.character(down), split=",")[[1]])
  up <- as.numeric(strsplit(as.character(up), split=",")[[1]])

  # Binarize feature
  for (i in 1:nrow(df)) {
    if (df[i,]$value %in% down) {
      df[i,]$value <- -1
    } else if (df[i,]$value %in% up) {
      df[i,]$value <- 1
    } else {
      #      stop("problem with feature values! Halp!"
      df[i,]$value <- 0
    }
  }

  # Resample (with replacement) if necessary (for bootstrap)
  if (resample) {
    if (exactsample > 0) {
      sam <- sample(1:nrow(df), size=exactsample, replace=TRUE)
    } else {
      sam <- sample(1:nrow(df), size=nrow(df), replace=TRUE)
    }
  } else {
    sam <- 1:nrow(df)
  }

  # Cycle through languages in feature's language sample
  upspins <- 0
  downspins <- 0
  reactives <- 0
  reactotals <- 0
  for (i in sam) {
    # Only if language is in the desired region
    thisregion <- infodf[infodf$wals.code == df$wals.code[i], ]$macroarea
    if (is.null(regions) || thisregion %in% regions) {
      # Find nearest neighbour
      selfcode <- df$wals.code[i]
      neicode <- hash[hash$id == as.character(selfcode), ]$nn

      # Is 'self' an upspin or a downspin
      if (df[i,]$value == 1) {
        upspins <- upspins + 1
      } else if (df[i,]$value == -1) {
        downspins <- downspins + 1
      }

      # Do 'self' and 'nei' DISagree?
      selfspin <- df[i,]$value
      neispin <- df[df$wals.code == neicode, ]$value
      #print(c(as.character(selfcode), neicode))
      #print(c(selfspin, neispin))
      if (selfspin != 0 && neispin != 0) {
        reactotals <- reactotals + 1
        if (neispin != selfspin) {
          reactives <- reactives + 1
        }
      }
    }
  }

  # Compute rho, sigma, sample size
  rho <- upspins/(upspins + downspins)
  sigma <- reactives/reactotals
  samsiz <- upspins + downspins

  # Estimate alpha
  emp_halpha <- sigma/(2*rho*(1 - rho))
  diff <- abs(alphahash$halpha - emp_halpha)
  closest <- which.min(diff)
  alpha <- alphahash$alpha[closest]

  # Return. First element of this vector is rho, second sigma, third is the
  # sample size, fourth is estimate of alpha.
  c(rho, sigma, samsiz, alpha)
}


# wals_maslova()
#
# Calculate Maslova stability estimates for WALS data, as outlined in
# the supplementary material to Dediu and Cysouw's meta-study.
#
# featuresize:      language sample size lower bound
# exactsample:      if positive, take samples of this exact size for each
#                   feature
#
wals_maslova <- function(folder = "../data/wals-20170718/features/",
                          featurefile = "../data/unsilly-features.csv",
                          infofile = "../data/wals-20170718/languoid.tab",
                          nnfile = "../data/wals-20170718/adjacencies.csv",
                          glob = ".tab.pruned.csv",
                          featurelim = NULL,
                          featuresize = 1,
                          exactsample = -1,
                          regions = NULL,
                          bootsize = 1,
                          confval = 0.025,
                          alpha_min = 0.000001,
                          alpha_max = 100,
                          alpha_res = 10000,
                          verbose = TRUE) {
  # Read in features dataframe and nearest neighbours table
  features <- read.csv(featurefile)
  nns <- read.csv(nnfile, stringsAsFactors=FALSE)

  # Limit analysis to subset of features, if desired (this functionality is
  # needed for analysis on Condor
  if (!is.null(featurelim)) {
    features <- features[features$feature %in% featurelim, ]
  }

  # Results will be collected in a dataframe which is "rbound" feature by
  # feature (we know, this is computationally ineffective, shhh...)
  res <- NULL

  # Loop through features
  valid_features_ctr <- 0
  for (i in 1:nrow(features)) {
    # Print status for the impatient
    if (verbose) {
      print(paste("Working on feature ", i, " of ", nrow(features), sep=""))
    }

    # We only take feature into account if it has at least 'featuresize'
    # languages in its language sample (to get rid of data that's too
    # noisy).
    if (as.numeric(features[i,]$sample.size) >= featuresize) {
      valid_features_ctr <- valid_features_ctr + 1
      thisfeat <- features[i,]$feature

      # Get nearest neighbours data for this particular feature
      thisnns <- nns[nns$feature == thisfeat, ]

      # Determine feature's domain
      area <- features[i,]$area
      hyperdomain <- features[i,]$our_area
      ourname <- paste(valid_features_ctr, hyperdomain, sep="")
      if (hyperdomain == "P") {
        hyperdomain <- "phonology"
        domno <- 1
      }
      else if (hyperdomain == "M") {
        hyperdomain <- "morphology"
        domno <- 2
      }
      else if (hyperdomain == "S") {
        hyperdomain <- "syntax"
        domno <- 3
      }
      else if (hyperdomain == "L") {
        hyperdomain <- "lexicon"
        domno <- 4
      }
      else {
        hyperdomain <- "other"
        domno <- 5
      }

      # Calculate rho and sigma for feature, bootstrapping
      bsdf <- rep(NA, times=bootsize)
      for (bs in 1:bootsize) {
        bsdf[bs] <- wals_maslova_onefeature(folder=folder, feature=thisfeat, down=features[i,]$down, up=features[i,]$up, resample=TRUE, glob=glob, regions=regions, infofile=infofile, exactsample=exactsample)
      }
      M <- median(bsdf, na.rm=TRUE)
      M_bot <- M - quantile(M - bsdf, na.rm=TRUE, prob=c(confval, 1-confval))[2]
      M_top <- M - quantile(M - bsdf, na.rm=TRUE, prob=c(confval, 1-confval))[1]

      thisfeatrow <- data.frame(ourname, thisfeat, as.numeric(features[i,]$sample.size), domno, hyperdomain, area, as.numeric(M_bot), as.numeric(M), as.numeric(M_top))
      names(thisfeatrow) <- c("feature", "WALS_feature", "sample_size", "domain_no", "domain", "WALS_domain", "M_bot", "M", "M_top")

      # Push to results dataframe
      res <- rbind(res, thisfeatrow)
    }
  }
  res
}


# wals_maslova_onefeature()
#
# Helper routine, called by wals_analysis().
#
wals_maslova_onefeature <- function(folder,
                                  feature,
                                  down,
                                  up,
                                  resample,
                                  exactsample,
                                  regions,
                                  infofile,
                                  glob) {
  # Read in feature and infodf
  file <- paste(folder, "/", feature, glob, sep="")
  infodf <- read.csv(infofile, sep="\t", stringsAsFactors=FALSE)
  df <- read.csv(file, sep="\t")
  copydf <- df

  # Down and up spin specifications into vectors
  down <- as.numeric(strsplit(as.character(down), split=",")[[1]])
  up <- as.numeric(strsplit(as.character(up), split=",")[[1]])

  # Binarize feature
  for (i in 1:nrow(df)) {
    if (df[i,]$value %in% down) {
      df[i,]$value <- -1
    } else if (df[i,]$value %in% up) {
      df[i,]$value <- 1
    } else {
      #      stop("problem with feature values! Halp!"
      df[i,]$value <- 0
    }
  }

  # Resample (with replacement) if necessary (for bootstrap)
  if (resample) {
    if (exactsample > 0) {
      sam <- sample(1:nrow(df), size=exactsample, replace=TRUE)
    } else {
      sam <- sample(1:nrow(df), size=nrow(df), replace=TRUE)
    }
  } else {
    sam <- 1:nrow(df)
  }

  # Break into two subsamples. We need to do this until P(A1) and P(A2) are
  # different, for otherwise Maslova stability estimation gives NaN.
  A = c(0,0)
  while (A[1] == A[2]) {
  sample1 <- sample(sam, size=floor(length(sam)/2), replace=FALSE)
  sample2 <- sample(sam, size=floor(length(sam)/2), replace=FALSE)

  # Cycle through languages in feature's language sample
  k <- 0
  upspins <- c(0,0)
  downspins <- c(0,0)
  reactives <- c(0,0)
  reactotals <- c(0,0)
  for (sam in list(sample1, sample2)) {
    k <- k + 1
  for (i in sam) {
    # Only if language is in the desired region
    thisregion <- infodf[infodf$wals.code == df$wals.code[i], ]$macroarea
    if (is.null(regions) || thisregion %in% regions) {
      # Find genus compatriot, sampling at random
      selfcode <- as.character(df$wals.code[i])
      selfgenus <- as.character(infodf[infodf$wals.code==selfcode, ]$genus)
      friends <- copydf[copydf$genus==selfgenus, ]$wals.code
      neicode <- as.character(sample(friends, size=1))

      # Is 'self' an upspin or a downspin
      if (df[i,]$value == 1) {
        upspins[k] <- upspins[k] + 1
      } else if (df[i,]$value == -1) {
        downspins[k] <- downspins[k] + 1
      }

      # Do 'self' and 'nei' DISagree?
      selfspin <- df[i,]$value
      neispin <- df[df$wals.code == neicode, ]$value
      #print(c(as.character(selfcode), neicode))
      #print(c(selfspin, neispin))
      if (selfspin != 0 && neispin != 0) {
        reactotals[k] <- reactotals[k] + 1
        if (neispin != selfspin) {
          reactives[k] <- reactives[k] + 1
        }
      }
    }
  }
  }

  # Compute rho, sigma, sample size
  rho <- c(0,0)
  Migma <- c(0,0)
  for (k in 1:2) {
    rho[k] <- upspins[k]/(upspins[k] + downspins[k])
    Migma[k] <- reactives[k]/reactotals[k]
  }
  A <- rho
  D <- Migma
  }

  lmdf <- data.frame(cbind(A, D))
  names(lmdf) <- c("A", "D")
  maslin <- lm(D~A, lmdf)
  alpha <- 0.5*maslin$coefficients[2]
  beta <- 0.5*maslin$coefficients[1]

  # Estimate Maslova stability
  #alpha <- 0.5*((D[1] - D[2])/(A[1] - A[2]))
  #beta <- 0.5*((A[1]*D[2] - A[2]*D[1])/(A[1] - A[2]))
  p_e_plus <- (1 + alpha + sqrt( (1-alpha)^2 - 4*beta))/2
  p_e_minus <- (1 + alpha - sqrt( (1-alpha)^2 - 4*beta))/2
  p_i_plus <- (1 - alpha + sqrt( (1-alpha)^2 - 4*beta))/2
  p_i_minus <- (1 - alpha - sqrt( (1-alpha)^2 - 4*beta))/2

  p_i <- p_i_minus
  p_e <- p_e_minus

  # Return.
  if (beta > 0 && beta > -alpha) {
    M <- 1 - (p_i + p_e)/2
  } else {
    M <- NA
  }
  M
}




# concat_condor()
#
# Concatenate result data frames from parallel Condor analysis.
#
concat_condor <- function(folder,
                          glob = ".res.csv") {
  files <- list.files(path=folder, pattern=glob, full.names=TRUE)
  res <- NULL
  for (file in files) {
    res <- rbind(res, read.csv(file, as.is=TRUE))
  }
  res[res$WALS_feature == "143",]$WALS_feature <- "143E"
  res
}


# get_halpha()
#
# Return h(alpha) as a function of alpha, as predicted by theory. Here,
# alpha equals tau, not -tau (old, inconsistent notation).
#
get_halpha <- function(alpha) {
  (pi*(1+alpha))/(2*get_K(1/(1+alpha))) - alpha
}


# get_K()
#
# Return our version of the complete elliptic integral of the first kind,
# computed via the arithmetic-geometric mean. Called by get_halpha().
#
get_K <- function(k) {
  (pi/2)/pracma_agmean(1, sqrt(1 - k^2))$agm
}


# make_table_of_halpha()
#
# Construct a lookup table of alpha and h(alpha) values. Used to numerically
# invert h(alpha), in order to arrive at empirical estimates of alpha for
# the WALS features.
#
make_table_of_halpha <- function(alpha = exp(seq(from=log(0.00001), to=log(100), length.out=10000))) {
  df <- cbind(alpha, sapply(alpha, get_halpha))
  df <- data.frame(df)
  names(df)<- c("alpha", "halpha")
  df
}


# estimate_alpha_for_feature()
#
# Estimates the value of alpha for a single feature from data.
#
# df:       dataframe outputted by wals_analysis()
# feature:  feature for which alpha is to be estimated
# hash:     alpha-h(alpha) lookup table outputted by make_table_of_halpha()
#
estimate_alpha_for_feature <- function(df,
                                       feature,
                                       hash) {
  tdf <- df[df$feature==feature, ]
  emp_halpha <- tdf$sigma/(2*tdf$rho*(1 - tdf$rho))
  diff <- abs(hash$halpha - emp_halpha)
  closest <- which.min(diff)
  hash$alpha[closest]
}


# estimate_alpha_for_df()
#
# Estimate alpha for an entire dataframe (i.e. for each feature in the
# dataframe). Output is in the form of the same dataframe with additional
# column for the alpha estimates, ordered by increasing alpha if desired.
#
# df:           dataframe outputted by wals_analysis()
# alpharange:   set of alphas to consider, i.e. "resolution"
# order:        Boolean
#
estimate_alpha_for_df <- function(df,
                                  alpharange = exp(seq(from=log(0.000001), to=log(10), length.out=10000)),
                                  order = FALSE) {
  hash <- make_table_of_halpha(alpha=alpharange)
  feats <- df$feature
  alphas <- rep(NA, length(feats))
  ctr <- 0
  for (feat in feats) {
    ctr <- ctr + 1
    alphas[ctr] <- estimate_alpha_for_feature(df=df, feature=feat, hash=hash)
  }
  out <- cbind(df, alphas)
  out <- data.frame(out)
  names(out)[ncol(out)] <- c("alpha")
  if (order) {
    out <- out[order(out$alpha), ]
  }
  out
}


# pracma_agmean()
#
# We borrow the agmean() function from the package 'pracma' here, since
# installing packages on Condor is a pain in the you know where.
#
pracma_agmean <- function (a, b) {
  eps <- .Machine$double.eps
  stopifnot(is.numeric(a) || is.complex(a), is.numeric(b) || 
            is.complex(b))
  if (is.numeric(a) && any(a < 0) || is.numeric(b) && any(b < 
                                                          0)) {
    a <- as.complex(a)
    b <- as.complex(b)
  }
  if (length(a) == 1) {
    n <- length(b)
    a <- rep(a, n)
  }
  else if (length(b) == 1) {
    n <- length(a)
    b <- rep(b, n)
  }
  else if (length(a) != length(b)) 
    stop("Arguments must have the same length or one has length 1.")
  niter = 0
  while (any(abs(a - b) >= eps)) {
    niter = niter + 1
    a1 <- (a + b)/2
    b1 <- sqrt(a * b)
    if (max(abs(a - a1)) < eps && max(abs(b - b1)) < eps) 
      break
    a <- a1
    b <- b1
  }
  return(list(agm = (a + b)/2, niter = niter, prec = max(abs(b - 
                                                             a))))
}


# Shepard-interpolation of the spatial distribution of a WALS feature.
#
interpolate_feature <- function(feature,
                                folder = "../data/wals-20170718/features",
                                glob = ".tab.pruned.csv",
                                featurefile = "../data/unsilly-features.csv",
                                p = 4,
                                xlim = c(-180, 180),
                                ylim = c(-90, 90),
                                xreso = 360,
                                yreso = 180) {
  require(phylin)
  require(maptools)
  require(stringr)

  # Construct a grid
  mygrid <- function(
                     xlim = c(-180,180),
                     ylim = c(-90,90),
                     length.out.x = 360,
                     length.out.y = 180) {
    mtx <- matrix(NA, nrow=length.out.x*length.out.y, ncol=2)
    ctr <- 0
    for (x in seq(from=xlim[1], to=xlim[2], length.out=length.out.x)) {
      for (y in seq(from=ylim[1], to=ylim[2], length.out=length.out.y)) {
        ctr <- ctr + 1
        mtx[ctr,1] <- x
        mtx[ctr,2] <- y
      }
    }
    mtx <- data.frame(mtx)
    names(mtx) <- c("x", "y")
    mtx
  }

  data(wrld_simpl)
  df <- read.csv(paste(folder, "/", feature, glob, sep=""), sep="\t")
  fdf <- read.csv(featurefile)
  ups <- as.numeric(str_split(fdf[fdf$feature == feature, ]$up, pattern=",")[[1]])
  downs <- as.numeric(str_split(fdf[fdf$feature == feature, ]$down, pattern=",")[[1]])
  for (i in 1:nrow(df)) {
    if (df[i,]$value %in% ups) {
      df[i,]$value <- 1
    }
    else if (df[i,]$value %in% downs) {
      df[i,]$value <- 0
    }
    else {
      df[i,]$value <- NA
    }
  }
  df <- df[!is.na(df$value), ]
  thisgrid <- mygrid(xlim=xlim, ylim=ylim, length.out.x=xreso, length.out.y=yreso)
  # see http://gis.stackexchange.com/questions/75033/given-a-lat-and-lon-identify-if-the-point-is-over-land-or-ocean
  pts <- SpatialPoints(thisgrid, proj4string=CRS(proj4string(wrld_simpl)))
  whereisit <- over(pts, wrld_simpl)$FIPS
  thisgrid <- data.frame(cbind(thisgrid, whereisit))

  # for (i in 1:nrow(thisgrid)) {
  # 	pts <- SpatialPoints(thisgrid[i,], proj4string=CRS(proj4string(wrld_simpl)))
  # 	if (is.na(over(pts, wrld_simpl)$FIPS) || over(pts, wrld_simpl)$FIPS == "AY") {
  # 		thisgrid[i,] <- c(NA, NA)
  # 	}
  # }
  thisgrid <- thisgrid[!is.na(thisgrid$whereisit) & thisgrid$whereisit != "AY", ]
  thisgrid <- thisgrid[,1:2]
  outdf <- cbind(thisgrid, idw(values=df$value, coords=cbind(df$longitude, df$latitude), p=p, grid=thisgrid))
  outdf <- data.frame(outdf)
  names(outdf) <- c("x", "y", "z")
  outdf
}


# wals_geocomparison()
#
# Compare two geographical subsets of WALS.
#
wals_geocomparison <- function(df1,
                               df2) {
  # can only compare features which are attested in both datasets
  df1 <- df1[df1$WALS_feature %in% df2$WALS_feature, ]
  df2 <- df2[df2$WALS_feature %in% df1$WALS_feature, ]
  df1$rho <- df1$rho - df2$rho
  df1$sigma <- df1$sigma - df2$sigma
  df1
}


# find_outliers()
#
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
  results
}


# feature_interpolations()
#
# Interpolate features for paper.
#
feature_interpolations <- function(mapreso = 2,
                                   first = "37A",
                                   second = "83A",
                                   interp_p = 10,
                                   simulations = FALSE) {
  xreso <- mapreso*360
  yreso <- mapreso*180
  xlim <- c(-30, 150)
  ylim_map <- c(-60, 90)
  ylim <- c(-60, 90)
  one <- interpolate_feature(first, p=interp_p, xlim=xlim, ylim=ylim, xreso=xreso, yreso=yreso)
  if (simulations) {
    one_simu <- interpolate_feature(first, folder="../simulations-final/", glob=".csv", p=interp_p, xlim=xlim, ylim=ylim, xreso=xreso, yreso=yreso)
  } else {
    one_simu <- NA
  }
  two <- interpolate_feature(second, p=interp_p, xlim=xlim, ylim=ylim, xreso=xreso, yreso=yreso)
  if (simulations) {
    two_simu <- interpolate_feature(second, folder="../simulations-final/", glob=".csv", p=interp_p, xlim=xlim, ylim=ylim, xreso=xreso, yreso=yreso)
  } else {
    two_simu <- NA
  }
  list(one=one, two=two, one_simu=one_simu, two_simu=two_simu)
}





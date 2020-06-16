###########
# analyse.R
#
# WALS data analysis.
#
# NB. Alpha means tau, passim. This is because notation changed midway through
# the preparation of this paper.
#
# Henri Kauhanen 2016-2019
##########################


wals_analysis_correlations <- function(featurefile,
                                          datafolder,
                                          sample_size = 300,
                                          neighbourhood = 4,
                                          km = FALSE,
                                          tau_min = 0.000001,
                                          tau_max = 100,
                                          tau_res = 10000) {
  # Set up a tau hash. This is used to invert the H(tau) function.
  tauhash <- make_table_of_halpha(alpha=exp(seq(from=log(tau_min), to=log(tau_max), length.out=tau_res)))

  # Set up features
  features <- read.csv(featurefile)
  features <- features[features$sample.size >= sample_size, ]
  features <- features$feature

  # Partition the sample of 'sample_size' into two equal-sized non-intersecting
  # portions
  sub1 <- sample(1:sample_size, size=floor(sample_size/2), replace=FALSE)
  sub2 <- (1:sample_size)[which(!((1:sample_size) %in% sub1))]
  subs <- list(sub1, sub2)
  tocorr <- expand.grid(subset=1:2, feature=features, tau=NA)
  for (feature in features) {
    for (subb in 1:2) {
    tocorr[tocorr$subset==subb & tocorr$feature==feature, ]$tau <- wals_analysis_correlations_onefeature(file=paste0(datafolder, "/", feature, ".binarized.csv"), sample_size=sample_size, sub=subs[[subb]], neighbourhood=neighbourhood, km=km, tauhash=tauhash)$tau
    }
  }
  correl <- cor.test(tocorr[tocorr$sub==1, ]$tau, tocorr[tocorr$sub==2, ]$tau, method="spearman")
  data.frame(sample_size=sample_size, neighbourhood=neighbourhood, km=km, spearman=correl$estimate, p.value=correl$p.value)
}


wals_analysis_correlations_onefeature <- function(file,
                             sample_size,
                             sub,
                             neighbourhood,
                             km,
                             tauhash) {
  # Read in the datafile for the requested feature.
  df <- read.csv(file)

  # Put output in this dataframe.
  feature <- strsplit(file, "/")[[1]]
  feature <- feature[length(feature)]
  feature <- strsplit(feature, "\\.")[[1]]
  feature <- feature[1]
  out <- data.frame(feature=feature, sample_size=sample_size, neighbourhood=neighbourhood, km=km, rho=NA, sigma=NA, tau=NA)

  # Analyse.
  rep <- 1
    # Take a subset of the feature's language sample.
    subsample <- sample(1:nrow(df), size=sample_size, replace=FALSE)
    repdf <- df[subsample, ]
    subdf <- repdf[sub, ]

    # Variables to hold absolute numbers of up-spins and isoglosses
    ups <- 0
    isos <- 0
    interfaces <- 0

    # Cycle through languages
    for (lang in 1:nrow(subdf)) {
      # If language is an up-spin, increment ups counter
      if (subdf[lang, ]$value == 1) {
        ups <- ups + 1
      }

      # Find nearest neighbours
      if (km) {
      nn <- nearest_neighbours_km(x=lang, df=subdf, lim=neighbourhood)
      } else {
      nn <- nearest_neighbours(x=lang, df=subdf, which=1:neighbourhood)
      }

      # Add the number of isoglosses and the total number of interfaces
      # to relevant counters
      if (length(nn) > 0) {
        isos <- isos + sum(subdf[nn, ]$value != subdf[lang, ]$value)
        interfaces <- interfaces + length(nn)
      }
    }

    # Feature frequency rho is number of up-spins divided by number of
    # languages
    rho <- ups/sample_size
    out[rep, ]$rho <- rho

    # Isogloss density sigma is number of isoglosses divided by number
    # of interfaces
    if (interfaces != 0) {
      sigma <- isos/interfaces
    } else {
      sigma <- NA
    }
    out[rep, ]$sigma <- sigma

    # For temperature tau, we calculate h(tau) and find the closest
    # value in the tau hash
    if (!is.na(sigma)) {
      htau <- sigma/(2*rho*(1-rho))
      closest <- closest_in_vector(x=htau, vec=tauhash$halpha)
      tau <- tauhash[closest, ]$alpha
      out[rep, ]$tau <- tau
    } else {
      out[rep, ]$tau <- NA
  }

  # Return
  out
}



wals_analysis_km <- function(file,
                             sample_size,
                             kmlimit,
                             reps,
                             tau_min = 0.000001,
                             tau_max = 100,
                             tau_res = 10000) {
  # Set up a tau hash. This is used to invert the H(tau) function.
  tauhash <- make_table_of_halpha(alpha=exp(seq(from=log(tau_min), to=log(tau_max), length.out=tau_res)))

  # Read in the datafile for the requested feature.
  df <- read.csv(file)

  # Put output in this dataframe.
  feature <- strsplit(file, "/")[[1]]
  feature <- feature[length(feature)]
  feature <- strsplit(feature, "\\.")[[1]]
  feature <- feature[1]
  out <- data.frame(feature=feature, rep=1:reps, sample_size=sample_size, kmlimit=kmlimit, rho=NA, sigma=NA, tau=NA)

  # Analyse.
  for (rep in 1:reps) {
    # Variables to hold absolute numbers of up-spins and isoglosses
    ups <- 0
    isos <- 0
    interfaces <- 0

    # Take a subset of the feature's language sample.
    subsample <- sample(1:nrow(df), size=sample_size, replace=FALSE)
    subdf <- df[subsample, ]

    # Cycle through languages
    for (lang in 1:nrow(subdf)) {
      # If language is an up-spin, increment ups counter
      if (subdf[lang, ]$value == 1) {
        ups <- ups + 1
      }

      # Find nearest neighbours
      nn <- nearest_neighbours_km(x=lang, df=subdf, lim=kmlimit)

      # Add the number of isoglosses and the total number of interfaces
      # to relevant counters
      if (length(nn) > 0) {
        isos <- isos + sum(subdf[nn, ]$value != subdf[lang, ]$value)
        interfaces <- interfaces + length(nn)
      }
    }

    # Feature frequency rho is number of up-spins divided by number of
    # languages
    rho <- ups/sample_size
    out[rep, ]$rho <- rho

    # Isogloss density sigma is number of isoglosses divided by number
    # of interfaces
    if (interfaces != 0) {
      sigma <- isos/interfaces
    } else {
      sigma <- NA
    }
    out[rep, ]$sigma <- sigma

    # For temperature tau, we calculate h(tau) and find the closest
    # value in the tau hash
    if (!is.na(sigma)) {
      htau <- sigma/(2*rho*(1-rho))
      closest <- closest_in_vector(x=htau, vec=tauhash$halpha)
      tau <- tauhash[closest, ]$alpha
      out[rep, ]$tau <- tau
    } else {
      out[rep, ]$tau <- NA
    }
  }

  # Return
  out
}


nearest_neighbours_km <- function(x,
                                  df,
                                  lim) {
  # R trigonometric functions expect angles in radians; WALS has degrees
  df$latitude <- (pi/180)*df$latitude
  df$longitude <- (pi/180)*df$longitude

  # Add a column containing the index of self
  df$self <- x

  # To expedite calculations
  lat_self <- df[x, ]$latitude
  lon_self <- df[x, ]$longitude

  # Columns for other and distance
  df$other <- 1:nrow(df)
  df$dist <- NA

  # Go through all other languages, computing Haversine distance, put
  # in the data frame
  for (i in 1:nrow(df)) {
    df[i,]$dist <- haversine(lat1=lat_self, lon1=lon_self, lat2=df[i,]$latitude, lon2=df[i,]$longitude, R=6371)
  }

  # Order by increasing distance, remove self (distance == 0)
  df <- df[order(df$dist, decreasing=FALSE), ]
  df <- df[df$self != df$other, ]

  # Return
  df[df$dist <= lim, ]$other
}



wals_analysis <- function(file,
                          sample_size,
                          neighbourhood_size,
                          inclusive_neighbourhood = TRUE,
                          reps,
                          tau_min = 0.000001,
                          tau_max = 100,
                          tau_res = 10000) {
  # Set up a tau hash. This is used to invert the H(tau) function.
  tauhash <- make_table_of_halpha(alpha=exp(seq(from=log(tau_min), to=log(tau_max), length.out=tau_res)))

  # Read in the datafile for the requested feature.
  df <- read.csv(file)

  # Put output in this dataframe.
  feature <- strsplit(file, "/")[[1]]
  feature <- feature[length(feature)]
  feature <- strsplit(feature, "\\.")[[1]]
  feature <- feature[1]
  out <- data.frame(feature=feature, rep=1:reps, sample_size=sample_size, neighbourhood_size=neighbourhood_size, inclusive_neighbourhood=inclusive_neighbourhood, rho=NA, sigma=NA, tau=NA)

  # Analyse.
  for (rep in 1:reps) {
    # Variables to hold absolute numbers of up-spins and isoglosses
    ups <- 0
    isos <- 0

    # Take a subset of the feature's language sample.
    subsample <- sample(1:nrow(df), size=sample_size, replace=FALSE)
    subdf <- df[subsample, ]

    # Cycle through languages
    for (lang in 1:nrow(subdf)) {
      # If language is an up-spin, increment ups counter
      if (subdf[lang, ]$value == 1) {
        ups <- ups + 1
      }

      # Find nearest neighbours
      if (inclusive_neighbourhood) {
        nn <- nearest_neighbours(x=lang, df=subdf, which=1:neighbourhood_size)
      } else {
        nn <- nearest_neighbours(x=lang, df=subdf, which=neighbourhood_size)
      }

      # Add the number of isoglosses to isogloss counter
      isos <- isos + sum(subdf[nn, ]$value != subdf[lang, ]$value)
    }

    # Feature frequency rho is number of up-spins divided by number of
    # languages
    rho <- ups/sample_size
    out[rep, ]$rho <- rho

    # Isogloss density sigma is number of isoglosses divided by number
    # of interfaces
    if (inclusive_neighbourhood) {
      n_int <- neighbourhood_size*sample_size
    } else {
      n_int <- sample_size
    }
    sigma <- isos/n_int
    out[rep, ]$sigma <- sigma

    # For temperature tau, we calculate h(tau) and find the closest
    # value in the tau hash
    htau <- sigma/(2*rho*(1-rho))
    closest <- closest_in_vector(x=htau, vec=tauhash$halpha)
    tau <- tauhash[closest, ]$alpha
    out[rep, ]$tau <- tau
  }

  # Return
  out
}


nearest_neighbours <- function(x,
                               df,
                               which = 1) {
  # R trigonometric functions expect angles in radians; WALS has degrees
  df$latitude <- (pi/180)*df$latitude
  df$longitude <- (pi/180)*df$longitude

  # Add a column containing the index of self
  df$self <- x

  # To expedite calculations
  lat_self <- df[x, ]$latitude
  lon_self <- df[x, ]$longitude

  # Columns for other and distance
  df$other <- 1:nrow(df)
  df$dist <- NA

  # Go through all other languages, computing Haversine distance, put
  # in the data frame
  for (i in 1:nrow(df)) {
    df[i,]$dist <- haversine(lat1=lat_self, lon1=lon_self, lat2=df[i,]$latitude, lon2=df[i,]$longitude, R=6371)
  }

  # Order by increasing distance, remove self (distance == 0)
  df <- df[order(df$dist, decreasing=FALSE), ]
  df <- df[df$self != df$other, ]

  # Return
  df$other[which]
}


haversine <- function(lat1,
                      lat2,
                      lon1,
                      lon2,
                      R) {
  2*R*asin(sqrt((sin(0.5*(lat2-lat1)))^2 + cos(lat1)*cos(lat2)*(sin(0.5*(lon2-lon1)))^2))
}


# closest_in_vector()
#
# Find the index of the element in a vector that has the smallest
# (Euclidean) distance to a reference value
#
closest_in_vector <- function(x,
                              vec) {
  dists <- (vec - x)^2
  which.min(dists)
}


# concat_condor()
#
# Concatenate result data frames from parallel Condor analysis.
#
concat_condor <- function(folder,
                          glob = "^res.") {
  files <- list.files(path=folder, pattern=glob, full.names=TRUE)
  res <- NULL
  for (file in files) {
    res <- rbind(res, read.csv(file, as.is=TRUE))
  }
  res[res$feature == "143",]$feature <- "143E"
  res
}


# concat_condor2()
#
# Concatenate result data frames from parallel Condor analysis.
#
concat_condor2 <- function(folder,
                          glob = "^res.") {
  files <- list.files(path=folder, pattern=glob, full.names=TRUE)
  res <- NULL
  for (file in files) {
    res <- rbind(res, read.csv(file, as.is=TRUE))
  }
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
make_table_of_halpha <- function(alpha = exp(seq(from=log(0.0000001), to=log(1000), length.out=10000))) {
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

wals_runs_regressions_twosets <- function(df1,
                                          df2,
                                          reps) {
  out <- wals_runs_regressions(df1, reps=reps)
  out$daata <- "df1"
  out2 <- wals_runs_regressions(df2, reps=reps)
  out2$daata <- "df2"
  out <- rbind(out, out2)
  out <- out[out$neighbourhood_size != 299, ]
  out
}


wals_runs_regressions <- function(df,
                                  scales = NA,
                                  reps = 1,
                                  replace = FALSE,
                                  coefficient = "spearman",
                                  alpha = 0.05) {
  if (!is.na(scales)) {
    df <- df[df$neighbourhood_size %in% scales, ]
  }
  out <- expand.grid(rep=1:reps, neighbourhood_size=unique(df$neighbourhood_size), coefficient=coefficient, value=NA, p.value=NA, alphalevel=alpha, significant=FALSE)
  for (i in 1:nrow(out)) {
    thisdf <- df[df$neighbourhood_size==out[i,]$neighbourhood_size, ]

    # take two reps at random
    reps <- sample(unique(thisdf$rep), size=2, replace=replace)
    thisdf1 <- thisdf[thisdf$rep == reps[1], ]
    thisdf2 <- thisdf[thisdf$rep == reps[2], ]

    # correlate
    cort <- cor.test(thisdf1$tau, thisdf2$tau, method=coefficient)
    out[i,]$value <- cort$estimate
    out[i,]$p.value <- cort$p.value
    if (!is.na(cort$p.value)) {
      if (cort$p.value < alpha) {
        out[i,]$significant <- TRUE
      }
    }
  }
  out
}


correlate_within_radii <- function(df,
                                   radii = unique(df$kmlimit)) {
  reps <- unique(df$rep)
  out <- expand.grid(kmlimit=radii, subset1=reps, subset2=reps, spearman=NA, p.value=NA)
  for (i in 1:nrow(out)) {
    if (out[i,]$subset1 != out[i,]$subset2) {
      df1 <- df[df$rep==out[i,]$subset1 & df$kmlimit==out[i,]$kmlimit, ]
      df2 <- df[df$rep==out[i,]$subset2 & df$kmlimit==out[i,]$kmlimit, ]
      correl <- cor.test(df1$tau, df2$tau, method="spearman")
      out[i,]$spearman <- correl$estimate
      out[i,]$p.value <- correl$p.value
    }
  }
  out <- out[!is.na(out$spearman), ]
  out
}

##################
# interpolations.R
#
# WALS map interpolations.
#
# NB. Alpha means tau, passim. This is because notation changed midway through
# the preparation of this paper.
#
# Henri Kauhanen 2016-2019
##########################



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


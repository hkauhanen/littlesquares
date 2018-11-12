# simulate.R
#
# Run simulations of model.
#
# Henri Kauhanen 2016-2017
##########################


# simu_walsian()
#
# Run simulations of model on a directed graph of nearest-neighbour
# adjacencies determined from WALS data, within a given longitude-
# latitude bounding box.
#
simu_walsian <- function(feature,
                         outfile,
                         xlim = c(-180, 180),
                         ylim = c(-90, 90),
                         iter = 100,
                         pF,
                         qF,
                         r,
                         folder = "../data/wals-20170718/features/",
                         featurefile = "../data/unsilly-features.csv",
                         infofile = "../data/wals-20170718/languoid.tab",
                         nnfile = "../data/wals-20170718/adjacencies.csv") {
  # Get adjacencies, supply table with latitudes and longitudes, prune
  # those points falling outside bounding box
  df <- read.csv(nnfile, stringsAsFactors=FALSE)
  df <- df[df$feature == feature,]
  df <- cbind(df, rep(0, nrow(df)), rep(0, nrow(df)))
  names(df)[ncol(df)-1] <- "longitude"
  names(df)[ncol(df)] <- "latitude"
  featdata <- read.csv(paste(folder, "/", feature, ".tab.pruned.csv", sep=""), stringsAsFactors=FALSE, sep="\t")
  for (i in 1:nrow(df)) {
    this_id <- df[i,]$id
    this_lon <- featdata[featdata$wals.code == this_id, ]$longitude
    this_lat <- featdata[featdata$wals.code == this_id, ]$latitude
    df[i,]$longitude <- this_lon
    df[i,]$latitude <- this_lat
  }
  df <- df[df$longitude >= xlim[1] & df$longitude <= xlim[2], ]
  df <- df[df$latitude >= ylim[1] & df$latitude <= ylim[2], ]

  # Initialize spins. Spin values we get from <featurefile>, so that the
  # results of this simulation can be easily fed to our plotting routines.
  features <- read.csv(featurefile, stringsAsFactors=FALSE)
  features <- features[features$feature==feature, ]
  upspin <- strsplit(features$up, ",")[[1]][1]
  downspin <- strsplit(features$down, ",")[[1]][1]
  df <- cbind(df, sample(c(downspin,upspin), size=nrow(df), replace=TRUE))
  names(df)[ncol(df)] <- "spin"

  # Simulate
  for (simustep in 1:iter) {
    # Pick a site at random
    locus <- sample(1:nrow(df), size=1)

    # Get neighbour's spin and copy to locus
    neigh_id <- df[locus,]$nn
    neigh_row <- df[df$id == neigh_id,]

    # Some id's don't have a neighbour within the bounding box, in which
    # case neigh_row is a dataframe with 0 rows. Just skip these, treating
    # them as constant boundary conditions.
    if (nrow(neigh_row) != 0) {
      # Spatial event
      if (runif(1) < r) {
        neigh_spin <- neigh_row$spin
        df[locus,]$spin <- neigh_spin
      } else {
        # Non-spatial event
        if (df[locus,]$spin == upspin) {
          if (runif(1) < qF) {
            df[locus,]$spin <- downspin
          }
        } else if (df[locus,]$spin == downspin) {
          if (runif(1) < pF) {
            df[locus,]$spin <- upspin
          }
        }
      }
    }
  }

  # Return
  names(df)[2] <- "wals.code"
  names(df)[ncol(df)] <- "value"
  write.table(df, file=outfile, sep="\t", row.names=FALSE)
}


# simu_walsian_wrapper()
#
# Wrapper for simu_walsian(); gets the simulation parameters from an
# empirical dataframe.
#
simu_walsian_wrapper <- function(walsdf,
                                 feature,
                                 xlim,
                                 ylim,
                                 iter,
                                 outfile,
                                 r,
                         folder = "../data/wals-20170718/features/",
                         featurefile = "../data/unsilly-features.csv",
                         infofile = "../data/wals-20170718/languoid.tab",
                         nnfile = "../data/wals-20170718/adjacencies.csv") {
  df <- walsdf[walsdf$WALS_feature==feature,]
  rho <- df$rho
  sigma <- df$sigma
  tau <- df$alpha
  pF <- (rho*tau*r)/(1-r)
  qF <- ((1-rho)/rho)*pF
  simu_walsian(feature=feature, outfile=outfile, xlim=xlim, ylim=ylim, iter=iter, pF=pF, qF=qF, r=r, folder=folder, featurefile=featurefile, infofile=infofile, nnfile=nnfile)
}


# simu_walsian_fortran()
#
# Like simu_walsian_wrapper(), but implemented in Fortran for added speed.
#
simu_walsian_fortran <- function(walsdf,
                                 feature,
                                 xlim,
                                 ylim,
                                 iter,
                                 outfile,
                                 r,
                                 tmpdir = "../tmp/",
                                 fortran_source = "../fortran/empirical.f90",
                         folder = "../data/wals-20170718/features/",
                         featurefile = "../data/unsilly-features.csv",
                         infofile = "../data/wals-20170718/languoid.tab",
                         nnfile = "../data/wals-20170718/adjacencies.csv") {
  oldsp <- getOption("scipen")
  options(scipen=100)

  df <- walsdf[walsdf$WALS_feature==feature,]
  rho <- df$rho
  sigma <- df$sigma
  tau <- df$alpha
  pF <- (rho*tau*r)/(1-r)
  qF <- ((1-rho)*tau*r)/(1-r)

  # Get adjacencies, supply table with latitudes and longitudes, prune
  # those points falling outside bounding box
  df <- read.csv(nnfile, stringsAsFactors=FALSE)
  df <- df[df$feature == feature,]
  df <- cbind(df, rep(0, nrow(df)), rep(0, nrow(df)))
  names(df)[ncol(df)-1] <- "longitude"
  names(df)[ncol(df)] <- "latitude"
  featdata <- read.csv(paste(folder, "/", feature, ".tab.pruned.csv", sep=""), stringsAsFactors=FALSE, sep="\t")
  for (i in 1:nrow(df)) {
    this_id <- df[i,]$id
    this_lon <- featdata[featdata$wals.code == this_id, ]$longitude
    this_lat <- featdata[featdata$wals.code == this_id, ]$latitude
    df[i,]$longitude <- this_lon
    df[i,]$latitude <- this_lat
  }
  df <- df[df$longitude >= xlim[1] & df$longitude <= xlim[2], ]
  df <- df[df$latitude >= ylim[1] & df$latitude <= ylim[2], ]

  # Initialize spins. Spin values we get from <featurefile>, so that the
  # results of this simulation can be easily fed to our plotting routines.
  features <- read.csv(featurefile, stringsAsFactors=FALSE)
  features <- features[features$feature==feature, ]
  upspin <- strsplit(features$up, ",")[[1]][1]
  downspin <- strsplit(features$down, ",")[[1]][1]
  df <- cbind(df, sample(c(-1,1), size=nrow(df), replace=TRUE))
  names(df)[ncol(df)] <- "spin"
  outdf <- df

  # Turn language WALS codes into numbers for easier Fortran handling
  hash <- data.frame(cbind(unique(df$id), 1:length(unique(df$id))))
  names(hash) <- c("char", "num")
  for (i in 1:nrow(df)) {
    df[i,]$id <- hash[hash$char == df[i,]$id, ]$num
    if (nrow(hash[hash$char == df[i,]$nn, ]) == 0) {
      df[i,]$nn <- 0
    } else {
      df[i,]$nn <- hash[hash$char == df[i,]$nn, ]$num
    }
  }
  tofortran <- data.frame(cbind(as.numeric(df$id), as.numeric(df$nn), as.numeric(df$spin)))
  names(tofortran) <- c("id", "nn", "spin")
  tofortran <- tofortran[order(tofortran$id),]
  dimension <- nrow(tofortran)
  write.table(file=paste(tmpdir, "/tofortran.tab", sep=""), tofortran, sep="\t", row.names=FALSE, col.names=FALSE)

  # Compile and run Fortran routine
  system(paste("echo '#define _DIMENSION_ ", dimension, "\n' > ", tmpdir, "/empirical.f90", sep=""))
  system(paste("cat ", fortran_source, " >> ", tmpdir, "/empirical.f90", sep=""))
  system(paste("gfortran -cpp -o ", tmpdir, "/empirical ", tmpdir, "/empirical.f90", sep=""))
  system(paste(tmpdir, "/empirical ", tmpdir, "/tofortran.tab ", tmpdir, "/fort.tab ", pF, " ", qF, " ", r, " ", iter, sep=""))

  # Reimport to R, prettify
  fortout <- data.frame(read.csv(paste(tmpdir, "/fort.tab", sep=""), sep="", header=FALSE))
  names(fortout) <- c("id", "nn", "spin")
  for (i in 1:nrow(outdf)) {
    fortspin <- fortout[as.numeric(fortout$id) == as.numeric(hash[hash$char == outdf[i,]$id,]$num),]$spin
    if (fortspin == -1) {
      outdf[i,]$spin <- as.numeric(downspin)
    } else if (fortspin == 1) {
      outdf[i,]$spin <- as.numeric(upspin)
    } else {
      stop("Something weird happening")
    }
  }

  # Writeout
  names(outdf)[2] <- "wals.code"
  names(outdf)[ncol(outdf)] <- "value"
  write.table(outdf, file=outfile, sep="\t", row.names=FALSE)
  options(scipen=oldsp)
}


put_grid_on_world <- function(feature,
                              walsdf,
                              r,
                              iter,
                              outfile,
                              xlim,
                              ylim,
                              by,
                              tmpdir = "../tmp/",
                         featurefile = "../data/unsilly-features.csv",
                              fortran_source = "../fortran/grid_empirical.f90") {
  require(maptools)
  options(scipen=100)

  # Set model parameters
  df <- walsdf[walsdf$WALS_feature==feature,]
  rho <- df$rho
  sigma <- df$sigma
  tau <- df$alpha
  pF <- (rho*tau*r)/(1-r)
  qF <- ((1-rho)*tau*r)/(1-r)

  # Get spin values from <featurefile>, so that the results of this
  # simulation can be easily fed to our plotting routines
  features <- read.csv(featurefile, stringsAsFactors=FALSE)
  features <- features[features$feature==feature, ]
  upspin <- strsplit(features$up, ",")[[1]][1]
  downspin <- strsplit(features$down, ",")[[1]][1]

  # Construct grid
  x <- seq(from=xlim[1], to=xlim[2], by=by)
  y <- seq(from=ylim[1], to=ylim[2], by=by)
  grid <- expand.grid(x, y)
  grid <- cbind(grid, matrix(0, ncol=4, nrow=nrow(grid)), sample(c(-1,1), size=nrow(grid), replace=TRUE), rep("somewhere", nrow(grid)))
  grid <- data.frame(grid)
  names(grid) <- c("lon", "lat", "north", "south", "east", "west", "value", "isOver")

  # Fix boundaries
  #grid[grid$lon == xlim[1] | grid$lon == xlim[2], ]$value <- 0
  #grid[grid$lat == ylim[1] | grid$lat == ylim[2], ]$value <- 0

  # Remove points falling in water
  latslons <- cbind(grid$lon, grid$lat)
  pts <- SpatialPoints(latslons, proj4string=CRS(proj4string(wrld_simpl)))
  grid$isOver <- over(pts, wrld_simpl)$FIPS
  grid <- grid[!is.na(grid$isOver), ]

  # Give each row an index, get rid of isOver column
  grid <- data.frame(cbind(1:nrow(grid), grid[,-ncol(grid)]))
  names(grid)[1] <- "index"

  # Determine neighbours
  for (i in 1:nrow(grid)) {
    north <- c(grid[i,]$lon, grid[i,]$lat + by)
    south <- c(grid[i,]$lon, grid[i,]$lat - by)
    east <- c(grid[i,]$lon + by, grid[i,]$lat)
    west <- c(grid[i,]$lon - by, grid[i,]$lat)
    clist <- list(north, south, east, west)
    cardinals <- c("north", "south", "east", "west")
    for (j in 1:4) {
    if (nrow(grid[grid$lon==clist[[j]][1] & grid$lat==clist[[j]][2], ]) == 0) {
      grid[i,][[cardinals[j]]] <- 0
    } else if (nrow(grid[grid$lon==clist[[j]][1] & grid$lat==clist[[j]][2], ]) == 1) {
      grid[i,][[cardinals[j]]] <- grid[grid$lon==clist[[j]][1] & grid$lat==clist[[j]][2], ]$index
    }
    else {
      stop("Problem!")
    }
    }
  }

  # Write grid for use in Fortran
  write.table(file=paste(tmpdir, "/tofortran.tab", sep=""), grid, sep="\t", row.names=FALSE, col.names=FALSE)

  # Compile and run Fortran routine
  dimension <- nrow(grid)
  system(paste("echo '#define _DIMENSION_ ", dimension, "\n' > ", tmpdir, "/empirical.f90", sep=""))
  system(paste("cat ", fortran_source, " >> ", tmpdir, "/empirical.f90", sep=""))
  system(paste("gfortran -cpp -o ", tmpdir, "/empirical ", tmpdir, "/empirical.f90", sep=""))
  system(paste(tmpdir, "/empirical ", tmpdir, "/tofortran.tab ", tmpdir, "/fort.tab ", pF, " ", qF, " ", r, " ", iter, sep=""))

  # Reimport to R, prettify, writeout
  fortout <- data.frame(read.csv(paste(tmpdir, "/fort.tab", sep=""), sep="", header=FALSE))
  names(fortout) <- c("index", "longitude", "latitude", "north", "south", "east", "west", "value")
  outdf <- data.frame(cbind(rep(feature, nrow(fortout)), fortout))
  names(outdf)[1] <- "feature"
  for (i in 1:nrow(outdf)) {
    if (outdf[i,]$value == -1) {
      outdf[i,]$value <- as.numeric(downspin)
    } else if (outdf[i,]$value == 1) {
      outdf[i,]$value <- as.numeric(upspin)
    } else {
      stop("Something weird happened!")
    }
  }
  write.table(outdf, file=outfile, sep="\t", row.names=FALSE)
}

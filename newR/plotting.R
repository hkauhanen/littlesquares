############
# plotting.R
#
# Plotting routines.
#
# Henri Kauhanen 2016-2019
##########################


require(extrafont)
loadfonts()
#require(tidyverse)
require(shades)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(RColorBrewer)
#source("analyse.R")
source("../newR/tabulate.R")
source("../newR/stats.R")

# deffam
#
# Default font family for plots
#
deffam <- "Arial"

# defsize
#
# Default figure width and height
#
defsize <- 3.5

# defmar
#
# Default figure margins
#
defmar <- c(4.0,3,1.0,0) + 0.1

# domcol
#
# Default colour palette for domains.
#
domcol <- c("#e41a1c", "#377eb8", "#ff7f00", "#984ea3")

# geocol
#
# Default colour palette for maps.
#
#geocol <- rev(c("#a00000", "#ff0000", "white", "#0000ff", "#5050ff"))
geocol <- c("#c6c800", "#e3e600", "white", "#008ee6", "#0077c3")



# plot_all_for_paper()
#
# Plot all figures for the manuscript, (over)writing them in <folder>.
# We also pretty-print tables for the appendices.
#
# NB. Takes a while because of (plotting) the interpolations!
#
load("../newR/inters_20180411.RData")
plot_all_for_paper <- function(folder,
                               which = c(1,2,3,4,5,"A1","A2","A3"),
                               walsdf = results.main_analysis,
                               neighbourhood_size = 10,
                               crosshairs = TRUE,
                               interpolations = inters_20180411,
                               replot_interpolations = TRUE,
                               first_inter = "37A",
                               second_inter = "83A",
                               plot_inter_simulations = FALSE,
                               simudir = "../simulations-final/",
                               mapreso = 2) {
  require(stringr)

  # transform new-format data frame to be suitable with old plotting code
  full_walsdf <- walsdf
  df <- walsdf[walsdf$neighbourhood_size==neighbourhood_size, ]
  out <- data.frame(WALS_feature=unique(df$feature_ID), rho_bot=NA, rho=NA, rho_top=NA, sigma_bot=NA, sigma=NA, sigma_top=NA, alpha=NA)
  for (i in 1:nrow(out)) {
    thisdf <- df[df$feature_ID==out[i,]$WALS_feature, ]
    rhos <- quantile(thisdf$feature_frequency, probs=c(0.025, 0.5, 0.975), na.rm=TRUE)
    sigs <- quantile(thisdf$isogloss_density, probs=c(0.025, 0.5, 0.975), na.rm=TRUE)
    out[i,]$rho_bot <- rhos[1]
    out[i,]$rho <- rhos[2]
    out[i,]$rho_top <- rhos[3]
    out[i,]$sigma_bot <- sigs[1]
    out[i,]$sigma <- sigs[2]
    out[i,]$sigma_top <- sigs[3]
    out[i,]$alpha <- median(thisdf$temperature, na.rm=TRUE)
  }
  walsdf <- out

  if ("3" %in% which) {
  if (replot_interpolations) {
  # Plot the interpolations. First we set some global parameters.
  xlim <- c(-30, 150)
  #xlim <- c(-180, 180)
  ylim_map <- c(-60, 90)
  ylim <- c(-60, 90)
  bg <- "#ddf1ff"
  mapmar <- c(0,0,0,0)
  pointsize <- 3.0
  projection <- "albers"
  parameters <- c(10, 40)
  #projection <- "mollweide"
  #parameters <- NULL
  orientation <- c(90, 60, 0)
  width <- 1000
  height <- 800
  gridcol <- "black"
  cex.main <- 4.0

  # First interpolation
  one <- interpolations$one
  png("interp_1.png", width=width, height=height, family=deffam)
  par(mar=mapmar, oma=mapmar)
  plot_feature_interpolation(one, cex=1/mapreso, upcol=geocol[2], downcol=geocol[4], medicol=geocol[3], projection=projection, parameters=parameters, orientation=orientation, xlim=xlim, ylim=ylim, bg=bg, gridcol=gridcol, ylim_map=ylim_map)
  plot_feature(first_inter, projection=projection, parameters=parameters, orientation=orientation, cex=pointsize, upcol=geocol[1], downcol=geocol[5], add=TRUE, xlim=xlim, ylim=ylim)
  dev.off()

  # Second interpolation
  two <- interpolations$two
  png("interp_2.png", width=width, height=height, family=deffam)
  par(mar=mapmar, oma=mapmar)
  plot_feature_interpolation(two, cex=1/mapreso, upcol=geocol[2], downcol=geocol[4], medicol=geocol[3], projection=projection, parameters=parameters, orientation=orientation, xlim=xlim, ylim=ylim, bg=bg, gridcol=gridcol, ylim_map=ylim_map)
  plot_feature(second_inter, projection=projection, parameters=parameters, orientation=orientation, cex=pointsize, upcol=geocol[1], downcol=geocol[5], add=TRUE, xlim=xlim, ylim=ylim)
  dev.off()
  }

  pdf("newsimutheory.pdf", height=4.0, width=4.0)
  ggplot.simu_and_theory()
  dev.off()
  embed_fonts("newsimutheory.pdf")

  pdf("newmain.pdf", height=4.0, width=4.0)
  ggplot.main_result(full_walsdf)
  dev.off()
  embed_fonts("newmain.pdf")

  png("snapshot1.png", height=500, width=500, family=deffam)
  par(pty="s", mar=c(0,0,0,0)+0.0)
  lat <- matrix(as.numeric(read.csv("../fortran/results/snapshot_01.tab", sep="", header=FALSE)), nrow=50, ncol=50)
  image(lat, xaxt="n", yaxt="n", col=geocol[c(2,4)])
  dev.off()

  png("snapshot2.png", height=500, width=500, family=deffam)
  par(pty="s", mar=c(0,0,0,0)+0.0)
  lat <- matrix(as.numeric(read.csv("../fortran/results/snapshot_0001.tab", sep="", header=FALSE)), nrow=50, ncol=50)
  image(lat, xaxt="n", yaxt="n", col=geocol[c(2,4)])
  dev.off()

  # We compose the "main figure" with LaTeX; see mainfig.tex for the source
  system("xelatex newmainfig")
  system(paste("cp newmainfig.pdf", paste(folder, "Fig3.pdf", sep="/")))
  }


  if ("4" %in% which) {
  pdf("temperatures.pdf", height=6.0, width=8.0)
  ggplot.temperatures(full_walsdf, descriptions=TRUE)
  dev.off()
  embed_fonts("temperatures.pdf")
  system(paste("cp temperatures.pdf", paste(folder, "Fig4.pdf", sep="/")))
  }


  if ("5" %in% which) {
  # Comparisons with Dediu.
  pdf("dediu.pdf", height=4.0, width=8.0)
  ggplot.dediu(full_walsdf, neighbourhood_size=neighbourhood_size)
  dev.off()
  embed_fonts("dediu.pdf")
  system("xelatex dediufig")
  system(paste("cp dediufig.pdf", paste(folder, "Fig5.pdf", sep="/")))
  }

  if ("A3" %in% which) {
  pdf("app_sigmabias.pdf", height=4.0, width=8.0)
  appendix.sigmabias(results.sigmabias_genera, results.sigmabias_families)
  dev.off()
  embed_fonts("app_sigmabias.pdf")
  system(paste("cp app_sigmabias.pdf", paste(folder, "AppFig3.pdf", sep="/")))
  }

  if ("A2" %in% which) {
  pdf("app_geoscale.pdf", height=4.0, width=4.0)
  geodf <- correlate_over_neighbourhood_size(full_walsdf, testee=10)
  appendix.geoscale(geodf)
  dev.off()
  embed_fonts("app_geoscale.pdf")
  system(paste("cp app_geoscale.pdf", paste(folder, "AppFig2.pdf", sep="/")))
  }

  if ("A1" %in% which) {
  pdf("app_NN.pdf", height=4.0, width=8.0)
  appendix.NN(walsdist)
  dev.off()
  embed_fonts("app_NN.pdf")
  system(paste("cp app_NN.pdf", paste(folder, "AppFig1.pdf", sep="/")))
  }

  # Other figs from LaTex source.
  if ("1" %in% which) {
  system("xelatex problems_new")
  system(paste("cp problems_new.pdf", paste(folder, "Fig1.pdf", sep="/")))
  }
  if ("2" %in% which) {
  system("xelatex lattice")
  system(paste("cp lattice.pdf", paste(folder, "Fig2.pdf", sep="/")))
  }
  #system("xelatex markovchain")
  #system(paste("cp markovchain.pdf", paste(folder, "Fig2.pdf", sep="/")))

  # Scientific penalty, whether or not figures should be given in
  # scientific notation. This affects our results table.
  #oldsp <- getOption("scipen")
  #options(scipen=100)

  # Print tables for appendices
  print_feature_list(walsdf, outfile=paste(folder, "features.tex", sep="/"))
  print_results(walsdf, outfile=paste(folder, "results.tex", sep="/"), dec=5)

  # Print tau estimates for paper body
  print_taulist(walsdf, outfile=paste(folder, "tableoftaus.tex", sep="/"), dec=5)

  # Print WALS levels for SI
  print_WALS_levels(df=walsdf, outfile=paste(folder, "WALSlevels.tex", sep="/"))

  # Reset scipen
  #options(scipen=oldsp)
}


# plot_wals()
#
# Plot rho-sigma plot for the WALS data.
#
plot_wals <- function(df,
                      neighbourhood_size = 10,
                      pch = rep(c(0,15,1,16,2,17,8), 5),
                      bg = "white",
                      cex = 1.0,
                      featuresize = 0,
                      col = c(
                              rep(domcol[1], 7),
                              rep(domcol[2], 7),
                              rep(domcol[3], 7),
                              rep(domcol[4], 7),
                              rep("#001d70", 7)
                              ),
                      chcol = "grey40",
                      crosshairs = TRUE,
                      xlab = expression("Feature frequency"~rho),
                      ylab = expression("Isogloss density"~sigma),
                      cex.lab = 1.0,
                      lwd = 1.2,
                      xlim = c(0,1),
                      ylim = c(0, 0.5)) {
  #df <- df[df$sample_size >= featuresize,]
  df$sorter <- as.numeric(str_replace(df$WALS_feature, "[A-Z]", ""))
  df <- df[order(df$sorter), ]
  plot(0, 0, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, cex.lab=cex.lab)
  if (crosshairs) {
    for (i in 1:nrow(df)) {
      segments(x0=df[i,]$rho, y0=df[i,]$sigma_bot, y1=df[i,]$sigma_top, col=chcol)
      segments(y0=df[i,]$sigma, x0=df[i,]$rho_bot, x1=df[i,]$rho_top, col=chcol)
    }
  }
  points(sigma~rho, df, col=col, pch=pch, bg=bg, cex=cex, lwd=lwd)
  if (1 == 0) {
    jako <- c(1,12,24,35)
    jako <- c(1,14,28,35)
    df1 <- df[jako[1]:jako[2], ]
    df2 <- df[(jako[2]+1):jako[3], ]
    df3 <- df[(jako[3]+1):jako[4], ]
    legend(1.15, 0.51, col=col[jako[1]:jako[2]], pch=pch[jako[1]:jako[2]], legend=df1$WALS_feature, bty="n", cex=0.8)
    legend(1.45, 0.51, col=col[(jako[2]+1):jako[3]], pch=pch[(jako[2]+1):jako[3]], legend=df2$WALS_feature, bty="n", cex=0.8)
    legend(1.75, 0.51, col=col[(jako[3]+1):jako[4]], pch=pch[(jako[3]+1):jako[4]], legend=df3$WALS_feature, bty="n", cex=0.8)
  }
  jako1 <- 1:7
  jako2 <- 8:14
  jako3 <- 15:21
  jako4 <- 22:28
  jako5 <- 29:35
  df1 <- df[jako1, ]
  df2 <- df[jako2, ]
  df3 <- df[jako3, ]
  df4 <- df[jako4, ]
  df5 <- df[jako5, ]
  leghei <- 0.83
  leginc <- 0.25
  legsta <- -0.14
  legend(legsta + 0*leginc, leghei, col=col[jako1], pch=pch[jako1], legend=df1$WALS_feature, bty="n", cex=0.7)
  legend(legsta + 1*leginc, leghei, col=col[jako2], pch=pch[jako2], legend=df2$WALS_feature, bty="n", cex=0.7)
  legend(legsta + 2*leginc, leghei, col=col[jako3], pch=pch[jako3], legend=df3$WALS_feature, bty="n", cex=0.7)
  legend(legsta + 3*leginc, leghei, col=col[jako4], pch=pch[jako4], legend=df4$WALS_feature, bty="n", cex=0.7)
  legend(legsta + 4*leginc, leghei, col=col[jako5], pch=pch[jako5], legend=df5$WALS_feature, bty="n", cex=0.7)
}


# plot_alphas()
#
# Plot alpha estimates against feature rank (in terms of alpha).
#
plot_alphas <- function(df,
                        confint = TRUE,
                        featuresize = 0,
                        pch = 21,
                        bg = "white",
                        pal = domcol,
                        xlab = expression(tau*"-rank"),
                        ylab = expression("temperature"~tau),
                        legend = TRUE,
                        log = "xy") {
  df <- df[df$sample_size >= featuresize,]
  cats <- unique(df$domain)
  df <- df[order(df$alpha, decreasing=TRUE), ]
  #df <- df[1:50,]
  mini <- min(df$alpha_bot)
  maxi <- max(df$alpha_top)
  plot(1, 1, type="n", xlim=c(1, nrow(df)), log=log, ylim=c(mini, maxi), xlab=xlab, ylab=ylab)
  for (i in 1:nrow(df)) {
    segments(x0=i, x1=i, y0=df[i,]$alpha_bot, y1=df[i,]$alpha_top, col=pal[df[i,]$domain_no])
    points(i, df[i,]$alpha, col=pal[df[i,]$domain_no], pch=pch, bg=bg)
  }
  if (legend) {
    legend("bottomleft", col=pal, pch=pch, lty=1, lwd=1, legend=cats, bty="n", cex=0.9)
  }
}


# Plot an interpolation map.
#
plot_feature_interpolation <- function(df,
                                       depth = 360,
                                       projection = "albers",
                                       parameters = c(30,40),
                                       orientation = c(90,0,0),
                                       resolution = 0,
                                       xlim = c(-180,180),
                                       ylim = c(-90,90),
                                       ylim_map = c(-90,90),
                                       bg = "black",
                                       cex = 1.0,
                                       pch = 20,
                                       upcol = "red",
                                       medicol = "white",
                                       threecol = FALSE,
                                       downcol = "blue",
                                       gridlast = FALSE,
                                       gridfirst = TRUE,
                                       gridreso = 100,
                                       gridcol = "darkgrey"
                                       ) {
  require(maps)
  require(mapproj)

  gridlon <- seq(from=xlim[1], to=xlim[2], by=15)
  gridlat <- seq(from=ylim[1], to=ylim[2], by=15)

  #map("world", projection=projection, parameters=parameters, orientation=orientation, resolution=resolution, interior=FALSE, boundary=TRUE)
  map("world", projection=projection, parameters=parameters, orientation=orientation, resolution=resolution, interior=FALSE, boundary=TRUE, xlim=xlim, ylim=ylim_map, col=NA)

  poly_reso <- 1000
  poly_lat <- seq(from=xlim[1], to=xlim[2], length.out=poly_reso)
  poly_lon <- seq(from=ylim[1], to=ylim[2], length.out=poly_reso)
  poly_x <- c(poly_lat, rep(xlim[2], poly_reso), rev(poly_lat), rep(xlim[1], poly_reso))
  poly_y <- c(rep(ylim[2], poly_reso), rev(poly_lon), rep(ylim[1], poly_reso), poly_lon)
  poly_proj_x <- mapproject(poly_x, poly_y, projection=projection, parameters=parameters, orientation=orientation)$x
  poly_proj_y <- mapproject(poly_x, poly_y, projection=projection, parameters=parameters, orientation=orientation)$y
  polygon(poly_proj_x, poly_proj_y, col=bg)

  pontsut_y <- mapproject(xlim, ylim, projection=projection, parameters=parameters, orientation=orientation)$y
  pontsut_x <- mapproject(xlim, ylim, projection=projection, parameters=parameters, orientation=orientation)$x

  if (gridfirst) {
    for (lon in gridlon) {
      full_lat <- seq(from=ylim[1], to=ylim[2], length.out=gridreso)
      these_lats <- rep(NA, times=length(full_lat))
      these_lons <- rep(NA, times=length(full_lat))
      for (l in 1:length(full_lat)) {
        these_lats[l] <- mapproject(lon, full_lat[l], projection=projection, parameters=parameters, orientation=orientation)$y
        these_lons[l] <- mapproject(lon, full_lat[l], projection=projection, parameters=parameters, orientation=orientation)$x
      }
      points(x=these_lons, y=these_lats, type="l", col=gridcol)
    }
    for (lat in gridlat) {
      full_lon <- seq(from=xlim[1], to=xlim[2], length.out=gridreso)
      these_lons <- rep(NA, times=length(full_lon))
      these_lats <- rep(NA, times=length(full_lon))
      for (l in 1:length(full_lon)) {
        these_lons[l] <- mapproject(full_lon[l], lat, projection=projection, parameters=parameters, orientation=orientation)$x
        these_lats[l] <- mapproject(full_lon[l], lat, projection=projection, parameters=parameters, orientation=orientation)$y
      }
      points(x=these_lons, y=these_lats, type="l", col=gridcol)
    }
  }

  df$col <- ceiling(df$z*depth)
  if (threecol) {
    pal <- colorRampPalette(c(downcol, medicol, upcol), space="rgb")(depth+1)
  } else {
    pal <- colorRampPalette(c(downcol, upcol), space="rgb")(depth+1)
  }
  for (i in 1:nrow(df)) {
    #pts <- SpatialPoints(t(as.matrix(c(df[i,]$x, df[i,]$y))), proj4string=CRS(proj4string(wrld_simpl)))
    #if (!is.na(over(pts, wrld_simpl)$FIPS) & over(pts, wrld_simpl)$FIPS != "AY") {
    coords <- mapproject(df[i,]$x, df[i,]$y, projection=projection, parameters=parameters, orientation=orientation)
    points(coords, col=pal[df[i,]$col], pch=pch, cex=cex)
    #}
  }
  #map("world", projection=projection, parameters=parameters, orientation=orientation, resolution=resolution, interior=FALSE, boundary=TRUE, add=TRUE)
  map("world", projection=projection, parameters=parameters, orientation=orientation, resolution=resolution, interior=FALSE, boundary=TRUE, add=TRUE, xlim=xlim, ylim=ylim, bg=bg)
  if (gridlast) {
    for (lon in gridlon) {
      full_lat <- seq(from=-90, to=90, length.out=gridreso)
      these_lats <- rep(NA, times=length(full_lat))
      these_lons <- rep(NA, times=length(full_lat))
      for (l in 1:length(full_lat)) {
        these_lats[l] <- mapproject(lon, full_lat[l], projection=projection, parameters=parameters, orientation=orientation)$y
        these_lons[l] <- mapproject(lon, full_lat[l], projection=projection, parameters=parameters, orientation=orientation)$x
      }
      points(x=these_lons, y=these_lats, type="l", col=gridcol)
    }
    for (lat in gridlat) {
      full_lon <- seq(from=-180, to=180, length.out=gridreso)
      these_lons <- rep(NA, times=length(full_lon))
      these_lats <- rep(NA, times=length(full_lon))
      for (l in 1:length(full_lon)) {
        these_lons[l] <- mapproject(full_lon[l], lat, projection=projection, parameters=parameters, orientation=orientation)$x
        these_lats[l] <- mapproject(full_lon[l], lat, projection=projection, parameters=parameters, orientation=orientation)$y
      }
      points(x=these_lons, y=these_lats, type="l", col=gridcol)
    }
  }
}


# plot_feature()
#
# Plot WALS data points on a map, adding to an existing map if desired.
#
plot_feature <- function(feature,
                         folder = "../data/wals-20170718/features",
                         glob = ".tab.pruned.csv",
                         featurefile = "../newR/unsilly-features.csv",
                         projection = "albers",
                         parameters = c(30,40),
                         orientation = c(90,0,0),
                         resolution = 0,
                         cex = 1.0,
                         xlim = c(-180,180),
                         ylim = c(-90,90),
                         uppch = 20,
                         downpch = 20,
                         upcol = "red",
                         downcol = "blue",
                         add = FALSE) {
  require(maps)
  require(mapproj)
  require(stringr)
  if (!add) {
    map("world", projection=projection, parameters=parameters, orientation=orientation, resolution=resolution, interior=FALSE, boundary=TRUE)
  }
  df <- read.csv(paste(folder, "/", feature, glob, sep=""), sep="\t")
  fdf <- read.csv(featurefile)
  ups <- as.numeric(str_split(fdf[fdf$feature == feature, ]$up, pattern=",")[[1]])
  downs <- as.numeric(str_split(fdf[fdf$feature == feature, ]$down, pattern=",")[[1]])
  for (i in 1:nrow(df)) {
    thislon <- df[i,]$longitude
    thislat <- df[i,]$latitude
    if (thislon > xlim[1] & thislon < xlim[2] & thislat > ylim[1] & thislat < ylim[2]) {
      coords <- mapproject(df[i,]$longitude, df[i,]$latitude, projection=projection, parameters=parameters, orientation=orientation)
      if (df[i,]$value %in% ups) {
        points(coords, pch=uppch, cex=cex, col=upcol)
      }
      else if (df[i,]$value %in% downs) {
        points(coords, pch=downpch, cex=cex, col=downcol)
      }
    }
  }
}


plot_simu_and_theory <- function(tau,
                                 col = "black",
                                 pch = 1,
                                 lty = 1,
                                 lwd = 1.2,
                                 simufile,
                                 add = FALSE) {
  simf <- read.csv(simufile, sep="")
  trho <- seq(from=0, to=1, length.out=1000)
  alpha <- -1*tau
  Karg <- 1/(1 - alpha)
  tsigma <- 2*trho*(1-trho)*(alpha + (pi*(1-alpha))/(2*get_K(k=Karg)))
  if (!add) {
    plot(0, 0, type="n", xlim=c(0,1), ylim=c(0,0.5), xlab=expression("Feature frequency"~rho), ylab=expression("Isogloss density"~sigma))
  }
  points(sigma~rho, simf, pch=pch, col=col, lwd=lwd)
  points(trho, tsigma, type="l", lty=lty, col=col, lwd=lwd)
}


# dediu_comparison()
#
# Compare our tau estimates with Dediu's PC1 for binary features.
dediu_comparison <- function(dediufile = "../data/dediu-2010/binary.csv",
                             walsdf,
                             folder,
                             sizeadjuster = 0.2,
                             pch = c(1,4),
                             col = c("black", "red"),
                             lty = c(1,1),
                             lwd = 1.5,
                             reglwd = 1.5,
                             ylim = c(-140,120),
                             xlim = c(-0.025,0.775),
                             ylim2 = c(0,22000),
                             howmanyoutliers = 4,
                             featurefile = "../newR/unsilly-features.csv") {
  df <- merge(read.csv(dediufile), walsdf, by.y="WALS_feature")
  df$logtau <- df$alpha
  print(max(df$logtau))
  #df$logtau <- log(df$alpha)
  limo <- lm(PC1~logtau, df)
  b <- limo$coefficients[1]
  a <- limo$coefficients[2]

  outl <- find_outliers(df=df, formula=PC1~logtau, variable="WALS_feature")
  outliers <- outl[1:howmanyoutliers,]$removed
  outl$outlier <- rep(1, nrow(outl))
  outl[1:howmanyoutliers,]$outlier <- 2
  pdf(paste(folder, "Fig5B.pdf", sep="/"), height=defsize + sizeadjuster, width=defsize + sizeadjuster, family=deffam)
  par(pty="s", mar=defmar)
  plot(improvement~iteration, outl, ylab="Reduction of error", xlab="Pruning iteration", col=col[outl$outlier], pch=pch[outl$outlier], ylim=ylim2, lwd=lwd, xaxt="n", yaxt="n")
  axis(1, at=c(1,5,10,15,20))
  axis(2, at=c(0, 5000, 10000, 15000, 20000), labels=c(0, "", 10000, "", 20000))
  outlierdf <- outl[1:howmanyoutliers,]
  text(y=as.numeric(outlierdf$improvement), x=as.numeric(outlierdf$iteration), labels=as.character(outlierdf$removed), pos=4, col=col[2], cex=0.8)
  #outlierdf <- outl[4,]
  #text(y=as.numeric(outlierdf$improvement)+500, x=as.numeric(outlierdf$iteration), labels=as.character(outlierdf$removed), pos=4, col=col[2], cex=0.9)
  #outlierdf <- outl[5,]
  #text(y=as.numeric(outlierdf$improvement)-500, x=as.numeric(outlierdf$iteration), labels=as.character(outlierdf$removed), pos=4, col=col[2], cex=0.9)
  #text(x=22.5, y=15000, labels="B", cex=1.2)
  dev.off()
  embed_fonts(paste(folder, "Fig5B.pdf", sep="/"))

  outdf <- df[!(df$WALS_feature %in% outliers), ]
  outlimo <- lm(PC1~logtau, outdf)
  outb <- outlimo$coefficients[1]
  outa <- outlimo$coefficients[2]

  df$outlier <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    if (df[i,]$WALS_feature %in% outliers) {
      df[i,]$outlier <- 2
    } else {
      df[i,]$outlier <- 1
    }
  }

  pdf(paste(folder, "Fig5A.pdf", sep="/"), height=defsize + sizeadjuster, width=defsize + sizeadjuster, family=deffam)
  par(pty="s", mar=defmar)
  plot(PC1~logtau, df, ylab="Dediu's PC1", xlab=expression("Temperature"~tau), pch=pch[df$outlier], col=col[df$outlier], ylim=ylim, xlim=xlim, lwd=lwd, yaxt="n")
  axis(2, at=c(-100, -50, 0, 50, 100))
  outlierdf <- df[df$WALS_feature %in% outliers, ]
  text(y=outlierdf$PC1, x=outlierdf$logtau, labels=outlierdf$WALS_feature, pos=4, cex=0.9, col=col[2])
  seku <- seq(from=min(df$logtau), to=max(df$logtau), length.out=2)
  lines(seku, a*seku + b, col=col[2], lty=lty[2], lwd=reglwd)
  lines(seku, outa*seku + outb, col=col[1], lty=lty[1], lwd=reglwd)
  df$resid <- abs(df$PC1 - (a*df$logtau + b))
  df$resid <- df$PC1 - (a*df$logtau + b)
  #text(x=-0.011, y=111, labels="A", cex=1.2)

  text(x=0.761, y=66, "less stable", srt=90, cex=0.8, pos=1)
  arrows(x0=0.77, x1=0.77, y0=94, y1=114, length=0.07)
  text(x=0.761, y=-60, "more stable", srt=90, cex=0.8, pos=1)
  arrows(x0=0.77, x1=0.77, y0=-114, y1=-134, length=0.07)
  text(x=0.01, y=-138, "more stable", cex=0.8, pos=4)
  arrows(x0=0.020, x1=-0.015, y0=-138, y1=-138, length=0.07)
  text(x=0.738, y=-138, "less stable", cex=0.8, pos=2)
  arrows(x0=0.73, x1=0.765, y0=-138, y1=-138, length=0.07)

  dev.off()
  embed_fonts(paste(folder, "Fig5A.pdf", sep="/"))

  df <- df[order(df$resid), ]

  if (1 == 0) {
    pdf("../tmp/dediu2.pdf", height=6, width=8)
    unsillies <- read.csv(featurefile)
    df$desc <- NULL
    for (i in 1:nrow(df)) {
      df$desc[i] <- as.character(unsillies[unsillies$feature == as.character(df$WALS_feature[i]), ]$description)
    }
    op <- par(mar=c(5,20,4,2) + 0.1)
    plot(df$resid, 1:nrow(df), xlab="value of residual", yaxt="n", ylab="")
    axis(2, at=1:nrow(df), labels=df$desc, las=1, cex.axis=0.8)
    par(op)
    dev.off()
  }
  print(paste("no. of data points before pruning:", nrow(df)))
  print(cor.test(df$PC1, df$logtau, method="pearson"))
  print(cor.test(df$PC1, df$logtau, method="spearman"))
  print(paste("no. of data points after pruning:", nrow(outdf)))
  print(cor.test(outdf$PC1, outdf$logtau, method="pearson"))
  print(cor.test(outdf$PC1, outdf$logtau, method="spearman"))
}


plot_taulist <- function(walsdf,
                         featurefile = "../newR/unsilly-features.csv",
                         howmanyonbothsides = 7,
                         plotdens = FALSE,
                         plotmean = FALSE,
                         leftmar = 20,
                         textcex = 0.9,
                         densmag = 10,
                         denscol = "blue",
                         log = FALSE,
                         plotlog = "x") {
  # for prettier logarithmic axes
  require(sfsmisc)

  # set margins
  par(mar=c(5,leftmar,2,2) + 0.1)

  # get feature descriptions from 'featurefile'
  featdf <- read.csv(featurefile)
  names(featdf)[1] <- "WALS_feature"
  walsdf <- merge(walsdf, featdf, by.y="WALS_feature")
  walsdf$name <- paste(walsdf$description, " (", walsdf$WALS_feature, ")", sep="")

  # put df into another variable; we may later use this to draw a
  # density plot
  fulldf <- walsdf

  # order
  walsdf <- walsdf[order(walsdf$alpha), ]

  # take first 'howmanyonbothsides' rows
  df <- walsdf[1:howmanyonbothsides, ]
  # take last 'howmanyonbothsides' rows
  df <- rbind(df, walsdf[(nrow(walsdf) - (howmanyonbothsides - 1)):nrow(walsdf), ])

  # work with log of tau
  xlab <- expression(tau~"(temperature)")
  if (log) {
    df$alpha <- log(df$alpha)
    fulldf$alpha <- log(fulldf$alpha)
    xlab <- expression(log(tau))
  }

  # put dashed line in between highest and lowest
  df1 <- df[1:howmanyonbothsides, ]
  df2 <- df[(howmanyonbothsides+1):nrow(df),]
  df <- rbind(df1, rep(NA, ncol(df)), df2)

  # plot
  plot(type="n", df$alpha, 1:nrow(df), yaxt="n", ylab="", xlab=xlab, log=plotlog, xaxt="n")
  #text(x=df$alpha, y=1:nrow(df), labels=round(df$alpha, 3), pos=4, cex=0.8)
  eaxis(1, at=c(0.5, 0.05, 0.005, 0.0005, 0.00005), cex.axis=0.8)
  axis(2, at=c(1:howmanyonbothsides, (howmanyonbothsides+2):nrow(df)), labels=df$name[-(howmanyonbothsides+1)], las=1, cex.axis=textcex)
  abline(h=howmanyonbothsides+1, lty=2)
  if (plotmean) {
    abline(v=mean(fulldf$alpha), lty=1)
  }
  if (plotdens) {
    densi <- density(fulldf$alpha)
    lines(densi$x, densmag*densi$y, col=denscol)
  }
  points(df$alpha, 1:nrow(df), yaxt="n", ylab="")
}


plot_bootstrap <- function(df,
                           conf = 0.95) {
  #df$alpha <- log(df$tau)
  df$alpha <- df$sigma
  lowconf <- (1-conf)/2
  highconf <- 1-lowconf
  cprobs <- c(lowconf, highconf)
  df2 <- df %>%
    group_by(feature) %>%
    mutate(rho.med=median(rho), sigma.med=median(sigma), alpha.med=median(alpha), rho.min=quantile(rho, cprobs)[1], rho.max=quantile(rho, cprobs)[2], sigma.min=quantile(sigma, cprobs)[1], sigma.max=quantile(sigma, cprobs)[2], alpha.min=quantile(alpha, cprobs)[1], alpha.max=quantile(alpha, cprobs)[2])
  df2 <- df2[, !(names(df2) %in% c("sample_size", "rep", "rho", "sigma", "alpha", "tau"))]
  df2 <- df2[!duplicated(df2), ]
  g <- ggplot(df, aes(x=rho, y=alpha, color=feature)) + theme_bw()
  g <- g + geom_point(alpha=0.07, show.legend=FALSE)
  g <- g + xlim(0,1)
  g <- g + xlab(expression("feature frequency"~rho)) + ylab(expression("isogloss density"~sigma))
  g <- g + geom_point(data=df2, aes(x=rho.med, y=alpha.med), size=3.0, color="black", alpha=0.3)
  g <- g + geom_segment(data=df2, aes(x=rho.min, xend=rho.max, y=alpha.med, yend=alpha.med), color="black", alpha=0.3, lwd=2.0)
  g <- g + geom_segment(data=df2, aes(x=rho.med, xend=rho.med, y=alpha.min, yend=alpha.max), color="black", alpha=0.3, lwd=2.0)
  g <- g + geom_label_repel(data=df2, inherit.aes=FALSE, aes(x=rho.med, y=alpha.med, label=feature, fill=feature), show.legend=FALSE, alpha=0.6, box.padding=2.00)
  h <- g

  df2 <- df2[order(df2$alpha.med, decreasing=TRUE), ]
  df2$rank <- 1:nrow(df2)
  g <- ggplot(df2, aes(x=rank, y=alpha.med)) + geom_point()
  g <- g + geom_segment(data=df2, aes(x=rank, xend=rank, y=alpha.min, yend=alpha.max))
  print(h)
}


make_geoscale_spearmans <- function(df1,
                                    df2,
                                    scales = intersect(unique(df1$neighbourhood_size), unique(df2$neighbourhood_size)),
                                    averages = TRUE,
                                    limit = 0.8,
                                    alphalevel = 0.05) {
  scales <- scales[-length(scales)]
  require(tidyverse)
  variable <- "tau"

  if (averages) {
    variable <- "taum"
    df1 <- df1 %>%
      group_by(feature, neighbourhood_size) %>%
      mutate(taum=median(tau))
    df1 <- df1[, !(names(df1) %in% c("sample_size", "rep", "rho", "sigma", "tau"))]
    df1 <- df1[!duplicated(df1), ]

    df2 <- df2 %>%
      group_by(feature, neighbourhood_size) %>%
      mutate(taum=median(tau))
    df2 <- df2[, !(names(df2) %in% c("sample_size", "rep", "rho", "sigma", "tau"))]
    df2 <- df2[!duplicated(df2), ]

    # factors are the Devil's invention
    df1 %>% mutate_if(is.factor, as.character) -> df1
    df2 %>% mutate_if(is.factor, as.character) -> df2
  }

  # cross-regress
  df3 <- NULL
  for (i in scales) {
    for (j in scales) {
      if (i < j) {
        firstdf <- df1[df1$neighbourhood_size==i, ]
        firstdf <- firstdf[order(firstdf$feature), ]
        seconddf <- df1[df1$neighbourhood_size==j, ]
        seconddf <- seconddf[order(seconddf$feature), ]
        cordf <- data.frame(tau1=firstdf[[variable]],
                            tau2=seconddf[[variable]])
        correl <- cor.test(cordf$tau1, cordf$tau2, method="spearman")
        coeff <- correl$estimate
        thisdf <- data.frame(first=i, second=j, spearman=abs(coeff), sign=sign(coeff), p.value=correl$p.value, daata="df1", great=FALSE)
        if (!is.na(coeff)) {
          if (coeff >= limit) {
            thisdf$sign <- 2
          }
        }
        df3 <- rbind(df3, thisdf)
      }
      else if (i > j) {
        firstdf <- df2[df2$neighbourhood_size==i, ]
        firstdf <- firstdf[order(firstdf$feature), ]
        seconddf <- df2[df2$neighbourhood_size==j, ]
        seconddf <- seconddf[order(seconddf$feature), ]
        cordf <- data.frame(tau1=firstdf[[variable]],
                            tau2=seconddf[[variable]])
        correl <- cor.test(cordf$tau1, cordf$tau2, method="spearman")
        coeff <- correl$estimate
        thisdf <- data.frame(first=i, second=j, spearman=abs(coeff), sign=sign(coeff), p.value=correl$p.value, daata="df2", great=FALSE)
        if (!is.na(coeff)) {
          if (coeff >= limit) {
            thisdf$sign <- 2
          }
        }
        df3 <- rbind(df3, thisdf)
      }
      else {
        thisdf <- data.frame(first=i, second=j, spearman=NA, p.value=0, daata="ident", great=FALSE)
        thisdf$sign <- 3
        df3 <- rbind(df3, thisdf)
      }
    }
  }
  df4 <- df3
  df4 <- df4[!duplicated(df4), ]
  df4$cross <- 0
  for (i in 1:nrow(df4)) {
    if (df4[i,]$p.value >= alphalevel) {
      df4[i,]$cross <- 0.5
    }
  }

  df4
}

plot_geoscale_spearmans <- function(df,
                                    limit = 0.9,
                                    pal = c("#e41a1c", "#4daf4a", "#377eb8")) {
  df4 <- df
  scales <- sort(unique(df$first))
  hash1 <- data.frame(from=sort(scales), to=1:length(scales))
  hash2 <- data.frame(from=sort(scales), to=1:length(scales))
  for (i in 1:nrow(df4)) {
    df4[i,]$first <- hash1[hash1$from==df4[i,]$first, ]$to
    df4[i,]$second <- hash2[hash2$from==df4[i,]$second, ]$to
  }

  # plot
  defpar <- par()
  par(pty="s", xpd=TRUE, mar=c(4, 5, 0, 10.5)+0.1)
  plot(second~first, df4, type="n", xaxt="n", yaxt="n", xlab="First spatial scale", ylab="Second spatial scale")
  axis(1, at=1:length(scales), labels=scales, las=2)
  axis(2, at=1:length(scales), labels=scales, las=2)
  for (i in 1:nrow(df4)) {
    if (df4[i,]$sign == -1) {
      col <- pal[1]
    } else if (df4[i,]$sign == 1) {
      col <- pal[2]
    } 
    if (!is.na(df4[i,]$spearman)) {
      if (df4[i,]$spearman >= limit) {
        col <- pal[3]
      }
    }
    if (df4[i,]$sign == 3) {
      cex <- 0
    } else {
      cex <- 2*df4[i,]$spearman
    }
    if (df4[i,]$daata == "df2") {
      bg <- col
    } else {
      bg <- "white"
    }
    pch <- 21
    lwd <- 2
    points(second~first, df4[i,], col=col, cex=cex, pch=pch, bg=bg, lwd=lwd)
  }
  points(second~first, df4, col="black", cex=3*cross, pch="/", lwd=1.5)
  legend(x=26, y=23, pch=21, pt.lwd=lwd, bty="n", title="Spearman correlation", xjust=0,
         pt.cex=rev(2*c(1, limit, 0.7, 0.5, 0.3, 0.1, 0.1, 0.3, 0.5, 0.7, limit, 1)),
         legend=rev(c("-1.0",-limit,-0.7,-0.5, -0.3, -0.1, 0.1, 0.3, 0.5,0.7,limit,"1.0")),
         col=rev(c(rep(pal[1], 6), rep(pal[2], 4), rep(pal[3], 2))),
         pt.bg=rev(c(rep(pal[1], 6), rep(pal[2], 4), rep(pal[3], 2))))
  legend(x=26, y=5, pch=21, col="black", pt.cex=2, pt.lwd=lwd, bty="n", title="Neighbourhood type",
         pt.bg=c("black", "white"),
         legend=c("inclusive", "singular"))
  legend(x=26, y=8.5, pch="/", pt.cex=3*0.5, pt.lwd=1.5, col="black", legend="correlation N.S.", bty="n")
  par(defpar)
}



plot_geoscale_spearmans_ggplot <- function(df) {
  df4 <- df
  scales <- sort(unique(df$first))
  hash1 <- data.frame(from=sort(scales), to=1:length(scales))
  hash2 <- data.frame(from=sort(scales), to=1:length(scales))
  for (i in 1:nrow(df4)) {
    df4[i,]$first <- hash1[hash1$from==df4[i,]$first, ]$to
    df4[i,]$second <- hash2[hash2$from==df4[i,]$second, ]$to
  }

  # plot
  g <- ggplot(df4, aes(x=first, y=second, size=spearman))
  g <- g + geom_point(data=df4[df4$daata=="df1", ], pch=21, aes(color=ifelse(great==TRUE, "blue", factor(sign))), bg="white", stroke=1)
  g <- g + geom_point(data=df4[df4$daata=="df2", ], pch=21, aes(bg=factor(sign), color=ifelse(great==TRUE, "blue", factor(sign))), stroke=1)
  g <- g + theme_bw() + scale_radius() + theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  g <- g + scale_x_continuous(breaks=1:length(scales), labels=as.character(sort(scales)))
  g <- g + scale_y_continuous(breaks=1:length(scales), labels=sort(scales))
  g <- g + xlab("first neighbourhood size") + ylab("second neighbourhood size")
  g <- g + geom_point(aes(size=ifelse(cross==0, NA, cross)), pch=4, color="black", show.legend=FALSE)
  print(g)
}



plot_geoscale_regressions <- function(df,
                                      scales = c(1,4,7,10),
                                      variable = "tau.med",
                                      conf = 0.95) {
  require(tidyverse)

  df <- df[df$neighbourhood_size %in% scales, ]
  df <- df[order(df$neighbourhood_size), ]

  # factors are the Devil's invention
  df %>% mutate_if(is.factor, as.character) -> df

  # means and confidence intervals by group
  lowconf <- (1-conf)/2
  highconf <- 1-lowconf
  cprobs <- c(lowconf, highconf)
  df2 <- df %>%
    group_by(feature, neighbourhood_size) %>%
    mutate(rho.med=median(rho), sigma.med=median(sigma), tau.med=median(tau), rho.min=quantile(rho, cprobs)[1], rho.max=quantile(rho, cprobs)[2], sigma.min=quantile(sigma, cprobs)[1], sigma.max=quantile(sigma, cprobs)[2], tau.min=quantile(tau, cprobs)[1], tau.max=quantile(tau, cprobs)[2])
  df2 <- df2[, !(names(df2) %in% c("sample_size", "rep", "rho", "sigma", "tau"))]
  df2 <- df2[!duplicated(df2), ]

  # cross-regress
  df3 <- NULL
  for (i in unique(as.numeric(df2$neighbourhood_size))) {
    for (j in unique(as.numeric(df2$neighbourhood_size))) {
      if (i < j) {
        firstdf <- df2[df2$neighbourhood_size==i, ]
        firstdf <- firstdf[order(firstdf$feature), ]
        seconddf <- df2[df2$neighbourhood_size==j, ]
        seconddf <- seconddf[order(seconddf$feature), ]
        thisdf <- data.frame(tau1=firstdf[[variable]],
                             tau2=seconddf[[variable]])
        thisdf$comparison <- paste("X = ", i, ", Y = ", j, sep="")
        thisdf$first <- i
        thisdf$second <- j
        thisdf$spearman <- cor.test(log(thisdf$tau1), log(thisdf$tau2), method="spearman")$estimate
        df3 <- rbind(df3, thisdf)
      }
      else if (i > j) {
        firstdf <- df2[df2$neighbourhood_size==i, ]
        firstdf <- firstdf[order(firstdf$feature), ]
        seconddf <- df2[df2$neighbourhood_size==j, ]
        seconddf <- seconddf[order(seconddf$feature), ]
        thisdf <- data.frame(tau1=firstdf[[variable]],
                             tau2=seconddf[[variable]])
        thisdf$comparison <- paste("X = ", i, ", Y = ", j, sep="")
        thisdf$first <- i
        thisdf$second <- j
        thisdf$spearman <- cor.test(log(thisdf$tau1), log(thisdf$tau2), method="spearman")$estimate
        df3 <- rbind(df3, thisdf)
      }
    }
  }
  df4 <- df3[, c("first", "second", "comparison", "spearman")]
  df4 <- df4[!duplicated(df4), ]

  # plot
  ggplot(df3, aes(x=tau1, y=tau2)) + geom_point() + geom_smooth(method=lm) + scale_x_log10() + scale_y_log10() + facet_grid(second~first, scales="fixed") + theme_bw() + geom_text(data=df4, aes(x=1.3, y=0.003, label=paste0("r = ", round(spearman, 3))), size=5, hjust="right") + xlab(expression(tau[X])) + ylab(expression(tau[Y]))
}


plot_subsample_regressions <- function(df,
                                       conf = 0.95,
                                       variable = "value") {
  lowconf <- (1-conf)/2
  highconf <- 1-lowconf
  cprobs <- c(lowconf, highconf)
  out <- expand.grid(neighbourhood_size=unique(df$neighbourhood_size), y=NA, ymin=NA, ymax=NA, daata=unique(df$daata))
  for (i in 1:nrow(out)) {
    thisdf <- df[df$neighbourhood_size==out[i,]$neighbourhood_size & df$daata==out[i,]$daata, ]
    out[i,]$y <- mean(thisdf[[variable]])
    out[i,]$ymin <- quantile(thisdf[[variable]], na.rm=TRUE, cprobs)[1]
    out[i,]$ymax <- quantile(thisdf[[variable]], na.rm=TRUE, cprobs)[2]
  }
  g <- ggplot(out, aes(x=neighbourhood_size, y=y, color=daata)) + geom_point()
  g <- g + geom_segment(aes(x=neighbourhood_size, xend=neighbourhood_size, y=ymin, yend=ymax))
  g <- g + ylim(0,1)
  g <- g + scale_x_continuous(breaks=1:10)
  g <- g + scale_x_log10()
  g <- g + geom_smooth(se=FALSE)
  g <- g + theme_bw() + geom_jitter(data=df, aes(y=value, x=neighbourhood_size), alpha=0.1)
  g
}


plot_wals_regressions <- function(df,
                                  pal = c("salmon", "lightblue"),
                                  ylim = c(-0.05, 1)) {
  boxplot(value~daata+neighbourhood_size, df, ylim=ylim, xlab="Spatial scale", ylab="Spearman correlation", beside=TRUE, col=rev(pal), lty=1, outline=FALSE, xaxt="n", yaxt="n")
  axis(1, at=2*(1:length(unique(df$neighbourhood_size)))-0.5, labels=unique(df$neighbourhood_size), las=2)
  axis(2, at=seq(from=0, to=1, by=0.1), las=2)
  out <- expand.grid(neighbourhood_size=unique(df$neighbourhood_size), daata=unique(df$daata))
  sizes <- sort(unique(df$neighbourhood_size))
  for (i in 1:nrow(out)) {
    thisdf <- df[df$neighbourhood_size==out[i,]$neighbourhood_size & df$daata==out[i,]$daata, ]
    if (sum(thisdf$significant==FALSE) == 0) {
      if (unique(thisdf$daata)=="df2") {
        x <- 2*which.min(abs(out[i,]$neighbourhood_size - sizes))
        thiscol <- pal[1]
      } else {
        x <- 2*which.min(abs(out[i,]$neighbourhood_size - sizes)) - 1
        thiscol <- pal[2]
      }
      points(x=x, y=1, pch=8, col=thiscol)
    }
  }
  legend(x=1, y=0.15, bty="n", legend=c("inclusive neighbourhood", "singular neighbourhood"), fill=pal)
  legend(x=1.7, seg.len=0.8, lty=1, lwd=0, pt.lwd=1, y=0.03, bty="n", legend="all correlations statistically significant", pch=8, col="black")
}


levenshtein_across_kmlimit <- function(df) {
  kmlimits <- unique(df$kmlimit)
  out <- expand.grid(kmlimit=kmlimits, edit=NA, cumuledit=NA)
  cumdist <- 0
  df$feature <- factor(df$feature)
  levels(df$feature) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9")

  # order features according to mean tau-rank
  thisdf <- df[df$kmlimit == kmlimits[1], ]
  thisdf <- subset(thisdf, select=c("feature", "kmlimit", "tau"))
  meandf <- aggregate(.~feature, thisdf[-2], mean)
  meandf <- meandf[order(meandf$tau), ]
  feats <- meandf$feature
  feats <- paste(feats, collapse="")
  prevstring <- feats

  for (i in 2:nrow(out)) {
    kml <- out[i,]$kmlimit
    # order features according to mean tau-rank
    thisdf <- df[df$kmlimit == kml, ]
    thisdf <- subset(thisdf, select=c("feature", "kmlimit", "tau"))
    meandf <- aggregate(.~feature, thisdf[-2], mean)
    meandf <- meandf[order(meandf$tau), ]
    feats <- meandf$feature
    feats <- paste(feats, collapse="")
    dist <- adist(prevstring, feats)
    cumdist <- cumdist + dist
    out[i,]$edit <- dist
    out[i,]$cumuledit <- cumdist
    prevstring <- feats
  }
  out
}

plot_spearman_across_kmlimit <- function(df,
                                         aggregate = TRUE,
                                         log = "x",
                                         ylim = c(0.5, 1),
                                         xlim = c(100, 20000)) {
  kmlimits <- unique(df$kmlimit)
  out <- expand.grid(kmlimit=kmlimits, spearman=NA, p.value=NA)

  thisdf <- df[df$kmlimit == kmlimits[1], ]
  if (aggregate) {
    thisdf <- subset(thisdf, select=c("feature", "kmlimit", "tau"))
    thisdf <- aggregate(.~feature, thisdf[-2], mean)
  }
  prevdf <- thisdf

  for (i in 2:nrow(out)) {
    kml <- out[i,]$kmlimit
    thisdf <- df[df$kmlimit == kml, ]
    if (aggregate) {
      thisdf <- subset(thisdf, select=c("feature", "kmlimit", "tau"))
      thisdf <- aggregate(.~feature, thisdf[-2], mean)
    }
    out[i,]$spearman <- cor.test(thisdf$tau, prevdf$tau, method="spearman")$estimate
    out[i,]$p.value <- cor.test(thisdf$tau, prevdf$tau, method="spearman")$p.value
    prevdf <- thisdf
  }
  out <- out[order(out$kmlimit), ]
  plot(spearman~kmlimit, out, log=log, xlim=xlim, ylim=ylim, xlab="Neighbourhood radius", ylab="Spearman correlation with preceding radius")
  lines(spearman~kmlimit, out)
  out
}

spearman_against_allothers <- function(df,
                                       aggregate = TRUE) {
  kmlimits <- unique(df$kmlimit)
  out <- expand.grid(kmlimit=kmlimits, spearman=NA, p.value=NA)

  for (i in 2:nrow(out)) {
    kml <- out[i,]$kmlimit
    thisdf <- df[df$kmlimit == kml, ]
    othersdf <- df[df$kmlimit != kml, ]
    if (aggregate) {
      thisdf <- subset(thisdf, select=c("feature", "kmlimit", "tau"))
      thisdf <- aggregate(.~feature, thisdf[-2], mean)
      othersdf <- subset(othersdf, select=c("feature", "kmlimit", "tau"))
      othersdf <- aggregate(.~feature, othersdf[-2], mean)
    }
    out[i,]$spearman <- cor.test(thisdf$tau, othersdf$tau, method="spearman")$estimate
    out[i,]$p.value <- cor.test(thisdf$tau, othersdf$tau, method="spearman")$p.value
  }
  out
}


plot_branchingrate_regressions <- function(df,
                                           ...) {
  require(plyr)
  brates <- sort(unique(df$branching_rate))
  out <- data.frame(branching_rate=brates, spearman=NA, p.value=NA)
  for (brate in brates) {
    thisdf <- df[df$branching_rate==brate, ]
    meandf <- ddply(thisdf, .(feature), summarize, tau=median(tau), tau_theoretical=median(tau_theoretical))
    correl <- cor.test(meandf$tau, meandf$tau_theoretical, method="spearman")
    out[out$branching_rate==brate, ]$spearman <- correl$estimate
    out[out$branching_rate==brate, ]$p.value <- correl$p.value
  }
  plot(spearman~branching_rate, out, ...)
  #ggplot(out, aes(x=branching_rate, y=spearman)) + geom_point() + geom_smooth() + scale_x_log10() + theme_bw() + ylim(0,1)
}


ggplot.main_result <- function(df,
                               ns = 10) {
  df <- df[df$neighbourhood_size==ns, ]
  commonalpha <- 1
  dfmed <- aggregate(df[, 2:4], list(df$feature_ID), median, na.rm=TRUE)
  names(dfmed)[1] <- "feature_ID"
  g <- ggplot(df, aes(x=feature_frequency, y=isogloss_density, color=feature_ID)) + stat_ellipse(alpha=commonalpha)
  g <- g + geom_point(data=dfmed, aes(x=feature_frequency, y=isogloss_density, color=feature_ID)) + geom_text_repel(data=dfmed, aes(label=feature_ID), box.padding=0.75, alpha=commonalpha)
  mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nrow(dfmed))
  #mycolors <- c("#38170B","#BF1B0B", "#FFC465", "#66ADE5", "#252A52")
  #mycolors <- rep(mycolors, 7)
  g <- g + theme_bw() + xlim(0, 1) + ylim(0, 0.5) + theme(legend.position="none") + scale_color_manual(values=brightness(saturation(mycolors, 1), 0.9)) + xlab(expression("Feature frequency"~rho)) + ylab(expression("Isogloss density"~sigma)) + theme(panel.grid.minor = element_blank(), axis.text=element_text(size=12, color="black"), axis.title=element_text(size=14))
  print(g)
}


ggplot.simu_and_theory <- function(taus = c(0.001, 0.01, 0.1, 1),
                                   folder = "../fortran/results",
                                   files = c("tau_0.001.tab", "tau_0.01.tab", "tau_0.1.tab", "tau_1.tab")) {
  #mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(length(files))
  mycolors <- rep("black", length(files))
  g <- ggplot(read.csv(paste(folder, files[[1]], sep="/"), sep=""), aes(x=rho, y=sigma))
  for (i in 1:length(files)) {
    df <- read.csv(paste(folder, files[[i]], sep="/"), sep="")
    g <- g + geom_point(data=df, aes(x=rho, y=sigma), pch=i, lwd=2.0, color=saturation(mycolors[i], 1))

    tau <- taus[i]
    alpha <- -1*tau
    Karg <- 1/(1 - alpha)
    trho <- seq(from=0, to=1, length.out=1000)
    tdf <- data.frame(trho=trho, tsigma=2*trho*(1-trho)*(alpha + (pi*(1-alpha))/(2*get_K(k=Karg))))
    g <- g + geom_line(data=tdf, aes(x=trho, y=tsigma), color=saturation(mycolors[i], 1))
  }

  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.01, label="tau==0.001"), parse=TRUE, hjust=0)
  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.04, label="tau==0.01"), parse=TRUE, hjust=0)
  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.07, label="tau==0.1"), parse=TRUE, hjust=0)
  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.1, label="tau==1"), parse=TRUE, hjust=0)

  g <- g + geom_point(data=data.frame(x=0.40, y=0.01), aes(x=x, y=y), pch=1, lwd=2.0)
  g <- g + geom_point(data=data.frame(x=0.40, y=0.04), aes(x=x, y=y), pch=2, lwd=2.0)
  g <- g + geom_point(data=data.frame(x=0.40, y=0.07), aes(x=x, y=y), pch=3, lwd=2.0)
  g <- g + geom_point(data=data.frame(x=0.40, y=0.1), aes(x=x, y=y), pch=4, lwd=2.0)

  g <- g + theme_bw() + xlim(0,1) + ylim(0, 0.5) + xlab(expression("Feature frequency"~rho)) + ylab(expression("Isogloss density"~sigma)) + theme(panel.grid.minor = element_blank(), axis.text=element_text(size=12, color="black"), axis.title=element_text(size=14))
  print(g)
}


ggplot.temperatures <- function(df,
                                neighbourhood_size = 10,
                                descriptions = TRUE,
                                sillyfile = "../newR/unsilly-features.csv") {
  df <- df[df$neighbourhood_size==neighbourhood_size, ]
  if (descriptions) {
  sillies <- read.csv(sillyfile)
  df$feature_ID2 <- NA
  for (i in 1:nrow(df)) {
    stringi <- paste(df[i,]$feature_ID, sillies[sillies$feature==as.character(df[i,]$feature_ID), ]$description, sep=": ")
    stringi <- stringr::str_replace_all(stringi, pattern="\\\\emph\\{", replacement="")
    stringi <- stringr::str_replace_all(stringi, pattern="\\}", replacement="")
    df[i,]$feature_ID2 <- stringi
  }
  df$feature_ID <- df$feature_ID2
  }
  mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(35)
  mycolors <- brightness(saturation(mycolors, 1), 0.9)
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
  g <- ggplot(df, aes(x=reorder(df$feature_ID, temperature, FUN=median, na.rm=TRUE), y=temperature, color=feature_ID)) + geom_boxplot(outlier.shape=NA) + scale_y_log10(breaks=breaks, minor_breaks=minor_breaks, limits=c(1e-06, 20)) + coord_flip() + theme_bw() + theme(legend.position="none", axis.text.x=element_text(color="black", size=10), axis.text.y=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank()) + scale_color_manual(values=mycolors) + ylab(expression("Temperature"~tau)) + xlab("")
  print(g)
}


ggplot.dediu <- function(df,
                         neighbourhood_size = 10,
                         no_of_knockouts = 4,
                         method = "pearson") {
  df2 <- df
  df <- merge_with_dediu(df, neighbourhood_size=neighbourhood_size)
  knocks <- correlate_with_dediu(df2, neighbourhood_size=neighbourhood_size, no_of_knockouts=no_of_knockouts, method=method)$knockee
  knockdf <- df[df$feature_ID %in% knocks, ]
  others <- df[!(df$feature_ID %in% knocks), ]

  g <- ggplot(others, aes(x=temperature, y=PC1)) + geom_point(lwd=2.5, pch=1)
  g <- g + geom_point(data=knockdf, aes(x=temperature, y=PC1), pch=4, color="red", lwd=3.0)
  g <- g + geom_text_repel(data=knockdf, aes(label=feature_ID), color="red")
  g <- g + geom_smooth(data=others, method=lm, color="black", se=F, lwd=0.6)
  g <- g + geom_smooth(data=df, method=lm, color="red", se=F, lwd=0.6)
  g <- g + theme_bw() + theme(axis.text=element_text(color="black", size=12), axis.title=element_text(size=14), panel.grid.minor=element_blank())
  g <- g + xlab(expression("Temperature"~tau)) + ylab("Dediu's PC1")
  g <- g + theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

  df <- hipster::knockout_lm(df, PC1~temperature, id.var="feature_ID")
  knockdf <- df[df$knockee %in% knocks, ]
  others <- df[!(df$knockee %in% knocks), ]
  h <- ggplot(others, aes(x=iteration, y=RSS_reduction)) + geom_point(lwd=2.5, pch=1)
  h <- h + geom_point(data=knockdf, aes(x=iteration, y=RSS_reduction), color="red", lwd=3.0, pch=4)
  h <- h + geom_text_repel(data=knockdf, aes(label=knockee), color="red")
  h <- h + theme_bw() + theme(axis.text=element_text(color="black", size=12), axis.title=element_text(size=14), panel.grid.minor=element_blank())
  h <- h + xlab("Pruning iteration") + ylab("Reduction of error")
  h <- h + theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

  print(grid.arrange(g, h, ncol=2))
}




appendix.sigmabias <- function(df.genus,
                               df.family) {
  df <- rbind(df.genus, df.family)
  df$Units <- df$units
  g <- ggplot(df, aes(x=sigma, fill=Units)) + geom_density() + facet_wrap(.~Units)
  g <- g + xlab("Proportion disagreeing language pairs") + ylab("Density")
  g <- g + theme_bw() + theme(axis.text=element_text(color="black", size=12), axis.title=element_text(size=14), panel.grid.minor=element_blank(), legend.position="none", strip.text.x = element_text(size = 12))
  g <- g + scale_fill_brewer(palette="Set1")
  print(g)
}


appendix.geoscale <- function(cordf) {
  g <- ggplot(cordf, aes(x=neighbourhood_size, y=correlation)) + geom_point() + ylim(0.5, 1)
  g <- g + theme_bw() + theme(axis.text=element_text(color="black", size=12), axis.title=element_text(size=14), panel.grid.minor=element_blank(), legend.position="none", strip.text.x = element_text(size = 12))
  g <- g + ylab("Spearman correlation coefficient") + xlab("Neighbourhood size")
  print(g)
}


appendix.NN <- function(distdf) {
  distdf <- distdf[distdf$rank <= 10, ]
  distdf$rank <- paste("k =", distdf$rank)
  distdf$rank <- factor(distdf$rank, levels=paste("k =", 1:10))
  g <- ggplot(distdf, aes(x=distance)) + geom_density() + facet_wrap(.~rank, ncol=5)
  g <- g + theme_bw() + theme(axis.text=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank(), legend.position="none", strip.text.x = element_text(size = 12))
  g <- g + xlab("Distance (km) to kth nearest neighbour") + ylab("Density")
  print(g)
}

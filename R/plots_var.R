require(ggplot2)
require(ggrepel)
require(gridExtra)
require(RColorBrewer)
require(shades)
require(maps)
require(mapproj)
source("../R/stats.R")
source("../R/tables.R")


# plot all figures for paper; NB xelatex is needed!
plot_everything <- function(plot_interpolations = FALSE) {
  # interpolations
  if (plot_interpolations) {
    plot_interpolations()
  }

  # simulations and theory
  pdf("../plots/simutheory.pdf", height=3.5, width=3.5)
  ggplot.simu_and_theory()
  dev.off()

  # main result
  pdf("../plots/mainresult.pdf", height=3.5, width=3.5)
  ggplot.main_result()
  dev.off()

  # snapshots
  plot_snapshots()

  # temperatures
  pdf("../plots/Fig4.pdf", height=6, width=8)
  ggplot.temperatures.box()
  dev.off()

  if (1 == 0) {
  # correlations with Dediu
  pdf("../plots/correlations.pdf", height=4, width=8)
  ggplot.dediu()
  dev.off()
  }

  # compose TeX figures
  old_wd <- getwd()
  setwd("../tex")
  system("xelatex Fig1.tex")
  system("xelatex Fig2.tex")
  system("xelatex Fig3_alt.tex")
  system("xelatex Fig5.tex")
  system("cp Fig1.pdf Fig2.pdf Fig3_alt.pdf Fig5.pdf ../plots")
  setwd(old_wd)

  if (1 == 0) {
  # Table 1
  print_results(outfile="../plots/Table1.tex")

  # SI, WALS levels
  print_WALS_levels()

  # SI, fig 1
  pdf("../plots/SI_Fig1.pdf", height=2.7, width=8)
  ggplot.fullmodel()
  dev.off()

  # SI, fig 2
  pdf("../plots/SI_Fig2.pdf", height=4, width=8)
  ggplot.spatialscale()
  dev.off()

  # SI, fig 3
  pdf("../plots/SI_Fig3.pdf", height=2.7, width=8)
  ggplot.fullmodel(read.csv("../simulations/graph/results.csv"))
  dev.off()
  }
}


# fancy scientific notation; adapted from https://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot
fancy_scientific <- function(l) {
     # turn in to character string in scientific notation
     l <- format(l, scientific = TRUE)
     # quote the part before the exponent to keep all the digits
     l <- gsub("^(.*)e", "'\\1'e", l)
     # turn the 'e+' into plotmath format
     l <- gsub("e\\+","e",l)
     l <- gsub("e", "%*%10^", l)
     l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
     # return this as an expression
     parse(text=l)
}


# Main result; features on a sigma-against-rho plot, with confidence ellipses
ggplot.main_result <- function(df = read.csv("../data/main-analysis.csv")) {

  commonalpha <- 1
  dfmed <- aggregate(df[, 2:4], list(df$feature_ID), median, na.rm=TRUE)
  names(dfmed)[1] <- "feature_ID"

  parabs <- expand.grid(feature_ID=unique(dfmed$feature_ID), feature_frequency=seq(from=0, to=1, length.out=100))
  parabs$temperature <- dfmed$temperature
  parabs$isogloss_density <- 2*Htau(parabs$temperature)*parabs$feature_frequency*(1 - parabs$feature_frequency)

  dfmed$feature_ID <- ifelse(dfmed$feature_ID == "37A", "definite marker", ifelse(dfmed$feature_ID == "83A", "OV word order", NA))
    #g <- ggplot(df, aes(x=feature_frequency, y=isogloss_density, color=feature_ID)) + stat_ellipse(alpha=commonalpha)
  g <- ggplot(df, aes(x=feature_frequency, y=isogloss_density, group=feature_ID, color=log(temperature))) 
  g <- g + geom_line(data=parabs, aes(x=feature_frequency, y=isogloss_density, color=log(temperature)), alpha=0.5)
  #g <- g + stat_ellipse(alpha=0.3)
  #g <- g + geom_point(data=dfmed, aes(x=feature_frequency, y=isogloss_density, color=feature_ID)) + geom_text_repel(data=dfmed, aes(label=feature_ID), box.padding=0.75, alpha=commonalpha)
  g <- g + geom_point(data=dfmed, aes(x=feature_frequency, y=isogloss_density), size=2) + geom_label_repel(data=dfmed, aes(label=feature_ID), box.padding=0.75, alpha=0.8, color="black", size=6, nudge_y=c(1, -1), nudge_x=c(0.1,-0.1))
  g <- g + theme_bw() + xlim(0, 1) + ylim(0, 0.5) + theme(legend.position="none") 
  mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nrow(dfmed))
  #g <- g + scale_color_manual(values=brightness(saturation(mycolors, 1), 0.9))
  #g <- g + scale_color_viridis()
  #g <- g + scale_color_distiller(palette="RdYlBu")
  g <- g + xlab(expression("Feature frequency"~rho)) + ylab(expression("Isogloss density"~sigma)) + theme(panel.grid.minor = element_blank(), axis.text=element_text(size=12, color="black"), axis.title=element_text(size=14))

  print(g)
}


# Temperatures of features
ggplot.temperatures <- function(df = read.csv("../data/main-analysis.csv"),
                                descriptions = TRUE,
                                sillyfile = "../conf/unsilly-features.csv") {
  if (descriptions) {
  sillies <- read.csv(sillyfile)
  df$feature_ID2 <- NA
  for (i in 1:nrow(df)) {
    #stringi <- paste(df[i,]$feature_ID, sillies[sillies$feature==as.character(df[i,]$feature_ID), ]$description, sep=": ")
    stringi <- sillies[sillies$feature==as.character(df[i,]$feature_ID), ]$description
    #stringi <- stringr::str_to_sentence(stringi)
    stringi <- paste(stringi, " (", df[i,]$feature_ID, ")", sep="")
    stringi <- stringr::str_replace_all(stringi, pattern="\\\\emph\\{", replacement="")
    stringi <- stringr::str_replace_all(stringi, pattern="\\}", replacement="")
    df[i,]$feature_ID2 <- stringi
  }
  df$feature_ID <- df$feature_ID2
  }

  #df <- df[df$temperature < 1000, ]

  dfmed <- aggregate(df[, 2:4], list(df$feature_ID), median, na.rm=TRUE)
  names(dfmed)[1] <- "feature_ID"
  names(dfmed)[2] <- "med_temp"
  dfmed$CI_low <- 0
  dfmed$CI_high <- 0
  #df <- merge(df, dfmed, by="feature_ID")

  for (i in 1:nrow(dfmed)) {
    dfhere <- df[df$feature_ID==dfmed[i,]$feature_ID, ]
    values <- quantile(dfhere$temperature, probs=c(0.025, 0.975), na.rm=TRUE)
    dfmed[i,]$CI_low <- values[1]
    dfmed[i,]$CI_high <- values[2]
  }

  mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(35)
  mycolors <- brightness(saturation(mycolors, 1), 0.9)
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
  #g <- ggplot(df, aes(x=reorder(df$feature_ID, temperature, FUN=median, na.rm=TRUE), y=temperature, color=feature_ID)) + geom_boxplot(outlier.shape=NA) + scale_y_log10(breaks=breaks, minor_breaks=minor_breaks, limits=c(1e-06, 20)) + coord_flip() + theme_bw() + theme(legend.position="none", axis.text.x=element_text(color="black", size=10), axis.text.y=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank()) + scale_color_manual(values=mycolors) + ylab(expression("Temperature"~tau)) + xlab("")
  #g <- ggplot(df, aes(x=reorder(df$feature_ID, temperature, FUN=median, na.rm=TRUE), y=temperature, group=feature_ID, fill=log(med_temp))) + geom_boxplot(outlier.shape=NA, alpha=0.5) + scale_y_log10(breaks=breaks, minor_breaks=minor_breaks, labels=fancy_scientific, limits=c(1e-06, 20)) + coord_flip() + theme_bw() + theme(legend.position="none", axis.text.x=element_text(color="black", size=10), axis.text.y=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank()) + ylab(expression("Temperature"~tau)) + xlab("")
  g <- ggplot(dfmed, aes(x=reorder(dfmed$feature_ID, med_temp), y=med_temp, fill=log(med_temp))) + geom_point(shape=21) + geom_pointrange(aes(ymin=CI_low, ymax=CI_high)) + coord_flip() + theme_bw() + scale_y_log10(breaks=breaks, minor_breaks=minor_breaks, labels=fancy_scientific, limits=c(1e-06, 20)) + coord_flip() + theme_bw() + theme(legend.position="none", axis.text.x=element_text(color="black", size=10), axis.text.y=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank()) + ylab(expression("Temperature"~tau)) + xlab("")
  g <- g + scale_fill_distiller(palette="RdYlBu")
  #g <- g + scale_x_discrete(position="top")
  print(g)
}


# Temperatures of features; boxplots
ggplot.temperatures.box <- function(df = read.csv("../data/main-analysis.csv"),
                                descriptions = TRUE,
                                sillyfile = "../conf/unsilly-features.csv") {
  if (descriptions) {
  sillies <- read.csv(sillyfile)
  df$feature_ID2 <- NA
  for (i in 1:nrow(df)) {
    #stringi <- paste(df[i,]$feature_ID, sillies[sillies$feature==as.character(df[i,]$feature_ID), ]$description, sep=": ")
    stringi <- sillies[sillies$feature==as.character(df[i,]$feature_ID), ]$description
    #stringi <- stringr::str_to_sentence(stringi)
    stringi <- paste(stringi, " (", df[i,]$feature_ID, ")", sep="")
    stringi <- stringr::str_replace_all(stringi, pattern="\\\\emph\\{", replacement="")
    stringi <- stringr::str_replace_all(stringi, pattern="\\}", replacement="")
    df[i,]$feature_ID2 <- stringi
  }
  df$feature_ID <- df$feature_ID2
  }

  dfmed <- aggregate(df[, 2:4], list(df$feature_ID), median, na.rm=TRUE)
  names(dfmed)[1] <- "feature_ID"
  names(dfmed)[2] <- "med_temp"
  df <- merge(df, dfmed, by="feature_ID")

  mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(35)
  mycolors <- brightness(saturation(mycolors, 1), 0.9)
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
  #g <- ggplot(df, aes(x=reorder(df$feature_ID, temperature, FUN=median, na.rm=TRUE), y=temperature, color=feature_ID)) + geom_boxplot(outlier.shape=NA) + scale_y_log10(breaks=breaks, minor_breaks=minor_breaks, limits=c(1e-06, 20)) + coord_flip() + theme_bw() + theme(legend.position="none", axis.text.x=element_text(color="black", size=10), axis.text.y=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank()) + scale_color_manual(values=mycolors) + ylab(expression("Temperature"~tau)) + xlab("")
  g <- ggplot(df, aes(x=reorder(df$feature_ID, temperature, FUN=median, na.rm=TRUE), y=temperature, group=feature_ID, fill=log(med_temp))) + geom_boxplot(outlier.shape=NA, alpha=0.5) + scale_y_log10(breaks=breaks, minor_breaks=minor_breaks, labels=fancy_scientific, limits=c(1e-06, 20)) + coord_flip() + theme_bw() + theme(legend.position="none", axis.text.x=element_text(color="black", size=10), axis.text.y=element_text(color="black", size=9), axis.title=element_text(size=14), panel.grid.minor=element_blank()) + ylab(expression("Temperature"~tau)) + xlab("")
  g <- g + scale_fill_distiller(palette="RdYlBu")
  g <- g + scale_x_discrete(position="top")
  print(g)
}


# Correlations with Dediu's results
ggplot.dediu <- function(df = read.csv("../data/main-analysis.csv"),
                         no_of_knockouts = 4,
                         method = "spearman") {
  df2 <- df
  df <- merge_with_dediu(df)
  knocks <- correlate_with_dediu(no_of_knockouts=no_of_knockouts, method=method)$knockee
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


# Illustration of stationary distribution of model
ggplot.stationary <- function(file = "../simulations/forfigure/output/res.csv") {
  df <- read.csv(file)
  g <- ggplot(df, aes(x=rho, y=sigma, color=factor(tau_predicted))) + geom_point() + xlim(0,1) + ylim(0,0.5)
  g <- g + theme_bw()
  taus <- unique(df$tau_predicted)
  taudf <- expand.grid(tau_predicted=taus, rho=seq(from=0, to=1, length.out=100))
  taudf$sigma <- 2*Htau(taudf$tau_predicted)*taudf$rho*(1 - taudf$rho)
  g <- g + geom_line(data=taudf, aes(x=rho, y=sigma, color=factor(tau_predicted)))
  g
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
                                       #bg = "#ddf1ff",
                                       bg = "white",
                                       cex = 1.0,
                                       pch = 20,
                         upcol = "#2c7fb8",
                         downcol = "#edf8b1",
                                       medicol = "white",
                                       threecol = FALSE,
                                       gridlast = FALSE,
                                       gridfirst = TRUE,
                                       gridreso = 100,
                                       gridcol = "black"
                                       ) {
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
                         df = ritwals::WALS,
                         projection = "albers",
                         parameters = c(30,40),
                         orientation = c(90,0,0),
                         resolution = 0,
                         cex = 1.0,
                         xlim = c(-180,180),
                         ylim = c(-90,90),
                         uppch = 20,
                         downpch = 20,
                         upcol = "#0f3c5b",
                         downcol = "#6f754f",
                         add = TRUE) {
  require(maps)
  require(mapproj)
  require(stringr)
  if (!add) {
    map("world", projection=projection, parameters=parameters, orientation=orientation, resolution=resolution, interior=FALSE, boundary=TRUE)
  }

  featureinfo <- featurelist[featurelist$feature==feature, ]
  ups <- as.numeric(unlist(strsplit(as.character(featureinfo$up), ",")))
  downs <- as.numeric(unlist(strsplit(as.character(featureinfo$down), ",")))
  df <- df[df$feature_ID == feature, ]
  df <- df[df$value_ID %in% c(ups, downs), ]
  df$value <- 0
  df$value <- ifelse(df$value_ID %in% ups, 1, 0)

  for (i in 1:nrow(df)) {
    thislon <- df[i,]$longitude
    thislat <- df[i,]$latitude
    if (thislon > xlim[1] & thislon < xlim[2] & thislat > ylim[1] & thislat < ylim[2]) {
      coords <- mapproject(df[i,]$longitude, df[i,]$latitude, projection=projection, parameters=parameters, orientation=orientation)
      if (df[i,]$value == 1) {
        points(coords, pch=uppch, cex=cex, col=upcol)
      }
      else if (df[i,]$value == 0) {
        points(coords, pch=downpch, cex=cex, col=downcol)
      }
    }
  }
}


# Wrapper for plotting both feature interpolations and writing into PNG
plot_interpolations <- function(width = 2000,
                                height = 1000,
                                projection = "mollweide",
                                parameters = NULL) {
  load("../data/interpol_37A.RData")
  load("../data/interpol_83A.RData")

  png("../plots/interpol_37A.png", width=width, height=height)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot_feature_interpolation(interpol_37A, projection=projection, parameters=parameters, cex=1.0)
  plot_feature("37A", projection=projection, parameters=parameters, cex=2.0)
  dev.off()

  png("../plots/interpol_83A.png", width=width, height=height)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot_feature_interpolation(interpol_83A, projection=projection, parameters=parameters, cex=1.0)
  plot_feature("83A", projection=projection, parameters=parameters, cex=2.0)
  dev.off()
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
    tdf <- data.frame(trho=trho, tsigma=2*Htau(tau)*trho*(1-trho))
    g <- g + geom_line(data=tdf, aes(x=trho, y=tsigma), color=saturation(mycolors[i], 1))
  }

  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.01, label="tau==0.001"), parse=TRUE, hjust=0, size=5)
  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.05, label="tau==0.01"), parse=TRUE, hjust=0, size=5)
  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.09, label="tau==0.1"), parse=TRUE, hjust=0, size=5)
  g <- g + geom_text(data=data.frame(), aes(x=0.45, y=0.13, label="tau==1"), parse=TRUE, hjust=0, size=5)

  g <- g + geom_point(data=data.frame(x=0.40, y=0.005), aes(x=x, y=y), pch=1, lwd=2.0)
  g <- g + geom_point(data=data.frame(x=0.40, y=0.045), aes(x=x, y=y), pch=2, lwd=2.0)
  g <- g + geom_point(data=data.frame(x=0.40, y=0.085), aes(x=x, y=y), pch=3, lwd=2.0)
  g <- g + geom_point(data=data.frame(x=0.40, y=0.125), aes(x=x, y=y), pch=4, lwd=2.0)

  g <- g + theme_bw() + xlim(0,1) + ylim(0, 0.5) + xlab(expression("Feature frequency"~rho)) + ylab(expression("Isogloss density"~sigma)) + theme(panel.grid.minor = element_blank(), axis.text=element_text(size=12, color="black"), axis.title=element_text(size=14))
  print(g)
}


# fortran simulation snapshots
plot_snapshots <- function() {
  png("../plots/snapshot1.png", height=300, width=300)
  par(pty="s", mar=c(0,0,0,0))
  lat <- matrix(as.numeric(read.table("../fortran/results/snapshot_01.tab", sep="")), ncol=50)
  image(lat, xaxt="n", yaxt="n", col=c("#edf8b1", "#2c7fb8"))
  dev.off()

  png("../plots/snapshot2.png", height=300, width=300)
  par(pty="s", mar=c(0,0,0,0))
  lat <- matrix(as.numeric(read.table("../fortran/results/snapshot_0001.tab", sep="")), ncol=50)
  image(lat, xaxt="n", yaxt="n", col=c("#edf8b1", "#2c7fb8"))
  dev.off()
}


# full model correlations between predictions and measurements
ggplot.fullmodel <- function(df = read.csv("../simulations/fullmodel/results.csv")) {
  df$rho_predicted <- ((1 - df$voter_rate)*df$ingress_rate + df$voter_rate*df$horiz_ingress_rate)/((1 - df$voter_rate)*(df$ingress_rate + df$egress_rate) + df$voter_rate*(df$horiz_ingress_rate + df$horiz_egress_rate))
  df$sigma_predicted <- 2*Htau(df$tau_predicted)*df$rho_predicted*(1 - df$rho_predicted)

  g1 <- ggplot(df, aes(x=rho, y=rho_predicted)) + geom_point(alpha=0.5) + geom_smooth(method=lm) + xlim(0,1) + ylim(0,1) + theme_bw() + xlab(expression(rho~"measured")) + ylab(expression(rho~"predicted")) + theme(panel.grid.minor=element_blank()) + coord_flip()
  g2 <- ggplot(df, aes(x=sigma, y=sigma_predicted)) + geom_point(alpha=0.5) + geom_smooth(method=lm) + xlim(0,0.5) + ylim(0, 0.5) + theme_bw() + xlab(expression(sigma~"measured")) + ylab(expression(sigma~"predicted")) + theme(panel.grid.minor=element_blank()) + coord_flip()
  g3 <- ggplot(df[df$tau < 1000, ], aes(x=tau, y=tau_predicted)) + geom_point(alpha=0.5) + geom_smooth(method=lm) + scale_x_log10() + scale_y_log10() + theme_bw() + xlab(expression(tau~"measured")) + ylab(expression(tau~"predicted")) + theme(panel.grid.minor=element_blank()) + coord_flip()

  g <- grid.arrange(g1, g2, g3, ncol=3)
  print(g)
}


# spatial scale correlations
ggplot.spatialscale <- function() {
  df <- correlate_over_neighbourhood_size()
  df2 <- read.csv("../conf/wals-distances-100closest.csv")
  df2 <- df2[df2$rank %in% c(1, 10, 50), ]
  g <- ggplot(df, aes(x=neighbourhood_size, y=correlation)) + geom_point() + theme_bw() + theme(panel.grid.minor=element_blank()) + xlab(expression("neighbourhood size"~italic(k))) + ylab(expression("Spearman correlation"~italic(r)[italic(S)]~"against"~italic(k)==10)) + ylim(0.85,1)
  g2 <- ggplot(df2, aes(x=distance, fill=factor(rank))) + geom_density(alpha=0.5) + theme_bw() + theme(panel.grid.minor=element_blank(), legend.position="top") + scale_fill_brewer(palette="Dark2", name="neighbour rank") + ylab("") + xlab("distance (km) to neighbour")
  g <- grid.arrange(g, g2, ncol=2)
  print(g)
}

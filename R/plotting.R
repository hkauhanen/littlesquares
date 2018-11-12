############
# plotting.R
#
# Plotting routines.
#
# Henri Kauhanen 2016-2017
##########################


require(extrafont)
loadfonts()
source("analyse.R")
source("tabulate.R")

# deffam
#
# Default font family for plots
#
deffam <- "Arial"
#deffam <- "Helvetica"

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
#geocol <- rev(c("#0000ff", "#6666ff", "white", "#fff200", "#797200"))
geocol <- rev(c("#a00000", "#ff0000", "white", "#0000ff", "#5050ff"))



# plot_all_for_paper()
#
# Plot all figures for the manuscript, (over)writing them in <folder>.
# We also pretty-print tables for the appendices.
#
# NB. Takes a while because of (plotting) the interpolations!
#
plot_all_for_paper <- function(folder,
                               walsdf = walsdata_sep18_b,
                               interpolations = inters_20180411,
                               first_inter = "37A",
                               second_inter = "83A",
                               plot_inter_simulations = FALSE,
                               simudir = "../simulations-final/",
                               mapreso = 2) {
  require(stringr)

    # Plot the interpolations. First we set some global parameters.
    xlim <- c(-30, 150)
    ylim_map <- c(-60, 90)
    ylim <- c(-60, 90)
    bg <- "#ddf1ff"
    mapmar <- c(0,0,0,0)
    pointsize <- 3.0
    projection <- "albers"
    parameters <- c(10, 40)
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

  # Now plot things other than the interpolations.
  pdf("wals.pdf", height=4.5, width=7.0, family=deffam)
  par(pty="s", xpd=TRUE, mar=c(1.5,4.5,4,2) + 0.1, mfrow=c(1,2))

  # Theory and simulations
  theocol <- "black"
  plot_simu_and_theory(tau=1, simufile="../fortran/results/tau_1.tab", pch=16, col=theocol)
  plot_simu_and_theory(tau=0.1, simufile="../fortran/results/tau_0.1.tab", add=TRUE, pch=4, col=theocol)
  plot_simu_and_theory(tau=0.01, simufile="../fortran/results/tau_0.01.tab", add=TRUE, pch=0, col=theocol)
  plot_simu_and_theory(tau=0.001, simufile="../fortran/results/tau_0.001.tab", add=TRUE, pch=1, col=theocol)
  legend(0.33, 0.16, pch=c(16,4,0,1), lty=1, legend=expression(tau==1, tau==0.1, tau==0.01, tau==0.001), col="black", cex=0.75, bty="n", lwd=1.2)

  plot_wals(walsdf, ylim=c(0,0.5), crosshairs=FALSE)
  dev.off()
  embed_fonts("wals.pdf")

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
  system("xelatex mainfig")
  system(paste("cp mainfig.pdf", paste(folder, "Fig4.pdf", sep="/")))

  # Comparisons with Dediu.
  dediu_comparison(walsdf=walsdf, folder=".")
  system("xelatex dediufig")
  system(paste("cp dediufig.pdf", paste(folder, "Fig5.pdf", sep="/")))

  # Other figs from LaTex source.
  system("xelatex problems_new")
  system("xelatex markovchain")
  system("xelatex lattice")
  system(paste("cp problems_new.pdf", paste(folder, "Fig1.pdf", sep="/")))
  system(paste("cp markovchain.pdf", paste(folder, "Fig2.pdf", sep="/")))
  system(paste("cp lattice.pdf", paste(folder, "Fig3.pdf", sep="/")))

  # Scientific penalty, whether or not figures should be given in
  # scientific notation. This affects our results table.
  oldsp <- getOption("scipen")
  options(scipen=100)

  # Print tables for appendices
  print_feature_list(walsdf, outfile=paste(folder, "features.tex", sep="/"))
  print_results(walsdf, outfile=paste(folder, "results.tex", sep="/"), dec=8)

  # Print tau estimates for paper body
  print_taulist(walsdf, outfile=paste(folder, "tableoftaus.tex", sep="/"), dec=8)

  # Print WALS levels for SI
  print_WALS_levels(df=walsdf, outfile=paste(folder, "WALSlevels.tex", sep="/"))

  # Reset scipen
  options(scipen=oldsp)
}


# plot_all_for_paper_OLD()
#
# Plot all figures for the manuscript, (over)writing them in <folder>.
# We also pretty-print tables for the appendices.
#
# NB. Takes a while because of (plotting) the interpolations!
#
# NB!! This is an OLD version of the function, included as a backup.
# Use this to revert back to the figures that went in the arXiv submission.
#
plot_all_for_paper_OLD <- function(folder,
                               plot_interpolations = TRUE,
                               first = "37A",
                               second = "83A",
                               plot_simulations = FALSE,
                               interpolations = inters_oct22,
                               walsdf = walsdata_sep18_b,
                               simudir = "../simulations-final/",
                               mapreso = 2) {
  require(stringr)

  # First, plot things other than the interpolations.
  pdf("wals.pdf", height=4.5, width=7.0, family=deffam)
  par(pty="s", xpd=TRUE, mar=c(1.5,4.5,4,2) + 0.1, mfrow=c(1,2))

  # Theory and simulations
  #  pdf(paste(folder, "theorsimu.pdf", sep="/"), height=defsize, width=defsize, family=deffam)
  #  par(pty="s", mar=defmar)
  theocol <- "black"
  plot_simu_and_theory(tau=1, simufile="../fortran/results/tau_1.tab", pch=16, col=theocol)
  plot_simu_and_theory(tau=0.1, simufile="../fortran/results/tau_0.1.tab", add=TRUE, pch=4, col=theocol)
  plot_simu_and_theory(tau=0.01, simufile="../fortran/results/tau_0.01.tab", add=TRUE, pch=0, col=theocol)
  plot_simu_and_theory(tau=0.001, simufile="../fortran/results/tau_0.001.tab", add=TRUE, pch=1, col=theocol)
  legend(0.33, 0.16, pch=c(16,4,0,1), lty=1, legend=expression(tau==1, tau==0.1, tau==0.01, tau==0.001), col="black", cex=0.75, bty="n", lwd=1.2)
  text(x=0.05, y=0.475, labels="A", cex=1.2)
  #  dev.off()

  #par(pty="s", mar=defmar)
  plot_wals(walsdf, ylim=c(0,0.5), crosshairs=FALSE)
  text(x=0.05, y=0.475, labels="B", cex=1.2)
  dev.off()
  embed_fonts("wals.pdf")

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

  system("xelatex mainfig")
  system(paste("cp mainfig.pdf", folder))

  #pdf(paste(folder, "alphas.pdf", sep="/"), height=5.0, width=5.0, family="Times")
  #par(pty="s", mar=c(5,3,2,0)+0.1)
  #plot_alphas(walsdf, pal=rep("black", 4), legend=FALSE)
  #dev.off()

  if (plot_interpolations) {
    # Now plot the interpolations. First we set some global parameters.
    xlim <- c(-30, 150)
    ylim_map <- c(-60, 90)
    ylim <- c(-60, 90)
    bg <- "#ddf1ff"
    mapmar <- c(0,0,0,0)
    pointsize <- 2.0
    abcd.cex <- 3.0

    # Tone, empirical
    int13A <- interpolations$int13A
    png(paste(folder, "interp_13A.png", sep="/"), width=1000, height=800, family=deffam)
    par(mar=mapmar, oma=mapmar)
    plot_feature_interpolation(int13A, cex=1/mapreso, upcol=geocol[2], downcol=geocol[4], medicol=geocol[3], projection="albers", parameters=c(10,40), orientation=c(90,60,0), xlim=xlim, ylim=ylim, bg=bg, gridcol="black", ylim_map=ylim_map)
    plot_feature(first, projection="albers", parameters=c(10,40), orientation=c(90,60,0), cex=pointsize, upcol=geocol[1], downcol=geocol[5], add=TRUE, xlim=xlim, ylim=ylim)
    #legend("top", legend=c("tone", "no tone"), pch=20, col=geocol[c(1,5)], cex=3.5, bty="n")
    #title(main="A", cex.main=4.0)
    #text(x=-20, y=80, labels="A")
    text(mapproject(-90, 50, projection="albers", parameters=c(10,40), orientation=c(90,60,0)), "A", cex=abcd.cex)
    dev.off()

    if (plot_simulations) {
      # Tone, simulation
      int13A_simu <- interpolations$int13A_simu
      png(paste(folder, "interp_13A_simu.png", sep="/"), width=1000, height=800, family=deffam)
      par(mar=mapmar, oma=mapmar)
      plot_feature_interpolation(int13A_simu, cex=1/mapreso, upcol=geocol[2], downcol=geocol[4], medicol=geocol[3], projection="albers", parameters=c(10,40), orientation=c(90,60,0), xlim=xlim, ylim=ylim, bg=bg, gridcol="black", ylim_map=ylim_map)
      plot_feature("13A", folder=simudir, glob=".csv", projection="albers", parameters=c(10,40), orientation=c(90,60,0), cex=pointsize, upcol=geocol[1], downcol=geocol[5], add=TRUE, xlim=xlim, ylim=ylim)
      #legend("top", legend=c("tone (simulation)", "no tone (simulation)"), pch=c(20, 20), col=geocol[c(1,5)], cex=3.5, bty="n")
      #title(main="C", cex.main=4.0)
      text(mapproject(-90, 50, projection="albers", parameters=c(10,40), orientation=c(90,60,0)), "C", cex=abcd.cex)
      dev.off()
    }

    # Article, empirical
    int38A <- interpolations$int38A
    png(paste(folder, "interp_38A.png", sep="/"), width=1000, height=800, family=deffam)
    par(mar=mapmar, oma=mapmar)
    plot_feature_interpolation(int38A, cex=1/mapreso, upcol=geocol[2], downcol=geocol[4], medicol=geocol[3], projection="albers", parameters=c(10,40), orientation=c(90,60,0), xlim=xlim, ylim=ylim, bg=bg, gridcol="black", ylim_map=ylim_map)
    plot_feature(second, projection="albers", parameters=c(10,40), orientation=c(90,60,0), cex=pointsize, upcol=geocol[1], downcol=geocol[5], add=TRUE, xlim=xlim, ylim=ylim)
    #legend("top", legend=c("indefinite article", "no indefinite article"), pch=20, col=geocol[c(1,5)], cex=3.5, bty="n")
    #title(main="B", cex.main=4.0)
    text(mapproject(-90, 50, projection="albers", parameters=c(10,40), orientation=c(90,60,0)), "B", cex=abcd.cex)
    dev.off()

    if (plot_simulations) {
      # Article, simulation
      int38A_simu <- interpolations$int38A_simu
      png(paste(folder, "interp_38A_simu.png", sep="/"), width=1000, height=800, family=deffam)
      par(mar=mapmar, oma=mapmar)
      plot_feature_interpolation(int38A_simu, cex=1/mapreso, upcol=geocol[2], downcol=geocol[4], medicol=geocol[3], projection="albers", parameters=c(10,40), orientation=c(90,60,0), xlim=xlim, ylim=ylim, bg=bg, gridcol="black", ylim_map=ylim_map)
      plot_feature("38A", folder=simudir, glob=".csv", projection="albers", parameters=c(10,40), orientation=c(90,60,0), cex=pointsize, upcol=geocol[1], downcol=geocol[5], add=TRUE, xlim=xlim, ylim=ylim)
      #legend("top", legend=c("indefinite article (simulation)", "no indefinite article (simulation)"), pch=c(20, 20), col=geocol[c(1,5)], cex=3.5, bty="n")
      #title(main="D", cex.main=4.0)
      text(mapproject(-90, 50, projection="albers", parameters=c(10,40), orientation=c(90,60,0)), "D", cex=abcd.cex)
      dev.off()
    }
  }

  # Comparisons with Dediu.
  dediu_comparison(walsdf=walsdf, folder=folder)

  # tau estimates
  #pdf(paste(folder, "taus.pdf", sep="/"), height=4.5, width=7.0, family="Times")
  #plot_taulist(walsdf, leftmar=20)
  #dev.off()

  # Scientific penalty, whether or not figures should be given in
  # scientific notation. This affects our results table.
  oldsp <- getOption("scipen")
  options(scipen=100)

  # Print tables for appendices
  print_feature_list(walsdf, outfile=paste(folder, "features.tex", sep="/"))
  print_results(walsdf, outfile=paste(folder, "results.tex", sep="/"), dec=8)

  # Print tau estimates for paper body
  print_taulist(walsdf, outfile=paste(folder, "tableoftaus.tex", sep="/"), dec=8)

  # Print WALS levels for SI
  print_WALS_levels(df=walsdf, outfile=paste(folder, "WALSlevels.tex", sep="/"))

  # Reset scipen
  options(scipen=oldsp)
}


# plot_wals()
#
# Plot rho-sigma plot for the WALS data.
#
plot_wals <- function(df,
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
  df <- df[df$sample_size >= featuresize,]
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
  if (0 == 1) {
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


# plot_wals_old()
#
# Plot rho-sigma plot for the WALS data.
#
plot_wals_old <- function(df,
                          pch = 21,
                          bg = "white",
                          cex = 1.0,
                          featuresize = 0,
                          col = domcol,
                          crosshairs = TRUE,
                          chcol = "darkgrey",
                          xlab = expression("Feature frequency"~rho),
                          ylab = expression("Isogloss density"~sigma),
                          cex.lab = 1.0,
                          lwd = 1.5,
                          legend = TRUE,
                          xlim = c(0,1),
                          ylim = c(0, 0.6)) {
  df <- df[df$sample_size >= featuresize,]
  plot(0, 0, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, cex.lab=cex.lab)
  if (crosshairs) {
    for (i in 1:nrow(df)) {
      #segments(x0=df[i,]$rho, y0=df[i,]$sigma_bot, y1=df[i,]$sigma_top, col=col[df[i,]$domain_no])
      #segments(y0=df[i,]$sigma, x0=df[i,]$rho_bot, x1=df[i,]$rho_top, col=col[df[i,]$domain_no])
      segments(x0=df[i,]$rho, y0=df[i,]$sigma_bot, y1=df[i,]$sigma_top, col=chcol)
      segments(y0=df[i,]$sigma, x0=df[i,]$rho_bot, x1=df[i,]$rho_top, col=chcol)
    }
  }
  for (i in 1:nrow(df)) {
    points(sigma~rho, df[i,], col=col[df[i,]$domain_no], pch=pch, bg=bg, cex=cex, lwd=lwd)
  }
  df <- df[order(df$domain_no), ]
  cats <- unique(df$domain)
  if (legend) {
    legend("topright", col=col, pch=pch, legend=cats, bty="n", cex=0.9)
  }
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
                         featurefile = "../data/unsilly-features.csv",
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
                             xlim = c(-0.025,0.425),
                             ylim2 = c(0,15500),
                             howmanyoutliers = 5,
                             featurefile = "../data/unsilly-features.csv") {
  df <- merge(read.csv(dediufile), walsdf, by.y="WALS_feature")
  df$logtau <- df$alpha
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
  plot(improvement~iteration, outl, ylab="Reduction of error", xlab="Pruning iteration", col=col[outl$outlier], pch=pch[outl$outlier], ylim=ylim2, lwd=lwd, xaxt="n")
  axis(1, at=c(1,5,10,15,20))
  outlierdf <- outl[1:3,]
  text(y=as.numeric(outlierdf$improvement), x=as.numeric(outlierdf$iteration), labels=as.character(outlierdf$removed), pos=4, col=col[2], cex=0.8)
  outlierdf <- outl[4,]
  text(y=as.numeric(outlierdf$improvement)+500, x=as.numeric(outlierdf$iteration), labels=as.character(outlierdf$removed), pos=4, col=col[2], cex=0.9)
  outlierdf <- outl[5,]
  text(y=as.numeric(outlierdf$improvement)-500, x=as.numeric(outlierdf$iteration), labels=as.character(outlierdf$removed), pos=4, col=col[2], cex=0.9)
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

  text(x=0.411, y=66, "less stable", srt=90, cex=0.8, pos=1)
  arrows(x0=0.42, x1=0.42, y0=94, y1=114, length=0.07)
  text(x=0.411, y=-60, "more stable", srt=90, cex=0.8, pos=1)
  arrows(x0=0.42, x1=0.42, y0=-114, y1=-134, length=0.07)
  text(x=0.01, y=-138, "more stable", cex=0.8, pos=4)
  arrows(x0=0.020, x1=-0.015, y0=-138, y1=-138, length=0.07)
  text(x=0.388, y=-138, "less stable", cex=0.8, pos=2)
  arrows(x0=0.38, x1=0.415, y0=-138, y1=-138, length=0.07)

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
                         featurefile = "../data/unsilly-features.csv",
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


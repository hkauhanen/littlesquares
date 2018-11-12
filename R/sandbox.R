###########
# sandbox.R
#
# A sand box - to play around.
#
# Henri Kauhanen 2016-2017
##########################



test_boot_robust <- function(boots = c(5, 10, 50, 100, 500, 1000, 5000, 10000)) {
  out <- data.frame(cbind(boots, boots))
  names(out) <- c("bootsize", "interval")
  ctr <- 0
  for (boot in boots) {
    ctr <- ctr + 1
    print(paste("Working on test", ctr, "out of", length(boots)))
    erkki <- wals_analysis(featuresize=1, exactsample=-1, featurefile="../data/test-features.csv", bootsize=boot, verbose=FALSE)
    out[ctr,]$interval <- mean(erkki$alpha_top - erkki$alpha_bot)
  }
  out
}


plot_simus <- function() {
  plot_simu_and_theory(tau=0.9, simufile="../fortran/nollaysi.tab")
  plot_simu_and_theory(tau=0.1, simufile="../fortran/nollayks.tab", add=TRUE, col="red")
  plot_simu_and_theory(tau=0.01, simufile="../fortran/nollanollayks.tab", add=TRUE, col="blue")
  legend("topright", legend=expression(tau==0.9, tau==0.1, tau==0.01), col=c("black", "red", "blue"), lty=1, pch=1)
}



# maslova_comparison()
#
# Compare our tau estimates with Maslova-type estimates for binary features.
maslova_comparison <- function(maslovadf,
                             walsdf,
                             featurefile = "../data/unsilly-features.csv") {
  masdf <- data.frame(maslovadf$WALS_feature, maslovadf$M)
  names(masdf) <- c("WALS_feature", "M")
  df <- merge(walsdf, masdf, by.y="WALS_feature")
  print(df)
  df$logtau <- log(df$alpha)
  limo <- lm(M~logtau, df)
  b <- limo$coefficients[1]
  a <- limo$coefficients[2]

  pdf("../tmp/maslova1.pdf", height=6, width=6)
  plot(M~logtau, df, ylab="M-stability", xlab=expression(log(tau)))
  seku <- seq(from=min(df$logtau), to=max(df$logtau), length.out=2)
  lines(seku, a*seku + b)
  df$resid <- abs(df$M - (a*df$logtau + b))
  df$resid <- df$M - (a*df$logtau + b)
  dev.off()

  df <- df[order(df$resid), ]

  pdf("../tmp/maslova2.pdf", height=6, width=8)
  unsillies <- read.csv(featurefile)
  df$desc <- NULL
  for (i in 1:nrow(df)) {
    df$desc[i] <- as.character(unsillies[unsillies$feature == as.character(df$WALS_feature[i]), ]$description)
  }
  op <- par(mar=c(5,20,4,2) + 0.1)
  plot(df$resid, 1:nrow(df), xlab="absolute value of residual", yaxt="n", ylab="")
  axis(2, at=1:nrow(df), labels=df$desc, las=1, cex.axis=0.8)
  par(op)
  dev.off()

  cor.test(df$M, df$logtau)
}



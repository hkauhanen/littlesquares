require(ggplot2)
require(ggrepel)
require(gridExtra)
require(RColorBrewer)
require(shades)


# Main result; features on a sigma-against-rho plot, with confidence ellipses
ggplot.main_result <- function(df = read.csv("../data/main-analysis.csv")) {
  commonalpha <- 1
  dfmed <- aggregate(df[, 2:4], list(df$feature_ID), median, na.rm=TRUE)
  names(dfmed)[1] <- "feature_ID"
  g <- ggplot(df, aes(x=feature_frequency, y=isogloss_density, color=feature_ID)) + stat_ellipse(alpha=commonalpha)
  g <- g + geom_point(data=dfmed, aes(x=feature_frequency, y=isogloss_density, color=feature_ID)) + geom_text_repel(data=dfmed, aes(label=feature_ID), box.padding=0.75, alpha=commonalpha)
  mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nrow(dfmed))
  g <- g + theme_bw() + xlim(0, 1) + ylim(0, 0.5) + theme(legend.position="none") + scale_color_manual(values=brightness(saturation(mycolors, 1), 0.9)) + xlab(expression("Feature frequency"~rho)) + ylab(expression("Isogloss density"~sigma)) + theme(panel.grid.minor = element_blank(), axis.text=element_text(size=12, color="black"), axis.title=element_text(size=14))
  print(g)
}


# Temperatures of features; boxplots
ggplot.temperatures <- function(df = read.csv("../data/main-analysis.csv"),
                                descriptions = TRUE,
                                sillyfile = "../conf/unsilly-features.csv") {
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


# Correlations with Dediu's results
source("../R/stats.R")
ggplot.dediu <- function(df = read.csv("../data/main-analysis.csv"),
                         neighbourhood_size = 10,
                         no_of_knockouts = 4,
                         method = "spearman") {
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



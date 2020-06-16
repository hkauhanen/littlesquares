# tabulate.R
#
# Pretty-print tables of features in LaTeX format for inclusion in the paper.
#
# Henri Kauhanen 2016-2019
##########################


# print_WALS_levels()
#
# Prints out the WALS feature (variable) levels.
#
print_WALS_levels <- function(df,
                              featurefile = "../newR/unsilly-features.csv",
                              outfile,
                              featuredir = "../data/wals-20170718/features",
                              pars = "../data/wals-20170718/Parameters.csv",
                              glob = ".tab.pruned.csv") {
  require(stringr)
  outcon <- file(outfile, "w")
  featdf <- read.csv(featurefile, stringsAsFactors=FALSE)
  pardf <- read.csv(pars, stringsAsFactors=FALSE)
  df <- cbind(df, rep("asd", nrow(df)))
  names(df)[ncol(df)] <- "description"
  df$description <- as.character(df$description)
  for (i in 1:nrow(df)) {
    df$description[i] <- as.character(featdf[as.character(featdf$feature) == as.character(df[i,]$WALS_feature), ]$description)
  }
  df <- df[order(df$description), ]

  ictr <- 0
  for (i in 1:nrow(df)) {
    ictr <- ictr + 1
    writeLines(paste("\\subsection*{", ictr, ". ", df[i,]$description, "}\n", sep=""), outcon)
    writeLines("\\begin{itemize}", outcon)
    writeLines(paste("\\item[--] WALS feature mined: `", pardf[pardf$id == df[i,]$WALS_feature, ]$name, "' (", df[i,]$WALS_feature, ")\n\\item[--] Values:\n", sep=""), outcon)
    feature <- read.csv(paste(featuredir, "/", df[i,]$WALS_feature, glob, sep=""), sep="\t")
    writeLines("\\begin{enumerate}", outcon)
    jctr <- 0
    for (j in sort(unique(feature$value))) {
      jctr <- jctr + 1
      ups <- as.numeric(unlist(strsplit(as.character(featdf[featdf$feature == df[i,]$WALS_feature, ]$up), split=",")))
      downs <- as.numeric(unlist(strsplit(as.character(featdf[featdf$feature == df[i,]$WALS_feature, ]$down), split=",")))
      if (j %in% ups) {
        abspres <- "present"
      } else if (j %in% downs) {
        abspres <- "absent"
      } else {
        abspres <- "N/A"
      }
      writeLines(paste("\\item[", jctr, ":] ", str_replace(string=as.character(feature[feature$value==j, ]$description[1]), pattern="&", replacement="\\\\&"), " (\\emph{", abspres, "})", sep=""), outcon)
    }
    writeLines("\\end{enumerate}", outcon)
    writeLines("\\end{itemize}\n", outcon)
  }

  close(outcon)
}


# print_feature_list()
#
# Prints out the list of features in nice table format, ordered by
# feature description (alphabetically).
#
print_feature_list <- function(df,
                               featurefile = "../newR/unsilly-features.csv",
                               outfile) {
  require(stringr)
  outcon <- file(outfile, "w")
  #writeLines("\\begin{tabular}{rllllr}", outcon)
  writeLines("Feature & WALS ID & Abs. & Pres. & $N$ \\\\", outcon)
  writeLines("\\hline", outcon)
  featdf <- read.csv(featurefile, stringsAsFactors=FALSE)
  df <- cbind(df, rep("asd", nrow(df)))
  names(df)[ncol(df)] <- "description"
  df$description <- as.character(df$description)
  for (i in 1:nrow(df)) {
    df$description[i] <- as.character(featdf[as.character(featdf$feature) == as.character(df[i,]$WALS_feature), ]$description)
  }
  df <- df[order(df$description), ]
  for (i in 1:nrow(df)) {
    thisfeat <- featdf[as.character(featdf$feature) == as.character(df[i,]$WALS_feature), ]
    #    p <- paste(i, ".", sep="")
    p <- NULL
    p <- c(p, as.character(thisfeat$description))
    p <- c(p, as.character(df[i,]$WALS_feature))
    down <- thisfeat$down
    up <- thisfeat$up
    if (str_detect(down, ",")) {
      parts <- str_split(down, ",")[[1]]
      firstpart <- parts[1]
      lastpart <- parts[length(parts)]
      p <- c(p, paste(firstpart, "--", lastpart, sep=""))
    } else {
      p <- c(p, as.character(down))
    }
    if (str_detect(up, ",")) {
      parts <- str_split(up, ",")[[1]]
      firstpart <- parts[1]
      lastpart <- parts[length(parts)]
      p <- c(p, paste(firstpart, "--", lastpart, sep=""))
    } else {
      p <- c(p, as.character(up))
    }
    p <- c(p, as.character(thisfeat$sample.size))
    p <- paste(p, collapse=" & ")
    p <- paste(p, "\\\\")
    writeLines(p, outcon)
  }
  writeLines("\\hline", outcon)
  #writeLines("\\end{tabular}", outcon)
  close(outcon)
}


# print_results()
#
# Prints the results (rho, sigma, tau), ordered by increasing tau.
#
print_results <- function(df,
                          featurefile = "../newR/unsilly-features.csv",
                          dec = 5,
                          outfile) {
  require(stringr)
  outcon <- file(outfile, "w")
  #writeLines("\\begin{tabular}{rlrrr}", outcon)
  writeLines("Rank & Feature & WALS ID & $\\rho$ & $\\sigma$ & $\\tau$ \\\\", outcon)
  writeLines("\\hline", outcon)
  featdf <- read.csv(featurefile, stringsAsFactors=FALSE)
  df <- cbind(df, rep("asd", nrow(df)))
  names(df)[ncol(df)] <- "description"
  df$description <- as.character(df$description)
  for (i in 1:nrow(df)) {
    df$description[i] <- as.character(featdf[as.character(featdf$feature) == as.character(df[i,]$WALS_feature), ]$description)
  }
  df <- df[order(df$alpha, decreasing=FALSE), ]
  for (i in 1:nrow(df)) {
    thisfeat <- featdf[as.character(featdf$feature) == as.character(df[i,]$WALS_feature), ]
    p <- paste(i, ".", sep="")
    p <- c(p, as.character(thisfeat$description))
    p <- c(p, as.character(df[i,]$WALS_feature))
    p <- c(p, format(nsmall=dec, round(df[i,]$rho, dec)))
    p <- c(p, format(nsmall=dec, round(df[i,]$sigma, dec)))
    p <- c(p, format(nsmall=dec, round(df[i,]$alpha, dec)))
    p <- paste(p, collapse=" & ")
    p <- paste(p, "\\\\")
    writeLines(p, outcon)
  }
  writeLines("\\hline", outcon)
  #writeLines("\\end{tabular}", outcon)
  close(outcon)
}


# print_taulist()
#
# Prints a table of the 'howmanyonbothsides' highest and lowest tau features
#
print_taulist <- function(walsdf,
                          featurefile = "../newR/unsilly-features.csv",
                          howmanyonbothsides = 7,
                          dec = 5,
                          outfile) {
  require(stringr)

  # get feature descriptions from 'featurefile'
  featdf <- read.csv(featurefile)
  names(featdf)[1] <- "WALS_feature"
  walsdf <- merge(walsdf, featdf, by.y="WALS_feature")
  walsdf$name <- paste(walsdf$description, " (", walsdf$WALS_feature, ")", sep="")

  # order
  walsdf <- walsdf[order(walsdf$alpha, decreasing=FALSE), ]

  # take first 'howmanyonbothsides' rows
  df <- walsdf[1:howmanyonbothsides, ]
  # take last 'howmanyonbothsides' rows
  df <- rbind(df, walsdf[(nrow(walsdf) - (howmanyonbothsides - 1)):nrow(walsdf), ])

  outcon <- file(outfile, "w")
  writeLines("\\begin{tabular}{lr}", outcon)
  writeLines("Feature (WALS ID) & $\\tau$ \\\\", outcon)
  writeLines("\\hline", outcon)
  for (i in 1:howmanyonbothsides) {
    p <- paste(df[i,]$name, "&", format(nsmall=dec, round(df[i,]$alpha, dec)), "\\\\", sep="")
    writeLines(p, outcon)
  }
  writeLines("$\\vdots$ & $\\vdots$ \\\\", outcon)
  for (i in (howmanyonbothsides+1):nrow(df)) {
    p <- paste(df[i,]$name, "&", format(nsmall=dec, round(df[i,]$alpha, dec)), "\\\\", sep="")
    writeLines(p, outcon)
  }
  writeLines("\\hline", outcon)
  writeLines("\\end{tabular}", outcon)
  close(outcon)
}


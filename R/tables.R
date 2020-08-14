# print_results()
#
# Prints the results (rho, sigma, tau), ordered by decreasing tau.
#
print_results <- function(df = read.csv("../data/main-analysis.csv"),
                          featurefile = "../conf/unsilly-features.csv",
                          digits = 5,
                          outfile) {
  df <- aggregate(df[, 2:4], by=list(df$feature_ID), FUN=median, na.rm=TRUE)
  names(df)[1] <- "feature_ID"

  featdf <- read.csv(featurefile, stringsAsFactors=FALSE)
  featdf$feature_ID <- featdf$feature

  df <- merge(df, featdf, by="feature_ID")

  df <- df[order(df$temperature, decreasing=TRUE), ]
  df$rank <- paste0(1:nrow(df), ".")
  df <- df[, c("rank", "description", "feature_ID", "feature_frequency", "isogloss_density", "temperature")]
  df$feature_frequency <- hipster::prettyround(df$feature_frequency, digits=digits)
  df$isogloss_density <- hipster::prettyround(df$isogloss_density, digits=digits)
  df$temperature <- hipster::prettyround(df$temperature, digits=digits)

  before <- "Rank & Feature & WALS ID & $\\rho$ & $\\sigma$ & $\\tau$ \\\\\n\\hline"
  hipster::df2tex(df=df, file=outfile, col.names=FALSE, row.names=FALSE, header="\\begin{tabular}{rllrrr}", before=before, after="\\hline")
}


# print WALS feature levels for the SI (not technically a table)
print_WALS_levels <- function(features = ritwals::WALS_features,
                              unsillies = read.csv("../conf/unsilly-features.csv"),
                              outfile = "../plots/SI_WALS_levels.tex") {
  unsillies <- unsillies[unsillies$sample.size > 300, ]
  unsillies <- unsillies[order(unsillies$description), ]

  con <- file(outfile, "w")
  
  for (i in 1:nrow(unsillies)) {
    writeLines(paste0("\\subsection*{", i, ". ", unsillies[i,]$description, "}\n"), con)
    writeLines("\\begin{itemize}", con)
    writeLines(paste0("  \\item[--] WALS feature: ", unique(features[features$feature_ID==as.character(unsillies[i,]$feature), ]$feature), " (", unsillies[i,]$feature, ")"), con)
    writeLines("  \\item[--] Values:", con)
    writeLines("  {\\small", con)
    writeLines("  \\begin{enumerate}", con)
    thishere <- features[features$feature_ID==as.character(unsillies[i,]$feature), ]
    for (v in 1:nrow(thishere)) {
      towrite <- paste0("    \\item[", v, ":] ", thishere[thishere$value_ID==v, ]$value)
      towrite <- stringr::str_replace_all(towrite, pattern="\\&", replacement="\\\\&")
      towrite <- stringr::str_replace_all(towrite, pattern="'one'", replacement="`one'")
      if (v %in% as.numeric(unlist(strsplit(as.character(unsillies[i,]$up), ",")))) {
        towrite <- paste0(towrite, " (\\emph{present})")
      } else if (v %in% as.numeric(unlist(strsplit(as.character(unsillies[i,]$down), ",")))) {
        towrite <- paste0(towrite, " (\\emph{absent})")
      } else {
        towrite <- paste0(towrite, " (\\emph{N/A})")
      }
      writeLines(towrite, con)
    }
    writeLines("  \\end{enumerate}", con)
    writeLines("  }", con)
    writeLines("\\end{itemize}\n\n", con)
  }

  close(con)
}

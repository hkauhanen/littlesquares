# most of the functionality is in these development versions of
# the 'ritwals' and 'dyntyp' packages
require(ritwals)
require(dyntyp)

# universal sample size for the study
uni_sample_size <- 300

# universal neighbourhood size
uni_neighbourhood_size <- 1

# feature list
featurelist <- read.csv("unsilly-features.csv")
featurelist <- featurelist[featurelist$sample.size >= uni_sample_size, ]

# calculate rho, sigma and tau for one feature
do_one_feature <- function(feature,
                           sample_size = uni_sample_size,
                           neighbourhood_size = uni_neighbourhood_size,
                           data = ritwals::WALS) {
  featureinfo <- featurelist[featurelist$feature==feature, ]
  ups <- as.numeric(unlist(strsplit(as.character(featureinfo$up), ",")))
  downs <- as.numeric(unlist(strsplit(as.character(featureinfo$down), ",")))
  res <- dyntyp::temperature(id=feature,
                             up=ups,
                             down=downs,
                             resample_size=sample_size,
                             neighbourhood_size=neighbourhood_size,
                             data=data)
  res
}

# calculate rho, sigma and tau for each feature in featurelist
do_all_features <- function(sample_size = uni_sample_size,
                            neighbourhood_size = uni_neighbourhood_size) {
  out <- NULL
  for (feature in unique(featurelist$feature)) {
    res <- do_one_feature(feature=feature,
                          sample_size=sample_size,
                          neighbourhood_size=neighbourhood_size,
                          data=ritwals::WALS)
    out <- rbind(out, res)
  }
  out
}

# Old World vs. New World comparison à la Wichmann & Holman
OW_NW_features <- function(sample_size = 0,
                           neighbourhood_size = uni_neighbourhood_size) {
  OW <- c("Africa", "Eurasia")
  NW <- c("South America", "North America")
  WALS_OW <- WALS[WALS$macroarea %in% OW, ]
  WALS_NW <- WALS[WALS$macroarea %in% NW, ]
  out <- NULL
  for (feature in unique(featurelist$feature)) {
    res_OW <- do_one_feature(feature=feature,
                             sample_size=sample_size,
                             neighbourhood_size=neighbourhood_size,
                             data=WALS_OW)
    res_NW <- do_one_feature(feature=feature,
                             sample_size=sample_size,
                             neighbourhood_size=neighbourhood_size,
                             data=WALS_NW)
    res <- merge(res_OW, res_NW, by="feature_ID")
    out <- rbind(out, res)
  }
  out
}

# break down set of languages into two, calculate statistics for each 
# (for later correlation purposes)
correlate_features <- function(sample_size = uni_sample_size,
                               neighbourhood_size = uni_neighbourhood_size) {
  out <- NULL
  for (feature in unique(featurelist$feature)) {
    # feature's representation in WALS
    featureWALS <- WALS[WALS$feature_ID==feature, ]

    # random sample of sample_size size
    rows <- sample(1:nrow(featureWALS), size=sample_size, replace=FALSE)

    # two subsamples (partition of the random sample)
    rows1 <- sample(rows, size=floor(sample_size/2), replace=FALSE)
    rows2 <- rows[!(rows %in% rows1)]
    subsample1 <- featureWALS[rows1, ]
    subsample2 <- featureWALS[rows2, ]

    # computations
    res1 <- do_one_feature(feature=feature,
                           sample_size=0,
                           neighbourhood_size=neighbourhood_size,
                           data=subsample1)
    res2 <- do_one_feature(feature=feature,
                           sample_size=0,
                           neighbourhood_size=neighbourhood_size,
                           data=subsample2)
    res <- merge(res1, res2, by="feature_ID")
    out <- rbind(out, res)
  }
  out
}

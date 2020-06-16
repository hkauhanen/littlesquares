# calculate rho, sigma and tau for one feature
do_one_feature <- function(feature,
                           sample_size,
                           neighbourhood_size,
                           data,
                           featurefile) {
  featurelist <- read.csv(featurefile)
  featurelist <- featurelist[featurelist$sample.size >= uni_sample_size, ]
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

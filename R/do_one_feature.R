# Calculate rho, sigma and tau for one feature

do_one_feature <- function(feature,
                           resample_size = 0,
                           bootstrap = TRUE,
                           neighbourhood_size = 10,
                           reps = 1,
                           featurefile = "../conf/unsilly-features.csv",
                           data = ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ],
                           verbose = TRUE) {
  featurelist <- read.csv(featurefile)
  featureinfo <- featurelist[featurelist$feature==feature, ]
  ups <- as.numeric(unlist(strsplit(as.character(featureinfo$up), ",")))
  downs <- as.numeric(unlist(strsplit(as.character(featureinfo$down), ",")))
  res <- replicate(n=reps, 
                   simplify=FALSE, 
                   temperature(id=feature,
                               up=ups,
                               down=downs,
                               resample_size=resample_size,
                               bootstrap=bootstrap,
                               neighbourhood_size=neighbourhood_size,
                               data=data,
                               verbose=verbose))
  do.call(rbind, res)
}

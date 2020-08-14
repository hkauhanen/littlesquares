# Calculate rho, sigma and tau for one feature

do_one_feature <- function(feature,
                           resample_size = 0,
                           bootstrap = TRUE,
                           neighbourhood_size = 10,
                           reps = 1,
                           data = ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ],
                           genetic = FALSE,
                           genetic_unit = "family",
                           verbose = TRUE) {
  featureinfo <- featurelist[featurelist$feature==feature, ]
  ups <- as.numeric(unlist(strsplit(as.character(featureinfo$up), ",")))
  downs <- as.numeric(unlist(strsplit(as.character(featureinfo$down), ",")))
  if (!genetic) {
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
  } else {
    res <- replicate(n=reps, 
                     simplify=FALSE, 
                     temperature_genetic(id=feature,
                                         up=ups,
                                         down=downs,
                                         unit=genetic_unit,
                                         resample_size=resample_size,
                                         bootstrap=bootstrap,
                                         neighbourhood_size=neighbourhood_size,
                                         data=data,
                                         verbose=verbose))
  }
  do.call(rbind, res)
}

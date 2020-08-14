system("tar xzf R.tar.gz")
setwd("R")
source("libraries.R")
install_required("libs")
source("analyse.R")

args <- commandArgs(TRUE)

res <- correlate_features(sample_size=300,
                          neighbourhood_size=1)

res$rep <- paste("r", args[1], args[2], sep=".")

write.csv(res, row.names=FALSE, 
          file=paste0("../res.correlations_geo.", args[1], ".", args[2], ".csv"))


system("tar xzf R.tar.gz")
setwd("R")
source("libraries.R")
install_required("libs")
source("analyse.R")

args <- commandArgs(TRUE)

res <- OW_NW_features(sample_size=50,
                      neighbourhood_size=1)

res$rep <- paste(args[1], args[2], sep=".")

write.csv(res, row.names=FALSE, 
          file=paste0("../res.correlations_OWNW.", args[1], ".", args[2], ".csv"))


system("tar xzf R.tar.gz")
setwd("R")
source("libraries.R")
install_required("libs")
source("analyse.R")

args <- commandArgs(TRUE)

res <- do_all_features(neighbourhood_size=as.numeric(args[3]))

res$rep <- paste(args[1], args[2], args[3], sep=".")

write.csv(res, row.names=FALSE, 
          file=paste0("../res.correlations-scale.", args[1], ".", args[2], ".", args[3], ".csv"))


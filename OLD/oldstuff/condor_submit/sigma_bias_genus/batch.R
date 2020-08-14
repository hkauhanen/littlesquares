system("tar xzf R.tar.gz")
setwd("R")
source("libraries.R")
install_required("libs")
source("analyse.R")
source("sigmaunits.R")

args <- commandArgs(TRUE)

res <- sigmaunits(unit="genus")

res$rep <- paste("r", args[1], args[2], sep=".")

write.csv(res, row.names=FALSE, 
          file=paste0("../res.sigma_bias_genus.", args[1], ".", args[2], ".csv"))


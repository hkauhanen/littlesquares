system("tar xzf R.tar.gz")
setwd("R")
source("libraries.R")
install_required("libs")
source("analyse.R")

args <- commandArgs(TRUE)

res <- do_all_features()

res$rep <- paste(args[1], args[2], sep=".")

write.csv(res, row.names=FALSE, 
          file=paste0("../res.main-analysis.", args[1], ".", args[2], ".csv"))


# command-line arguments from SGE submit script
args <- commandArgs(trailingOnly=TRUE)
job <- as.numeric(args[2])

# NB we now have to install the packages node-side, since different nodes appear to have
# different versions of R, leading to errors if the libraries are installed before
# deploying the jobs. Hence the following hacky solution.
libdir <- paste0("./lib/lib", job)
dir.create(libdir)
install.packages("../../pkg/ritwals_0.0.0.9101.tar.gz", repos=NULL, lib=libdir)
install.packages("../../pkg/hipster_0.0.0.9010.tar.gz", repos=NULL, lib=libdir)
library(ritwals, lib.loc=libdir)
library(hipster, lib.loc=libdir)

source("../../R/do_one_feature.R")
source("../../R/temperature.R")

# seed RNG
seed <- hipster::seed_seed(seed=2020+2, n=job)

# prepare
load("../../conf/wals_neighbours.RData")
load("../../conf/tau_hash.RData")
load("../../conf/featurelist.RData")
unsillies <- featurelist
unsillies <- unsillies[unsillies$sample.size >= 300, ]
feat <- hipster::factor2character(unsillies[job, ]$feature)
reps <- 100
WALS_nosign <- ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ]

# run
myfun <- function(ns) {
	reshere <- do_one_feature(feat,
				  resample_size=0,
				  bootstrap=TRUE,
				  neighbourhood_size=ns,
				  reps=reps,
				  data=WALS_nosign,
				  verbose=FALSE)
	reshere$rep <- 1:reps
	reshere
}
res <- do.call(rbind, lapply(X=1:50, FUN=myfun))

# writeout
write.csv(res, file=paste("results/res", args[2], feat, "csv", sep="."), row.names=FALSE)

# remove libdir
unlink(libdir, recursive=TRUE)

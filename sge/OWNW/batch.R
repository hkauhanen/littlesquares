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
seed <- hipster::seed_seed(seed=2020+3, n=job)

# prepare
load("../../conf/wals_neighbours.RData")
load("../../conf/tau_hash.RData")
load("../../conf/featurelist.RData")
unsillies <- featurelist
unsillies <- unsillies[unsillies$sample.size >= 300, ]
feat <- hipster::factor2character(unsillies[job, ]$feature)
reps <- 1000
WALS_nosign <- ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ]
WALS_OW <- WALS_nosign[WALS_nosign$longitude >= 0 & WALS_nosign$longitude <= 180, ]
WALS_NW <- WALS_nosign[WALS_nosign$longitude < 0 & WALS_nosign$longitude >= -180, ]

# run
res_OW <- do_one_feature(feat,
			 resample_size=0,
			 bootstrap=TRUE,
			 neighbourhood_size=10,
			 reps=reps,
			 data=WALS_OW,
			 verbose=FALSE)
res_OW$rep <- 1:reps
res_OW$hemisphere <- "OW"

res_NW <- do_one_feature(feat,
			 resample_size=0,
			 bootstrap=TRUE,
			 neighbourhood_size=10,
			 reps=reps,
			 data=WALS_NW,
			 verbose=FALSE)
res_NW$rep <- 1:reps
res_NW$hemisphere <- "NW"


# writeout
write.csv(res_OW, file=paste("results/res_OW", args[2], feat, "csv", sep="."), row.names=FALSE)
write.csv(res_NW, file=paste("results/res_NW", args[2], feat, "csv", sep="."), row.names=FALSE)

# remove libdir
unlink(libdir, recursive=TRUE)

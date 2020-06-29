# required stuff
library(ritwals, lib.loc="./lib")
library(hipster, lib.loc="./lib")
source("../../R/do_one_feature.R")
source("../../R/temperature.R")

# command-line arguments from SGE submit script
args <- commandArgs(trailingOnly=TRUE)
job <- as.numeric(args[2])

# seed RNG
seed <- hipster::seed_seed(seed=2020+1, n=job)

# prepare
wals_neighbours <- read.csv("../../conf/wals-distances-100closest.csv")
tau_hash <- read.csv("../../conf/tau-hash.csv")
unsillies <- read.csv("../../conf/unsilly-features.csv")
unsillies <- unsillies[unsillies$sample.size >= 300, ]
feat <- hipster::factor2character(unsillies[job, ]$feature)
reps <- 1000
WALS_nosign <- ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ]

# run
res <- do_one_feature(feat,
                      resample_size=0,
                      bootstrap=TRUE,
                      neighbourhood_size=10,
                      reps=reps,
                      featurefile="../../conf/unsilly-features.csv",
                      data=WALS_nosign,
		      verbose=FALSE)
res$rep <- 1:reps

# writeout
write.csv(res, file=paste("results/res", args[2], feat, "csv", sep="."), row.names=FALSE)

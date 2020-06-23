# required stuff
library(ritwals, lib.loc="./lib")
library(hipster, lib.loc="./lib")
load("newR/temperature_hash.RData")
source("newR/temperature.R")
source("newR/toolbox.R")

# command-line arguments from Condor submit script
args <- commandArgs(trailingOnly=TRUE)
job <- as.numeric(args[2])

# seed RNG
seed <- hipster::seed_seed(seed=2020+1, n=job)

# prepare
unsillies <- read.csv("newR/unsilly-features.csv")
unsillies <- unsillies[unsillies$sample.size >= 300, ]
pargrid <- expand.grid(feature=unsillies$feature, nsize=seq(from=1, to=50, by=1))
feat <- pargrid[job, ]$feature
nsize <- pargrid[job, ]$nsize
reps <- 1000
WALS_nosign <- ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ]

# run
NN_hash <- read.csv("newR/wals-distances-100closest-nosign.csv")
featureinfo <- unsillies[unsillies$feature==feat, ]
ups <- as.numeric(unlist(strsplit(as.character(featureinfo$up), ",")))
downs <- as.numeric(unlist(strsplit(as.character(featureinfo$down), ",")))
res <- do.call(rbind, replicate(temperature(id=feat, up=ups, down=downs, resample_size=-1, bootstrap=TRUE, neighbourhood_size=nsize, NN_hash=NN_hash, data=WALS_nosign), n=reps, simplify=FALSE))
res$rep <- 1:reps

# writeout
write.csv(res, file=paste("results/res", args[2], feat, "csv", sep="."), row.names=FALSE)

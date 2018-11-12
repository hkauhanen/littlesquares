###########
# preproc.R
#
# Routines for preprocessing the WALS data: download, prune, and establish
# nearest neighbours.
#
# Henri Kauhanen 2016-2017
##########################



# get_wals()
#
# Download WALS data. Need a list of features in file <featurelist>.
# Invokes wget and sed for downloading and pruning of the WALS metadata,
# respectively.
#
get_wals <- function(server = "http://wals.info/",
                     folder = "feature",
                     featurelist = "../data/wals-20170718/Parameters.csv",
                     prune_metadata = TRUE,
                     outfolder = "../data/wals-20170718/features") {
  featlist <- read.csv(featurelist)
  features <- unique(featlist$id)
  for (feature in features) {
    system(paste("wget ", server, folder, "/", as.character(feature), ".tab -P ", outfolder, sep=""))
    if (prune_metadata) {
      outfile <- paste(outfolder, "/", as.character(feature), ".tab", sep="")
      print(outfile)
      system(paste("sed -e '1,7d' < ", outfile, " > ", outfile, ".pruned.csv", sep=""))
    }
  }
}


# near_neighs()
#
# Find nearest neighbour language pairs vis-à-vis each feature. Output is in
# form of a table that gives feature, language code <id>, nearest neighbour
# code <nn> and distance between the two languages <dist>. If language does
# not belong to the feature sample, <nn> and <dist> are NA. Such cases are
# optionally pruned from the table before output (this is done by default,
# since those cases are never needed for calculations later on).
#
near_neighs <- function(featurelist = "../data/wals-20170718/Parameters.csv",
                        folder = "../data/wals-20170718/features",
                        extension = ".tab.pruned.csv",
                        latlong_file = "../data/wals-20170718/languoid.tab",
                        outfile = "../data/adjacencies.csv",
                        earth_rad = 6371,
                        prune = TRUE,
                        debug = FALSE,
                        verbose = FALSE) {
  # subroutine, does the actual dirty work via haversine formula
  find_nn <- function(df, id, earth_rad) {
    dists <- rep(NA, times=nrow(df))
    phi1 <- as.numeric(df[df$wals.code==id, ]$latitude)*(pi/180)
    lam1 <- as.numeric(df[df$wals.code==id, ]$longitude)*(pi/180)
    for (j in 1:nrow(df)) {
      phi2 <- as.numeric(df[j,]$latitude)*(pi/180)
      lam2 <- as.numeric(df[j,]$longitude)*(pi/180)

      # haversine
      dists[j] <- 2*earth_rad*asin(sqrt((sin(0.5*(phi2-phi1)))^2 + cos(phi1)*cos(phi2)*(sin(0.5*(lam2-lam1)))^2))
    }

    # get rid of self
    dists[which.min(dists)] <- max(dists) + 1

    # find neighbour
    nei <- which.min(dists)
    neicode <- df[nei,]$wals.code

    # return
    c(neicode, dists[nei])
  }

  # set things up
  features <- unique(read.csv(featurelist)$id)
  world <- read.csv(latlong_file, sep="\t")
  countries <- world$wals.code
  df <- expand.grid(features, countries)
  df <- cbind(df, rep(NA, nrow(df)), rep(0, nrow(df)))
  df <- data.frame(df)
  names(df) <- c("feature", "id", "nn", "dist")

  # cycle through features
  for (i in 1:nrow(df)) {
    if (verbose) {
      print(paste("Operating on case", i, "out of", nrow(df)))
    }
    if (debug) {
      print(df[i,]$feature)
      print(df[i,]$id)
    }
    thisfeature <- read.csv(paste(folder, "/", df[i,]$feature, extension, sep=""), sep="\t", stringsAsFactors=FALSE)
    if (df[i,]$id %in% thisfeature$wals.code) {
      result <- find_nn(df=thisfeature, id=df[i,]$id, earth_rad=earth_rad)
    } else {
      result <- c(NA, NA)
    }
    df[i,]$nn <- result[1]
    df[i,]$dist <- as.numeric(result[2])
  }

  # write to file
  if (prune) {
    df <- df[!is.na(df$nn), ]
  }
  write.csv(file=outfile, df, row.names=FALSE)
}


# prep_condor_script()
#
# Prepares a Condor submit script, so that analysis of features may be
# parallelised.
#
prep_condor_script <- function(outfile,
                               featurefile = "../data/unsilly-features.csv") {
  con <- file(outfile, open="w")
  features <- read.csv(featurefile)$feature
  
  writeLines(con=con, text='universe = vanilla
requirements = ((opsys == "LINUX") && (arch == "X86_64") && (HAS_R_3_2 =?= True))
notification = never
request_memory = 2000
request_disk = 1000000
executable = /opt/R-3.2.4/bin/R
input = batch.R
should_transfer_files = yes
transfer_executable = false
transfer_input_files = unsilly-features.csv,wals-20170718.tar.gz,analyse.R,batch.R
when_to_transfer_output = ON_EXIT
log = R.log
output = R.out
error = R.err
Rank=kflops')

for (feature in features) {
  writeLines(con=con, text=paste("arguments = --vanilla --args $(Process)", feature))
  writeLines(con=con, text="queue")
}

close(con)
}

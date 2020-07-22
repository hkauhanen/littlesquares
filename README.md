# littlesquares

This repository contains simulation code and data analysis routines for the estimation of linguistic temperatures from the WALS atlas, as explained in the following paper:

> Kauhanen, Henri, Deepthi Gopal, Tobias Galla & Ricardo Bermúdez-Otero (2018) "Geospatial distributions reflect rates of evolution of features of language" <https://arxiv.org/abs/1801.09637>

Data analysis routines are written in R (see folder `R`); to facilitate bootstraps, the folder `sge` contains scripts that deploy the data analysis to run on a parallel computing cluster, one WALS feature per node. The model put forward in the above publication can be simulated using the programs in the `cpp` folder.


## Contents at a glance

* `conf`: Configuration files
* `cpp`: Code (C++) for simulating the model either on a lattice or an arbitrary graph.
* `data`: Results of data analyses
* `pkg`: R packages the data analysis code depends on
* `plots`: Plots
* `R`: Code (R) for data analysis
* `Rsession`: R session for data analysis
* `sge`: Scripts to conduct analyses on a SGE (Son of Grid Engine) based parallel computing cluster, plus results of these analyses
* `simulations`: Simulations, along with input parameter files and output
* `OLD`: Old stuff. Will be deleted (or archived) eventually, once all the important bits have been incorporated into the new directory structure.


## How to reproduce the analyses

### 1. Dependencies

The scripts depend on the availability of two R packages, [hipster](https://github.com/hkauhanen/hipster) and [ritwals](https://hkauhanen.github.io/ritwals). They are included in the `pkg` folder and need to be installed and loaded first. Supposing `Rsession` to be the working directory, execute:

``` r
install.packages("../pkg/hipster_0.0.0.9010.tar.gz", repos=NULL)
install.packages("../pkg/ritwals_0.0.0.9101.tar.gz", repos=NULL)
library(hipster)
library(ritwals)
```

The scripts also require a hash table for the inversion of the H(tau) function introduced in the paper. This can be generated and loaded into memory as follows:

``` r
source("../R/preprocess-tauhash.R")
load("../conf/tau_hash.RData")
```

Furthermore, the scripts require information about the nearest neighbours of each language in WALS. This information is supplied in the file `conf/wals_neighbours.RData` (also available in CSV format as the file `conf/wals-distances-100closest.csv`), whose generation is detailed [elsewhere](https://github.com/hkauhanen/wals-distances). Load this:

``` r
load("../conf/wals_neighbours.RData")
```

Moreover, we need some metadata about the features themselves:

``` r
load("../conf/featurelist.RData")
```

Finally, we need to source the scripts themselves:

``` r
source("../R/do_one_feature.R")
source("../R/temperature.R")
```


### 2. To calculate the temperature of one feature

To estimate the temperature of one WALS feature, use the function `do_one_feature`. It takes the following arguments (with defaults as indicated):

* `feature`: Feature's WALS ID
* `resample_size = 0`: If positive, take a resample of the feature's language sample of this size. If 0 or negative, all languages in the sample are taken into consideration
* `bootstrap = TRUE`: Whether to take a bootstrap (sample with replacement) of the feature's language sample
* `neighbourhood_size = 10`: How many nearest geographical neighbour languages to consider in the calculation of isogloss density
* `reps = 1`: How many Monte Carlo repetitions to take
* `data = ritwals::WALS[ritwals::WALS$genus != "Sign Languages", ]`: Dataset to use; by default, all non-sign languages in WALS are considered
* `verbose = TRUE`: Whether to print progress information

For example, the following function call will estimate the temperature of WALS feature 9A, taking just one repetition and without bootstrapping:

``` r
do_one_feature(feature="9A", bootstrap=FALSE)
```


### 3. To calculate all temperatures ("main analysis")

To generate bootstrap confidence intervals, the analysis in the paper repeats the temperature estimation procedure 1,000 times for each feature. This takes about 15 minutes on an ordinary computer per feature. So it makes sense to parallelize the operation. The `sge` directory contains the necessary infrastructure to carry this out on an SGE (Son of Grid Engine) based system: see the instructions there.


### 4. Numerical simulations of model

FIXME


### 5. Plotting routines


## How to cite

All code in this repository is provided as free software under the [GNU GPL-3.0 license](LICENSE). If you use/adapt it, please provide a link to the repository and cite the above-mentioned publication.

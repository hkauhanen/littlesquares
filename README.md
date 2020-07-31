# Temperatures of linguistic features

This repository contains simulation code and data analysis routines for the estimation of linguistic temperatures from the WALS atlas, as explained in the following paper:

> Kauhanen, Henri, Deepthi Gopal, Tobias Galla & Ricardo Bermúdez-Otero (2018) "Geospatial distributions reflect rates of evolution of features of language". <https://arxiv.org/abs/1801.09637>

Data analysis routines are written in R (see folder `R`); to facilitate bootstraps, the folder `sge` contains scripts that deploy the data analysis to run on a parallel computing cluster, one WALS feature per node. The model put forward in the above publication can be simulated using the programs in the `cpp` folder.


## Contents at a glance

* `conf`: Configuration files
* `cpp`: Code (C++) for simulating the model either on a lattice or an arbitrary graph
* `data`: Results of data analyses
* `fortran`: Old code for simulations; only used for one visualization now
* `pkg`: R packages the data analysis code depends on
* `plots`: Plots
* `R`: Code (R) for data analysis
* `Rsession`: R session for data analysis
* `sge`: Scripts to conduct analyses on a SGE (Son of Grid Engine) based parallel computing cluster, plus results of these analyses
* `simulations`: Simulations, along with input parameter files and output
* `tex`: LaTeX source for some figures and tables
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

A third package, [pracma](https://cran.r-project.org/package=pracma), is needed for the computation of the arithmetic–geometric mean, which is used to approximate the complete elliptic integral of the first kind, which appears in the expression for H(tau). It can be installed directly from CRAN:

``` r
install.packages("pracma")
library(pracma)
```

The scripts require a hash table for the inversion of the H(tau) function introduced in the paper. This can be generated and loaded into memory as follows:

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

For example, the following function call will estimate the temperature of [WALS feature 9A](https://wals.info/feature/9A), taking just one repetition and without bootstrapping:

``` r
do_one_feature(feature="9A", bootstrap=FALSE)
```


### 3. To calculate all temperatures ("main analysis")

To generate bootstrap confidence intervals, the analysis in the paper repeats the temperature estimation procedure 1,000 times for each feature. This takes about 15 minutes on an ordinary computer per feature. So it makes sense to parallelize the operation. The `sge/main-analysis` directory contains the necessary infrastructure to carry this out on an SGE (Son of Grid Engine) based system, so that each feature is given its own processor: see the instructions there.


### 4. Variation in neighbourhood size

The main analysis considers the 10 nearest neighbours of each language for the calculation of isogloss densities. In the SI to the paper, the number of nearest neighbours was systematically varied to examine if the choice of this number had an effect on the resulting temperature estimates. To implement this, it is easiest to use an `rbind`–`lapply` wrapper on `do_one_feature`, e.g.:

``` r
my_neighbourhood_sizes <- c(10, 20, 50)
my_wrapper <- function(ns) {
  do_one_feature("9A", neighbourhood_size=ns)
}
do.call(rbind, lapply(X=my_neighbourhood_sizes, FUN=my_wrapper))
```

The scripts in the directory `sge/neighbourhoods` can be used to deploy this analysis on an SGE cluster, one feature per node, 100 Monte Carlo repetitions per feature.


### 5. OW–NW comparison

The Old World – New World comparison reported in the SI can be effected by subsetting the WALS dataset suitably in the call to `do_one_feature`. The scripts in `sge/OWNW` deploy this on SGE.


### 6. Autocorrelation analysis


### 7. Numerical simulations of model

FIXME


### 8. Statistics, figures and tables

Assuming that the temperature analyses have been run on SGE as above outlined, the results (which are scattered across multiple CSV files) can be aggregated together as follows:

``` r
source("../R/aggregate_data.R")
sge_to_data("../sge/main-analysis/results", "../data/main-analysis.csv")
sge_to_data("../sge/neighbourhoods/results", "../data/neighbourhoods.csv")
sge_to_data("../sge/OWNW/results", "../data/OWNW.csv")
```

Statistics and plotting routines look for these aggregated CSV files (in the `data` folder) by default.

#### Statistics

First, source the necessary scripts:

``` r
source("../R/stats.R")
```

To run the correlation test of temperatures against the PC1 values from [Dediu's phylogenetic stability analysis](https://doi.org/10.1098/rspb.2010.1595):

``` r
correlate_with_dediu(no_of_knockouts=4)
```

To correlate temperatures measured on the two hemispheres separately:

``` r
correlate_OWNW()
```

To correlate temperatures resulting from assuming *k* = 10 nearest neighbours in the geospatial analysis against assuming other values of *k*:

``` r
correlate_over_neighbourhood_size(reference=10)
```

#### Figures

Source scripts:

``` r
source("../R/plots.R")
source("../R/interpolations.R")
```

The packages ggplot2, ggrepel, gridExtra, mapproj, maps, maptools, phylin, RColorBrewer, shades and stringr are required by the plotting routines; if any of these are missing, install first.

First, we generate the required data for the feature interpolation maps:

``` r
interpol_37A <- interpolate_feature("37A")
interpol_83A <- interpolate_feature("83A")
save(interpol_37A, file="../data/interpol_37A.RData")
save(interpol_83A, file="../data/interpol_83A.RData")
```

To plot everything for the main paper, use the following function – note that this will call `xelatex` to compose some of the figures, and that the total runtime is on the order of 2 minutes:

``` r
plot_everything()
```

Output is to `../plots/Fig*.pdf`.

To plot the figures for the SI:

FIXME

#### Tables

FIXME


## How to cite

All code in this repository is provided as free software under the [GNU GPL-3.0 license](LICENSE). If you find it useful, please provide a link to the repository (<https://github.com/hkauhanen/littlesquares/>) and cite the paper:

> Kauhanen, Henri, Deepthi Gopal, Tobias Galla & Ricardo Bermúdez-Otero (2018) "Geospatial distributions reflect rates of evolution of features of language". <https://arxiv.org/abs/1801.09637>

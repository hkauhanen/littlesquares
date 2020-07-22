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

The scripts depend on the availability of two R packages, [hipster](https://github.com/hkauhanen/hipster) and [ritwals](https://hkauhanen.github.io/ritwals). They are included in the `pkg` folder and need to be installed and loaded first. Supposing `Rsession` to be the working directory, execute:

``` r
install.packages("../pkg/hipster_0.0.0.9010.tar.gz", repos=NULL)
install.packages("../pkg/ritwals_0.0.0.9101.tar.gz", repos=NULL)
library(hipster)
library(ritwals)
```


## How to cite

All code in this repository is provided as free software under the [GNU GPL-3.0 license](LICENSE). If you use/adapt it, please provide a link to the repository and cite the above-mentioned publication.

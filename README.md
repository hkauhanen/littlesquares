# littlesquares

This repository contains simulation code and data analysis routines for the estimation of linguistic temperatures from the WALS atlas, as explained in the following paper:

> Kauhanen, H., Gopal, D., Galla, T. & Bermúdez-Otero, R. (2018) "Geospatial distributions reflect rates of evolution of features of language" <https://arxiv.org/abs/1801.09637>

Data analysis routines are written in R (see folder `R`); to facilitate bootstraps, the folder `sge` contains scripts that deploy the data analysis to run on a parallel computing cluster, one WALS feature per node. The model put forward in the above publication can be simulated using the programs in the `cpp` folder.


## Contents at a glance

* `conf`: Configuration files
* `cpp`: Code (C++) for simulating the model either on a lattice or an arbitrary graph.
* `pkg`: R packages the data analysis code depends on
* `plots`: Plots
* `R`: Code (R) for data analysis
* `Rsession`: R session for data analysis
* `sge`: Scripts to conduct analyses on a SGE (Son of Grid Engine) based parallel computing cluster, plus results of these analyses
* `simulations`: Simulations, along with input parameter files and output
* `OLD`: Old stuff. Will be deleted (or archived) eventually, once all the important bits have been incorporated into the new directory structure.


## How to cite

All code in this repository is provided as free software under the GNU GPL license. If you use/adapt it, please provide a link to the repository and cite the above-mentioned publication.

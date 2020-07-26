# littlesquares

ver. 0.2

Henri Kauhanen 2019–2020


## Description

Program for simulating the cultural evolution of languages. This version of the program assumes languages are distributed on a regular square lattice with periodic boundary conditions. The accompanying `littlesquares-graph` program (see `README-graph.md`) relaxes this assumption in favour of an arbitrary adjacency graph.

Languages are vectors of binary features. Three processes are implemented, relative to a given language (which, normally, would be sampled uniformly at random at each iteration of the program):

1. Noisy vertical process. A single feature value is acquired with a certain probability (ingress), if not already present in the language, and lost with a certain probability (egress), if already present in the language.
2. Noisy voter process. A single feature value is copied from a language into one of its neighbour languages. This copying is subject to errors: the value flips from 0 to 1 (horizontal ingress) or from 1 to 0 (horizontal egress) with certain probabilities.
3. Branching process. A copy of an entire language (all features) is placed into one of its neighbours.

For one application of this model, see:

Kauhanen, H., Gopal, D., Galla, T. & Bermúdez-Otero, R. (2018) "Geospatial distributions reflect rates of evolution of features of language". https://arxiv.org/abs/1801.09637


## Usage

```
./littlesquares --iterations I --measurementphase M --sidelength S --branchingrate B --voterrate V --parameterfile P --outfile O --taufile T
```

It is necessary to specify each argument, otherwise the program will not run. The arguments are:

* I: number of simulation iterations
* M: length of the phase (at the very end of the simulation) during which the important model behaviour quantities rho, sigma and tau are measured
* S: length of the lattice side (i.e. there will be S^2 lattice nodes)
* B: probability of branching event
* V: probability of voter event
* P: a CSV file, each row constituting one feature, first column constituting the feature's ingress rate,second column that feature's egress rate, third column that feature's horizontal ingress rate, and fourth column that feature's horizontal egress rate
* O: write the output gather during the measurement phase to this CSV file
* T: hash for inverting the H(tau) function, first column containing values of tau and the second the corresponding values of H(tau).

For the meaning of all this, see Kauhanen et al. (2018) cited above. For examples of P and T, see the `input` folder.


## Interpreting the output

Output is in the form of a comma-separated values file with the following columns:

* `iteration`: simulation iteration
* `feature`: feature identifier
* `no_of_iterations`: length of simulation
* `no_of_languages`: total number of languages (lattice nodes)
* `no_of_features`: total number of features
* `branching_rate`: branching rate
* `voter_rate`: voter rate
* `ingress_rate`: ingress rate
* `egress_rate`: egress rate
* `horiz_ingress_rate`: horizontal ingress rate
* `horiz_egress_rate`: horizontal egress rate
* `rho`: empirical feature frequency
* `sigma`: empirical isogloss density
* `tau`: empirical temperature
* `tau_predicted`: theoretical temperature predicted from model parameters, assuming zero branching and zero horizontal errors
* `tau_predicted_lambda`: theoretical temperature predicted from model parameters, assuming zero branching and non-zero horizontal branching ("extended model")


## Compiling

The program is written in C++. To compile, make sure GCC and GNU make are installed. Then compilation is as easy as:

```
cd src
make
cd ..
```


## License

This software is released under the GNU GPL license (see LICENSE). You are free to modify and distribute it as long as you follow that license.


## Contact & bug reports

henri@henr.in

littlesquares
ver. 0.1

Henri Kauhanen 2019


DESCRIPTION
-----------

Code for simulating the cultural evolution of languages. Initially the idea was
to place languages on a regular 2D lattice with periodic boundary cnoditions.
But the code can easily be modified/generalized for arbitrary adjacency graphs
by replacing the LatticeWorld class with something appropriate.

Languages are vectors of binary features. Four processes are implemented,
relative to a given language (which, normally, would be sampled uniformly at
random at each iteration of the program):

  1) Branching. A copy of the language is placed into one of its neighbours
     (which is assumed to have died previously).
  2) Voter process. A single feature is copied to a neighbour.
  3) Ingress. A single feature value is acquired, if not already present in the
     language.
  4) Egress. A single feature value is lost, if not already present in the
     language.

For one application of this model, see:

Kauhanen, Gopal, Galla & Bermúdez-Otero (2018)
"Geospatial distributions reflect rates of evolution of features of language"
https://arxiv.org/abs/1801.09637


USAGE
-----

./littlesquares --iterations I --measurementphase M --sidelength S
--branchingrate B --voterrate V --parameterfile P --outfile O --taufile T

It is necessary to specify each argument, otherwise the program will not run.
The arguments are:

  - I: number of simulation iterations
  - M: length of the phase (at the very end of the simulation) during which
       the important model behaviour quantities rho, sigma and tau are measured
  - S: length of the lattice side
  - B: probability of branching event
  - V: probability of voter event
  - P: a CSV file, each row constituting one feature, first column constituting
       the feature's ingress rate and the second column that feature's egress
       rate
  - O: write the output gather during the measurement phase to this CSV file
  - T: hash for inverting the H(tau) function, first column containing values
       of tau and the second the corresponding values of H(tau).

For the meaning of all this, see Kauhanen et al. (2018) cited above. For
examples of P and T, see the 'input' folder.


COMPILING
---------

The program is written in C++. To compile, make sure GCC and GNU make are
installed. Then compilation is as easy as:

cd src
make
cd ..


LICENSE
-------

This software is released under the GNU GPL license (see LICENSE). You are
free to modify and distribute it as long as you follow that license.


CONTACT & BUG REPORTS
---------------------

nenahuak@gmail.com

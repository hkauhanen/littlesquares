littlesquares-graph
ver. 0.1

Henri Kauhanen 2019


DESCRIPTION
-----------

Like the program 'littlesquares', but on a graph instead of a lattice.
See README.txt.


USAGE
-----

./littlesquares-graph --iterations I --measurementphase M --graphfile G
--branchingrate B --voterrate V --parameterfile P --outfile O --taufile T

It is necessary to specify each argument, otherwise the program will not run.
The arguments are:

  - I: number of simulation iterations
  - M: length of the phase (at the very end of the simulation) during which
       the important model behaviour quantities rho, sigma and tau are measured
  - G: CSV file supplying the adjacency graph. Three columns: first column is
       language's ID (e.g. WALS ID), second column is language's index (just
       a number running from 1 to total number of languages), and third
       column gives language's neighbours as a colon-separated string of
       indices. If language has no neighbours, set value in third column
       to NA.
  - B: probability of branching event
  - V: probability of voter event
  - P: a CSV file, each row constituting one feature, first column constituting
       the feature's ingress rate and the second column that feature's egress
       rate
  - O: write the output gather during the measurement phase to this CSV file
  - T: hash for inverting the H(tau) function, first column containing values
       of tau and the second the corresponding values of H(tau).

For examples of G, P and T, see the 'input' folder.


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

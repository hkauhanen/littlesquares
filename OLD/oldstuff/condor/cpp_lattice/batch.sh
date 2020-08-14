#!/bin/bash
tar xzf cpp.tar.gz
cd cpp
./littlesquares --iterations 12250000 --measurementphase 1 --sidelength 50 --voterrate 0.5 --parameterfile input/ingress_egress_rates.csv --taufile input/tau_hash.csv --branchingrate 0.0 --outfile ../res.cpp_lattice.${1}.${2}.csv
cd ..

#!/bin/bash
tar xzf cpp.tar.gz
cd cpp
./littlesquares-graph --iterations 12250000 --measurementphase 1 --graphfile input/WALS_neighbours_4closest.csv --voterrate 0.5 --parameterfile input/ingress_egress_rates.csv --taufile input/tau_hash.csv --branchingrate 0.0 --outfile ../cpp_wals.res.${1}.${2}.csv
cd ..

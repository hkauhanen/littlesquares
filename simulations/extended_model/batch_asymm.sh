#!/bin/bash

for q in 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
do
  ../../cpp/littlesquares --iterations 50000000 --measurementphase 1 --sidelength 50 --branchingrate 0 --voterrate $q --parameterfile ingress_egress_rates_extended_asymm.csv --outfile output_asymm/res.$q.csv --taufile ../../cpp/input/tau_hash.csv
done

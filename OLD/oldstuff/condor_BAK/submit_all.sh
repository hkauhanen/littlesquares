#!/bin/bash

for folder in correlations_geo correlations_OWNW correlations_scale cpp_lattice cpp_wals main_analysis sigma_bias
do
  cd $folder
  condor_submit submit.txt
  cd ..
done

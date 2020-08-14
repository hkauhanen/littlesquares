#!/bin/sh
for FOLDER in correlations_geo correlations_OWNW correlations_scale cpp_lattice cpp_wals main_analysis sigma_bias_family sigma_bias_genus
do
  cd $FOLDER
  rm res.*
  rm CONDOR.*
  cd ..
done

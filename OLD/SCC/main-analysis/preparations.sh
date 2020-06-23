#!/bin/bash
cp ../../*.tar.gz .
rm -rf newR
tar xzf newR.tar.gz
rm -rf results
mkdir results
rm -rf errout
mkdir errout
rm -rf lib
mkdir lib
Rscript install_pkg.R
rm -f *.tar.gz

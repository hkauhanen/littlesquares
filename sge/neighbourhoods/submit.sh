#!/bin/bash
#$ -e errout
#$ -o errout
#$ -t 1-35

# load R
module load R

# run script
Rscript batch.R --args $SGE_TASK_ID


# These functions are used to aggregate data from analyses conducted
# on the SGE parallel computing cluster into human-friendly CSV files,
# placed in the 'data' folder.


sge_to_data <- function(infolder,
                        outfile) {
  write.csv(hipster::cat_csv(infolder), file=outfile, row.names=FALSE)
}

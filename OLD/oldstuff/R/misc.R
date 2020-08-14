print_condor_script_scale_correlations <- function(reps = 50,
                                                   neighbourhood_sizes = unique(floor(exp(seq(from=log(1), to=log(100), length.out=42)))),
                                                   outfile) {
  sink(outfile)
  out <- paste0("arguments = --vanilla --args $(Cluster) $(Process) ", neighbourhood_sizes)
  for (i in 1:length(out)) {
    cat(out[i]);
    cat("\n");
    cat(paste0("queue ", reps))
    cat("\n");
  }
  sink()
}

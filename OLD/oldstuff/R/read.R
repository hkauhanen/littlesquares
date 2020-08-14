source("toolbox.R")

# read condor results into a list of dataframes
read_condor_results <- function(folder = "../condor",
                                pattern = "^res.") {
  datasets <- c("correlations_geo", "correlations_OWNW", "correlations_scale", "cpp_lattice", "cpp_wals", "main_analysis", "sigma_bias_family", "sigma_bias_genus")
  out <- vector("list", length(datasets))
  names(out) <- datasets
  for (d in datasets) {
    out[[d]] <- cat_csv(folder=paste0(folder, "/", d), pattern=pattern)
  }
  out
}

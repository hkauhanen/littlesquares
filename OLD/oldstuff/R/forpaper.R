source("stats.R")

stats_for_appendix <- function(res = final_results,
                               lim = 150) {
  cat("Median data subset tau correlation (section 4.1): ")
  cat(median(compute_tau_correlations_by_rep(res$correlations_geo)$spearman))
  cat("\n")

  cat("Mean data subset tau correlation (section 4.1): ")
  cat(mean(compute_tau_correlations_by_rep(res$correlations_geo)$spearman))
  cat("\n")

  cat("Max data subset tau correlation (section 4.1): ")
  cat(max(compute_tau_correlations_by_rep(res$correlations_geo)$spearman))
  cat("\n")

  cat("Min data subset tau correlation (section 4.1): ")
  cat(min(compute_tau_correlations_by_rep(res$correlations_geo)$spearman))
  cat("\n")

  cat("OW-NW tau correlation (section 4.2): ")
  only150 <- res$correlations_OWNW[res$correlations_OWNW$sample_size.x >= lim &
                                   res$correlations_OWNW$sample_size.y >= lim, ]
  cat(unique(compute_tau_correlations_by_rep(only150)$spearman))
  cat("\n")
}

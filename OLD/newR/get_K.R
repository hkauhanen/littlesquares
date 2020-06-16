# get_K()
#
# Return our version of the complete elliptic integral of the first kind,
# computed via the arithmetic-geometric mean.
#
get_K <- function(k) {
  (pi/2)/pracma::agmean(1, sqrt(1 - k^2))$agm
}


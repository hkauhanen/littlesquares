# Helper functions


# Return element in vector that is closest (in terms of Euclidean distance)
# to the target value.
#
# NB. Whether this element is unique is not checked for!

closest_in_vector <- function(x,
                              vec) {
  df <- data.frame(vec=vec, dist=(vec - x)^2)
  df <- df[order(df$dist, decreasing=FALSE), ]
  df[1, ]$vec
}


# Return the index of the element in vector that is closest (in terms of 
# Euclidean distance) to the target value.
#
# NB. Whether this element is unique is not checked for!

index_of_closest_in_vector <- function(x,
                                       vec) {
  df <- data.frame(index=1:length(vec), dist=(vec - x)^2)
  df <- df[order(df$dist, decreasing=FALSE), ]
  df[1, ]$index
}

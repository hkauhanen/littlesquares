filter_gen_units <- function(data,
                             wals = ritwals::WALS,
                             column = 1,
                             lim) {
  numbers <- table(wals[[names(data)[column]]])
  data$N <- NA
  for (i in 1:nrow(data)) {
    data[i,]$N <- numbers[[data[i, column]]]
  }
  data[data$N >= lim, ]
}

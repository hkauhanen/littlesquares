# compute "sigma" for genealogical units (families, genera)

sigmaunits <- function(data = ritwals::WALS,
                       unit = "family",
                       k,
                       verbose = FALSE) {
  # pare down dataset to the essentials
  data <- data[, c("language_ID", "language", "family", "genus", "latitude", "longitude")]
  data <- data[!duplicated(data), ]

  # get list of units
  units <- unique(data[[unit]])

  # outframe
  out <- data.frame(x=units, sigma=NA)
  names(out)[1] <- unit

  # calculate sigma for each unit
  ctr <- 0
  for (unitvalue in units) {
    if (verbose) {
      ctr <- ctr + 1
      cat(paste0("Doing unit ", ctr, " out of ", length(units), "\n"))
    }
    reactive_interfaces <- 0
    total <- 0
    lges_in_this_unit <- unique(data[data[[unit]] == unitvalue, ]$language_ID)
    for (lge in lges_in_this_unit) {
      neighbours <- ritwals::nearest_geo_neighbours(id=lge, k=k, data=data)$language_ID
      for (neighbour in neighbours) {
        total <- total + 1
        if (data[data$language_ID==lge, ][[unit]] != data[data$language_ID==neighbour, ][[unit]]) {
          reactive_interfaces <- reactive_interfaces + 1
        }
      }
    }
    out[out[[unit]] == unitvalue, ]$sigma <- reactive_interfaces/total
  }

  # return
  out
}

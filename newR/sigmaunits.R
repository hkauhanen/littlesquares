# compute "sigma" for genealogical units (families, genera)

sigmaunits <- function(data = ritwals::WALS,
                       unit = "family",
                       k,
                       cutoff = 10,
                       neihash = read.csv("../newR/wals-distances-100closest-nosign.csv"),
                       verbose = FALSE) {
  # pare down dataset to the essentials
  data <- data[data$genus != "Sign Languages", ]
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
      cat(paste0("Doing unit ", ctr, " out of ", length(units), " (", unitvalue, ")", "\n"))
    }
    reactive_interfaces <- 0
    total <- 0
    lges_in_this_unit <- unique(data[data[[unit]] == unitvalue, ]$language_ID)
    if (verbose) {
      cat(paste0("  Languages in this unit: ", length(lges_in_this_unit), "\n"))
    }
    if (length(lges_in_this_unit) >= cutoff) {
    for (lge in lges_in_this_unit) {
      #neighbours <- ritwals::nearest_geo_neighbours(id=lge, k=k, data=data)$language_ID
      neighbours <- neihash[neihash$language_ID==lge & neihash$rank <= k, ]$neighbour_ID
      for (neighbour in neighbours) {
        total <- total + 1
        if (data[data$language_ID==lge, ][[unit]] != data[data$language_ID==neighbour, ][[unit]]) {
          reactive_interfaces <- reactive_interfaces + 1
        }
      }
    }
    out[out[[unit]] == unitvalue, ]$sigma <- reactive_interfaces/total
    if (verbose) {
      cat(paste0("  Sigma found to be: ", reactive_interfaces/total, "\n"))
    }
    } else {
    out[out[[unit]] == unitvalue, ]$sigma <- NA
    if (verbose) {
      cat(paste0("  Below cutoff, sigma NOT calculated", "\n"))
    }
    }
  }

  # return
  out$units <- unit
  out$units <- ifelse(out$units=="family", "families", "genera")
  names(out)[1] <- "unitname"
  out[!is.na(out$sigma), ]
}

# figure out how many neighbours within a certain km radius
#
howmanyneighbours <- function(df,
                              km_limit) {
  all_lges <- unique(df$language_ID)
  df <- df[df$distance <= km_limit, ]
  lges <- unique(df$language_ID)
  out <- data.frame(language_ID=lges, neighbours=NA)
  pb <- txtProgressBar(style=3, min=0, max=length(lges))
  ctr <- 0
  for (lge in lges) {
    ctr <- ctr + 1
    thisdf <- df[df$language_ID==lge, ]
    out[out$language_ID==lge, ]$neighbours <- max(thisdf$rank)
    setTxtProgressBar(pb, ctr)
  }
  close(pb)

  zeroes <- setdiff(all_lges, lges)
  out <- rbind(out, data.frame(language_ID=zeroes, neighbours=0))
  out
}


# turn nearest neighbours dataframe into format understood by cpp routine
#
NN2cpp <- function(df,
                   howmany = 10) {
  df <- df[df$rank <= howmany, ]
  out <- data.frame(language_ID=unique(df$language_ID), num_ID=NA, neighbours=NA)
  out$num_ID <- 1:nrow(out)
  for (i in 1:nrow(out)) {
    id <- out[i,]$language_ID
    neis <- df[df$language_ID==id, ]$neighbour_ID
    neis <- which(out$language_ID %in% neis)
    out[i,]$neighbours <- paste(neis, collapse=":")
  }
  out
}

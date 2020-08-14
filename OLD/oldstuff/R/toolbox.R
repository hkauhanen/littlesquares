# toolbox.R
#
# Henri Kauhanen 2018
#
# Universally useful data rotating and other tools.



# cat_csv()
#
# rbind several csv files into one dataframe.
#
cat_csv <- function(folder,
                    pattern) {
  tmpfile <- tempfile()
  files <- list.files(folder, pattern=pattern, full.names=TRUE)
  if (length(files) == 1) {
#    warning("Only one file in folder - nothing to concatenate!")
    return(read.csv(files[[1]]))
  }
  cat("Concatenating...\n")
  pb = txtProgressBar(min = 0, max = length(files), initial = 0, style=3)
  system(paste("cp", files[[1]], tmpfile))
  setTxtProgressBar(pb,1)
  for (i in 2:length(files)) {
    system(paste("sed '1d'", files[[i]], ">>", tmpfile))
    setTxtProgressBar(pb,i)
  }
  cat("\nReading temporary file into data frame...")
  out <- read.csv(tmpfile)
  cat(" done!\nData frame created: ")
  cat(paste0(nrow(out), " rows, ", ncol(out), " columns.\n"))
  out
}



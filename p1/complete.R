complete <- function(directory, id = 1:332) {
  nobs_value <- numeric(0)
  for (index in id) {
    path <- paste(directory,"/", sprintf('%03d',index), ".csv", sep="")
    pollutant_data <- read.csv(path, sep=",")
    nobs_value <- c(nobs_value, nrow(na.omit(pollutant_data)))
  }
  data.frame(id = id, nobs = nobs_value)
}
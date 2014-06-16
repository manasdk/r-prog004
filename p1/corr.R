corr <- function(directory, threshold = 0) {
  correlations <- numeric(0)
  for (index in 1:332) {
    path <- paste(directory,"/", sprintf('%03d',index), ".csv", sep="")
    pollutant_data <- na.omit(read.csv(path, sep=","))
    if (nrow(pollutant_data) >= threshold) {
      correlations <- c(correlations, 
                        cor(pollutant_data["sulfate"],
                            pollutant_data["nitrate"]))
    }
  }
  correlations
}
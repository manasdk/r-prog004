pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutant_data <- data.frame()
  for (index in id) {
    path <- paste(directory,"/", sprintf('%03d',index), ".csv", sep="")
    pollutant_data <- rbind(pollutant_data, read.csv(path, sep=","))
  }
  pollutant_mean = mean(pollutant_data[,pollutant],na.rm=TRUE)
  print(round(pollutant_mean, digits=3))
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
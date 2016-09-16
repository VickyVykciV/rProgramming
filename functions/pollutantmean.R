pollutantmean <- function(directory, pollutant, id = 1:332){  
  setwd(directory);
  files <- list.files(directory)
  selectedFiles <- file.path(files[id])
  dataList <- lapply(selectedFiles, read.csv)
  polData <- do.call(rbind.data.frame, dataList)
  mean <- mean(polData[,pollutant], na.rm = T)
  return(mean)  
}
corr <- function(directory, threshold = 0){
  setwd(directory)
  files <- list.files(directory)
  corrVector <- as.numeric()
  
  for (i in 1:332){
    selectedFile <- file.path(files[i])
    polData <- read.csv(selectedFile, header = T)
    sumCompleteData <- sum(!is.na(polData$sulfate) & !is.na(polData$nitrate))
    if(sumCompleteData > threshold){
      polData <- polData[complete.cases(polData),]
      corrVal <- cor(polData$sulfate, polData$nitrate)
      corrVector <- c(corrVector, corrVal)
    }
  }
  return(corrVector)
}
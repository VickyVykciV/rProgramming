complete <- function(directory, id = 1:332){
  setwd(directory);
  files <- list.files(directory)
  casesPerId <- data.frame()
  
  for(i in id){
    selectedFile <- file.path(files[i])
    polData <- read.csv(selectedFile, header = T)
    polData <- polData[complete.cases(polData),]
    numCases <- nrow(polData)
    currentFileCount <- cbind(i, numCases)
    casesPerId <- rbind(casesPerId, currentFileCount)
  }
  
  colnames(casesPerId) <- c("id", "nobs")
  return(casesPerId)
}
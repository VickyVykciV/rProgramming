complete <- function(directory, id){
  setwd(directory);
  files <- list.files(directory)
  casesPerId <- data.frame()
  
  for(i in id){
    selectedFile <- file.path(files[i])
    polData <- read.csv(selectedFile, header = T)
    numCases <- nrow(polData)
    currentFileCount <- cbind(i, numCases)
    casesPerId <- rbind(casesPerId, currentFileCount)
  }
  
  colnames(casesPerId) <- c("id", "nobs")
  return(casesPerId)
}
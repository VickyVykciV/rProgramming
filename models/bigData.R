#Installing and Adding libraries
setwd(dir = "C:/Users/vignesh.venugopal/Documents/GitHub/rProgramming/dataFiles/")
#source('https://bioconductor.org/biocLite.R')
#biocLite('rhdf5')
library(rhdf5)
library(sparklyr)
library(dplyr)
library(moments)
library(corrplot)
#source("http://goo.gl/UUyEzD")

## Reading the h5 file
completeData <- h5read(file="C:/Users/vignesh.venugopal/Documents/GitHub/rProgramming/dataFiles/train.h5", name="train")
columns <- completeData$axis0
completeData <- data.frame(as.double(completeData$block0_values[1,]), as.double(completeData$block0_values[2,]), t(completeData$block1_values))
colnames(completeData) <- columns
completeData <- completeData[, -c(1, 2)]
rm(columns)
H5close()
completeY <- data.frame(completeData[, ncol(completeData)])
colnames(completeY) <- 'y'
completeData <- completeData[-ncol(completeData)]

## Numeric Summmary

numericSummaryDF <- data.frame(matrix(0, ncol = 20, nrow = ncol(completeData)))
colnames(numericSummaryDF) <- c('Feature', 'Length', 'MissingValues', 'ZeroValues', 'UniquePCT', 'Mean', 'STD', 'X0',
                                'X10', 'X20', 'X30', 'X40', 'X50', 'X60', 'X70', 'X80', 'X90', 'X95', 'X99', 'X100')

NumericSummary <- function(){
  for (i in 1:ncol(completeData)){
    x <- completeData[, i]
    numericSummaryDF[i, ] <- data.frame(colnames(completeData)[i],
                               length(x),
                               sum(is.na(x)),
                               sum(x == 0,na.rm = T),
                               length(unique(x))/length(x),
                               mean(x, na.rm = T),
                               sd(x, na.rm = T),
                               1,0,0,0,0,0,0,0,0,0,0,0,0)
  }
}

NumericSummary()

## Calculating NA/NaNs percentage in each variable

NAperc <- data.frame(colMeans(is.na(completeData))*100)
NAperc$Var<-rownames(NAperc)
colnames(NAperc)[1] <- 'Percentage'
validVariableList <- NAperc%>%filter(Percentage < 15)%>%select(Var)

rm(NAperc)

## Removing variables containing NA/NaNs greater than 15%

validVariableList <- as.vector(validVariableList$Var)
trimmedData <- completeData[, colnames(completeData) %in% validVariableList]

rm(completeData)


## Outlier Removal

for (i in 1:ncol(trimmedData)){
  outlier <- boxplot.stats(eval(trimmedData[,i]))$out
  var_data <- ifelse(trimmedData[,i] %in% outlier, NA, trimmedData[,i])
  trimmedData[,i] <- var_data
}

rm(i, outlier, var_data)

imputedData <- trimmedData

## Replacing NAs with column means

for(i in 1:ncol(imputedData)){
  imputedData[is.na(imputedData[,i]), i] <- mean(imputedData[,i], na.rm = TRUE)
}

rm(i)

## Checking for change in Skewness and Kurtosis of data after imputation

numberOfVaraibles <- length(validVariableList)
compareDist <- data.frame(matrix(0, ncol = 5, nrow = numberOfVaraibles))
colnames(compareDist) <- c('VariableName', 'SkwenessBeforeImputation', 'SkwenessAfterImputation', 'KurtosisBeforeImputation', 'KurtosisAfterImputation')

for(i in 1:numberOfVaraibles){
  compareDist$VariableName[i] <- colnames(imputedData)[i]
  compareDist$SkwenessBeforeImputation[i] <- skewness(trimmedData[, i], na.rm = T)
  compareDist$SkwenessAfterImputation[i] <- skewness(imputedData[, i])
  compareDist$KurtosisBeforeImputation[i] <- kurtosis(trimmedData[, i], na.rm = T)
  compareDist$KurtosisAfterImputation[i] <- kurtosis(imputedData[, i])
}

rm(i, trimmedData , numberOfVaraibles, validVariableList)

## Merging Dependent and Independent variables before splitting

imputedData <- cbind.data.frame(imputedData, completeY)

rm(completeY)

## Correlation Matrix

corrplot(cor(imputedData), method = "circle")

## Splitting data into train and test

smp_size <- floor(0.70 * nrow(imputedData))
set.seed(1)
train_ind <- sample(seq_len(nrow(imputedData)), size = smp_size)
trainData <- imputedData[train_ind, ]
testData <- imputedData[-train_ind, ]

rm(imputedData, smp_size, train_ind)



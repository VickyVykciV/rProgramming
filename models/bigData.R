#Installing and Adding libraries
setwd(dir = "C:/Users/vignesh.venugopal/Documents/GitHub/rProgramming/")
#source('https://bioconductor.org/biocLite.R')
#biocLite('rhdf5')
library(rhdf5)
library(sparklyr)
library(dplyr)
library(moments)
library(corrplot)
library(Boruta)
#source("http://goo.gl/UUyEzD")
options(scipen=999)

## Reading the h5 file
completeData <- h5read(file="dataFiles/train.h5", name="train")
columns <- completeData$axis0
completeData <- data.frame(as.double(completeData$block0_values[1,]), as.double(completeData$block0_values[2,]), t(completeData$block1_values))
colnames(completeData) <- columns
completeData <- completeData[, -c(1, 2)]
rm(columns)
H5close()
completeY <- data.frame(completeData[, ncol(completeData)])
colnames(completeY) <- 'y'
completeData <- completeData[-ncol(completeData)]

## Removing technical_16 variable, as it is having all Zeros

completeData <- completeData[, -which(names(completeData) %in% c('technical_16'))]

## Numeric Summmary

numericSummaryDF <- data.frame(matrix(0, ncol = 19, nrow = ncol(completeData)))
colnames(numericSummaryDF) <- c('Length', 'MissingValues', 'ZeroValues', 'UniquePCT', 'Mean', 'STD', 'X0',
                                'X10', 'X20', 'X30', 'X40', 'X50', 'X60', 'X70', 'X80', 'X90', 'X95', 'X99', 'X100')
rownames(numericSummaryDF) <- colnames(completeData)

fn_numericSummary <- function(var,datafile){
  x <- datafile[,var]
  return(data.frame(length = length(x),
                    MissingValues = sum(is.na(x)),
                    ZeroValues = sum(x==0,na.rm=T),
                    UniquePCT = length(unique(x))/length(x),
                    Mean = mean(x,na.rm=T),
                    STD = sd(x,na.rm=T),
                    data.frame(t(quantile(x,p=sort(c(seq(0,1,.1),.95,.99)), na.rm = T)))
  ))
}

for(i in 1:ncol(completeData)){
  numericSummaryDF[i,] <- fn_numericSummary(colnames(completeData)[i], completeData)[1, ]
}

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
compareDist <- data.frame(matrix(0, ncol = 4, nrow = numberOfVaraibles))
colnames(compareDist) <- c('SkwenessBeforeImputation', 'SkwenessAfterImputation', 'KurtosisBeforeImputation', 'KurtosisAfterImputation')
rownames(compareDist) <- colnames(trimmedData)

for(i in 1:numberOfVaraibles){
  compareDist$SkwenessBeforeImputation[i] <- skewness(trimmedData[, i], na.rm = T)
  compareDist$SkwenessAfterImputation[i] <- skewness(imputedData[, i])
  compareDist$KurtosisBeforeImputation[i] <- kurtosis(trimmedData[, i], na.rm = T)
  compareDist$KurtosisAfterImputation[i] <- kurtosis(imputedData[, i])
}

rm(i, trimmedData , numberOfVaraibles, validVariableList)

## Correlation Matrix, between all Dependent Variable and each Independent Variable

correlationDF <- data.frame(matrix(0, ncol = 1, nrow = ncol(imputedData)))
colnames(correlationDF) <- 'Corrleation Value'
rownames(correlationDF) <- colnames(imputedData)

for (i in 1:ncol(imputedData)){
  correlationDF[i,1] <- as.numeric(cor(imputedData[, i], completeY))
}

rm(i)

## Merging Dependent and Independent variables before splitting

imputedData <- cbind.data.frame(imputedData, completeY)

rm(completeY)

## Splitting data into train and test

smp_size <- floor(0.70 * nrow(imputedData))
set.seed(1)
train_ind <- sample(seq_len(nrow(imputedData)), size = smp_size)
trainData <- imputedData[train_ind, ]
testData <- imputedData[-train_ind, ]

rm(imputedData, smp_size, train_ind)

## Starting a Spark Session

config <- spark_config()
config$spark.local.dir <- 'C:/Users/vignesh.venugopal/Documents/GitHub/rProgramming/models'
sc <- spark_connect(master = "local", config = config)

## Copying the Train and Test data to Spark

testDF <- copy_to(sc, testData)
trainDF <- copy_to(sc, trainData)

## Linear - Model

independentVar <- colnames(trainData[, -ncol(trainData)])
fit <- trainDF%>%ml_linear_regression(response = 'y', features = independentVar)
summary(fit)

# fit1 <- lm(y~., data = trainData)
# summary(fit1)
# boruta.train <- Boruta(y~., data = trainData, doTrace = 2)

rfModel <- ml_random_forest(trainDF, response = 'y', features = independentVar)
summary(rfModel)

xgboostModel <- ml_gradient_boosted_trees(trainDF, response = 'y', features = independentVar, type = 'regression')
summary(xgboostModel)

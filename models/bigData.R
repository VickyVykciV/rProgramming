#Installing and Adding libraries
setwd(dir = "C:/Users/vignesh.venugopal/Documents/GitHub/rProgramming/dataFiles/")
#source('https://bioconductor.org/biocLite.R')
#biocLite('rhdf5')
library(rhdf5)
library(sparklyr)
library(dplyr)

# Reading the h5 file
data <- h5read(file="C:/Users/vignesh.venugopal/Documents/GitHub/rProgramming/dataFiles/train.h5", name="train")
columns <- data$axis0
data <- data.frame(as.double(data$block0_values[1,]), as.double(data$block0_values[2,]), t(data$block1_values))
colnames(data) <- columns
data <- data[, -c(1, 2)]
rm(columns)
H5close()

## Calculating NA percentage in each variable

NAperc <- data.frame(colMeans(is.na(data))*100)
NAperc$Var<-rownames(NAperc)
colnames(NAperc)[1] <- 'Percentage'
validVariableList <- NAperc%>%filter(Percentage < 15)%>%select(Var)

rm(NAperc)

## Removing variables containing NAs greater than 15%

validVariableList <- as.vector(validVariableList$Var)
data <- data[, colnames(data) %in% validVariableList]

rm(validVariableList)

## Replacing NAs with column means

for(i in 1:ncol(data)){
  data[is.nan(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

rm(i)

## Splitting data into train and test

smp_size <- floor(0.70 * nrow(data))
set.seed(1)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
trainData <- data[train_ind, ]
testData <- data[-train_ind, ]

rm(data, smp_size, train_ind)

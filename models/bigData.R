#Reading the data file train.csv
library(data.table)
data <- read.csv('train.csv', header = T)
#Removing the 1st column, which is created while converting h5 to excel
traindata <- data[, -1]
#Copying the data to Spark Connect
traindata_tbl <- copy_to(sc, traindata)

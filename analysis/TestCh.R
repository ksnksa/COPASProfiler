library(dplyr)
library(tidyverse)
source(paste(getwd(),'/Rsrc/ReadDataFunc.R',sep=''))
Idmatrix <- ReadData(paste(getwd(),'/data/TestingReading',sep=''))
completematrix <- read.csv((paste(getwd(),'/data/TestN2.csv',sep='')), header=FALSE)
# For loop that appends values of the Id and corresponding mean to IdMean
IdMean <- data.frame('Id' = double(), 'Mean' = double())
for (Id in Idmatrix[,1]) { 
  columnnum <- which(completematrix[1,]==Id)
  #sometimes in the data set there's more values after the first 0 (then we can probably find a way to distinguish that too)
  rownum <- which.min(completematrix[, columnnum]==0) #changed to which max (which min always gives off 1 but which max)
  rownum <- rownum - 1 
  temp <- completematrix[2:rownum, columnnum]
  temp_mean <- mean(temp)
  temp_df <- data.frame('Id' = as.numeric(Id), 'Mean' = temp_mean)
  IdMean <- rbind(IdMean,temp_df)
}
#plotting
ggplot(data = IdMean, aes(x = Id, y = Mean)) + geom_point()

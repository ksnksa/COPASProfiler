library(e1071)
library(ggplot2)
library(reshape)
library(dplyr)
Ch0D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch0_prf.txt',sep=''))
Ch1D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch1_prf.txt',sep=''))
Ch2D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch2_prf.txt',sep=''))
Ch3D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch3_prf.txt',sep=''))
source((paste(getwd(),'/Rsrc','/','ReadChannel.R',sep='')))
channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)
#Still using the original filtering before the clustering 
source((paste(getwd(),'/Rsrc','/','FilterChannel.R',sep='')))
Index <- FilterChannel(channellist[[1]],64000,40,500)
channellist[[1]] <- channellist[[1]][-c(Index),]
#Assigning the stages
TOF <- matrix(,nrow=length(channellist[[1]][,1]),ncol = 2)
for (x in 1:length(channellist[[1]][,1])) {
  TOF[x,1] <- which.min(channellist[[1]][x,])
  if (as.numeric(TOF[x,1]) < 60 ) {TOF[x,2] <- "NA"} # I wasn't sure what to call things less than L1
  else if (as.numeric(TOF[x,1]) >= 60 & as.numeric(TOF[x,1]) < 90 ) {TOF[x,2] <- "L1"} 
  else if (as.numeric(TOF[x,1]) >= 90 & as.numeric(TOF[x,1]) < 200 ) {TOF[x,2] <- "L2/L3"} 
  else if (as.numeric(TOF[x,1]) >= 200 & as.numeric(TOF[x,1]) < 300 ) {TOF[x,2] <- "L4"} 
  else if (as.numeric(TOF[x,1]) >= 300) {TOF[x,2] <- "Adult"}
}
L1 <- channellist[[1]][which(TOF[,2]=='L1'),] 
L23 <- channellist[[1]][which(TOF[,2]=='L2/L3'),]
L4 <- channellist[[1]][which(TOF[,2]=='L4'),]
Adult <- channellist[[1]][which(TOF[,2]=='Adult'),]
#Finding the "good" worms in Adult
##Loading up the good IDs and creating the training set
GoodIDs <- read.delim(paste(getwd(),'/data/N2','/GoodIDsN2.csv',sep=''), sep=',',header=TRUE) 
GoodIndex <- as.matrix(GoodIDs[2:length(GoodIDs)])
GoodIndex <- matrix(GoodIndex, ncol = ncol(GoodIndex), dimnames = NULL)
#For loop that randomly picks a specific number of good worms (from the training set) to try and predict the remaining worms
AdultGoodIndex <- matrix(GoodIndex[1,which(GoodIndex[1,] %in% rownames(Adult))]) 
BadWorms <- Adult[-which(rownames(Adult) %in% AdultGoodIndex),]
Accuracy <- data.frame()
NumberOfRuns <- 3
for (l in 1:(length(AdultGoodIndex)-1)) {
  
  for (o in 1:NumberOfRuns) {
    #Change the size to change how many are picked
    SampleSize <- l
    RandomIndex <- sample(c(1:length(AdultGoodIndex)), size=SampleSize, replace = FALSE)
    RandomIndex <- sort(RandomIndex)
    ComplementVec <- 1:length(AdultGoodIndex)
    ComplementVec <- ComplementVec[-RandomIndex]
    RandomIndex <- AdultGoodIndex[RandomIndex]
    PredIndex <- AdultGoodIndex[ComplementVec]
    IndexMax <- max( as.numeric(sub('X','',RandomIndex)))
    PredBadWormsIndex <- sample(c(1:nrow(BadWorms)),size = (2*length(ComplementVec)), replace = FALSE)
    PredBadWormsIndex <- sort(PredBadWormsIndex)
    PredictionSet <- as.data.frame(rbind(BadWorms[PredBadWormsIndex,],Adult[as.numeric(sub('X','',which(rownames(Adult) %in% PredIndex))),]))
    #Matrix with randomly picked worms 
    TrainingSet <- as.data.frame(rbind(BadWorms[-PredBadWormsIndex,]),Adult[as.numeric(sub('X','',which(rownames(Adult) %in% RandomIndex))),])
    #training_Adult <- Adult[1:(which(as.numeric(sub('X','',rownames(Adult))) >= IndexMax)[1]),] #Creating a dataset that has all the worms up to the highest index of the good worms 
    #All the good worms 
    #FullTrainingSet <- Adult[1:(which(as.numeric(sub('X','',rownames(Adult))) >= max(as.numeric(sub('X','',GoodIndex))))[1]),] #Creating a prediction table with all the worms 
    #Creating the truth table 
    #training_Adult <- as.data.frame(training_Adult)
    TrainingSet$Factor <- as.numeric(1) #creating a true false table
    TrainingSet$Factor[(nrow(TrainingSet)-(SampleSize - 1)):nrow(TrainingSet)] <- as.numeric(2) #changing it to true for the good worms (true being 2 and false is 1)
    x <- lapply(TrainingSet[,1:1124],as.numeric) #it's up to 1124 (becease we added one column factor, so we take it off by doing this to make training set same size as prediction set?)
    m <- as.data.frame(x)
    y <- TrainingSet$Factor #make y variable(dependent)
    model <- svm(m, y,type='C-classification',
                 scale=TRUE,
                 kernel="polynomial")
    pred <- predict(model, PredictionSet)
    #Sanity check, are all the training data in the predection?
    #PredLength <- length(which(pred == 2)) 
    #CorrectPredictionPercent <- 1 - (length(ComplementVec) - PredLength)/length(ComplementVec)
    #PredictionMean <- (PredictionMean + CorrectPredictionPercent) /2
    Positive <- rownames(PredictionSet[which(pred==2),])
    TP <- sum((Positive %in% GoodIDs), na.rm = TRUE)
    Negative <- rownames(PredictionSet[which(pred==1),])
    TN <- length(Negative %in% GoodIDs) - sum((Negative %in% GoodIDs), na.rm = TRUE)
    Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
  }
}
y<-as.numeric(Accuracy[1,])
x<-1:length(y)
df<-data.frame(x=x,y=y)
df_molten=melt(df,id.vars="x")
ggplot(df_molten) + geom_line(aes(x=x,y=y,color=variable))
geom_line( color="grey") +
  theme_minimal() +
  ylab("Accuracy %") + xlab("Run number") + 
  ggtitle('Accuracy')


    #print(PredictionMean)

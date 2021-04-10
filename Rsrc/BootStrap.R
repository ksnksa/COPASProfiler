library(e1071)
library(ggplot2)
library(reshape)
library(dplyr)

library(cowplot)
StartTime <- Sys.time()
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
BadWorms <- BadWorms[1:which(rownames(BadWorms) == 'X365'),]
Accuracy <- data.frame()
NumberOfRuns <- 100
SetNumber <-  c(51, 40, 25, 15, 5, 1)
BadWormSampleSize <- 51
for (l in 1:length(SetNumber)) {
  
  for (o in 1:NumberOfRuns) {
    #Change the size to change how many are picked
    SampleSize <- SetNumber[l]
    RandomIndex <- sample(c(1:length(AdultGoodIndex)), size=SampleSize, replace = FALSE)
    RandomIndex <- sort(RandomIndex)
    ComplementVec <- 1:length(AdultGoodIndex)
    ComplementVec <- ComplementVec[-RandomIndex]
    RandomIndex <- AdultGoodIndex[RandomIndex]
    PredIndex <- AdultGoodIndex[ComplementVec]
    IndexMax <- max( as.numeric(sub('X','',RandomIndex)))
    PredBadWormsIndex <- sample(c(1:nrow(BadWorms)),size = BadWormSampleSize, replace = FALSE)
    PredBadWormsIndex <- sort(PredBadWormsIndex)
    PredictionSet <- as.data.frame(rbind(BadWorms[-PredBadWormsIndex,],Adult[as.numeric(sub('X','',which(rownames(Adult) %in% PredIndex))),]))

    TrainingSet <- as.data.frame(rbind(BadWorms[PredBadWormsIndex,],Adult[as.numeric(sub('X','',which(rownames(Adult) %in% RandomIndex))),]))
    
    TrainingSet$Factor <- as.numeric(1) #creating a true false table
    TrainingSet$Factor[(dim(BadWorms[PredBadWormsIndex,])[1]+1):nrow(TrainingSet)] <- as.numeric(2) 
    x <- lapply(TrainingSet[,1:(ncol(TrainingSet)-1)],as.numeric) 
    m <- as.data.frame(x)
    y <- TrainingSet$Factor #make y variable(dependent)
    model <- svm(m, y,type='C-classification',
                 scale=TRUE,
                 kernel="polynomial")
    pred <- predict(model, PredictionSet)
    #Calculating Accuracy 
    Positive <- rownames(PredictionSet[which(pred==2),])
    TP <- sum((Positive %in% GoodIDs), na.rm = TRUE)
    Negative <- rownames(PredictionSet[which(pred==1),])
    TN <- length(Negative %in% GoodIDs) - sum((Negative %in% GoodIDs), na.rm = TRUE)
    Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
  }
}
p <- list()
for (l in 1:length(SetNumber)) {
  y<-as.numeric(Accuracy[l,])
  AccuracyMean <- mean(y)
  x<-1:length(y)
  df<-data.frame(x=x,y=y)
  df_molten=melt(df,id.vars="x")
 p[[l]] <-ggplot(df_molten) + geom_line(aes(x=x,y=y,color=variable)) +
    theme_minimal() + theme(plot.title = element_text(size=7)) + 
    ylab("Accuracy %") + xlab("Run number") +
    ggtitle(paste('Accuracy with ',SetNumber[l],' Samples out of ',length(AdultGoodIndex),' W/ mean of ',format(round(AccuracyMean, 2), nsmall = 2), sep = ''))

}
png(paste(BadWormSampleSize,'.png',sep=''))
plot_grid(p[[1]], p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],nrow = 3,ncol = 2, labels = "AUTO")

dev.off()
EndTime <- Sys.time()

EndTime - StartTime

library(e1071)
library(ggplot2)
#library(reshape)
#library(dplyr)

#library(cowplot)
#TRY TO NORMALIZE AND COMPARE ACCURACY
StartTime <- Sys.time()
Ch0D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch0_prf.txt',sep=''))
Ch1D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch1_prf.txt',sep=''))
Ch2D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch2_prf.txt',sep=''))
Ch3D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch3_prf.txt',sep=''))
#Ch0 <- read.delim(Ch0D1, header=TRUE)
#Ch1 <- read.delim(Ch1D1, header=TRUE)
#Ch2 <- read.delim(Ch2Directory, header=TRUE)
#Ch3 <- read.delim(Ch3Directory, header=TRUE)
source((paste(getwd(),'/Rsrc','/','WormBootStrap.R',sep='')))
MaxTOF = 64000
MinLength = 40
MaxLength = 500
GoodIDD =paste(getwd(),'/data/N2','/GoodIDsNew.csv',sep='')
NumberOfRuns  = 25
NumberOfBadWorms  = 25
NumbersOfGoodWorms  = c(75,65,50, 45, 25, 15, 10, 5)
ChannelToCluster  = 4
WormStage  = 'Adult'
Plots <- WormBootStrap(Ch0D1,Ch1D1,Ch2D1,Ch3D1,MaxTOF1,MinLength1,MaxLength1,GoodIDD1,NumberOfRuns1,NumberOfBadWorms1, NumbersOfGoodWorms1,ChannelToCluster1,WormStage1)
#source((paste(getwd(),'/Rsrc','/','WormCount',sep='')))
#plots <- PlotAccuracy(Accuracy)

source((paste(getwd(),'/Rsrc','/','ReadChannel.R',sep='')))
channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)
#Still using the original filtering before the clustering
source((paste(getwd(),'/Rsrc','/','FilterChannel.R',sep='')))
Index <- FilterChannel(channellist[[1]],64000,40,500)
channellist[[1]] <- channellist[[1]][-c(Index),]
#Assigning the stages
#TOF <- matrix(,nrow=length(channellist[[1]][,1]),ncol = 2)
#for (x in 1:length(channellist[[1]][,1])) {
#  TOF[x,1] <- which.min(channellist[[1]][x,])
#  if (as.numeric(TOF[x,1]) < 60 ) {TOF[x,2] <- "NA"} # I wasn't sure what to call things less than L1
#  else if (as.numeric(TOF[x,1]) >= 60 & as.numeric(TOF[x,1]) < 90 ) {TOF[x,2] <- "L1"}
#  else if (as.numeric(TOF[x,1]) >= 90 & as.numeric(TOF[x,1]) < 200 ) {TOF[x,2] <- "L2/L3"}
#  else if (as.numeric(TOF[x,1]) >= 200 & as.numeric(TOF[x,1]) < 300 ) {TOF[x,2] <- "L4"}
#  else if (as.numeric(TOF[x,1]) >= 300) {TOF[x,2] <- "Adult"}
#}

#WormNumber <- unique(which(Adult[,1:80] > 20000, arr.ind = TRUE))
#WormNumber <- sort(unique(WormNumber[,1]))
#Adult <- Adult[-c(WormNumber),]
#Finding the "good" worms in Adult
##Loading up the good IDs and creating the training set
#GoodIDs <- read.delim(paste(getwd(),'/data/N2','/GoodIDsNew.csv',sep=''), sep=',',header=TRUE)
#GoodIndex <- as.matrix(GoodIDs[2:length(GoodIDs)])
#GoodIndex <- matrix(GoodIndex, ncol = ncol(GoodIndex), dimnames = NULL)
#For loop that randomly picks a specific number of good worms (from the training set) to try and predict the remaining worms
source((paste(getwd(),'/Rsrc','/','AssignStage.R',sep='')))
StageList <- AssignStage(channellist[[1]])
#min-max normalization
#NormalizedChannel <- matrix(nrow = dim(StageList[[4]])[1],ncol = dim(StageList[[4]])[2])

#
#for (x in 1:dim(StageList[[4]])[1]) {
#temp <- sapply(StageList[[4]][x,], function(m) ((m - as.numeric(min(StageList[[4]][x,]))) / (as.numeric(max(StageList[[4]][x,])) - as.numeric(min(StageList[[4]][x,])))))
#NormalizedChannel[x,] <- temp
#}
L1 <- StageList[[1]]
L23 <- StageList[[2]]
L4 <- StageList[[3]]
Adult <- StageList[[4]]
#Adult <- NormalizedChannel
#remove zeros in the corners
source((paste(getwd(),'/Rsrc','/','CreateTrainingSetIDs.R',sep='')))
AdultIDs <- CreateTrainingSetIDs(Adult,paste(getwd(),'/data/N2','/GoodIDsNew.csv',sep=''))
BadWorms <- Adult[which(rownames(Adult) %in% AdultIDs[[2]]),]
#BadWorms <- Adult[as.numeric(sub('X','',AdultIDs[[2]])),]
#StageList[[4]] <- NormalizedChannel

#AdultIDs <- CreateTrainingSet(StageList[[4]],paste(getwd(),'/data/N2','/GoodIDsNew.csv',sep=''))


#AdultGoodIndex <- matrix(GoodIndex[1,which(GoodIndex[1,] %in% rownames(Adult))])

#BadWorms <- BadWorms[1:which(rownames(BadWorms) == 'X495'),]
BadWorms <- BadWorms[1:which(rownames(BadWorms) == AdultIDs[[2]][dim(AdultIDs[[2]])[1],]),]
source((paste(getwd(),'/Rsrc','/','RandomTrainingSet.R',sep='')))


source((paste(getwd(),'/Rsrc','/','GetPrediction.R',sep='')))

for (x in 1:15) {
  SetList<- RandomTrainingSet(Adult,AdultIDs,15,15)
  Pred <- GetPrediction(SetList)
  Positive <- rownames(SetList[[2]][which(pred==2),])
  TP <- sum((Positive %in% AdultIDs[[1]]), na.rm = TRUE)
  Negative <- rownames(SetList[[2]][which(pred==1),])
  TN <- length(Negative %in% AdultIDs[[2]]) - sum((Negative %in% AdultIDs[[1]]), na.rm = TRUE)
  Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
}


source((paste(getwd(),'/Rsrc','/','PlotAccuracy.R',sep='')))
p <- PlotAccuracy(Accuracy)

Accuracy <- data.frame()
NumberOfRuns <- 25
SetNumber <-  c(75,65,50, 45, 25, 15, 10, 5)
#SetNumber <-  c(50, 45, 25, 15, 10, 5)
BadWormSampleSize <- 45
for (l in 1:length(SetNumber)) {

  for (o in 1:NumberOfRuns) {
    #Change the size to change how many are picked
    SampleSize <- SetNumber[l]
    RandomIndex <- sample(c(1:length(AdultIDs[[1]])), size=SampleSize, replace = FALSE)
    RandomIndex <- sort(RandomIndex)
    ComplementVec <- 1:length(AdultIDs[[1]])
    ComplementVec <- ComplementVec[-RandomIndex]
    RandomIndex <- AdultIDs[[1]][RandomIndex]
    PredIndex <- as.matrix(AdultIDs[[1]][ComplementVec])
    IndexMax <- max( as.numeric(sub('X','',RandomIndex)))
    PredBadWormsIndex <- sample(c(1:nrow(BadWorms)),size = BadWormSampleSize, replace = FALSE)
    PredBadWormsIndex <- sort(PredBadWormsIndex)
    PredictionSet <- as.data.frame(rbind(BadWorms[-PredBadWormsIndex,],Adult[as.numeric(as.numeric(which(rownames(Adult) %in% PredIndex))),]))


    TrainingSet <- as.data.frame(rbind(BadWorms[PredBadWormsIndex,],Adult[as.numeric(which(rownames(Adult) %in% RandomIndex)),]))

    TrainingSet$Factor <- as.numeric(1) #creating a true false table
    TrainingSet$Factor[(dim(BadWorms[PredBadWormsIndex,])[1]+1):nrow(TrainingSet)] <- as.numeric(2)
    x <- lapply(TrainingSet[,1:(ncol(TrainingSet)-1)],as.numeric)
    m <- as.data.frame(x)
    y <- TrainingSet$Factor #make y variable(dependent)
    model <- svm(m, y,type='C-classification',
                 scale=TRUE,
                 kernel="linear")
    pred <- predict(model, PredictionSet)
    #Calculating Accuracy
    Positive <- rownames(PredictionSet[which(pred==2),])
    TP <- sum((Positive %in% AdultIDs[[1]]), na.rm = TRUE)
    Negative <- rownames(PredictionSet[which(pred==1),])
    TN <- length(Negative %in% AdultIDs[[2]]) - sum((Negative %in% AdultIDs[[1]]), na.rm = TRUE)
    Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
  }
}
#plots <- list()
y <- matrix()
std <- matrix()
NumberOfGoodWorms <- matrix()
for (x in 1:dim(Accuracy)[1]) {
  y[x] <- mean(as.numeric(Accuracy[x,]))
  std[x] <- sd(as.numeric(Accuracy[x,]))
  NumberOfGoodWorms[x] <- as.character(SetNumber[x])
}
df <- data.frame(Mean=y, sd=std, NumberOfGoodWorms = NumberOfGoodWorms)
df <- df[dim(df)[1]:1,]
df$NumberOfGoodWorms <- factor(df$NumberOfGoodWorms, levels=unique(df$NumberOfGoodWorms))
plots <- ggplot(df,aes(x=NumberOfGoodWorms))+geom_boxplot(aes(lower=Mean-sd,upper=Mean+sd,middle=Mean,ymin=Mean-3*sd,ymax=Mean+3*sd),stat="identity") +
  theme_minimal() + theme(plot.title = element_text(size=15)) +
  ylab("Accuracy %")  +
  ggtitle(paste('Accuracy with ',BadWormSampleSize,' bad worms.', sep = ''))


#Creating data frame with the good catagorized worms from the clustering
#GoodAdults <- Adult[which(pred == 2),]
GoodAdults <- as.data.frame(rbind(BadWorms,Adult[as.numeric(sub('X','',which(rownames(Adult) %in% AdultGoodIndex))),]))
#change it to which == 2 for good adults
#GoodAdults <- Adult[AdultGoodIndex[,1],]
#plotting the good adults
maxcol <- 0
rowmean <- 0
for (k in 1:dim(GoodAdults)[1]) { #finding the max length for this cluster
  temp2 <- as.numeric(which.min(GoodAdults[k,]))
  if (maxcol > temp2) {
  } else {
    maxcol <- temp2
  }

}
for (m in 1:dim(GoodAdults)[1]) {
  ID <- toString(rownames(GoodAdults)[m])
  if (m == 1) {
    test <- data.frame(placeholder = as.numeric(GoodAdults[m,1:maxcol]))
    names(test)[m] <- ID
  } else {
    test$placeholder <- as.numeric(GoodAdults[m,1:maxcol])
    names(test)[m] <- ID
  }
}
clustermean <- data.frame(Length = 1:maxcol, Mean = rowMeans(test))
test$Length <- 1:maxcol
maxL = nrow(GoodAdults)
newdatafr <- melt(test, id.vars = "Length", measure.vars = c(rownames(GoodAdults)[1:maxL]))
stdmean <- 0
for (j in nrow(GoodAdults)) {
  stdmean <- stdmean + sd(GoodAdults[j,])
}
stdmean <- stdmean/nrow(GoodAdults)


AllWorms <- ggplot(newdatafr, aes(Length, value,col = variable)) +
  geom_line(color="grey") +
  geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line
  theme_minimal() +
  ylab("amp") + xlab("") +
  ggtitle(paste('Adults', ' With ',nrow(GoodAdults),' lucky worms',' std=',stdmean,sep=''))

save(AllWorms,file = 'AllWorms.RData')



#p <- lapply(1:6, function(i){
#  y<-as.numeric(Accuracy[i,])
#  AccuracyMean <- mean(y)
#  x<-1:length(y)
#  z<- std(Accura)
#  df<-data.frame(x=x,y=y)
#  df_molten=melt(df,id.vars="x")
#  ggplot(df_molten) + geom_line(aes(x=x,y=y,color=variable)) +
#    theme_minimal() + theme(plot.title = element_text(size=7)) +
#    ylab("Accuracy %") + xlab("Run number") +
#    ggtitle(paste('Accuracy with ',SetNumber[i],' Samples out of ',length(AdultGoodIndex),' W/ mean of ',format(round(AccuracyMean, 2), nsmall = 2), sep = ''))
#assign(plotlist[l],p)
#})
#png(paste(BadWormSampleSize,'.png',sep=''))
#CombinedPlots <- plot_grid(p[[1]], p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],nrow = 3,ncol = 2, labels = "AUTO")
#CombinedPlots
#dev.off()
#save(plots,file = paste(BadWormSampleSize,'.RData',sep=''))
save(plots,file = paste(BadWormSampleSize,'.RData',sep=''))
#EndTime <- Sys.time()

#EndTime - StartTime


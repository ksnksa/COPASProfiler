
##Loading the libraries
library(e1071)
library(ggplot2)
library(reshape)
library(WormSorter)
#Setting up the parameters we need for the analysis
setwd("C:/Users/pCd/Desktop/WormSorter")
Ch0D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch0_prf.txt',sep=''))
Ch1D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch1_prf.txt',sep=''))
Ch2D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch2_prf.txt',sep=''))
Ch3D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch3_prf.txt',sep=''))
MaxTOF = 64000
MinLength = 40
MaxLength = 500
## To get the annotated IDs, you can go to WormSorter.com(whatever) and do your own categorization
GoodIDD =paste(getwd(),'/data/N2','/GoodIDsNew.csv',sep='')
NumberOfRuns  = 100
ChannelToCluster  = 1

##Load up the data and set up the stages
channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)
Index <- FilterChannel(channellist[[ChannelToCluster]],MaxTOF,MinLength,MaxLength)
channellist[[ChannelToCluster]] <- as.data.frame(channellist[[ChannelToCluster]][-c(Index),])
StageList <- AssignStage(channellist[[ChannelToCluster]])
## stage list is 4 because we picked adult
WormIDs <- CreateTrainingSetIDs(StageList[[2]],GoodIDD)

## Let's find how many worms are in our training set
print(WormCount(StageList[[2]],WormIDs))
## Now we know we only have 83 annotated good worms and 86 bad worms in the training set so we can't go over that
NumberOfBadWorms  = 10
NumbersOfGoodWorms  = c(20,15,10,5)
## My version of bootstrap analysis, feel free to use whatever method to find your models accuracy
Accuracy <- data.frame()
for (l in 1:length(NumbersOfGoodWorms)) {
  for (o in 1:NumberOfRuns) {
    SetList<- RandomTrainingSet(StageList[[2]],WormIDs,NumbersOfGoodWorms[l],NumberOfBadWorms)
    Pred <- GetPrediction(SetList)[[1]]
    Positive <- rownames(SetList[[2]][which(Pred==2),])
    TP <- sum((Positive %in% WormIDs[[1]]), na.rm = TRUE)
    Negative <- rownames(SetList[[2]][which(Pred==1),])
    TN <- length(Negative %in% WormIDs[[2]]) - sum((Negative %in% WormIDs[[1]]), na.rm = TRUE)
    Accuracy[l,o] <- (TP + TN) / (length(Positive) + length(Negative))
  }

}
p <- PlotAccuracy(Accuracy, NumbersOfGoodWorms, NumberOfBadWorms)
## view the accuracy plot to get a sense of your model's efficiency
plot(p)

## Now what if we just wanted to create a model out of a training set and simply run it againts a data set
## We'll use the sample data set we provided (since the training set was taken from the data set, there will be a bias
## with the catagorizing, because the training set wasn't removed.)

## our data set is in StageList[[4]]
## from my analysis I found the best performing number in the training list is 25 good worms and 25 bad worms
SetList<- RandomTrainingSet(StageList[[2]],WormIDs,10,20)

## Let's create the model out of the training set
model <- CreateModel(SetList[[2]])
## Now we want to run our model and get the predicted output
Pred <- RunModel(StageList[[2]], model)
## we can now plot the results and see what we got
Plots <- PlotPred(Pred, StageList[[2]])
## To view them we can simply use the plot function

## Plot 1: The over all plot of all the worms
plot(Plots[[1]])
## Plot 2: The bad worms
plot(Plots[[2]])
## Plot 3: The good worms
plot(Plots[[3]])


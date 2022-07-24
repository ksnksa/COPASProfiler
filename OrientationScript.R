#Using SVM to find the orientation of the worm. 
load('/ibex/scratch/naimks/SAHAnalysis/ch2.Rdata')
#Ch0 = t(Ch0[,-c(which(is.na(Ch0[1,])))])
Ch0Directory <- '/home/ksnksa/Desktop/COPAS/Ch.txt'
Ch <- read.delim(Ch0Directory, header=TRUE) 
save(Ch,file = 'Ch.Rdata')
ch0 <- matrix(nrow = 74600, ncol = 1106)
ch1 <- matrix(nrow = 74600, ncol = 1106)
ch2 <- matrix(nrow = 74600, ncol = 1106)
ch3 <- matrix(nrow = 74600, ncol = 1106)
Sequence =  1:74600
counter <- 0
for (x in seq(from = 1, to = which(is.na(Ch[1,])) - 5, by = 4)) {
  counter <- counter + 1
  ch0[Sequence[counter],] <- t(Ch[,x])
  ch1[Sequence[counter],] <- t(Ch[,x+1])
  ch2[Sequence[counter],] <- t(Ch[,x+2])
  ch3[Sequence[counter],] <- t(Ch[,x+3])
}

row.names(ch0) <- paste('X',1:74600,sep = '')
row.names(ch1) <- paste('X',1:74600,sep = '')
row.names(ch2) <- paste('X',1:74600,sep = '')
row.names(ch3) <- paste('X',1:74600,sep = '')
ch0 <- as.data.frame(ch0)
ch1 <- as.data.frame(ch1)
ch2 <- as.data.frame(ch2)
ch3 <- as.data.frame(ch3)
#CSV file containing the levels (or annotation) for each worm in our training set. 
load('/Users/khalifaalnaim/Desktop/WormSorter/git/WormSorter/ModDataAllCh1.Rdata')
GoodIDD <- '/home/ksnksa/Desktop/OrientationID.csv'

MaxAmp <- 35000
MinLength <- 40
MaxLength <- 500
Stage <- 4
#ChannelToCluster <- 2
#channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)

Index <- FilterChannel(ch1,MaxAmp,MinLength,MaxLength)
ch1 <- as.data.frame(ch1[-c(Index),])
ModifiedData <- matrix(nrow= dim(ch1)[1], ncol =100)
for (z in 1:dim(ch1)[1]) {
  if (max(which(ch1[z,]!=0)) < 100 ) { 
    ModifiedData[z,] <- as.numeric(binning(ch1[z,1:100],100))
  } else { ModifiedData[z,] <- as.numeric(binning(ch1[z,1:(max(which(ch1[z,]!=0)) + 1)],100))
  }
  
  
}
RowNames <- rownames_to_column(ch1,'ID')[,1]

#ModifiedData <- channellist[[ChannelToCluster]]
ModData <- data.frame(ModifiedData,row.names = RowNames)
for (row in 1:dim(ModData)[1]) {
  ModData[row,] <- rescale(as.numeric(ModData[row,]), to = c(0,100))
}
WormIDs <- CreateTrainingSetIDs(ModData,GoodIDD)

BadWorms <- ModData[which(rownames(ModData) %in% WormIDs[[2]]),]
GoodWorms <- ModData[which(rownames(ModData) %in% WormIDs[[1]]),]
TrainingSet <- as.data.frame(rbind(BadWorms,GoodWorms))

plotID <- function(Data, ID) { 
  BadWorms <- Data[ID,]
  maxcol <- 0
  rowmean <- 0
  for (k in 1:dim(BadWorms)[1]) {
    temp2 <- as.numeric(which.min(BadWorms[k,]))
    if (maxcol > temp2) {
    } else {
      maxcol <- temp2
    }
    
  }
  for (m in 1:dim(BadWorms)[1]) {
    ID <- toString(rownames(BadWorms)[m])
    if (m == 1) {
      test <- data.frame(placeholder = as.numeric(BadWorms[m,1:maxcol]))
      names(test)[m] <- ID
    } else {
      test$placeholder <- as.numeric(BadWorms[m,1:maxcol])
      names(test)[m] <- ID
    }
  }
  clustermean <- data.frame(Length = 1:maxcol, Mean = rowMeans(test))
  test$Length <- 1:maxcol
  maxL = nrow(BadWorms)
  newdatafr <- melt(test, id.vars = "Length", measure.vars = c(rownames(BadWorms)[1:maxL]))

  
  Plots <- ggplot(newdatafr, aes(Length, value,col = variable)) +
    geom_line(color="grey") +
    #geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') +
    theme_minimal() +
    ylab("amp") + xlab("") +
    ggtitle(paste(nrow(BadWorms),' Bad Worms',' std=',sep=''))
  
  return(Plots)
  
  }




ModelGood <- 10
ModelBad <- 10
# RandomTrainingSet creates a training set out of the following parameters: 
# The channel data, annotated worm IDs and how many good and bad worms will be in the model. 
SetList<- RandomTrainingSet(TrainingSet,WormIDs,ModelGood,ModelBad)

# CreateModel takes in the training set to create a model.
model <- CreateModel(SetList[[2]])
# RunModel runs the model againts the inputted channel data 
# and outputs the clustering prediction. 
# To run the model against another data set, simply change the StageList[[Stage]] variable. 
Pred <- RunModel(TrainingSet, model)
Plots <- PlotPred(Pred, TrainingSet)
# Plot 1: The over all plot of all the worms
plot(Plots[[1]])
# Plot 2: The bad worms
plot(Plots[[2]])
# Plot 3: The good worms
plot(Plots[[3]])

NumberOfBadWorms  = c(4,6,8,10,12)
NumbersOfGoodWorms  = c(4,6,8,10,12)

NumberOfRuns  = 100
Accuracy <- data.frame()
Accuracy <- list(data.frame(),data.frame(),data.frame(),data.frame(),data.frame())
for (k in 1:length(NumberOfBadWorms)) {
  for (l in 1:length(NumbersOfGoodWorms)) {
    for (o in 1:NumberOfRuns) {
      SetList<- RandomTrainingSet(TrainingSet,WormIDs,NumbersOfGoodWorms[l],NumberOfBadWorms[k])
      Pred <- GetPrediction(SetList)[[1]]
      Positive <- rownames(SetList[[2]][which(Pred==2),])
      TP <- sum((Positive %in% WormIDs[[1]]), na.rm = TRUE)
      Negative <- rownames(SetList[[2]][which(Pred==1),])
      TN <- sum(Negative %in% WormIDs[[2]],na.rm = TRUE)# - sum((Negative %in% WormIDs[[1]]), na.rm = TRUE)
      Accuracy[[k]][l,o] <- (TP + TN) / (length(Positive) + length(Negative))
    }
    
  }
}




load('/Users/khalifaalnaim/Desktop/WormSorter/git/WormSorter/ModDataAllStages.RData')
Ch0Data <- ModData
load('/Users/khalifaalnaim/ModDataAllCh2.Rdata')
Ch1Data <- ModData
Factors <- matrix(nrow=length(rownames(Ch1Data)[which(rownames(Ch1Data) %in% rownames(GoodWorms))]),ncol=1)
#RowNames <- rownames_to_column(Ch0Data,'ID')[,1]
RowNames <- rownames(Ch1Data)[which(rownames(Ch1Data) %in% rownames(GoodWorms))]
Factors <- data.frame(Factors,row.names = RowNames)
for (x in rownames(Ch1Data)[which(rownames(Ch1Data) %in% rownames(GoodWorms))]) {
  Index <- which.max(Ch1Data[x,])
  if (which(Ch1Data[x,] == 0)[1] > 100) {
    HalfWay <- round(which(Ch1Data[x,] == 0)[1]/2)
  } else { HalfWay <- 50}
  if (Index >= HalfWay) { 
    Factors[x,1] <- 'Good'
  } else { 
    Factors[x,1] <- 'Bad'
    }
  
}
OrientationIDs <- matrix(nrow=2,ncol=199)
OrientationIDs[2,] <- Factors[,1]
OrientationIDs[1,] <- RowNames
write.csv(OrientationIDs,'Ch2OrientationIDsFull.csv')

  if (which(Ch1Data[ID,] == 0)[1] > 100) {
    #HalfWay <- round(which(Ch1Data[ID,] == 0)[1]/2)
    Factors[]
  } else { HalfWay <- 50}
  if (Index >= HalfWay) { 
    Ch0Data[ID,1:(length(Ch0Data[1,])-1)] <- rev(Ch0Data[ID,1:(length(Ch0Data[1,])-1)])
  } 
  return(Ch0Data[ID,])
}



SwitchSides <- function(Ch0Data,Ch1Data,ID) { 
  Index <- which.max(Ch1Data[ID,])
  if (which(Ch1Data[ID,] == 0)[1] > 100) {
  HalfWay <- round(which(Ch1Data[ID,] == 0)[1]/2)
    } else { HalfWay <- 50}
  if (Index >= HalfWay) { 
    Ch0Data[ID,1:(length(Ch0Data[1,])-1)] <- rev(Ch0Data[ID,1:(length(Ch0Data[1,])-1)])
    } 
  return(Ch0Data[ID,])
  }

for(x in rownames(GoodWorms)) { 
  GoodWorms[x,] <- SwitchSides(GoodWorms,ModDataCh1,x)
  }

MakeIDs <- function(Ch0Data,Ch1Data,ID) { 
  IDMtx <- matrix(nrow=dim(Ch0Data)[1],ncol = 2)
  IDMtx[,1] <- rownames(Ch0Data)
  Index <- which.max(Ch1Data[ID,])
  if (which(Ch1Data[ID,] == 0)[1] > 100) { 
    HalfWay <- round(which(Ch1Data[ID,] == 0)[1]/2)} else { HalfWay <- 50}
  if (Index >= HalfWay) { 
    Ch0Data[ID,1:(length(Ch0Data[1,])-1)] <- rev(Ch0Data[ID,1:(length(Ch0Data[1,])-1)])
  } 
  return(Ch0Data[ID,])
}


NumberOfBadWorms <-seq(from = 10, to =360, by = 10)
NumbersOfGoodWorms <- seq(from = 10, to =70, by = 10)
# How many times the bootstrap loop will run 
NumberOfRuns  = 100
SizeBad <- seq(from = 10, to =360, by = 10)
SizeGood <- seq(from = 10, to =70, by = 10)


#Calculate Accuracy stuff 

#Calculate sensitivity 
Sensitivity <- list()
temp <- 0
for (k in 1:length(SizeBad)) { 
  for (l in 1:length(SizeGood)) { 
    for (z in 1:100) { 
      if (z == 1) { 
        temp <- Mtx[[k]][l,c(TRUE,FALSE,FALSE,FALSE)][z] / (Mtx[[k]][l,c(TRUE,FALSE,FALSE,FALSE)][z] + Mtx[[k]][l,c(FALSE,FALSE,FALSE,TRUE)][z])
      } else {       temp <- (temp + (Mtx[[k]][l,c(TRUE,FALSE,FALSE,FALSE)][z] / (Mtx[[k]][l,c(TRUE,FALSE,FALSE,FALSE)][z] + Mtx[[k]][l,c(FALSE,FALSE,FALSE,TRUE)][z])))/2
      
      }
      
    }
    Sensitivity[paste(k,'-',l,sep='')] <- temp 
    
    
  }}



#Calculate FalsePosRate 
FalsePosRate <- list()
temp <- 0
for (k in 1:length(SizeBad)) { 
  for (l in 1:length(SizeGood)) { 
    for (z in 1:100) { 
      if (z == 1) { 
        temp <- Mtx[[k]][l,c(FALSE,FALSE,TRUE,FALSE)][z] / (Mtx[[k]][l,c(FALSE,FALSE,TRUE,FALSE)][z] + Mtx[[k]][l,c(FALSE,TRUE,FALSE,FALSE)][z])
      } else {       temp <- (temp + (Mtx[[k]][l,c(FALSE,FALSE,TRUE,FALSE)][z] / (Mtx[[k]][l,c(FALSE,FALSE,TRUE,FALSE)][z] + Mtx[[k]][l,c(FALSE,TRUE,FALSE,FALSE)][z])))/2
      
      }
      
    }
    FalsePosRate[paste(k,'-',l,sep='')] <- temp 
    
    
  }}

for (k in 1:length(SizeBad)) {
  for (l in 1:length(SizeGood)) { 
    if (k == 1 & l == 1) { 
      RowNames <- paste(SizeBad[k],SizeGood[l],sep='-')} else { 
        RowNames <- c(RowNames,paste(SizeBad[k],SizeGood[l],sep='-'))
        }

  }}

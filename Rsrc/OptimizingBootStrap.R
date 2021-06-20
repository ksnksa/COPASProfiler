#Libraries
library(e1071)
library(ggplot2)
library(reshape)
library(WormSorter)
library(tibble)
library(prospectr)
library(scales)
library(NbClust)
library(dplyr)

#Load data
load('/Users/khalifaalnaim/Desktop/WormSorter/git/WormSorter/ModDataAdults.RData')
WormIDs <- CreateTrainingSetIDs(ModData,GoodIDD)

#Make training set
BadWorms <- ModData[which(rownames(ModData) %in% WormIDs[[2]]),]
GoodWorms <- ModData[which(rownames(ModData) %in% WormIDs[[1]]),]
TrainingSet <- as.data.frame(rbind(BadWorms,GoodWorms))
#Find number of annotated worms 
print(WormCount(TrainingSet,WormIDs))


#Functions for the bootstraep 


TestFunction <- function(Data, WormIDs, SizeGood, SizeBad) {
  Output <- list()
  Output[[1]] <- RandomTrainingSet(TrainingSet, WormIDs, SizeGood, SizeBad)
  Output[[2]] <- GetPrediction(Output[[1]])[[1]]
  return(Output)
}

AccuracyResults <- function(List, WormIDs) { 
  Positive <- rownames(List[[1]][[2]][which(List[[2]] == 2),])
  TP <- sum((Positive %in% WormIDs[[1]]), na.rm = TRUE)
  Negative <- rownames(List[[1]][[2]][which(List[[2]]==1),])
  TN <- sum(Negative %in% WormIDs[[2]]) #- sum((Negative %in% WormIDs[[1]]), na.rm = TRUE)
  Vector <- c(TP, TN, length(Positive)-TP,length(Negative)-TN)
  return(Vector)
}

LoopBad <- function(SizeGood,SizeBad,WormIDs, Data,Mtx,counter) { 
  for (x in 1:length(SizeBad)) { 
    List<-lapply(SizeGood, FUN  = TestFunction, WormIDs = WormIDs, Data = Data , SizeBad = SizeBad)
    Vec<-lapply(List, FUN = AccuracyResults, WormIDs = WormIDs)
    for (z in 1:length(SizeGood)) { 
      if (counter == 1) { 
        Mtx[[x]][z,1:4] <- Vec[[z]]
        
      } else { 
        Mtx[[x]][z,1:4] <- (Mtx[[x]][z,1:4] + Vec[[z]])/2
      }
      
    }
  }
  
  return(Mtx)}

#List <- TestFunction(TrainingSet, WormIDs, 10,10)
#Time  56.2937 secs
start_time <- Sys.time()
SizeGood <- c(10,12,13,9,8)
SizeBad <- c(10,12,13,9,8)
Mtx <- list(matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4))
Vec <- list()
counter <- 1
for (Runs in 1:100) { 
  Mtx %<-% LoopBad(SizeGood,SizeBad,WormIDs,TrainingSet,Mtx,counter)
  counter <- 2
}

counter <- 1
end_time <- Sys.time()
end_time - start_time

#Accuracy (10.54054) 12.46875
#11.50465



#Normal bootstrap 

#List <- TestFunction(TrainingSet, WormIDs, 10,10)
#Time  56.2937 secs
start_time <- Sys.time()
SizeGood <- c(10,12,13,9,8)
SizeBad <- c(10,12,13,9,8)
Mtx <- list(matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4))
Vec <- list()
counter <- 1
for (Runs in 1:100) { 
  Mtx <- lapply(SizeBad, FUN = LoopBad, Data = TrainingSet, SizeGood = SizeGood,WormIDs = WormIDs,Mtx = Mtx,counter=counter)
  
  counter <- 2
}

counter <- 1
end_time <- Sys.time()
end_time - start_time

#Accuracy (10.54054) 12.46875
#11.50465



#Normal Bootstrap

NumberOfBadWorms <- c(10,12,13,9,8)
NumbersOfGoodWorms <- c(10,12,13,9,8)
# How many times the bootstrap loop will run 
NumberOfRuns  = 100


start_time <- Sys.time()

Mtx <- list(matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4),matrix(nrow = 5, ncol = 4))
for (k in 1:length(NumberOfBadWorms)) {
  for (l in 1:length(NumbersOfGoodWorms)) {
    for (o in 1:NumberOfRuns) {
      SetList<- RandomTrainingSet(TrainingSet,WormIDs,NumbersOfGoodWorms[l],NumberOfBadWorms[k])
      Pred <- GetPrediction(SetList)[[1]]
      Positive <- rownames(SetList[[2]][which(Pred==2),])
      TP <- sum((Positive %in% WormIDs[[1]]), na.rm = TRUE)
      Negative <- rownames(SetList[[2]][which(Pred==1),])
      TN <- sum(Negative %in% WormIDs[[2]]) #- sum((Negative %in% WormIDs[[1]]), na.rm = TRUE)
      if (o == 1) { 
        Mtx[[k]][l,] <- c(TP,TN,length(Positive)-TP,length(Negative)-TN)
      } else { 
        Mtx[[k]][l,] <- (Mtx[[k]][l,] + c(TP,TN,length(Positive)-TP,length(Negative)-TN))/2
        
      }
    }
    
  }
}

end_time <- Sys.time()
end_time - start_time
#Accuracy <- data.frame()















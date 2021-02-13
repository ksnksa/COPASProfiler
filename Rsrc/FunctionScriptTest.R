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


#Finding the "good" worms in Adult (The rest of the good worms only have 10 or less worms in the training set, only Adult had enough to supervise the clustering so far)
##Loading up the good IDs and creating the training set
GoodIDs <- read.delim(paste(getwd(),'/data/N2','/GoodIDsN2.csv',sep=''), sep=',',header=TRUE) 
GoodIndex <- as.matrix(GoodIDs[2:length(GoodIDs)])
GoodIndex <- matrix(GoodIndex, ncol = ncol(GoodIndex), dimnames = NULL)
#ch0 <- as.data.frame(channellist[[1]])
#ch0_full <- ch0
AdultGoodIndex <- matrix(GoodIndex[1,which(GoodIndex[1,] %in% rownames(Adult))]) 
 #index number 
training_Adult <- Adult[1:(which(as.numeric(sub('X','',rownames(Adult))) >= 365)[1]),] #make it so the last number should be x365 (max row) (Did this because I picked worms up to 365, the rest I did not catagorize so I'm using the remaining worms as a prediction model)
#ch0_data <- ch0
Adult <- as.data.frame(Adult)
training_Adult <- as.data.frame(training_Adult)
training_Adult$Factor <- as.numeric(1) #creating a true false table
training_Adult$Factor[ as.numeric(sub('X','',which(rownames(training_Adult) %in% AdultGoodIndex)))] <- as.numeric(2) #changing it to true for the good worms (true being 2 and false is 1)
x <- lapply(training_Adult[,1:1124],as.numeric) #it's up to 1124 (becease we added one column factor, so we take it off by doing this to make training set same size as prediction set?)
m <- as.data.frame(x)
y <- training_Adult$Factor #make y variable(dependent)
model <- svm(m, y,type='C-classification',nu=0.15,
             scale=TRUE,
             kernel="polynomial") #I'm still not sure which parameters are better but it seems like polynomial makes the most sense 
pred <- predict(model, Adult)

#Creating data frame with the good catagorized worms from the clustering
GoodAdults <- Adult[which(pred == 2),]
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


ggplot(newdatafr, aes(Length, value,col = variable)) +
  geom_line(color="grey") +
  geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line 
  theme_minimal() +
  ylab("amp") + xlab("") + 
  ggtitle(paste('Adults', ' With ',nrow(GoodAdults),' lucky worms',' std=',stdmean,sep=''))











#Plotting the "good" cluster for L1 
# maxcol <- 0
# rowmean <- 0
#     for (k in 1:dim(L1)[1]) { #finding the max length for this cluster
#       temp2 <- as.numeric(which.min(L1[k,]))
#       if (maxcol > temp2) {
#       } else {
#         maxcol <- temp2 
#       }
#       
#     }
#     for (m in 1:dim(L1)[1]) {
#       ID <- toString(rownames(L1)[m])
#       if (m == 1) {
#         test <- data.frame(placeholder = as.numeric(L1[m,1:maxcol]))
#         names(test)[m] <- ID
#       } else {
#         test$placeholder <- as.numeric(L1[m,1:maxcol])
#         names(test)[m] <- ID
#       }
#     }
#     clustermean <- data.frame(Length = 1:maxcol, Mean = rowMeans(test))
#     test$Length <- 1:maxcol
#     if(nrow(L1) > 50){maxL = 50}else{maxL = nrow(L1)}
#     newdatafr <- melt(test, id.vars = "Length", measure.vars = c(rownames(L1)[1:maxL])) 
#     stdmean <- 0
#     for (j in nrow(L1)) {
#       stdmean <- stdmean + sd(L1[j,])
#     }
#     stdmean <- stdmean/nrow(L1)
#     
#     
#     p = ggplot(newdatafr, aes(Length, value,col = variable)) +
#       geom_line(color="grey") +
#       geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') + #the mean will be the red line 
#       theme_minimal() +
#       ylab("amp") + xlab("") + 
#       ggtitle(paste('L1', ' With ',nrow(L1),' lucky worms',' std=',stdmean,sep=''))
#   
# 
# #source((paste(getwd(),'/Rsrc','/','ClusterChannel.R',sep='')))
# #clusterlist <- ClusterChannel(channellist[[1]],10)
# #ch0 <- channellist[[1]]
# #source((paste(getwd(),'/Rsrc','/','PlotClusters.R',sep='')))
# #p <- PlotClusters(ch0,clusterlist)
# #ch1 <- ch1[which(clusterlist[[2]][,2] == 2),] #this makes the bug for some reason so lets see why this indexing doesnt work right 
# 
# 
# #reading the good worms and taking the ID 
# 
# 
# GoodIDs <- read.delim(paste(getwd(),'/data/N2','/GoodIDsN2.csv',sep=''), sep=',',header=TRUE) 
# GoodIndex <- as.matrix(GoodIDs[2:length(GoodIDs)])
# GoodIndex <- matrix(GoodIndex, ncol = ncol(GoodIndex), dimnames = NULL)
# ch0 <- as.data.frame(channellist[[1]])
# ch0_full <- ch0
# ch0 <- ch0[1:365,]
# ch0_data <- ch0
# ch0$Factor <- as.numeric(1)
# ch0$Factor[as.numeric(sub('X','',GoodIndex))] <- as.numeric(2)
# x <- lapply(ch0_data,as.numeric)
# m <- as.data.frame(x)
# y <- ch0$Factor #make y variable(dependent)
# model <- svm(m, y,type='C-classification',nu=0.10,
#              scale=TRUE,
#              kernel="polynomial")
# pred <- predict(model, ch0_full)
# 
# 


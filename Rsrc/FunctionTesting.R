#Testing if the functions work.. 
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
source((paste(getwd(),'/Rsrc','/','AssignStage.R',sep='')))
StageList <- AssignStage(channellist[[1]])
source((paste(getwd(),'/Rsrc','/','CreateTrainingSet.R',sep='')))
AdultIDs <- CreateTrainingSet(StageList[[4]],paste(getwd(),'/data/N2','/GoodIDsNew.csv',sep=''))

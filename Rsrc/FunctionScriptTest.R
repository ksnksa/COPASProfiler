
Ch0D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch0_prf.txt',sep=''))

Ch1D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch1_prf.txt',sep=''))

Ch2D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch2_prf.txt',sep=''))
         
Ch3D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch3_prf.txt',sep=''))
source((paste(getwd(),'/Rsrc','/','ReadChannel.R',sep='')))
channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)
ch0 <- channellist[[1]]
ch1 <- channellist[[2]]
ch2 <- channellist[[3]]
ch3 <- channellist[[4]]
source((paste(getwd(),'/Rsrc','/','FilterChannel.R',sep='')))
Index <- FilterChannel(ch0,64000,40,500)
ch1 <- ch1[-c(Index),]
source((paste(getwd(),'/Rsrc','/','ClusterChannel.R',sep='')))
clusterlist <- ClusterChannel(ch1,10)
source((paste(getwd(),'/Rsrc','/','PlotClusters.R',sep='')))
PlotClusters(ch1,clusterlist)

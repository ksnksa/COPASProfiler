
Ch0D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch0_prf.txt',sep=''))

Ch1D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch1_prf.txt',sep=''))

Ch2D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch2_prf.txt',sep=''))
         
Ch3D <- (paste(getwd(),'/data/N2','/','n2','_profil_ch3_prf.txt',sep=''))
source((paste(getwd(),'/Rsrc','/','ReadChannel.R',sep='')))
channellist <- ReadChannel(Ch0D,Ch1D,Ch2D,Ch3D)



source((paste(getwd(),'/Rsrc','/','FilterChannel.R',sep='')))
Index <- FilterChannel(channellist[[1]],64000,40,500)
channellist[[2]] <- channellist[[2]][-c(Index),]
source((paste(getwd(),'/Rsrc','/','ClusterChannel.R',sep='')))
clusterlist <- ClusterChannel(channellist[[2]],10)
ch1 <- channellist[[2]]
source((paste(getwd(),'/Rsrc','/','PlotClusters.R',sep='')))
p <- PlotClusters(ch1,clusterlist)

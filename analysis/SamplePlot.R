library(ggplot2)
source(paste(getwd(),'/Rsrc/ReadProfile.R',sep=''))
filedirectory <- 'data/N2/n2'
ID <- 1998 #My birth year 
profile <- ReadProfile(filedirectory,ID)
ggplot(profile, aes(x=Length, y=Amplitude, group=variable,color=variable)) + geom_line()+ scale_colour_manual(values = c('black','green','yellow','red')) + labs(color='Channel') + ggtitle(paste('Lucky worm #',ID,sep=''))

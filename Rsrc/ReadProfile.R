ReadProfile <- function(filename,ID){ #I wasn't sure what the max.size and min.size stood for, will the user input the length of the signals that they want?
  library(reshape)
  ch0 <- read.delim((paste(getwd(),'/',filename,'_profil_ch0_prf.txt',sep='')), header=TRUE)
  ch1 <- read.delim((paste(getwd(),'/',filename,'_profil_ch1_prf.txt',sep='')), header=TRUE)
  ch2 <- read.delim((paste(getwd(),'/',filename,'_profil_ch2_prf.txt',sep='')), header=TRUE)
  ch3 <- read.delim((paste(getwd(),'/',filename,'_profil_ch3_prf.txt',sep='')), header=TRUE)
  ID <- paste('X',ID,sep='')
  rownum0 <- which.min(ch0[,ID])
  rownum1 <- which.min(ch1[,ID])
  rownum2 <- which.min(ch2[,ID])
  rownum3 <- which.min(ch3[,ID])
  maxrow <- max(c(rownum0,rownum1,rownum2,rownum3))
  datafr <- data.frame('EXT' = as.numeric(ch0[1:maxrow,ID]),'Green' = as.numeric(ch1[1:maxrow,ID]),'Yellow' = as.numeric(ch2[1:maxrow,ID]),'Red' = as.numeric(ch3[1:maxrow,ID]))
  datafr$Length <- 1:maxrow 
  newdatafr <- melt(datafr, id.vars = "Length", measure.vars =c('EXT','Green','Yellow','Red'))
  colnames(newdatafr) = c('Length','variable','Amplitude')
  return(newdatafr)
   }
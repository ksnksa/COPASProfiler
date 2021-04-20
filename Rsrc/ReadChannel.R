ReadChannel <- function(Ch0Directory,Ch1Directory,Ch2Directory,Ch3Directory) {
  if(missing(Ch0Directory)){ 
    stop('Missing Channel 0 Directory')
  } else if(missing(Ch1Directory)) {
    stop('Missing Channel 1 Directory')
  } else if(missing(Ch2Directory)) {
    stop('Missing Channel 2 Directory')
  } else if(missing(Ch3Directory)) {
    stop('Missing Channel 3 Directory')
  }
  Ch0 <- read.delim(Ch0Directory, header=TRUE) 
  Ch1 <- read.delim(Ch1Directory, header=TRUE)
  Ch2 <- read.delim(Ch2Directory, header=TRUE)
  Ch3 <- read.delim(Ch3Directory, header=TRUE)
  if (nrow(Ch0) != nrow(Ch1) | ncol(Ch0) != ncol(Ch1)) {
    stop('Channel 0 and Channel 1 do not have the same dimensions')
  } else if (nrow(Ch0) != nrow(Ch2) | ncol(Ch0) != ncol(Ch2)) {
    stop('Channel 0 and Channel 2 do not have the same dimensions')
  } else if (nrow(Ch0) != nrow(Ch3) | ncol(Ch0) != ncol(Ch3)) {
    stop('Channel 0 and Channel 3 do not have the same dimensions')
  } 
  
  # sum(Ch0[1,]==Ch1[1,]) == ncol(Ch0)
  
  if (Ch0[1,1] == Ch1[1,1] & Ch0[1,2] == Ch1[1,2]) {
    stop('Channel 0 and Channel 1 are the same channel')
  } else if (Ch0[1,1] == Ch2[1,1]& Ch0[1,2] == Ch2[1,2]) {
    stop('Channel 0 and Channel 2 are the same channel')
  } else if (Ch0[1,1] == Ch3[1,1]& Ch0[1,2] == Ch3[1,2]) {
    stop('Channel 0 and Channel 3 are the same channel')
  } else if (Ch1[1,1] == Ch2[1,1]& Ch1[1,2] == Ch2[1,2]) {
    stop('Channel 1 and Channel 2 are the same channel')
  } else if (Ch1[1,1] == Ch3[1,1]& Ch1[1,2] == Ch3[1,2]) {
    stop('Channel 1 and Channel 3 are the same channel')
  } else if (Ch2[1,1] == Ch3[1,1]& Ch2[1,2] == Ch3[1,2]) {
    stop('Channel 2 and Channel 3 are the same channel')
  } 
  
  
  Ch0 = t(Ch0[,-c(which(is.na(Ch0[1,])))])
  Ch1 = t(Ch1[,-c(which(is.na(Ch1[1,])))])
  Ch2 = t(Ch2[,-c(which(is.na(Ch2[1,])))])
  Ch3 = t(Ch3[,-c(which(is.na(Ch3[1,])))])
  

  return(list(Ch0,Ch1,Ch2,Ch3))
}
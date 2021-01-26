FilterChannel <- function(Ch0,MaxTOF, MinLength,MaxLength) {
  #Filters the inputted channel by Max TOF or/and by min length and max length 
  if(missing(MaxTOF) & missing(MinLength) & missing(MaxLength)){ 
    stop('Please input the MaxTOF or/and the Minimum and Maximum Worm Length ')
  } else if(!missing(MinLength) & missing(MaxLength)) {
    stop('Minimum Worm Length was provided without the Max length')
  } else if(!missing(MaxLength) & missing(MinLength)) {
    stop('Maximum Worm Length was provided without the Min length')
  } else if(missing(Ch0)) {
    stop('Channel 0 was not provided')
  }
  if (!missing(MaxTOF) & !missing(MinLength)) {
    WormNumber <- which(Ch0 > MaxTOF, arr.ind = TRUE)
    WormNumber <- WormNumber[,1]
   for (x in 1:length(Ch0[,1])) {
     if (which.min(Ch0[x,]) > MaxLength | which.min(Ch0[x,]) < MinLength) {
       WormNumber <- c(WormNumber,x)
     }
   }
       WormNumber <- sort(unique(WormNumber))
  } else if (!missing(MinLength)) {
    for (x in 1:length(Ch0[,1])) {
      if (which.min(Ch0[x,]) > MaxLength | which.min(Ch0[x,]) < MinLength) {
        WormNumber <- c(WormNumber,x)
      }
    }
    WormNumber <- sort(unique(WormNumber))
  } else if (!missing(MaxTOF)) {
    WormNumber <- unique(which(Ch0 > MaxTOF, arr.ind = TRUE))
    WormNumber <- sort(unique(WormNumber[,1]))
  }
return(WormNumber)
  
}
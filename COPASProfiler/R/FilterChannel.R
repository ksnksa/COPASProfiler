#' Filter channel data
#'
#' This function takes in the channel data and filters it according to the input parameters. The parameters are:
#' Max Amplitude, Minimum length, and maximum length.
#'
#' @param ch Channel data
#' @param MaxAmp Maximum amplitude
#' @param MinLength Minimum length
#' @param MaxLength Maximum length
#'
#' @return Returns the index number of the filtered objects
#' @export
FilterChannel <- function(ch,MaxAmp, MinLength,MaxLength) {
  #Filters the channel by Max TOF or/and by min length and max length
  if(missing(MaxAmp) & missing(MinLength) & missing(MaxLength)){
    stop('Please input the MaxAmp or/and the Minimum and Maximum Worm Length ')
  } else if(!missing(MinLength) & missing(MaxLength)) {
    stop('Minimum Worm Length was provided without the Max length')
  } else if(!missing(MaxLength) & missing(MinLength)) {
    stop('Maximum Worm Length was provided without the Min length')
  } else if(missing(ch)) {
    stop('Channel 0 was not provided')
  }
  if (!missing(MaxAmp) & !missing(MinLength)) {
    WormNumber <- which(ch > MaxAmp, arr.ind = TRUE)
    WormNumber <- WormNumber[,1]
    for (x in 1:length(ch[,1])) {
      if (which.min(ch[x,]) > MaxLength | which.min(ch[x,]) < MinLength) {
        WormNumber <- c(WormNumber,x)
      }
    }
    WormNumber <- sort(unique(WormNumber))
  } else if (!missing(MinLength)) {
    for (x in 1:length(ch[,1])) {
      if (which.min(ch[x,]) > MaxLength | which.min(ch[x,]) < MinLength) {
        WormNumber <- c(WormNumber,x)
      }
    }
    WormNumber <- sort(unique(WormNumber))
  } else if (!missing(MaxAmp)) {
    WormNumber <- unique(which(ch > MaxAmp, arr.ind = TRUE))
    WormNumber <- sort(unique(WormNumber[,1]))
  }
  return(WormNumber)

}

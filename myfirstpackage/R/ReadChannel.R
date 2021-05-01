#' Read Channel Data
#'
#' The function takes in the four channel data directories and loads them up. The returned output
#' is reshaped to a compatible form with the downstream functions.
#'
#' @param Ch0Directory Path to the channel 0 data.
#' @param Ch1Directory Path to the channel 1 data.
#' @param Ch2Directory Path to the channel 2 data.
#' @param Ch3Directory Path to the channel 3 data.
#'
#' @return List with the four reshaped data sets.
#' @export
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

  if (sum((Ch0[1,1:100] == Ch1[1,1:100])[1,]) == 100) {
    stop('Channel 0 and Channel 1 are the same channel')
  } else if (sum((Ch0[1,1:100] == Ch2[1,1:100])[1,]) == 100) {
    stop('Channel 0 and Channel 2 are the same channel')
  } else if (sum((Ch0[1,1:100] == Ch3[1,1:100])[1,]) == 100) {
    stop('Channel 0 and Channel 3 are the same channel')
  } else if (sum((Ch1[1,1:100] == Ch2[1,1:100])[1,]) == 100) {
    stop('Channel 1 and Channel 2 are the same channel')
  } else if (sum((Ch1[1,1:100] == Ch3[1,1:100])[1,]) == 100) {
    stop('Channel 1 and Channel 3 are the same channel')
  } else if (sum((Ch2[1,1:100] == Ch3[1,1:100])[1,]) == 100) {
    stop('Channel 2 and Channel 3 are the same channel')
  }


  Ch0 = t(Ch0[,-c(which(is.na(Ch0[1,])))])
  Ch1 = t(Ch1[,-c(which(is.na(Ch1[1,])))])
  Ch2 = t(Ch2[,-c(which(is.na(Ch2[1,])))])
  Ch3 = t(Ch3[,-c(which(is.na(Ch3[1,])))])


  return(list(Ch0,Ch1,Ch2,Ch3))
}

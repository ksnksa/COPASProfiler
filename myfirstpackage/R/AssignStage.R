#' Assign worm stages
#'
#' The function takes in the channel data and sorts the worms into four different stages (L1, L2/3, L4, Adult)
#' depending on their size.
#'
#' @param Channel Channel data
#' @return A list with 4 stages of worms
#' @export
AssignStage <- function(Channel) {
  if(missing(Channel)){
    stop('Missing Channel input')
  }  else if(typeof(Channel) != 'list') {
    stop('Channel type is expected to be list, please provide the correct input type')
  }
  TOF <- matrix(,nrow=length(Channel[,1]),ncol = 2)
  for (x in 1:length(Channel[,1])) {
    TOF[x,1] <- which.min(Channel[x,])
    if (as.numeric(TOF[x,1]) < 60 ) {TOF[x,2] <- "NA"} # I wasn't sure what to call things less than L1
    else if (as.numeric(TOF[x,1]) >= 60 & as.numeric(TOF[x,1]) < 90 ) {TOF[x,2] <- "L1"}
    else if (as.numeric(TOF[x,1]) >= 90 & as.numeric(TOF[x,1]) < 200 ) {TOF[x,2] <- "L2/L3"}
    else if (as.numeric(TOF[x,1]) >= 200 & as.numeric(TOF[x,1]) < 300 ) {TOF[x,2] <- "L4"}
    else if (as.numeric(TOF[x,1]) >= 300) {TOF[x,2] <- "Adult"}
  }
  StagesList <- list()
  StagesList[[1]] <- Channel[which(TOF[,2]=='L1'),]
  StagesList[[2]] <- Channel[which(TOF[,2]=='L2/L3'),]
  StagesList[[3]] <- Channel[which(TOF[,2]=='L4'),]
  StagesList[[4]] <- Channel[which(TOF[,2]=='Adult'),]
  return(StagesList)
}

#' Counts number of worms
#'
#' The function takes in the channel data and the wormIDs and counts how many good worms and bad worms in the specfied
#' stage are in the provided annotated set.
#'
#' @param StageData Stage Data containing the data set of worms in one stage (e.g. Adult)
#' @param WormIDs File containing annotated worm IDs of the channel data.
#' @return A string containing the number of good and bad worms
#' @export
WormCount <- function(StageData, WormIDs) {
  if(missing(StageData)){
    stop('Missing StageData input')
  }  else if(typeof(StageData) != 'list') {
    stop('StageData type is expected to be list, please provide the correct input type')
  }
  BadWorms <- StageData[which(rownames(StageData) %in% WormIDs[[2]]),]
  GoodWorms <- StageData[which(rownames(StageData) %in% WormIDs[[1]]),]
  output <- paste('Number of Good Worms: ',dim(GoodWorms)[1,],' Number of Bad Worms: ',dim(BadWorms)[1,],sep = '')
  return(output)
}

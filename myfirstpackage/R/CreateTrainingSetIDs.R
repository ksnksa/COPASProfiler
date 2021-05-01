#' Read the categorized worm IDs file
#'
#' The function takes in the channel data and the directory of the GoodIDs file. The GoodIDs
#' is an output of the app WormProfiler which includes the worm IDs and whether the worm is a good worm
#' or a bad worm. The output is a list containing the IDs of the good worms that are in the channel data, and the IDs
#' of the bad worms.
#'
#' @param StageChannelData Channel Data.
#' @param IndexDirectory Path to the GoodIDs file.
#' @return A list with Good worms IDs and Bad worm IDs.
#' @export
CreateTrainingSetIDs <- function(StageChannelData, IndexDirectory) {

  if(missing(StageChannelData)){
    stop('Missing StageChannelData input')
  } else if(missing(IndexDirectory)) {
    stop('Missing IndexDirectory input')
  } else if(typeof(StageChannelData) != 'list') {
    stop('StageChannelData type is expected to be list, please provide the correct input type')
  } else if(typeof(IndexDirectory) != 'character') {
    stop('IndexDirectory type is expected to be character, please provide the correct input type')
  }
  ##1 is good and 2 is bad
  GoodIDs <- read.delim(IndexDirectory, sep=',',header=TRUE)
  GoodWormIndex <- which(GoodIDs[2,] == 'Good')
  BadWormIndex <- which(GoodIDs[2,] == 'Bad')
  GoodWormIndex <- unname(GoodIDs[1,GoodWormIndex])
  BadWormIndex <- unname(GoodIDs[1,BadWormIndex])

  Output <- list()
  Output[[1]] <- matrix(GoodWormIndex[which(GoodWormIndex[1,] %in% rownames(StageChannelData))])
  Output[[2]] <- matrix(BadWormIndex[which(BadWormIndex[1,] %in% rownames(StageChannelData))])

  return(Output)

}

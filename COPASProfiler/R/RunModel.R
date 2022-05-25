#' Runs the model againts a data set
#'
#' The function takes in an SVM model and a worm data set and outputs a categorized list of worms.
#'
#' @param StageData Stage Data containing the data set of worms in one stage (e.g. Adult)
#' @param Model The SVM model
#' @return Returns the prediction of this model
#' @export
RunModel <- function(StageData, Model) {
  if(missing(StageData)){
    stop('Missing StageData input')
  }  else if(typeof(StageData) != 'list') {
    stop('StageData type is expected to be list, please provide the correct input type')
  }  else if(missing(Model)) {
    stop('Missing Model input')
  }  else if(typeof(Model) != 'list') {
    stop('Model type is expected to be list, please provide the correct input type')
  }
  if(!require("e1071")){
    library("e1071")
  }
  Output <- predict(Model, StageData)
  return(Output)
}

#' Get prediction from a training set
#'
#' The function takes in a list input that has the training set data and the prediction set data. The function then
#' runs the svm function to create the model and then runs the model against the prediction data. The function outputs
#' both the prediction of the model, and the model itself.
#'
#' @param SetListr List input that has training data set and prediction data set.
#' @return List containing the prediction and the model
#' @export
GetPrediction <- function(SetList) {

  if(missing(SetList)){
    stop('Missing SetList input')
  }  else if(typeof(SetList) != 'list') {
    stop('SetList type is expected to be list, please provide the correct input type')
  }
  if(!require("e1071")){
    library("e1071")
  }

  x <- lapply(SetList[[1]][,1:(ncol(SetList[[1]])-1)],as.numeric)
  m <- as.data.frame(x)
  y <- SetList[[1]]$Factor
  model <- svm(m, y,type='C-classification',
               scale=FALSE,
               kernel="linear")
  output <- list()
  output[[1]] <- predict(model, SetList[[2]])
  output[[2]] <- model
  return(output)

}


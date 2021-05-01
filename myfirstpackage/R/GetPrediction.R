#' Get prediction from a training set
#'
#' The function takes in a list input that has the training set data and the prediction set data. The function then
#' runs the svm function to get a prediction and returns it to the user.
#'
#' @param SetListr List input that has training data set and prediction data set.
#' @return Prediction output
#' @export
GetPrediction <- function(SetList) {

  if(missing(SetList)){
    stop('Missing SetList input')
  }  else if(typeof(SetList) != 'list') {
    stop('SetList type is expected to be list, please provide the correct input type')
  }
  x <- lapply(SetList[[1]][,1:(ncol(SetList[[1]])-1)],as.numeric)
  m <- as.data.frame(x)
  y <- SetList[[1]]$Factor
  model <- svm(m, y,type='C-classification',
               scale=TRUE,
               kernel="linear")
  output <- predict(model, SetList[[2]])

  return(output)

}


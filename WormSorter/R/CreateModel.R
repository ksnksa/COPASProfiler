#' Creates an SVM model from training set
#'
#' The function takes in the training set to create a model that clusters good worms and bad worms together.
#'
#' @param TrainingSet List input that has training data set and prediction data set.
#' @return The SVM model
#' @export
CreateModel <- function(TrainingSet) {

  if(missing(TrainingSet)){
    stop('Missing SetList input')
  }  else if(typeof(TrainingSet) != 'list') {
    stop('SetList type is expected to be list, please provide the correct input type')
  }
  if(!require("e1071")){
    library("e1071")
  }

  x <- lapply(SetList[[1]][,1:(ncol(SetList[[1]])-1)],as.numeric)
  m <- as.data.frame(x)
  y <- SetList[[1]]$Factor
  Model <- svm(m, y,type='C-classification',
               scale=TRUE,
               kernel="linear")
  return(Model)

}

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

  Factor <- as.double(TrainingSet[,dim(TrainingSet)[2]])
  Model <- svm(Factor~.,data = TrainingSet,
               type = 'C-classification',
               scale=FALSE,
               kernel="linear")
  return(Model)

}

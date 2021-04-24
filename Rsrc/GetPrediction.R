
require(e1071)
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


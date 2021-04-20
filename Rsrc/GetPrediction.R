

GetPrediction <- function(SetList) {
  #create if statements to make sure everything is correct type and stuff
  x <- lapply(SetList[[1]][,1:(ncol(SetList[[1]])-1)],as.numeric) 
  m <- as.data.frame(x)
  y <- SetList[[1]]$Factor #make y variable(dependent)
  model <- svm(m, y,type='C-classification',
               scale=TRUE,
               kernel="linear")
  pred <- predict(model, SetList[[2]])

  return(pred)
  
}


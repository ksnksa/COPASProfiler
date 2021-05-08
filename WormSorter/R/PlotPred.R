#' Plots the data set, the model and the prediction results
#'
#' The function takes in the prediction results and the Stage data and plots them. The resulting plots are as follows:
#' 1- Plots all the worm data 2- Plots the predicted bad worms 3- Plots the predicted good worms
#'
#' @param Prediction Prediction results from the RunModel function
#' @param StageData Stage Data containing the data set of worms in one stage (e.g. Adult)
#' @return Returns three plots
#' @export
PlotPred <- function(Prediction,StageData) {
  if(missing(Prediction)){
    stop('Missing Prediction input')
  }  else if(typeof(Prediction) != 'integer') {
    stop('Prediction type is expected to be integer, please provide the correct input type')
  }  else if(missing(StageData)) {
    stop('Missing StageData input')
  }  else if(typeof(StageData) != 'list') {
    stop('StageData type is expected to be list, please provide the correct input type')
  }
  if(!require("ggplot2")){
    library("ggplot2")
  }
  if(!require("reshape")){
    library("reshape")
  }
  Plots <- list()

  maxcol <- 0
  rowmean <- 0
  for (k in 1:dim(StageData)[1]) {
    temp2 <- as.numeric(which.min(StageData[k,]))
    if (maxcol > temp2) {
    } else {
      maxcol <- temp2
    }

  }
  for (m in 1:dim(StageData)[1]) {
    ID <- toString(rownames(StageData)[m])
    if (m == 1) {
      test <- data.frame(placeholder = as.numeric(StageData[m,1:maxcol]))
      names(test)[m] <- ID
    } else {
      test$placeholder <- as.numeric(StageData[m,1:maxcol])
      names(test)[m] <- ID
    }
  }
  clustermean <- data.frame(Length = 1:maxcol, Mean = rowMeans(test))
  test$Length <- 1:maxcol
  maxL = nrow(StageData)
  newdatafr <- melt(test, id.vars = "Length", measure.vars = c(rownames(StageData)[1:maxL]))
  stdmean <- 0
  for (j in nrow(StageData)) {
    stdmean <- stdmean + sd(StageData[j,])
  }
  stdmean <- stdmean/nrow(StageData)


  Plots[[1]] <- ggplot(newdatafr, aes(Length, value,col = variable)) +
    geom_line(color="grey") +
    geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') +
    theme_minimal() +
    ylab("amp") + xlab("") +
    ggtitle(paste(nrow(StageData),' Overall Worms',' std=',stdmean,sep=''))

  BadWorms <- StageData[which(Prediction == 1),]
  maxcol <- 0
  rowmean <- 0
  for (k in 1:dim(BadWorms)[1]) {
    temp2 <- as.numeric(which.min(BadWorms[k,]))
    if (maxcol > temp2) {
    } else {
      maxcol <- temp2
    }

  }
  for (m in 1:dim(BadWorms)[1]) {
    ID <- toString(rownames(BadWorms)[m])
    if (m == 1) {
      test <- data.frame(placeholder = as.numeric(BadWorms[m,1:maxcol]))
      names(test)[m] <- ID
    } else {
      test$placeholder <- as.numeric(BadWorms[m,1:maxcol])
      names(test)[m] <- ID
    }
  }
  clustermean <- data.frame(Length = 1:maxcol, Mean = rowMeans(test))
  test$Length <- 1:maxcol
  maxL = nrow(BadWorms)
  newdatafr <- melt(test, id.vars = "Length", measure.vars = c(rownames(BadWorms)[1:maxL]))
  stdmean <- 0
  for (j in nrow(BadWorms)) {
    stdmean <- stdmean + sd(BadWorms[j,])
  }
  stdmean <- stdmean/nrow(BadWorms)


  Plots[[2]] <- ggplot(newdatafr, aes(Length, value,col = variable)) +
    geom_line(color="grey") +
    geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') +
    theme_minimal() +
    ylab("amp") + xlab("") +
    ggtitle(paste(nrow(BadWorms),' Bad Worms',' std=',stdmean,sep=''))


  GoodWorms <- StageData[which(Prediction == 2),]
  maxcol <- 0
  rowmean <- 0
  for (k in 1:dim(GoodWorms)[1]) {
    temp2 <- as.numeric(which.min(GoodWorms[k,]))
    if (maxcol > temp2) {
    } else {
      maxcol <- temp2
    }

  }
  for (m in 1:dim(GoodWorms)[1]) {
    ID <- toString(rownames(GoodWorms)[m])
    if (m == 1) {
      test <- data.frame(placeholder = as.numeric(GoodWorms[m,1:maxcol]))
      names(test)[m] <- ID
    } else {
      test$placeholder <- as.numeric(GoodWorms[m,1:maxcol])
      names(test)[m] <- ID
    }
  }
  clustermean <- data.frame(Length = 1:maxcol, Mean = rowMeans(test))
  test$Length <- 1:maxcol
  maxL = nrow(GoodWorms)
  newdatafr <- melt(test, id.vars = "Length", measure.vars = c(rownames(GoodWorms)[1:maxL]))
  stdmean <- 0
  for (j in nrow(GoodWorms)) {
    stdmean <- stdmean + sd(GoodWorms[j,])
  }
  stdmean <- stdmean/nrow(GoodWorms)


  Plots[[3]] <- ggplot(newdatafr, aes(Length, value,col = variable)) +
    geom_line(color="grey") +
    geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') +
    theme_minimal() +
    ylab("amp") + xlab("") +
    ggtitle(paste(nrow(GoodWorms),' Good Worms',' std=',stdmean,sep=''))
  return(Plots)
}

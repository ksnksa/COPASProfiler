PlotPredSet <- function(PredSet,WormIDs) { 
  
  Plots <- list()
  BadWorms <- PredSet[which(rownames(PredSet) %in% WormIDs[[2]]),]
  GoodWorms <- PredSet[which(rownames(PredSet) %in% WormIDs[[1]]),]
  #BadWorms <- StageData[which(Prediction == 1),]
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
  
  
  Plots[[1]] <- ggplot(newdatafr, aes(Length, value,col = variable)) +
    geom_line(color="grey") +
    geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') +
    theme_minimal() +
    ylab("amp") + xlab("") +
    ggtitle(paste(nrow(BadWorms),' Bad Worms',' std=',stdmean,sep=''))
  
  
  #GoodWorms <- StageData[which(Prediction == 2),]
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
  
  
  Plots[[2]] <- ggplot(newdatafr, aes(Length, value,col = variable)) +
    geom_line(color="grey") +
    geom_line(aes(x=Length, y=Mean),clustermean, color = 'red') +
    theme_minimal() +
    ylab("amp") + xlab("") +
    ggtitle(paste(nrow(GoodWorms),' Good Worms',' std=',stdmean,sep=''))
  
  return(Plots)
}

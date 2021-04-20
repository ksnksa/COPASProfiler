


PlotAccuracy <- function(Accuracy, SetNumber, BadWormSampleSize) {
  #create if statements to make sure everything is correct type and stuff
  y <- matrix()
  std <- matrix()
  NumberOfGoodWorms <- matrix()
  for (x in 1:dim(Accuracy)[1]) {
    y[x] <- mean(as.numeric(Accuracy[x,]))
    std[x] <- sd(as.numeric(Accuracy[x,]))
    NumberOfGoodWorms[x] <- as.character(SetNumber[x])
  }
  df <- data.frame(Mean=y, sd=std, NumberOfGoodWorms = NumberOfGoodWorms)
  df <- df[dim(df)[1]:1,]
  df$NumberOfGoodWorms <- factor(df$NumberOfGoodWorms, levels=unique(df$NumberOfGoodWorms))
  plots <- ggplot(df,aes(x=NumberOfGoodWorms))+geom_boxplot(aes(lower=Mean-sd,upper=Mean+sd,middle=Mean,ymin=Mean-3*sd,ymax=Mean+3*sd),stat="identity") +
    theme_minimal() + theme(plot.title = element_text(size=15)) + 
    ylab("Accuracy %")  +
    ggtitle(paste('Accuracy with ',BadWormSampleSize,' bad worms.', sep = ''))
  return(plots)
  
}


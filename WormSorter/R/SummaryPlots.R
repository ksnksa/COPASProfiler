#' Plots the provided worm sorter output files into a summary boxplot
#'
#' The function takes in the data set directories, the names of the strains and plots the desired fluorescence channel.
#' The boxplot provides a summary of the amplitude of the fluorescence of each strain.
#'
#' @param FileDirectories Directories to the worm sorter summary output file. In the format of <- c('Directory1','Directory2') and so on.
#' @param Names Names of the strains that will be used in the plots. In the format of <- c('Name1','Name2') and so on.
#' @param FluorescenceChannel Which channel to plot: G is green, R is red, and Y is yellow.
#'
#' @return Returns a summary boxplot of the fluorescence.
#' @export
SummaryPlots <- function (FileDirectories,Names,FluorescenceChannel,Classify = 'NA',Measure = 'I',Scale = 'Normal',TypeOfData = 'Summary',MinMaxRange = c(50,800), ModelDirectory = '', Ranges = c(50,75,150,225,500,800)) {
  if(missing(FileDirectories)){
    stop('Missing FileDirectories input')
  }  else if(typeof(FileDirectories) != 'character') {
    stop('FileDirectories type is expected to be character, please provide the correct input type')
  } else if(missing(Names)){
    stop('Missing Names input')
  }  else if(typeof(Names) != 'character') {
    stop('Names type is expected to be character, please provide the correct input type')
  } else if(missing(FluorescenceChannel)){
    stop('Missing FluorescenceChannel input')
  }  else if(typeof(FluorescenceChannel) != 'character') {
    stop('FluorescenceChannel type is expected to be character, please provide the correct input type') }
  if(!require("ggplot2")){
    library("ggplot2")
  }
  if(!require("dplyr")){
    library("dplyr")
  }
  if(!require("reshape2")){
    library("reshape2")
  }
  ChannelSummary <- list()
  DataList <- list()
  if (TypeOfData == 'Summary') {
    for (x in 1:length(FileDirectories)) {
      ChannelSummary[[x]] <- read.delim(FileDirectories[x], header=TRUE)
    }

    if (Measure == 'I') {
      ColumnNumber <- c(12,13,14)
    } else if (Measure == 'W') {
      ColumnNumber <- c(19,22,25)
    } else if (Measure == 'H') {
      ColumnNumber <- c(18,21,24)
    } else {
      stop('Incorrect Measure input')
    }
    if (Classify == 'NA') {

    } else {
      print('Classification was not perfomed due to input data type -Summary File-')

    }
    for (z in 1:length(ChannelSummary)) {
      IDTOF <- matrix(nrow=which.max(ChannelSummary[[z]][,1]),ncol=8)
      IDTOF <- data.frame(IDTOF)
      colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
      IDTOF[,1] <- ChannelSummary[[z]][1:dim(IDTOF)[1],1]
      IDTOF[,2] <- ChannelSummary[[z]][1:dim(IDTOF)[1],10]
      IDTOF[,3] <- ChannelSummary[[z]][1:dim(IDTOF)[1],11]
      IDTOF[,4] <- ChannelSummary[[z]][1:dim(IDTOF)[1],ColumnNumber[1]]
      IDTOF[,5] <- ChannelSummary[[z]][1:dim(IDTOF)[1],ColumnNumber[2]]
      IDTOF[,6] <- ChannelSummary[[z]][1:dim(IDTOF)[1],ColumnNumber[3]]
      IDTOF[,7] <- ChannelSummary[[z]][1:dim(IDTOF)[1],15]



      Stages <- c('50-75','75-150','150-225','225-500','500-800')
      for (x in 1:dim(IDTOF)[1]) {
        if (as.numeric(IDTOF[x,'TOF']) < 50) {IDTOF[x,'Stage'] <- 'TooSmall'}

        else if (as.numeric(IDTOF[x,'TOF']) >= 50 & as.numeric(IDTOF[x,'TOF']) < 75) {IDTOF[x,'Stage'] <- Stages[1]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 75 & as.numeric(IDTOF[x,'TOF']) < 150) {IDTOF[x,'Stage'] <- Stages[2]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 150 & as.numeric(IDTOF[x,'TOF']) < 225) {IDTOF[x,'Stage'] <- Stages[3]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 225 & as.numeric(IDTOF[x,'TOF']) < 500) {IDTOF[x,'Stage'] <- Stages[4]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 500 & as.numeric(IDTOF[x,'TOF']) <= 800) {IDTOF[x,'Stage'] <- Stages[5]}
        else if (as.numeric(IDTOF[x,'TOF']) > 800) {IDTOF[x,'Stage'] <- 'TooBig'}
      }
      for (x in 1:dim(IDTOF)[1]) {
        IDTOF[x,'ID'] <- paste('X',IDTOF[x,'ID'],sep='')
      }
      #Removing worms with amplitude of 35000 or higher
      #Removing worms that are not in the size we care about
      IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooSmall'),]
      IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooBig'),]
      IDTOF <- IDTOF[-which(IDTOF[,'PH.EXT'] > 35000),]
      DataList[[z]] <- IDTOF
    }






  } else if (TypeOfData == 'FullFile') {
    for (z in 1:length(FileDirectories)) {
      FullFile <- read.delim(FileDirectories[z])
      NewCh0 <- FullFile
      for (x in 1:dim(FullFile)[1]) {
        NewCh0[,x] <- na.omit(FullFile[,x])

      }

      x <- NewCh0[,-which(grepl('.', colnames(NewCh0), fixed = TRUE))]
      ChExt <- t(x)
      ChExt <- ChExt[-dim(ChExt)[1],]
      IDTOF <- matrix(nrow=dim(ChExt)[1],ncol=8)
      IDTOF <- data.frame(IDTOF)
      colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
      x <- NewCh0[,which(grepl('.1', colnames(NewCh0), fixed = TRUE))]
      Ch1 <- t(x)
      #Ch1 <- Ch1[-dim(Ch1)[1],]
      rownames(Ch1) <- str_replace(rownames(Ch1),fixed('.1'),'')
      x <- NewCh0[,which(grepl('.2', colnames(NewCh0), fixed = TRUE))]
      Ch2 <- t(x)
      #Ch2 <- Ch2[-dim(Ch2)[1],]
      rownames(Ch2) <- str_replace(rownames(Ch2),fixed('.2'),'')
      x <- NewCh0[,which(grepl('.3', colnames(NewCh0), fixed = TRUE))]
      Ch3 <- t(x)

      #  Ch3 <- Ch3[-dim(Ch3)[1],]
      rownames(Ch3) <- str_replace(rownames(Ch3),fixed('.3'),'')
      IDTOF[,'ID'] <- rownames(ChExt)
      for (x in 1:dim(ChExt)[1]) {
        IDTOF[x,'TOF'] <- which.min(ChExt[x,])
      }
      if (Measure == 'I') {
        for (x in 1:dim(ChExt)[1]) {
          IDTOF[x,'EXT'] <- as.numeric(trapz(1:which.min(ChExt[x,]), ChExt[x,1:which.min(ChExt[x,])]))
          IDTOF[x,'G'] <- as.numeric(trapz(1:which.min(Ch1[x,]), Ch1[x,1:which.min(Ch1[x,])]))
          IDTOF[x,'Y'] <- as.numeric(trapz(1:which.min(Ch2[x,]), Ch2[x,1:which.min(Ch2[x,])]))
          IDTOF[x,'R'] <- as.numeric(trapz(1:which.min(Ch3[x,]), Ch3[x,1:which.min(Ch3[x,])]))
          IDTOF[x,'PH.EXT'] <-  max(ChExt[x,])
        }


      } else if (Measure == 'W') {
        stop('Measure type not currently supported')
      } else if (Measure == 'H') {
        for (x in 1:dim(ChExt)[1]) {
          IDTOF[x,'EXT'] <-  max(ChExt[x,])
          IDTOF[x,'PH.EXT'] <-  max(ChExt[x,])
          IDTOF[x,'G'] <- max(Ch1[x,])
          IDTOF[x,'Y'] <- max(Ch2[x,])
          IDTOF[x,'R'] <- max(Ch3[x,])
        }

      }

      Stages <- c('50-75','75-150','150-225','225-500','500-800')
      for (x in 1:dim(IDTOF)[1]) {
        if (as.numeric(IDTOF[x,'TOF']) < 50) {IDTOF[x,'Stage'] <- 'TooSmall'}

        else if (as.numeric(IDTOF[x,'TOF']) >= 50 & as.numeric(IDTOF[x,'TOF']) < 75) {IDTOF[x,'Stage'] <- Stages[1]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 75 & as.numeric(IDTOF[x,'TOF']) < 150) {IDTOF[x,'Stage'] <- Stages[2]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 150 & as.numeric(IDTOF[x,'TOF']) < 225) {IDTOF[x,'Stage'] <- Stages[3]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 225 & as.numeric(IDTOF[x,'TOF']) < 500) {IDTOF[x,'Stage'] <- Stages[4]}
        else if (as.numeric(IDTOF[x,'TOF']) >= 500 & as.numeric(IDTOF[x,'TOF']) <= 800) {IDTOF[x,'Stage'] <- Stages[5]}
        else if (as.numeric(IDTOF[x,'TOF']) > 800) {IDTOF[x,'Stage'] <- 'TooBig'}
      }
      #Removing worms with amplitude of 35000 or higher
      #Removing worms that are not in the size we care about
      if (sum(as.numeric(IDTOF[,'TOF']) < MinMaxRange[1]) == 0) {

      } else {
        IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooSmall'),]

      }

      if (sum(as.numeric(IDTOF[,'TOF']) > MinMaxRange[2]) == 0) {


      } else { IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooBig'),] }
      IDTOF <- IDTOF[-which(IDTOF[,'PH.EXT'] > 35000),]
      if (Classify == 'NA') {

      } else {
        WormIDs <- RunClassification(FileDirectories[z],ModelDirectory,35000,50,800,TypeOfData = 'FullFile')
        BadWormIndex <- WormIDs[[2]]
        #BadWormIndex <- unname(GoodIDs[1,BadWormIndex])
        index <- which(IDTOF[,'ID'] %in% BadWormIndex)
        Temp <- IDTOF[-index,]
        IDTOF<- Temp
      }
      DataList[[z]] <- IDTOF
    }
   }  else {
    stop('TypeOfData has the wrong input')
  }



  for (x in 1:length(DataList)) {
    if (x == 1) {  maxrow <- dim(DataList[[x]])[1]}

    if (maxrow < dim(DataList[[x]])[1]) {
      maxrow <- dim(DataList[[x]])[1]}
  }
  temp <- matrix(nrow=maxrow, ncol= length(DataList))
  temp <- as.data.frame(temp)


  if (FluorescenceChannel == 'G') {
    ChannelName <- 'Green'
  } else if ((FluorescenceChannel == 'R')) {
    ChannelName <- 'Red'
  } else if ((FluorescenceChannel == 'Y')) {
    ChannelName <- 'Yellow'
  }
  NewOrder <- rbind(1:length(DataList),1:length(DataList))

  for (x in 1:length(DataList)) {
    NewOrder[2,x] <- median(DataList[[x]][,FluorescenceChannel])}

  count <- 0
  x <- 1
  while (x <= length(DataList)) {
    if (length(which(NewOrder[2,x] == sort(NewOrder[2,], decreasing = TRUE))) == 1) {
      NewOrder[1,x] <- which(NewOrder[2,x] == sort(NewOrder[2,], decreasing = TRUE))
     x <- x + 1
    } else {
      for (z in 1:length(which(NewOrder[2,x] == sort(NewOrder[2,], decreasing = TRUE)))) {
        NewOrder[1,x + count] <- which(NewOrder[2,x] == sort(NewOrder[2,], decreasing = TRUE))[z]
        count <- count + 1
      }
      x <- x + count
      count <- 0

    }
  }

  NewOrder2 <- 1:length(DataList)
  for (x in 1:length(DataList)) {
    NewOrder2[x] <- which(NewOrder[1,] == x)
  }

  colnames(temp) <- Names[NewOrder2]

  for (x in NewOrder[1,]) {
    temp[1:dim(DataList[[x]])[1],Names[x]] <- DataList[[x]][1:dim(DataList[[x]])[1],FluorescenceChannel]
  }
  Means <- NewOrder[2,NewOrder[1,]]

  output <- ggplot(data = melt(temp), aes(x=variable,y=value)) +
    geom_boxplot(aes(fill=variable)) +
    ggtitle(paste('Summary of The ',ChannelName, ' Channel', sep = '')) +
    xlab('Strains') +
    ylab('Fluorescence (A.U)') +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title='Strains'))

  if (Scale == 'Log10') {
    output<-   output +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
  } else if (Scale == 'Log2') {
    output<-   output +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
  } else if (Scale == 'Normal') {
    output<-   output + ylab('Fluorescence (A.U)')
  }

  Plots <- list()

  Stages <- paste(Ranges[1],Ranges[2],sep = '-')
  for (m in 3:length(Ranges)) {
    Stages <- c(Stages,paste(Ranges[m-1],Ranges[m],sep = '-'))
  }
  rowcounter <- c()
  for (z in 1:length(DataList)) {
    rowcounter[z] <-  dim(DataList[[z]])[1]

  }
  temp <- matrix(nrow=sum(rowcounter),ncol=9)
  temp <- data.frame(temp)
  colnames(temp) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage','Strain')
  NewCount <- rowcounter[[1]]
  for (z in 1:length(DataList)) {
    if (z == 1) {

      temp[1:NewCount,'Strain'] <- Names[z]
      temp[1:NewCount,1:8] <- DataList[[z]]
    } else {

      temp[(NewCount + 1):(NewCount + rowcounter[[z]]),'Strain'] <- Names[z]
      temp[(NewCount + 1):(NewCount + rowcounter[[z]]),1:8] <-DataList[[z]]
      NewCount <- rowcounter[[z]] + NewCount
    }

  }
  for (x in 1:length(Stages)) {
    if (FluorescenceChannel == 'G') {
      temp2 <- temp
      temp2[,9] <- as.factor(temp2[,9])
      temp2 <- temp2[which(temp2[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp2, aes(y = G, x = TOF)) +
        geom_jitter(width=0.3,alpha=1,aes(color = Strain, text = temp2[,'ID'])) +
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
        geom_smooth(method='lm', aes(color = Strain)) +
        #geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,

        # method.args = list(start = list(a = 0.1, b = 0.1))) +
        xlab('TOF') +
        theme(plot.title = element_text(hjust = 0.5))
      if (Scale == 'Log10') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
      } else if (Scale == 'Log2') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
      } else if (Scale == 'Normal') {
        Plots[[x]]<-   Plots[[x]] + ylab('Fluorescence (A.U)')
      }
      # SummaryTable[x,1:6] <- summary(temp[,'G'])
      # SummaryTable[x,7] <- length(temp[,'ID'])
      Plots[[x]] <- ggplotly(Plots[[x]]  ,tooltip = 'all', dynamicTicks = TRUE)

    }
    if (FluorescenceChannel == 'Y') {
      temp2 <- temp
      temp2[,9] <- as.factor(temp2[,9])
      temp2 <- temp2[which(temp2[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp2, aes(y = Y, x = TOF)) +
        geom_jitter(width=0.3,alpha=1,aes(color = Strain, text = temp2[,'ID'])) +
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
        geom_smooth(method='lm', aes(color = Strain)) +
        #geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,

        # method.args = list(start = list(a = 0.1, b = 0.1))) +
        xlab('TOF') +
        theme(plot.title = element_text(hjust = 0.5))
      if (Scale == 'Log10') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
      } else if (Scale == 'Log2') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
      } else if (Scale == 'Normal') {
        Plots[[x]]<-   Plots[[x]] + ylab('Fluorescence (A.U)')
      }
      # SummaryTable[x,1:6] <- summary(temp[,'G'])
      # SummaryTable[x,7] <- length(temp[,'ID'])
      Plots[[x]] <- ggplotly(Plots[[x]]  ,tooltip = 'all', dynamicTicks = TRUE)

    }
    if (FluorescenceChannel == 'R') {
      temp2 <- temp
      temp2[,9] <- as.factor(temp2[,9])
      temp2 <- temp2[which(temp2[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp2, aes(y = R, x = TOF)) +
        geom_jitter(width=0.3,alpha=1,aes(color = Strain, text = temp2[,'ID'])) +
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
        geom_smooth(method='lm', aes(color = Strain)) +
        #geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,

        # method.args = list(start = list(a = 0.1, b = 0.1))) +
        xlab('TOF') +
        theme(plot.title = element_text(hjust = 0.5))
      if (Scale == 'Log10') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
      } else if (Scale == 'Log2') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
      } else if (Scale == 'Normal') {
        Plots[[x]]<-   Plots[[x]] + ylab('Fluorescence (A.U)')
      }
      # SummaryTable[x,1:6] <- summary(temp[,'G'])
      # SummaryTable[x,7] <- length(temp[,'ID'])
      Plots[[x]] <- ggplotly(Plots[[x]]  ,tooltip = 'all', dynamicTicks = TRUE)

    }
  }

  Plots[[x+1]] <- output





  # stat_summary(geom = "errorbar", fun = mean,  linetype = "dashed",width = 1)
  return(Plots)
}

#' Plots the provided worm sorter output file (x Axis is Time of Flight and Y Axis is the amplitude of fluorescence)
#'
#' The function takes in the data set directory, the name of the strain and plots the desired fluorescence channel.
#' The function separates the objects in the data set based on their Time of Flight (TOF) and the Ranges input (the default is 50,75,150,225,500,800)
#' and returns 5 plots of the objects between those ranges. The user can also provide specific ranges instead.
#' The function also returns a sixth boxplot containing a summary of the grouped objects.
#' The user can also remove specific objects by providing the optional input: IDsFile which is a csv file containing the IDs of the objects
#' the user wants to remove from the data set.
#'
#' @param FileDirectory Directory to the worm sorter summary output file.
#' @param Name Name of the strain that will be used in the plots.
#' @param FluorescenceChannel Which channel to plot: G is green, R is red, and Y is yellow.
#' @param IDsFile Optional input: Directory to csv file containing IDs of objects to be removed from data set before plotting.
#' @param Ranges The TOF ranges to group the objects with. Default is 50,75,150,225,500,800.
#'
#' @return Returns a summary boxplot of the fluorescence.
#' @export
SummaryPlot <- function(FileDirectory, Name, FluorescenceChannel,Ranges = c(50,75,150,225,500,800),
                        TypeOfData = 'Summary',Measure = 'H',Scale = 'Normal',WormIDs = 'NA',
                        FluoRange = 'NA', FluoThreshold = 'NA') {
  ### If statements to check its the correct file type and is not empty
  if(missing(FileDirectory)){
    stop('Missing FileDirectory input')
  }  else if(typeof(FileDirectory) != 'character') {
    stop('FileDirectory type is expected to be character, please provide the correct input type')
  } else if(missing(Name)){
    stop('Missing Name input')
  }  else if(typeof(Name) != 'character') {
    stop('Name type is expected to be character, please provide the correct input type')
  } else if(missing(FluorescenceChannel)){
    stop('Missing FluorescenceChannel input')
  }  else if(typeof(FluorescenceChannel) != 'character') {
    stop('FluorescenceChannel type is expected to be character, please provide the correct input type')
  }
  if(!require("ggplot2")){
    library("ggplot2")
  }
  if(!require("dplyr")){
    library("dplyr")
  }
  if(!require("reshape2")){
    library("reshape2")
  }



  if (TypeOfData == 'Summary') {
    ChannelSummary <- read.delim(FileDirectory, header=TRUE)
    IDTOF <- matrix(nrow=which.max(ChannelSummary[,1]),ncol=8)
    IDTOF <- data.frame(IDTOF)
    colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
    if (Measure == 'I') {
      ColumnNumber <- c(12,13,14)
    } else if (Measure == 'W') {
      ColumnNumber <- c(19,22,25)
    } else if (Measure == 'H') {
      ColumnNumber <- c(18,21,24)
    } else {
      stop('Incorrect Measure input')
    }
    IDTOF[,1] <- ChannelSummary[1:dim(IDTOF)[1],1]
    IDTOF[,2] <- ChannelSummary[1:dim(IDTOF)[1],10]
    IDTOF[,3] <- ChannelSummary[1:dim(IDTOF)[1],11]
    IDTOF[,4] <- ChannelSummary[1:dim(IDTOF)[1],ColumnNumber[1]]
    IDTOF[,5] <- ChannelSummary[1:dim(IDTOF)[1],ColumnNumber[2]]
    IDTOF[,6] <- ChannelSummary[1:dim(IDTOF)[1],ColumnNumber[3]]
    IDTOF[,7] <- ChannelSummary[1:dim(IDTOF)[1],15]
  } else if (TypeOfData == 'FullFile') {
    FullFile <- read.delim(FileDirectory)
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
    if (FluoRange[1] == 'NA') {
      if (Measure == 'I') {
        for (x in 1:dim(ChExt)[1]) {
          IDTOF[x,'EXT'] <- as.numeric(trapz(1:which.min(ChExt[x,]), ChExt[x,1:which.min(ChExt[x,])]))
          IDTOF[x,'G'] <- as.numeric(trapz(1:which.min(Ch1[x,]), Ch1[x,1:which.min(Ch1[x,])]))
          IDTOF[x,'Y'] <- as.numeric(trapz(1:which.min(Ch2[x,]), Ch2[x,1:which.min(Ch2[x,])]))
          IDTOF[x,'R'] <- as.numeric(trapz(1:which.min(Ch3[x,]), Ch3[x,1:which.min(Ch3[x,])]))
          IDTOF[x,'PH.EXT'] <-  max(ChExt[x,])
        }


      }
      else if (Measure == 'W') {
        stop('Measure type not currently supported')
      }
      else if (Measure == 'H') {
        for (x in 1:dim(ChExt)[1]) {
          IDTOF[x,'EXT'] <-  max(ChExt[x,])
          IDTOF[x,'PH.EXT'] <-  max(ChExt[x,])
          IDTOF[x,'G'] <- max(Ch1[x,])
          IDTOF[x,'Y'] <- max(Ch2[x,])
          IDTOF[x,'R'] <- max(Ch3[x,])
        }

      }
      else {
        stop('Incorrect Measure input')
      }
    } else if (length(FluoRange) != 2) {
      stop('Fluorescence Range input does not have two numeric ranges')


        } else if (FluoRange[2] > 50) {
      stop('Fluorescence Range second input is more than 50')

          } else if (FluoRange[1] < 0) {
      stop('Fluorescence Range first input is less than 0')

            } else if (FluoRange[1] > FluoRange[2]) {
      stop('Fluorescence Range first input is bigger than the second input range')

              } else if (FluoRange[1] < FluoRange[2]) {
      SecondaryFluRange <- c(FluoRange[1] + 50,FluoRange[2] +50)

      if (Measure == 'I') {
        for (x in 1:dim(ChExt)[1]) {
          IDTOF[x,'EXT'] <- as.numeric(trapz(1:which.min(ChExt[x,]), ChExt[x,1:which.min(ChExt[x,])]))
          MinandMax <- c(floor(which.min(ChExt[x,1:which.min(ChExt[x,])]) * FluoRange[1] /100),floor(which.min(ChExt[x,2:which.min(ChExt[x,])]) * FluoRange[2] /100))
          MinandMax2 <- c(floor(which.min(ChExt[x,1:which.min(ChExt[x,])]) * SecondaryFluRange[1] /100),floor(which.min(ChExt[x,2:which.min(ChExt[x,])]) * SecondaryFluRange[2] /100))
          # G
          TempMaxG1 <- max(Ch1[x,MinandMax[1]:MinandMax[2]])
          TempMaxG2 <- max(Ch1[x,MinandMax2[1]:MinandMax2[2]])
          if (TempMaxG1 > TempMaxG2 | TempMaxG1 == TempMaxG2 ) {
            IDTOF[x,'G'] <- as.numeric(trapz(MinandMax[1]:MinandMax[2], Ch1[x,MinandMax[1]:MinandMax[2]]))
          } else if (TempMaxG1 < TempMaxG2) {
            IDTOF[x,'G'] <- as.numeric(trapz(MinandMax2[1]:MinandMax2[2], Ch1[x,MinandMax2[1]:MinandMax2[2]]))
          }
          #Y
         # MinandMax <- c(floor(which.min(Ch2[x,2:length(Ch2[x,])]) * FluoRange[1] /100),floor(which.min(Ch2[x,2:length(Ch2[x,])]) * FluoRange[2] /100))
        #  MinandMax2 <- c(floor(which.min(Ch2[x,2:length(Ch2[x,])]) * SecondaryFluRange[1] /100),floor(which.min(Ch2[x,2:length(Ch2[x,])]) * SecondaryFluRange[2] /100))
          TempMaxY1 <- max(Ch2[x,MinandMax[1]:MinandMax[2]])
          TempMaxY2 <- max(Ch2[x,MinandMax2[1]:MinandMax2[2]])
          if (TempMaxY1 > TempMaxY2 | TempMaxY1 == TempMaxY2 ) {
            IDTOF[x,'Y'] <- as.numeric(trapz(MinandMax[1]:MinandMax[2], Ch2[x,MinandMax[1]:MinandMax[2]]))
          } else if (TempMaxY1 < TempMaxY2) {
            IDTOF[x,'Y'] <- as.numeric(trapz(MinandMax2[1]:MinandMax2[2], Ch2[x,MinandMax2[1]:MinandMax2[2]]))
          }
          #R
         # MinandMax <- c(floor(which.min(Ch3[x,2:length(Ch3[x,])]) * FluoRange[1] /100),floor(which.min(Ch3[x,2:length(Ch3[x,])]) * FluoRange[2] /100))
        #  MinandMax2 <- c(floor(which.min(Ch3[x,2:length(Ch3[x,])]) * SecondaryFluRange[1] /100),floor(which.min(Ch3[x,2:length(Ch3[x,])]) * SecondaryFluRange[2] /100))
          TempMaxR1 <- max(Ch3[x,MinandMax[1]:MinandMax[2]])
          TempMaxR2 <- max(Ch3[x,MinandMax2[1]:MinandMax2[2]])
          if (TempMaxR1 > TempMaxR2 | TempMaxR1 == TempMaxR2 ) {
            IDTOF[x,'R'] <- as.numeric(trapz(MinandMax[1]:MinandMax[2], Ch3[x,MinandMax[1]:MinandMax[2]]))
          } else if (TempMaxR1 < TempMaxR2) {
            IDTOF[x,'R'] <- as.numeric(trapz(MinandMax2[1]:MinandMax2[2], Ch3[x,MinandMax2[1]:MinandMax2[2]]))
          }

          #IDTOF[x,'Y'] <- as.numeric(trapz(1:which.min(Ch2[x,]), Ch2[x,1:which.min(Ch2[x,])]))
          #IDTOF[x,'R'] <- as.numeric(trapz(1:which.min(Ch3[x,]), Ch3[x,1:which.min(Ch3[x,])]))
          IDTOF[x,'PH.EXT'] <-  max(ChExt[x,])
        }


      } else if (Measure == 'W') {
        stop('Measure type not currently supported')
      } else if (Measure == 'H') {
        for (x in 1:dim(ChExt)[1]) {
          IDTOF[x,'EXT'] <-  max(ChExt[x,])
          IDTOF[x,'PH.EXT'] <-  max(ChExt[x,])


          MinandMax <- c(floor(which.min(ChExt[x,1:which.min(ChExt[x,])]) * FluoRange[1] /100),floor(which.min(ChExt[x,2:which.min(ChExt[x,])]) * FluoRange[2] /100))
          MinandMax2 <- c(floor(which.min(ChExt[x,1:which.min(ChExt[x,])]) * SecondaryFluRange[1] /100),floor(which.min(ChExt[x,2:which.min(ChExt[x,])]) * SecondaryFluRange[2] /100))
          # G
          TempMaxG1 <- max(Ch1[x,MinandMax[1]:MinandMax[2]])
          TempMaxG2 <- max(Ch1[x,MinandMax2[1]:MinandMax2[2]])
          if (TempMaxG1 > TempMaxG2 | TempMaxG1 == TempMaxG2 ) {
            IDTOF[x,'G'] <- TempMaxG1
          } else if (TempMaxG1 < TempMaxG2) {
            IDTOF[x,'G'] <- TempMaxG2
          }
          #Y
          TempMaxY1 <- max(Ch2[x,MinandMax[1]:MinandMax[2]])
          TempMaxY2 <- max(Ch2[x,MinandMax2[1]:MinandMax2[2]])
          if (TempMaxY1 > TempMaxY2 | TempMaxY1 == TempMaxY2 ) {
            IDTOF[x,'Y'] <- TempMaxY1
          } else if (TempMaxY1 < TempMaxY2) {
            IDTOF[x,'Y'] <- TempMaxY2
          }
          #R
          TempMaxR1 <- max(Ch3[x,MinandMax[1]:MinandMax[2]])
          TempMaxR2 <- max(Ch3[x,MinandMax2[1]:MinandMax2[2]])
          if (TempMaxR1 > TempMaxR2 | TempMaxR1 == TempMaxR2 ) {
            IDTOF[x,'R'] <- TempMaxR1
          } else if (TempMaxR1 < TempMaxR2) {
            IDTOF[x,'R'] <- TempMaxR2
          }

          #IDTOF[x,'G'] <- max(Ch1[x,])
          #IDTOF[x,'Y'] <- max(Ch2[x,])
          #IDTOF[x,'R'] <- max(Ch3[x,])
        }

      } else {
        stop('Incorrect Measure input')
      }

      } }


  Stages <- c(paste(Ranges[1],Ranges[2],sep = '-'),paste(Ranges[2],Ranges[3],sep = '-'),paste(Ranges[3],Ranges[4],sep = '-')
              ,paste(Ranges[4],Ranges[5],sep = '-'),paste(Ranges[5],Ranges[6],sep = '-'))
  for (x in 1:dim(IDTOF)[1]) {
    if ((sum(as.numeric(IDTOF[,'TOF']) < Ranges[1]) == 0)) {
    } else {
      if (as.numeric(IDTOF[x,'TOF']) < Ranges[1]) {IDTOF[x,'Stage'] <- 'TooSmall'}
    }
    if (as.numeric(IDTOF[x,'TOF']) >= Ranges[1] & as.numeric(IDTOF[x,'TOF']) < Ranges[2]) {IDTOF[x,'Stage'] <- Stages[1]
    } else if (as.numeric(IDTOF[x,'TOF']) >= Ranges[2] & as.numeric(IDTOF[x,'TOF']) < Ranges[3]) {IDTOF[x,'Stage'] <- Stages[2]
    } else if (as.numeric(IDTOF[x,'TOF']) >= Ranges[3] & as.numeric(IDTOF[x,'TOF']) < Ranges[4]) {IDTOF[x,'Stage'] <- Stages[3]
    } else if (as.numeric(IDTOF[x,'TOF']) >= Ranges[4] & as.numeric(IDTOF[x,'TOF']) < Ranges[5]) {IDTOF[x,'Stage'] <- Stages[4]
    } else if (as.numeric(IDTOF[x,'TOF']) >= Ranges[5] & as.numeric(IDTOF[x,'TOF']) <= Ranges[6]) {IDTOF[x,'Stage'] <- Stages[5]
    }
    if(((sum(as.numeric(IDTOF[x,'TOF']) > Ranges[6]) == 0))) {

    } else {
      if (as.numeric(IDTOF[x,'TOF']) > Ranges[6]) {IDTOF[x,'Stage'] <- 'TooBig'}
    }
  }
  if (TypeOfData == 'Summary') {
    for (x in 1:dim(IDTOF)[1]) {
      IDTOF[x,'ID'] <- paste('X',IDTOF[x,'ID'],sep='')
    }
  }


  #Removing worms with amplitude of 35000 or higher
  #Removing worms that are not in the size we care about
  if (sum(as.numeric(IDTOF[,'TOF']) < Ranges[1]) == 0) {

  } else {
    IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooSmall'),]

  }
  if (FluoThreshold == 'NA') {

  } else if (typeof(FluoThreshold) == 'double' & FluoThreshold > 0) {
    if (sum(which(IDTOF[,FluorescenceChannel] >= FluoThreshold)) == 0) {

    } else if (sum(which(IDTOF[,FluorescenceChannel] < FluoThreshold)) <= 1){
      stop('No worms left in data set. Change max fluorescence parameter.')
    } else {
      IDTOF <- IDTOF[-which(IDTOF[,FluorescenceChannel] >= FluoThreshold),]

    }
  } else {
    stop('FluoThreshold is not a number or is 0 or less')
  }
  if (sum(as.numeric(IDTOF[,'TOF']) > Ranges[6]) == 0) {


  } else { IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooBig'),] }

  if (sum(as.numeric(which(IDTOF[,'PH.EXT'] > 35000))) == 0) {

  } else {
    IDTOF <- IDTOF[-which(IDTOF[,'PH.EXT'] > 35000),]

  }

  if (WormIDs[[1]][1] == 'NA') {

  } else {
    GoodWormIndex <- WormIDs[[1]]
    #BadWormIndex <- unname(GoodIDs[1,BadWormIndex])
    index <- which(IDTOF[,'ID'] %in% GoodWormIndex)
    Temp <- IDTOF[index,]
    IDTOF<- Temp
  }

  IDTOF <- IDTOF %>% mutate(Stage = factor(Stage, levels = Stages))
  SummaryTable <- matrix(nrow=length(Ranges) , ncol = 7)
  SummaryTable <- as.data.frame(SummaryTable)
  colnames(SummaryTable) <- c('Min','1st Qu','Median','Mean','3rd Qu','Max','Count')
  RowNames <- matrix(nrow=1,ncol=length(Ranges))
  for (x in 1:(length(Ranges)-1)) {
    RowNames[x] <- paste('TOF: ',Ranges[x],'-',Ranges[x+1], sep='')
  }
  RowNames[length(Ranges)] <- 'All TOFs'
  rownames(SummaryTable) <- RowNames
  if (FluorescenceChannel == 'G') {

    TOFPlots <- ggplot(IDTOF, aes(Stage,G, label = TOF)) +
      #geom_jitter(width=0.3,alpha=0.2) +
      #geom_point(aes(text = IDTOF[,'ID']),alpha = 0) +
      geom_boxplot(color='green',alpha=1) +
      ggtitle(paste(Name,'Green Channel',sep = ' ')) +
      xlab('Stages (by TOF)') +
      theme(plot.title = element_text(hjust = 0.5))
    if (Scale == 'Log10') {
      TOFPlots <- TOFPlots +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
    } else if (Scale == 'Log2') {
      TOFPlots <-  TOFPlots +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
    } else if (Scale == 'Normal') {
      TOFPlots <-  TOFPlots + ylab('Fluorescence (A.U)')
    }

    TOFPlots <- ggplotly(TOFPlots, tooltip='all', dynamicTicks = TRUE)
    hoverinfo <- with(IDTOF, paste0('ID: ',IDTOF[,'ID'],sep=''))

    TOFPlots$x$data[[1]]$text <- hoverinfo
    TOFPlots$x$data[[1]]$hoverinfo <- c('text','boxes')
    SummaryTable[6,1:6] <- summary(IDTOF[,'G'])
    SummaryTable[6,7] <- length(IDTOF[,'ID'])
  } else if (FluorescenceChannel == 'Y') {

    TOFPlots <- ggplot(IDTOF, aes(Stage,Y, label = TOF)) +
      #geom_jitter(width=0.3,alpha=1) +
      geom_boxplot(color='yellow',alpha=1) +
      ggtitle(paste(Name,'Yellow Channel',sep = ' ')) +
      xlab('Stages (by TOF)') +
      theme(plot.title = element_text(hjust = 0.5))
    if (Scale == 'Log10') {
      TOFPlots <- TOFPlots +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
    } else if (Scale == 'Log2') {
      TOFPlots <- TOFPlots +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
    } else if (Scale == 'Normal') {
      TOFPlots <- TOFPlots + ylab('Fluorescence (A.U)')
    }
    TOFPlots <- ggplotly(TOFPlots, tooltip='all', dynamicTicks = TRUE)
    hoverinfo <- with(IDTOF, paste0('ID: ',IDTOF[,'ID'],sep=''))

    TOFPlots$x$data[[1]]$text <- hoverinfo
   TOFPlots$x$data[[1]]$hoverinfo <- c('text','boxes')
    SummaryTable[6,1:6] <- summary(IDTOF[,'Y'])
    SummaryTable[6,7] <- length(IDTOF[,'ID'])
  } else if (FluorescenceChannel == 'R') {
    TOFPlots <- ggplot(IDTOF, aes(Stage,R, label = TOF)) +
      #geom_jitter(width=0.3,alpha=1) +
      geom_boxplot(color='red',alpha=1) +
      ggtitle(paste(Name,'Red Channel',sep = ' ')) +
      xlab('Stages (by TOF)') +
      theme(plot.title = element_text(hjust = 0.5))
    if (Scale == 'Log10') {
      TOFPlots <- TOFPlots +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
    } else if (Scale == 'Log2') {
      TOFPlots <-  TOFPlots +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
    } else if (Scale == 'Normal') {
      TOFPlots <-  TOFPlots + ylab('Fluorescence (A.U)')
    }
    SummaryTable[6,1:6] <- summary(IDTOF[,'R'])
    SummaryTable[6,7] <- length(IDTOF[,'ID'])
    TOFPlots <- ggplotly(TOFPlots, tooltip='all', dynamicTicks = TRUE)
    hoverinfo <- with(IDTOF, paste0('ID: ',IDTOF[,'ID'],sep=''))

    TOFPlots$x$data[[1]]$text <- hoverinfo
    TOFPlots$x$data[[1]]$hoverinfo <- c('text','boxes')
  }

  temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
  Plots <- list()
  if (FluorescenceChannel == 'G') {
    for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = G, x = TOF)) +
        geom_jitter(aes( text = temp[,'ID']),width=0.3,alpha=1) +
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
        geom_smooth(method='lm') +
        xlab('TOF') +
        theme(plot.title = element_text(hjust = 0.5))
      if (Scale == 'Log10') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
      } else if (Scale == 'Log2') {
        Plots[[x]]<- Plots[[x]] +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
      } else if (Scale == 'Normal') {
        Plots[[x]]<-   Plots[[x]] + ylab('Fluorescence (A.U)')
      }
      SummaryTable[x,1:6] <- summary(temp[,'G'])
      SummaryTable[x,7] <- length(temp[,'ID'])
      Plots[[x]] <- ggplotly(Plots[[x]]  ,tooltip = 'all', dynamicTicks = TRUE)
      #Plot2 <- Plots[[2]]
    }
  } else if (FluorescenceChannel == 'Y') {for (x in 1:length(Stages)) {
    temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
    Plots[[x]] <- ggplot(temp, aes(y = Y, x = TOF)) +
      geom_jitter(aes( text = temp[,'ID']),width=0.3,alpha=1) +
      ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
      xlab('TOF') +
      theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method='lm')
    if (Scale == 'Log10') {
      Plots[[x]]<-  Plots[[x]] +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
    } else if (Scale == 'Log2') {
      Plots[[x]]<-   Plots[[x]] +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
    } else if (Scale == 'Normal') {
      Plots[[x]]<-   Plots[[x]] + ylab('Fluorescence (A.U)')
    }
    Plots[[x]] <- ggplotly(Plots[[x]],tooltip = 'all', dynamicTicks = TRUE)
    SummaryTable[x,1:6] <- summary(temp[,'Y'])
    SummaryTable[x,7] <- length(temp[,'ID'])
  }}else if (FluorescenceChannel == 'R') {for (x in 1:length(Stages)) {
    temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
    Plots[[x]] <- ggplot(temp, aes(y = R, x = TOF)) +
      geom_jitter(aes( text = temp[,'ID']),width=0.3,alpha=1) +
      ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
      xlab('TOF') +
      theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method='lm')
    if (Scale == 'Log10') {
      Plots[[x]]<-   Plots[[x]] +  scale_y_continuous(trans='log10') + ylab('Log10(Fluorescence (A.U))')
    } else if (Scale == 'Log2') {
      Plots[[x]]<-   Plots[[x]] +  scale_y_continuous(trans='log2') + ylab('Log2(Fluorescence (A.U))')
    } else if (Scale == 'Normal') {
      Plots[[x]]<-   Plots[[x]] + ylab('Fluorescence (A.U)')
    }
    Plots[[x]] <- ggplotly(Plots[[x]],tooltip = 'all', dynamicTicks = TRUE)
    SummaryTable[x,1:6] <- summary(temp[,'R'])
              SummaryTable[x,7] <- length(temp[,'ID'])
  }}
  Plots[[6]] <- TOFPlots
  Plots[[7]] <- SummaryTable
  Plots[[8]] <- IDTOF
  return(Plots)
}





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
SummaryPlot <- function(FileDirectory, Name, FluorescenceChannel,IDsFile,Ranges = c(50,75,150,225,500,800)) {
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
  } else if(missing(IDsFile)){
    if(!require("ggplot2")){
      library("ggplot2")
    }
    if(!require("dplyr")){
      library("dplyr")
    }
    if(!require("reshape2")){
      library("reshape2")
    }

    ChannelSummary <- read.delim(FileDirectory, header=TRUE)
    IDTOF <- matrix(nrow=which.max(ChannelSummary[,1]),ncol=8)
    IDTOF <- data.frame(IDTOF)
    colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
    IDTOF[,1] <- ChannelSummary[1:dim(IDTOF)[1],1]
    IDTOF[,2] <- ChannelSummary[1:dim(IDTOF)[1],10]
    IDTOF[,3] <- ChannelSummary[1:dim(IDTOF)[1],11]
    IDTOF[,4] <- ChannelSummary[1:dim(IDTOF)[1],12]
    IDTOF[,5] <- ChannelSummary[1:dim(IDTOF)[1],13]
    IDTOF[,6] <- ChannelSummary[1:dim(IDTOF)[1],14]
    IDTOF[,7] <- ChannelSummary[1:dim(IDTOF)[1],15]
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
    for (x in 1:dim(IDTOF)[1]) {
      IDTOF[x,'ID'] <- paste('X',IDTOF[x,'ID'],sep='')
    }
    #Removing worms with amplitude of 35000 or higher
    #Removing worms that are not in the size we care about
    if (sum(as.numeric(IDTOF[,'TOF']) < Ranges[1]) == 0) {

    } else {
      IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooSmall'),]

    }

    if (sum(as.numeric(IDTOF[,'TOF']) > Ranges[6]) == 0) {


    } else { IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooBig'),] }
    IDTOF <- IDTOF[-which(IDTOF[,'PH.EXT'] > 35000),]
    IDTOF <- IDTOF %>% mutate(Stage = factor(Stage, levels = Stages))
    if (FluorescenceChannel == 'G') {
      TOFPlots <- ggplot(IDTOF, aes(Stage,G, label = TOF)) +
        geom_jitter(width=0.3,alpha=0.2) +
        geom_boxplot(color='green',alpha=1) +
        ggtitle(paste(Name,'Green Channel',sep = ' ')) +
        xlab('Stages (by TOF)') +
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (FluorescenceChannel == 'Y') {
      TOFPlots <- ggplot(IDTOF, aes(Stage,Y, label = TOF)) +
      geom_jitter(width=0.3,alpha=1) +
      geom_boxplot(color='yellow',alpha=1) +
      ggtitle(paste(Name,'Yellow Channel',sep = ' ')) +
      xlab('Stages (by TOF)') +
      ylab('Fluorescence (A.U)') +
      theme(plot.title = element_text(hjust = 0.5))
      } else if (FluorescenceChannel == 'R') {
      TOFPlots <- ggplot(IDTOF, aes(Stage,R, label = TOF)) +
      geom_jitter(width=0.3,alpha=1) +
      geom_boxplot(color='red',alpha=1) +
      ggtitle(paste(Name,'Red Channel',sep = ' ')) +
      xlab('Stages (by TOF)') +
      ylab('Fluorescence (A.U)') +
      theme(plot.title = element_text(hjust = 0.5))}

    temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
    Plots <- list()
    if (FluorescenceChannel == 'G') {
      for (x in 1:length(Stages)) {
        temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
        Plots[[x]] <- ggplot(temp, aes(y = G, x = TOF)) +
          geom_jitter(width=0.3,alpha=1) +
          ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
          xlab('TOF') +
          ylab('Fluorescence (A.U)') +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (FluorescenceChannel == 'Y') {for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = Y, x = TOF)) +
        geom_jitter(width=0.3,alpha=1) +
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
        xlab('TOF') +
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }}else if (FluorescenceChannel == 'R') {for (x in 1:length(Stages)) {
      temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
      Plots[[x]] <- ggplot(temp, aes(y = R, x = TOF)) +
        geom_jitter(width=0.3,alpha=1) +
        ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
        xlab('TOF') +
        ylab('Fluorescence (A.U)') +
        theme(plot.title = element_text(hjust = 0.5))
    }}
    Plots[[6]] <- TOFPlots

    return(Plots)
  }  else if(typeof(IDsFile) != 'character') {
    stop('IDsFile type is expected to be character, please provide the correct input type')
  }



                ChannelSummary <- read.delim(FileDirectory, header=TRUE)
                IDTOF <- matrix(nrow=which.max(ChannelSummary[,1]),ncol=8)
                IDTOF <- data.frame(IDTOF)
                colnames(IDTOF) <- c('ID','TOF','EXT','G','Y','R','PH.EXT','Stage')
                IDTOF[,1] <- ChannelSummary[1:dim(IDTOF)[1],1]
                IDTOF[,2] <- ChannelSummary[1:dim(IDTOF)[1],10]
                IDTOF[,3] <- ChannelSummary[1:dim(IDTOF)[1],11]
                IDTOF[,4] <- ChannelSummary[1:dim(IDTOF)[1],12]
                IDTOF[,5] <- ChannelSummary[1:dim(IDTOF)[1],13]
                IDTOF[,6] <- ChannelSummary[1:dim(IDTOF)[1],14]
                IDTOF[,7] <- ChannelSummary[1:dim(IDTOF)[1],15]
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
                for (x in 1:dim(IDTOF)[1]) {
                  IDTOF[x,'ID'] <- paste('X',IDTOF[x,'ID'],sep='')
                }
                #Removing worms with amplitude of 35000 or higher
                #Removing worms that are not in the size we care about
                if (sum(as.numeric(IDTOF[,'TOF']) < Ranges[1]) == 0) {

                } else {
                  IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooSmall'),]

                }
                if (sum(as.numeric(IDTOF[,'TOF']) > Ranges[6]) == 0) {


                } else {
                  IDTOF <- IDTOF[-which(IDTOF[,'Stage'] == 'TooBig'),]
                }
                IDTOF <- IDTOF[-which(IDTOF[,'PH.EXT'] > 35000),]
                IDTOF <- IDTOF %>% mutate(Stage = factor(Stage, levels = Stages))
                #removing specific ids
                GoodIDs <- read.delim(IDsFile, sep=',',header=TRUE)
                BadWormIndex <- which(GoodIDs[2,] == 'Bad')
                BadWormIndex <- unname(GoodIDs[1,BadWormIndex])
                index <- which(IDTOF[,'ID'] %in% BadWormIndex)
                Temp <- IDTOF[-index,]
                IDTOF<- Temp
                if (FluorescenceChannel == 'G') {
                  TOFPlots <- ggplot(IDTOF, aes(Stage,G, label = TOF)) +
                    geom_jitter(width=0.3,alpha=0.2) +
                    geom_boxplot(color='green',alpha=1) +
                    ggtitle(paste(Name,'Green Channel',sep = ' ')) +
                    xlab('Stages (by TOF)') +
                    ylab('Fluorescence (A.U)') +
                    theme(plot.title = element_text(hjust = 0.5))
                } else if (FluorescenceChannel == 'Y') {
                  TOFPlots <- ggplot(IDTOF, aes(Stage,Y, label = TOF)) +
                    geom_jitter(width=0.3,alpha=1) +
                    geom_boxplot(color='yellow',alpha=1) +
                    ggtitle(paste(Name,'Yellow Channel',sep = ' ')) +
                    xlab('Stages (by TOF)') +
                    ylab('Fluorescence (A.U)') +
                    theme(plot.title = element_text(hjust = 0.5))
                } else if (FluorescenceChannel == 'R') {
                  TOFPlots <- ggplot(IDTOF, aes(Stage,R, label = TOF)) +
                    geom_jitter(width=0.3,alpha=1) +
                    geom_boxplot(color='red',alpha=1) +
                    ggtitle(paste(Name,'Red Channel',sep = ' ')) +
                    xlab('Stages (by TOF)') +
                    ylab('Fluorescence (A.U)') +
                    theme(plot.title = element_text(hjust = 0.5))}

                temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
                Plots <- list()
                if (FluorescenceChannel == 'G') {
                  for (x in 1:length(Stages)) {
                    temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
                    Plots[[x]] <- ggplot(temp, aes(y = G, x = TOF)) +
                      geom_jitter(width=0.3,alpha=1) +
                      ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
                      xlab('TOF') +
                      ylab('Fluorescence (A.U)') +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                } else if (FluorescenceChannel == 'Y') {for (x in 1:length(Stages)) {
                  temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
                  Plots[[x]] <- ggplot(temp, aes(y = Y, x = TOF)) +
                    geom_jitter(width=0.3,alpha=1) +
                    ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
                    xlab('TOF') +
                    ylab('Fluorescence (A.U)') +
                    theme(plot.title = element_text(hjust = 0.5))
                }}else if (FluorescenceChannel == 'R') {for (x in 1:length(Stages)) {
                  temp <- IDTOF[which(IDTOF[,'Stage'] == Stages[x]),]
                  Plots[[x]] <- ggplot(temp, aes(y = R, x = TOF)) +
                    geom_jitter(width=0.3,alpha=1) +
                    ggtitle(paste('Worms with TOF in the range of ',Stages[x], sep = '')) +
                    xlab('TOF') +
                    ylab('Fluorescence (A.U)') +
                    theme(plot.title = element_text(hjust = 0.5))
                }}
                Plots[[6]] <- TOFPlots

                return(Plots)


}


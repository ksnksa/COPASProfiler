#' Pre-process data 
#'
#' The function takes in the selected channel data and performs multiple pre-processing steps. 
#'1) Removing profiles outside of the provided time of flight range and profiles with optical density that exceed the provided
#'maximum amplitude input 
#'2) Trimming trailing zeros for each profile
#'3) Applying Piecewise Aggregate Approximation (PAA function from the jmotif library) to uniform the length of all profiles to the length of the minimum time of flight minus one. 
#'4) Performing Z score normalization on the Y axis using the scale function. 
#'
#' @param ChannelData Channel list output from the ReadChannel function 
#' @param MinTOF Minimum TOF accepted for each profile
#' @param MaxTOF Maximum TOF accepted for each profile
#' @param MaxAmp Maximum amplitude accepted for each profile
#' @param ChannelNumber Which channel to perform the pre-processing on. 1 is the optical density channel, 2, 3 and 4 are the green, yellow and red fluorescence channels respectively. 
#' @return Pre-processed channel data ready to train SVM model, to validate an SVM model and/or to be classified from an SVM model. 
#' @export
PreProcessData <- function(ChannelData,MinTOF,MaxTOF,MaxAmp,ChannelNumber){
  if(missing(ChannelData)){
    stop('Missing Channel list input')
  }  else if(typeof(ChannelData) != 'list') {
    stop('Channel list type is expected to be list, please provide the correct input type')
  } else if(missing(MinTOF)) {
    stop('Missing minimum time of flight input')
  } else if(missing(MaxTOF)) {
    stop('Missing maximum time of flight input')
  } else if(missing(MaxAmp)) {
    stop('Missing maximum optical density input')
  } else if(typeof(MinTOF)!='double') {
    stop('Minimum TOF type is expected to be double, please provide the correct input type')
  } else if(typeof(MaxTOF)!='double') {
    stop('Maximum TOF type is expected to be double, please provide the correct input type')
  } else if(typeof(MaxAmp)!='double') {
    stop('Maximum amplitude type is expected to be double, please provide the correct input type')
  } else if(missing(ChannelNumber)) {
    stop('Missing Channel Number input')
  } else if(typeof(ChannelNumber)!='double'){
    stop('Channel Number type is expected to be double, please provide the correct input type')
  }
  
  #Removing profiles outside of the provided TOF range and with amplitude higher than the maximum threshold 
  Index <- FilterChannel(ChannelData[[ChannelNumber]],MaxAmp,MinTOF,MaxTOF)
  NewData <- as.data.frame(ChannelData[[ChannelNumber]][-c(Index),])
  ModData <- NewData[,1:(MinTOF-1)]
  #Removing trailing zeros for each profile then applying the paa function to unfirom the length 
  for (row in 1:(dim(NewData)[1]-1)){
    
    ModData[row,] <- paa(as.numeric(NewData[row,1:((which(as.numeric(NewData[row,]) == 0)[1] -1))]),(MinTOF-1))
  }
  #applying the scale function to perform z score normalization on each profile 
  for (row in 1:dim(ModData)[1]) {
    ModData[row,] <- scale(as.numeric(ModData[row,]))
  }
  
  return(NewData)
}

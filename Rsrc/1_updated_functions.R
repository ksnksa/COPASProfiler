#Original name CreateVectors
## It has been updated so now returns lists instead of vectors to the environment
##My only concern is that having multiple vectors of different length will cause us problems when plotting the data... but we can try them as it is

CreateVectorsObtainList <- function(Filename1,Filename2) {
  if(missing(Filename1)){ #Not quite necessary but a nice addition for documentation :)
    stop('Missing First File Name')
  } else if(missing(Filename2)) {
    stop('Missing Second File Name')
  }
  else {
    vectors=list() ## instead of multiple vectors and placing them into the environment, let's just produce a list with them and return it as and ouput
    
    Idmatrix <- ReadData(Filename1)
    completematrix <- read.csv(Filename2,header=FALSE)
    # For loop creates variables for each ID name and saves the TOF data for it, and add it to vector list
    for (Id in 1:length(Idmatrix[,1])) { #it is easier to define the index like this instead of doing a which later
      Samplename <- paste('Sample',Idmatrix[Id,1]),sep='')
      
      columnnum <- Idmatrix[Id,1]
      #sometimes in the data set there's more values after the first 0 (then we can probably find a way to distinguish that too)
      rownum <- which.min(completematrix[, columnnum]) ## FIXED### which retrieves all matches. which.min retrieves the lowest index that is TRUE to the condition
      rownum <- rownum - 1 
      temp <- completematrix[2:rownum, columnnum]
      #assign(Samplename, temp)
      #create entry to the vector list with name "Samplename"
      vectors[[Samplename]] <- temp
      
    }
    
    #return vector list
    return(vectors)
  }
}


##Read data
#updated which.min, other than that it looks ok to me :)
ReadData <- function(Filename,TOFmin,TOFmax) {
  if(missing(Filename)){
    stop('Missing File Name')
  } else if(!require(dplyr)) {
    stop('dplyr Library is not installed')
  } else if(missing(TOFmin) | missing(TOFmax)) {
    Filenamecsv <- paste(Filename,'.csv',sep = '')
    completematrix <- read.csv(Filenamecsv)
    maxrow <- which.min(completematrix[,2]) ## again which will return multiple elements as long as they are TRUE for the imposed condition; which.min is better for this context
    completematrix <- completematrix %>%
      slice(1:(maxrow[1]-1)) %>%
      transmute(ID = Id, TOF = TOF, EXT = Extinction, GF = Green, YF = Yellow, RF = Red) 
    return(completematrix)
  } else {
    Filenamecsv <- paste(Filename,'.csv',sep ='')
    completematrix <- read.csv(Filenamecsv)
    maxrow <- which.max(completematrix[,2])
    completematrix <- completematrix %>%
      slice(1:(maxrow[1]-1)) %>%
      transmute(ID = Id, TOF = TOF, EXT = Extinction, GF = Green, YF = Yellow, RF = Red) %>%
      dplyr::filter(TOF >= TOFmin & TOF <= TOFmax)
    return(completematrix)
  }
}
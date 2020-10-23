ReadData <- function(Filename,TOFmin,TOFmax) {
	if(missing(Filename)){
		stop('Missing File Name')
	} else if(!require(dplyr)) {
		stop('dplyr Library is not installed')
	} else if(missing(TOFmin) | missing(TOFmax)) {
		Filenamecsv <- paste(Filename,'.csv',sep = '')
		completematrix <- read.csv(Filenamecsv)
		maxrow <- which.max(completematrix[,2]=='') #changed to which.max (which min does not work as intended as it gets the index 1 for some reason)
		completematrix <- completematrix %>%
		slice(1:(maxrow[1]-1)) %>%
		transmute(ID = Id, TOF = TOF, EXT = Extinction, GF = Green, YF = Yellow, RF = Red) 
		return(completematrix)
	} else {
		Filenamecsv <- paste(Filename,'.csv',sep ='')
		completematrix <- read.csv(Filenamecsv)
		maxrow <- which.max(completematrix[,2]=='') #changed to which.max
		completematrix <- completematrix %>%
		slice(1:(maxrow[1]-1)) %>%
		transmute(ID = Id, TOF = TOF, EXT = Extinction, GF = Green, YF = Yellow, RF = Red) %>%
		dplyr::filter(TOF >= TOFmin & TOF <= TOFmax)
		return(completematrix)
	}
}





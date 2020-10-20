ReadData <- function(Filename,TOFmin,TOFmax) {
	if(missing(Filename)){
		stop('Missing File Name')
	} else if(!require(dplyr)) {
		stop('dplyr Library is not installed')
	} else if(missing(TOFmin) | missing(TOFmax)) {
		Filenamecsv <- paste(Filename,'.csv',sep = '')
		completematrix <- read.csv(Filenamecsv)
		maxrow <- which(completematrix[,2]=='')
		completematrix <- completematrix %>%
		slice(1:(maxrow[1]-1)) %>%
		transmute(ID = Id, TOF = TOF, EXT = Extinction, GF = Green, YF = Yellow, RF = Red) 
		return(completematrix)
	} else {
		Filenamecsv <- paste(Filename,'.csv',sep ='')
		completematrix <- read.csv(Filenamecsv)
		maxrow <- which(completematrix[,2]=='')
		completematrix <- completematrix %>%
		slice(1:(maxrow[1]-1)) %>%
		transmute(ID = Id, TOF = TOF, EXT = Extinction, GF = Green, YF = Yellow, RF = Red) %>%
		dplyr::filter(TOF >= TOFmin & TOF <= TOFmax)
		return(completematrix)
	}
}





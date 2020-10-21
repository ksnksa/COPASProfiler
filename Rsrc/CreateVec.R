CreateVectors <- function(Filename1,Filename2) {
	if(missing(Filename1)){
		stop('Missing First File Name')
	} else if(missing(Filename2)) {
		stop('Missing Second File Name')
	}
		else {
		Idmatrix <- ReadData(Filename1)
		completematrix <- read.csv(Filename2,header=FALSE)
		# For loop creates variables for each ID name and saves the TOF data for it 
			for (Id in Idmatrix[,1]) {
				Samplename <- paste('Sample',Id,sep='')
	
				columnnum <- which(completematrix[1,]==Id)
				#sometimes in the data set there's more values after the first 0 (then we can probably find a way to distinguish that too)
				rownum <- which(completematrix[, columnnum]==0)
				rownum <- rownum - 1 
				temp <- completematrix[2:rownum, columnnum]
			assign(Samplename, temp)
			}

	}
}
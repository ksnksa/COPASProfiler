library(plyr)
library(ggplot2) #needs to be called
library(reshape) #good for dataframe shapping; probably you have it already as its a dependency of tidyverse
source(paste(getwd(),'/Rsrc/ReadDataFunc.R',sep=''))
Idmatrix <- ReadData(paste(getwd(),'/data/TestingReading',sep=''))
completematrix <- read.csv((paste(getwd(),'/data/TestN2.csv',sep='')), header=FALSE) #probably the best is to use header equal to true and then if IDs are needeed you can use colnames() to retrieve them. But for now I will leave it as it is

###Your approach below its quite inneficient although nice :). I will recommend you to use "apply" functions (please see its help section, i.e, ?apply) and use which.min instead searching for the first zero.
means=apply(completematrix,2,function(x){return(mean(x[2:(which.min(x)-1)]))})
IdMean <- data.frame('Id' = as.numeric(completematrix[1,]), 'Mean' = means)


# For loop that appends values of the Id and corresponding mean to IdMean
#IdMean <- data.frame('Id' = double(), 'Mean' = double())
#for (Id in Idmatrix[,1]) { #updated index 
#  columnnum <- which(completematrix[1,]==Id)
#  #sometimes in the data set there's more values after the first 0 (then we can probably find a way to distinguish that too)
#  rownum <- which.max(completematrix[, columnnum]==0) #changed to which max (which min always gives off 1 but which max); the problem with which.max is that it will retrieve always until the lastrow
#Sorry, I noticed the mistake I did at explaining myself. I meant:
rownum <- which.min(completematrix[, columnnum]) 
##this function should now retrieve the first zero.
#  rownum <- rownum - 1 
#  temp <- completematrix[2:rownum, columnnum]
#  temp_mean <- mean(temp)
#  temp_df <- data.frame('Id' = as.numeric(Id), 'Mean' = temp_mean)
#  IdMean <- rbind(IdMean,temp_df)
#}


#plotting
ggplot(data = IdMean, aes(x = Id, y = Mean)) + geom_point()

#While plotting the mean is a good way to see the overall signall across the worm, it will be much better to plot and use the profiles (also the mean signal could have been calculated already with the summary table).
# For example, lets print war profiles together.
##Start from making a dataframe withouth the headers
datafr = completematrix[-1,]
##Now lets assign properly the headers and rownames
colnames(datafr)=paste("ID",as.character(completematrix[1,]),sep="_")
rownames(datafr)=as.character(1:nrow(datafr))

#Sanity check
head(datafr)

#Finally, to plot via ggplot we need to add data for the x-axis (TOF, row number)
datafr$TOF=1:nrow(datafr)
#and "melt" the other variables
Newdatafr=melt(datafr,id.vars = "TOF", measure.vars =paste("ID",as.character(completematrix[1,]),sep="_"))

#Plot profiles.. under your own rsik!!! haha nah, it takes time 
ggplot(Newdatafr, aes(x=TOF, y=value, group=variable)) + geom_line() + theme(legend.position = "none")

##Graphs like these are ok for showing individual data, but the best will be to group by time of flight values and display the mean of the group. E.g.
groupmeandf=ddply(Newdatafr,"TOF",.fun=function(x,col){mean(x[[col]])},"value")
colnames(groupmeandf)[2]="meanSignal"

ggplot(groupmeandf, aes(x=TOF, y=meanSignal)) + geom_line() + geom_point() + xlab("TOF")

#######YOUR TASK IF YOU DARE TO ACCEPT IT WILL BE...####
###To make a function that produces dataframes with the profile files and another which plots the three different signals together
##Prefentially they should be in the look of:
#readProfiles(filenames, max.size, min.size, ...)
#producing either a single dataframe or four per each signal (EXT, Green, Yellow, and Red)
#Then it will be nice to have a function that produces an individual (or set of individuals) daframe that can be then used into ggplot, eg
# individual = extractprofile(ID,dataframes)
#plotprofileggplot(individual)

####Once we have that, I can show you how we can make a simple interface via the shiny library so users can input their own data and plot it.

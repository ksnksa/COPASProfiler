
library(dplyr)
completematrix <- read.csv('TestingReading.csv')

badid <- completematrix %>%
transmute(wrongid = Id, Length = TOF) %>%
dplyr::filter(Length <1000 | Length > 3000)

goodid <- completematrix %>%
transmute(GoodId = Id, Length = TOF) %>%
dplyr::filter(Length >= 1000 & Length <=3000)
#prints the id of the second item
print(goodid[2,1])
plot(goodid[,1],goodid[,2])
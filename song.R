setwd("~/R")
#load data
Final.2014<- read.csv("~/R/2014F.csv")

lm1<-lm(Points ~ Youtube + Language, data=Final.2014)
#both youtube and language more coralated with total points instead of jury/televote

Votes <- read.csv("~/R/Voting 50-50.csv")


song.quality<-data.frame(data.frame(matrix(NA, nrow = 26, 
                                          ncol = 51)))
song.quality<-Votes[1:26,]
song.quality<-song.quality[order(song.quality[2]),]
song.quality<-song.quality[,c("Country", country.names )]
x<-as.character(song.quality[,1])
z<-colnames(song.quality)
y<-z[2:40]
#this next line doesn't work, something with c?
adjustments.2014F<-country.average[c(x),c(y)]
song.quality[,2:40]<-song.quality[,2:40] + adjustments.2014F
song.quality$average<-rowMeans(song.quality[,2:40], na.rm=TRUE)


Final.2014$song.quality<-
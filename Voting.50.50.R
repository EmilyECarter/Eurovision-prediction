#install psych package before run
setwd("~/R")
#load data
Voting.50.50 <- read.csv("~/R/Voting 50-50.csv")
#turn points into ranks by countries, voting
Voting.50.50[,3:49][Voting.50.50[,3:49] <1]<-0
Voting.50.50[,3:49][Voting.50.50[,3:49] == 10]<-9
Voting.50.50[,3:49][Voting.50.50[,3:49] == 12]<-10
Voting.50.50[,3:49]<-11-Voting.50.50[,3:49]

#apply(Voting.50[,3:49], c(1,2), function(x) if(x>0){11-x})
#not sure about above line, need to fix, only if want to use apply


#make average rank column
num.rows<-nrow(Voting.50.50)
for (i in 1:num.rows){
  Voting.50.50$Average[i]<-rowMeans(Voting.50.50[i,3:50], na.rm=TRUE)
}
#subtract in each case how the country devded from mean, postive means ranked 
#higher, negitive means ranked lower 
for(i in 1:num.rows){
  for (j in 3:50){
    x1<-paste0(Voting.50.50[i,2])
    yy<-colnames(Voting.50.50)
    y1<-paste0(yy[j])
    if(x1==y1){
      Voting.50.50[i,j]<-NA
    }
    #works for some, but not all, no idea why
    else
      samp<-Voting.50.50$Average[i]-Voting.50.50[i,j]
      Voting.50.50[i,j]<-samp
    }
  }
#replace nothing with NA
Voting.50.50[,3:49][is.na(Voting.50.50[,3:49])]<-NA
Voting.50.50[,3:49][Voting.50.50[,3:49]==0] <- NA
#fix country names, so all match
Voting.50.50$Country<-as.character(Voting.50.50$Country)
Voting.50.50$Country[Voting.50.50$Country=="Czech Republic"]<-"Czech.Republic"
Voting.50.50$Country[Voting.50.50$Country=="United Kingdom"]<-"United.Kingdom"
Voting.50.50$Country[Voting.50.50$Country=="San Marino"]<-"San.Marino"
colnames(Voting.50.50)[50]<-"United.Kingdom"
#data is fairly normaly distbuted

#Create a list of country names
country.names<-c("Albania","Armenia","Austria","Azerbaijan","Belarus","Belgium",
"Cyprus", "Czech.Republic","Denmark", "Estonia","Finland", "France", "Macedonia"
,"Georgia", "Germany", "Greece","Hungary","Iceland","Ireland","Israel","Italy",
"Latvia","Lithuania","Malta","Moldova","Montenegro","Netherlands","Norway",
"Poland","Portugal","Romania","Russia","San.Marino","Serbia","Slovenia","Spain",
"Sweden","Switzerland","United.Kingdom")
num.countries<-length(country.names)
#create a new table for averages with list as rownames and column names
country.average<-data.frame(data.frame(matrix(NA, nrow = num.countries, 
 ncol = num.countries)))
colnames(country.average)<-country.names
rownames(country.average)<-country.names

#find the average voting from each country x axis countries voting y axis who they
#voted for, ignore czech republic, as never recived votes in 50.50
#change to make more general for czech republic
for (i in 1:num.countries){
  for (j in 1:num.countries){
    x<-paste0(country.names[i])
    y<-paste0(country.names[j])
    z<-subset(Voting.50.50,Country==x)
    if(x=="Czech.Republic"){
      country.range<-NA
     }
    else{
      if(y %in% country.names){
        w<-z[paste0(y)]
        country.average[i,j]<-colMeans(w, na.rm=TRUE)}
      else{
        country.average[i,j]<-NA
      }
      if(x==y){
        country.average[i,j]<-NA
      }
    }
  }
}

#do the same thing for range, again remember czech republic, only 1 data point
country.range<-data.frame(data.frame(matrix(NA, nrow = num.countries, 
                                              ncol = num.countries)))
colnames(country.range)<-country.names
rownames(country.range)<-country.names
for (i in 1:num.countries){
  for (j in 1:num.countries){
    x<-paste0(country.names[i])
    y<-paste0(country.names[j])
    z<-subset(Voting.50.50,Country==x)
    if(x=="Czech.Republic" | y=="Czech.Republic"){
      country.range[i,j]<-NA
    }
    else{
     if(y %in% country.names){
      w<-z[paste0(y)]
      country.range[i,j]<-max(w, na.rm=TRUE)-min(w, na.rm=TRUE)}
      else{
      country.range[i,j]<-NA
      }
      if(x==y){
        country.range[i,j]<-NA
      }
    }
  }
}
#do the same thing but for standard deviation
#fix to make more general
country.sd<-data.frame(data.frame(matrix(NA, nrow = num.countries, 
                                            ncol = num.countries)))
colnames(country.sd)<-country.names
rownames(country.sd)<-country.names
for (i in 1:num.countries){
  for (j in 1:num.countries){
    x<-paste0(country.names[i])
    y<-paste0(country.names[j])
    z<-subset(Voting.50.50,Country==x)
    if(x=="Czech.Republic" | y=="Czech.Republic"){
      country.sd[i,j]<-NA
    }
    else{
      if(y %in% country.names){
        w<-z[paste0(y)]
        country.sd[i,j]<-SD(w, na.rm=TRUE)}
      else{
        country.sd[i,j]<-NA
      }
      if(x==y){
        country.sd[i,j]<-NA
      }
    }
  }
}
#simulate and put responces into new chart
#figure out what to do about NA's create list, run other program on Jury/public 
#to see if have enough data???
#maybe also chi test.goodness of fit test for normal dist?

#randomly generate possible points, figure this out more later
song.quality=0
#loop for one country to simulate voting paterns based on voting habits
country.votes<-data.frame(data.frame(matrix(NA, nrow = num.countries, 
                                    ncol = num.countries)))
rownames(country.votes)<-country.names
colnames(country.votes)<-country.names
num.sim<-10
country.list<-list()
#create new datframe for each country (j) modeling how they would vote for every
#other country(i)
for (j in 1:num.countries){
  country.list[[j]]<-data.frame(data.frame(matrix(0, nrow = num.countries, 
                                                ncol = num.sim)))
}
for (i in 1:num.countries){
  for (j in 1:num.countries){
    rownames(country.list[[j]])<-country.names
    x<-country.average[i,j]
    y<-country.sd[i,j]
    if (is.na(y) | (is.na(y))){
      country.list[[j]][i,]<-NA
    }
    else{
      country.list[[j]][i,]<-rnorm(num.sim, mean=x+song.quality, sd=y) 
    }
  }
}
#if column all NA (czech republic change to all 0)
#now rank each column, columns are each simulation
for (i in 1:num.sim ){
  for (j in 1:num.countries){
    if (all(is.na(country.list[[j]][i]))){
        country.list[[j]][i]<-0}
    else{
      rank.votes<-as.list(country.list[[j]])
      samp<-rank(-rank.votes[[i]])
      country.list[[j]][i]<-samp
    }
  }
}
#now ranked, assign each points
for (j in 1:num.countries){
  country.list[[j]][country.list[[j]]>10]<-NA
  country.list[[j]][country.list[[j]]==0]<-NA  
  country.list[[j]]<-11-country.list[[j]]
  country.list[[j]][country.list[[j]]==10]<-12
  country.list[[j]][country.list[[j]]==9]<-10
  country.list[[j]][is.na(country.list[[j]])]<-0
  country.votes[j]<-rowMeans(country.list[[j]])
}
country.votes$average<-rowMeans(country.votes)
country.votes$rank<-rank(country.votes$average)
View(country.votes)
#again maybe look to other forms of voting
#YAY 
#make song quality into dataframe so each country voting on same song
#look at orginal san marino spread sheet, might need to fix
#all places with just czech republic, generalise more to work with jury votes and
#public votes

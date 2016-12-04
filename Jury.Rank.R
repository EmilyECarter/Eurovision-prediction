setwd("~/R")
library("psych", lib.loc="/Library/Frameworks/R.framework/
        Versions/3.1/Resources/library")
#load data
Voting.50.50 <- read.csv("~/R/Voting 50-50.csv")
#turn points into ranks by countries, voting
Voting.50.50[,3:49][Voting.50.50[,3:49] == 10]<-9
Voting.50.50[,3:49][Voting.50.50[,3:49] == 12]<-10
Voting.50.50[,3:49]<-11-Voting.50.50[,3:49]

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
Voting.50.50[is.nan(Voting.50.50)] <- NA
#fix country names, so all match
Voting.50.50$Country<-as.character(Voting.50.50$Country)
Voting.50.50$Country[Voting.50.50$Country=="Czech Republic"]<-"Czech.Republic"
Voting.50.50$Country[Voting.50.50$Country=="United Kingdom"]<-"United.Kingdom"
colnames(Voting.50.50)[50]<-"United.Kingdom"
#data is fairly normaly distbuted

#Create a list of country names
country.names<-c("Albania","Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium",
"Cyprus", "Czech.Republic","Denmark", "Estonia","Finland", "France", "Macedonia"
,"Georgia", "Germany", "Greece","Hungary","Iceland","Ireland","Israel","Italy",
"Latvia","Lithuania","Malta","Moldova","Montenegro","Netherlands","Norway",
"Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovenia","Spain",
"Sweden","Switzerland","United.Kingdom")
num.countries<-length(country.names)
#create a new table for averages with list as rownames and column names
country.average<-data.frame(data.frame(matrix(NA, nrow = num.countries, 
 ncol = num.countries)))
colnames(country.average)<-country.names
rownames(country.average)<-country.names

#find the average voting from each country, x axis countries voting, y axis who they
#voted for, ignore czech republic, as never recived votes in 50.50
for (i in 1:num.countries){
  for (j in 1:num.countries){
    x<-paste0(country.names[i])
    y<-paste0(country.names[j])
    z<-subset(Voting.50.50,Country==x)
    if(x=="Czech.Republic"){
      country.range<-NA
     }
    else{
      if(y %in% voting.countries){
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
     if(y %in% voting.countries){
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
View(country.range)
#do the same thing but for standard deviation
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
      if(y %in% voting.countries){
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
View(country.sd)
#do bith avaergae and sd for percentiles
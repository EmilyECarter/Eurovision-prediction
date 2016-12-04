#install psych package before run
setwd("~/R")
#load data
Voting.50.50 <- read.csv("~/R/Voting 50-50.csv")
#turn points into ranks by countries, voting
Voting.50.50[,3:50]<-11-Voting.50.50[,3:50]
Voting.50.50[,3:50][Voting.50.50[,3:50] ==11]<-0
Voting.50.50[,3:50][Voting.50.50[,3:50] == 1]<-2
Voting.50.50[,3:50][Voting.50.50[,3:50] == -1]<-1
Voting.50.50[,3:50][is.na(Voting.50.50[,3:50])]<-0

#split the list into indivdual contests
contest<-split(Voting.50.50, f=Voting.50.50$Year)

cor<-data.frame(data.frame(matrix(NA, nrow = 48, ncol = 48)))
colnames(cor)<-colnames(Voting.50.50)[3:50]
rownames(cor)<-colnames(Voting.50.50)[3:50]
for (i in 3:50){
  for (j in 3:50){
    if (i==j){ cor[i-2,j-2]<-NA
      }
    else if (all(contest[[34]][i]==0) | all(contest[[34]][j]==0)){
      cor[i-2,j-2]<-NA
    }
    else{
      samp<-cor.test(contest[[k]][[i]], contest[[k]][[j]],
                     alternative =c("greater"), method=c("kendall"))
      cor[i-2,j-2]<-samp
    }
  }
}

# exact p vaule is given, of the corralation between the two
#can specifcy p vaule if looking for exact cut-off
#only for one contest currently.

cor<-data.frame(data.frame(matrix(NA, nrow = 48, ncol = 48)))
colnames(cor)<-colnames(Voting.50.50)[3:50]
rownames(cor)<-colnames(Voting.50.50)[3:50]
for (i in 3:50){
  for (j in 3:50){
    for (k in 1:34){
    if (i==j){ cor[i-2,j-2]<-NA
    }    
    else if (all(contest[[k]][i]==0) | all(contest[[k]][j]==0)){
      cor[i-2,j-2]<-NA
    }
    else{
      samp<-rep(list(list()),34)
      samp[[k]]<-cor.test(contest[[k]][[i]], contest[[k]][[j]],
                      alternative =c("greater"), method=c("kendall"), exact = FALSE)

      }
    }
      y<-vector()
      y<-c(paste0(samp[[1]][3]), paste0(samp[[2]][3]), paste0(samp[[3]][3]), 
           paste0(samp[[4]][3]), paste0(samp[[5]][3]), paste0(samp[[6]][3]),
           paste0(samp[[7]][3]), paste0(samp[[8]][3]), paste0(samp[[9]][3]), 
           paste0(samp[[10]][3]), paste0(samp[[11]][3]), paste0(samp[[12]][3]), 
           paste0(samp[[13]][3]), paste0(samp[[14]][3]), paste0(samp[[15]][3]), 
           paste0(samp[[16]][3]), paste0(samp[[17]][3]), paste0(samp[[18]][3]), 
           paste0(samp[[19]][3]), paste0(samp[[20]][3]), paste0(samp[[21]][3]), 
           paste0(samp[[22]][3]), paste0(samp[[23]][3]), paste0(samp[[24]][3]), 
           paste0(samp[[25]][3]), paste0(samp[[26]][3]), paste0(samp[[27]][3]), 
           paste0(samp[[28]][3]), paste0(samp[[29]][3]), paste0(samp[[30]][3]), 
           paste0(samp[[31]][3]), paste0(samp[[32]][3]), paste0(samp[[33]][3]), 
           paste0(samp[[34]][3]), )
      y<-as.numeric(y)
    if (length(y)>1) {
      y<-paste0(sumlog(y))
      cor[i-2,j-2]<-y[3]
    }
      #samp is for one year coralation, need to average for all years 
  }
}
rm(list = ls())
source("~/Desktop/jotarepos/inoutrab/codes/jarabe.R")
load("~/Desktop/jotarepos/inoutrab/data/datax.Rda")

# U
colu.u<-c(2:79)

# O
colu.o<-c(80:157)

## din_all stores decisions
din_all<-data.frame((matrix(NA, nrow = dim(dtick_w)[1], ncol = dim(dtick_w)[2]-1)))
din_all$tick<-dtick_w$tick
# we assume that first four periods the robot is IN. 
din_all[c(1:4),c(2:dim(dtick_w)[2])]<-1
colnames(din_all)<-colnames(dtick_w)

# Important parameters
rho<-.85
rf<- -8
ini.con<- 100
### for FMT .21 , .41 and all zero for RE

for(s in colu.o){
  xse<-dtick_w[,s]-ini.con
  xf<-fmt.o.forecast(xse,0.21,rho,0.41,2)
  belief.xo<-c(xse[1:4],xf)
  
  for(t in seq(4,(length(xse)-1))){
    aa<-din_all[t,s]
    din_all[t+1,s]<-fmt.decision(belief.xo[t+1], rf,aa)
  }
}


for(s in colu.u){
  xse<-dtick_w[,s]-ini.con
  beliefs<-rep(NA,length(xse)) #this vector fills the missing values with the RE belief 
  fmt.beliefs<-rep(NA,length(xse)) #this vector stores FMT beliefs
  fmt.beliefs[1:4]<-xse[1:4]
  beliefs[1:4]<-xse[1:4]

  for(t in seq(4,(length(xse)-1))){
    if(din_all[t,s]==0){
        beliefs[t]<- rho*beliefs[t-1]}
    else
    {beliefs[t]<- xse[t]}
    xf_u<-fmt.u.forecast(beliefs[1:t],din_all[1:t,s],0.21,rho,0.41,2)
    fmt.beliefs[t]<-xf_u
    din_all[t+1,s]<-fmt.decision(xf_u,rf,1)
}
}
din_all$tick<-c(1:160)
#save(din_all,file="rational.Rda")
din_all_lmt<-din_all
save(din_all_lmt,file="rational_lmt.Rda")


##append to initial data_set
rm(list = ls())
library(tidyr)
load("~/Desktop/jotarepos/inoutrab/data/rational_lmt.Rda")
load("~/Desktop/jotarepos/inoutrab/data/inoutdata.Rda")
din_all<-din_all_lmt
d1<-dim(din_all)[2]
data_long <- gather(din_all, "ids", "pre_decision",2:d1)
datainout<-merge(df,data_long,by=c("ids","tick"))
datafinal <- gather(datainout, "player", "choice",7:17)
datafinal$tre[datafinal$tre==13]<-1
datafinal$tre[datafinal$tre==23]<-0
save(datafinal,file="datafinal.Rda")
datafinal<-datafinal[!is.na(datafinal$choice),]

#write.csv(datafinal,file="datainout.csv")

#plot(dtick_w[,5]-100,col="blue",lty=2,type="l")
#lines(rf*(1-din_all[,5]))
#lines(rf*df$p2_decision[df$ids==130106],col="red")
sum(din_all[colu.u])/(160*length(colu.u))
sum(din_all[colu.o])/(160*length(colu.o))
sum(df[df$tre==13,c(6:15)])/(dim(df[df$tre==13,c(6:15)])[1])
sum(df[df$tre==23,c(6:15)])/(dim(df[df$tre==23,c(6:15)])[1])






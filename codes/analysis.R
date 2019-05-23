### Check whethere there is any difference between RE and LMT. 
rm(list = ls())
load("~/Desktop/jotarepos/inoutrab/data/datax.Rda")
#load("~/Desktop/jotarepos/inoutrab/data/rational.Rda")
load("~/Desktop/jotarepos/inoutrab/data/rational_lmt.Rda")

corr<-NULL

for(s in c(2:dim(din_all)[2])){
  temp<-cor(din_all[,s],din_all_lmt[,s])
  corr<-cbind(corr,temp)
}
hist(corr)

colnames(corr)<-names(din_all_lmt)[2:dim(din_all)[2]]
colnames(corr)[corr<.8]
plot(1:160,dtick_w[,"130115"],ylim=c(0,200),type="l")
points(din_all[,"130115"]*50)
points(din_all_lmt[,"130115"]*50,col="red",pch=21,cex=.3)

##Go and work with RE. 
## Analyze whether 

####
rm(list = ls())
load("~/Desktop/jotarepos/inoutrab/data/datafinal.Rda")
d<-datafinal
d<-d[!is.na(d$choice),]

summary(lm(d$pre_decision ~ d$tre))
summary(lm(d$choice ~ d$tre))
summary(lm(d$choice[d$round_number>9] ~ d$tre[d$round_number>9]))

write.csv(d,file="datainout.csv")

corr<-NULL
for(s in c(2:dim(din_all)[2])){
  temp<-cor(din_all[,s],din_all_lmt[,s])
  corr<-cbind(corr,temp)
}
hist(corr)

rango<-1:dim(d)[1]-1
rango1<-2:dim(d)[1]
attach(d)
d <- d[order(ids, player, tick),]
d$flag<-0
d$flag[rango1]<-d$choice[rango1]-d$choice[(rango)]

d$x_exit<- d$x_t*d$flag*-1
d$x_exit[d$x_exit<1]<-NA

d$x_enter<- d$x_t*d$flag
d$x_enter[d$x_enter<1]<-NA
d$player[d$player=="p1_decision"]<-1
d$player[d$player=="p2_decision"]<-2
d$player[d$player=="p3_decision"]<-3
d$player[d$player=="p4_decision"]<-4
d$player[d$player=="p5_decision"]<-5
d$player[d$player=="p6_decision"]<-6
d$player[d$player=="p7_decision"]<-7
d$player[d$player=="p8_decision"]<-8
d$player[d$player=="p9_decision"]<-9
d$player[d$player=="p10_decision"]<-10
d$player[d$player=="p11_decision"]<-11
d$player<-as.numeric(d$player)
d$idsu<-d$session*100+d$player
dsub<-d[,c("idsu","round_number","tre","x_enter","x_exit")]
dsub<-dsub[d$round_number>0,]
dsub<- aggregate(dsub,by = list(dsub$idsu),FUN = median,na.rm=T)
cdf_enter.o<-ecdf(dsub$x_enter[dsub$tre==0])
cdf_exit.o<-ecdf(dsub$x_exit[dsub$tre==0])
cdf_exit.u<-ecdf(dsub$x_exit[dsub$tre==1])
cdf_enter.u<-ecdf(dsub$x_enter[dsub$tre==1])

plot(cdf_enter.o,xlim=c(70,120),verticals=T,cex=0,lwd=2,lty=2)
lines(cdf_exit.o,verticals=T,cex=0,lwd=4)
lines(cdf_exit.u,col="red",verticals=T,cex=0,lwd=4)
lines(cdf_enter.u,col="red",verticals=T,cex=0,lwd=2,lty=2)

abline(v=92,lwd=2)
abline(v=100,lwd=2,col="gray")
abline(h=.5,lty=2,col="gray")


toplot<-c(d$session==1301 & d$player=="p6_decision")
hist(d$x_enter[toplot], breaks=80)
abline(v=92,lwd=2)
abline(v=median(d$x_enter[toplot],na.rm=T),lwd=4,col="red")
hist(d$x_exit[toplot], breaks=40)
abline(v=92,lwd=2)
abline(v=median(d$x_exit[toplot],na.rm=T),lwd=4,col="red")


d$dura<-(1-d$choice) * unlist(lapply(rle(1-d$choice)$lengths, seq_len))
d$durain<-(d$choice) * unlist(lapply(rle(d$choice)$lengths, seq_len))

d$flag<-0
d$flag[rango]<-d$choice[rango]-d$choice[(rango1)]


d$flag[d$tick==160 & d$flag==0 & d$choice==1]<- -1
d$flag[d$tick==160 & d$flag==0 & d$choice==0]<- 1
d$lenspell<-d$flag*d$dura

qchoice<-quantile(d$dura[d$tre==1 & d$flag<0],c(.1,.25,.5,.75,.9),na.rm=T)
qchoicein<-quantile(d$durain[d$tre==1 & d$flag>0],c(.1,.25,.5,.75,.9),na.rm=T)



d$group <- as.numeric(factor(interaction(d$ids, d$tick))) 
d$xgroup<-ave(d$x_t,d$group,FUN=mean)
d$xcross<-0
d$xcross[d$xgroup<92]<-1
d$xcrossmean<-ave(d$xcross,d$ids,FUN=mean)

th<-.3
sum(d$choice[d$tre==1 & d$xcrossmean>th])/length(d$choice[d$tre==1 & d$xcrossmean>th])
sum(d$pre_decision[d$tre==1 & d$xcrossmean>th])/length(d$pre_decision[d$tre==1 & d$xcrossmean>th])
sum(d$choice[d$tre==0 & d$xcrossmean>th])/length(d$choice[d$tre==0 & d$xcrossmean>th])
sum(d$pre_decision[d$tre==0 & d$xcrossmean>th])/length(d$pre_decision[d$tre==0 & d$xcrossmean>th])

th1<-.25
sum(d$choice[d$tre==1 & d$xcrossmean<th1])/length(d$choice[d$tre==1 & d$xcrossmean<th1])
sum(d$pre_decision[d$tre==1 & d$xcrossmean<th1])/length(d$pre_decision[d$tre==1 & d$xcrossmean<th1])
sum(d$choice[d$tre==0 & d$xcrossmean<th1])/length(d$choice[d$tre==0 & d$xcrossmean<th1])
sum(d$pre_decision[d$tre==0 & d$xcrossmean<th1])/length(d$pre_decision[d$tre==0 & d$xcrossmean<th1])



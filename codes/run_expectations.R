rm(list = ls())
source("~/Desktop/jotarepos/inoutrab/codes/jarabe.R")
load("~/Desktop/jotarepos/inoutrab/data/datax.Rda")

# U
colu.u<-c(2:19)

# O
colu.o<-c(20:37)

## din_all stores decisions
din_all<-data.frame((matrix(NA, nrow = dim(dtick_w)[1], ncol = dim(dtick_w)[2]-1)))
colnames(din_all)<-colnames(dtick_w)
din_all$tick<-dtick_w$tick

# we assume that first four periods the robot is IN. 
din_all[1:4,2:dim(dtick_w)[2]]<-1

# Important parameters
rho<-.85
rf<- -8
p1<-seq(n)

### for FMT .21 , .41 and all zero for RE

xse<-dtick_w[,20]

xf<-fmt.o.forecast(xse,0,rho,0,2)
belief.xo<-c(xse[1:4],xf)

for(t in seq(4,(length(xse)-1))){
  state.in.o[t+1]<-fmt.decision(belief.xo[t+1], rf)
}

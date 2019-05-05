rm(list = ls())
num.sim<- 3200
totalper.out.u<- rep(NA,num.sim)
totalper.out.o<- rep(NA,num.sim)

for(j in seq(num.sim)){
  source("fmtsimu.R")
  totalper.out.u[j]<-count.out_u/n
  totalper.out.o[j]<-count.out_o/n
}
sum(totalper.out.u)/num.sim
sum(totalper.out.o)/num.sim


hist(totalper.out.u)
plot(belief.xo,type="l")
lines(xse,col="blue",lty=2)
lines(fmt.beliefs,col="red")
abline(h= rf)
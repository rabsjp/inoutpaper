source("jarabe.R")
n<- 160
rho<-.85
ser<-12
rf<- -9
p1<-seq(n)
epsi<-rnorm(n)*ser
xini<-0
xse<-rep(NA,n)
xse[1]<-xini
for(i in seq(2,n)){
  xse[i] = rho*xse[i-1]  + epsi[i]
}
### .21 , .41
xf<-fmt.o.forecast(xse,0,rho,0,2)

## We analyze whether player following FMT decides IN or OUT
state.in<-rep(NA,length(xse))
state.in[1:4]<-1
state.in.o<-state.in
beliefs<-rep(NA,length(xse)) #this vector fills the missing values with the RE belief 
fmt.beliefs<-rep(NA,length(xse)) #this vector stores FMT beliefs
belief.xo<-c(xse[1:4],xf)
beliefs[1:4]<-xse[1:4]
fmt.beliefs[1:4]<-xse[1:4]

for(t in seq(4,(length(xse)-1))){
  if(state.in[t]==0){
    beliefs[t]<- rho*beliefs[t-1]}
  else
  {beliefs[t]<- xse[t]}
  xf_u<-fmt.u.forecast(beliefs[1:t],state.in[1:t],0,rho,0,2)
  fmt.beliefs[t]<-xf_u
  state.in[t+1]<-fmt.decision(xf_u, rf)
  state.in.o[t+1]<-fmt.decision(belief.xo[t+1], rf)
}

count.out_u<-sum(state.in==0)
count.out_o<-sum(state.in.o==0)


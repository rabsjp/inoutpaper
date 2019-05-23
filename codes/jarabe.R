## Computes Forecast accoring to LMT rules (including RE)
## The code works with 3 lags, 

## Compute LMT forecast for O treatment. 
fmt.o.forecast<-function(x,lambda,rho,gamma,lags){
  b = lambda
  g = gamma
  r = rho
  n = length(x)
  carnek0 = x[4:(n-1)]
  carnek1 = x[3:(n-2)]
  carnek2 = x[2:(n-3)]
  carnei = x[1:(n-4)]
  term1<-(carnek0-r*carnek1)+(carnek1-r*carnek2)*b+(carnek2-r*carnei)*b^2
  term2<- r*carnek0+carnek1*r^2*b+carnek2*r^3*b^2
  return(g*term1 + (1-b)*term2)
}

## Compute LMT forecast for U treatment. 
##this is more tricky given that U deletes obs. of x. we will replace the Unobservable with the rational expectation belief 
## in vector re.belief which is represented as "kale"

fmt.u.forecast<-function(kale,state,lambda,rho,gamma,pasado){
  b = lambda
  g = gamma
  r = rho
  n = length(kale)
  xbs<-kale[(n-pasado-1):n] #xo x1 x2 x3 
  state.in<-state[(n-pasado):n]
  term2<- r*xbs[4]+xbs[3]*r^2*b+xbs[2]*r^3*b^2
  term1<- c((xbs[4]-r*xbs[3]),b*(xbs[3]-r*xbs[2]),b^2*(xbs[2]-r*xbs[1]))%*%state.in
  return(g*term1+ (1-b)*term2)
}

## Opt for IN or OUT
fmt.decision<-function(belief,pay.out,state){
  risky_u<- (100+belief)
  
  if (state>0){
    safe_u<- (96+pay.out)
  }
  else{safe_u<-(105)}
  
  if(risky_u >= safe_u){
    a<-1}
  else{a<-0}
  return(a)
}








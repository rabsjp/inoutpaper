rm(list = ls())
setwd("~/Desktop/jotarepos/inoutrab/data/U")
files = list.files(pattern="*.csv")
library(tidyr)

##We create a new identifier that reads d$uses
### 1 digit: U (1) or O (2)
### 2 digit: high(4) low(3) rf
##  3-4 digits: session number

df<-NULL

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d$tre <- 13
  d$session<-1300+i
  df<-rbind(df,d)
}

setwd("~/Desktop/jotarepos/inoutrab/data/O")
files = list.files(pattern="*.csv")

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d$tre <- 23
  d$session<-2300+i
  df<-rbind(df,d)
}

## Deleting crashed and practice rounds
df<-df[!(df$round_number==15 & df$session_id=="vuqt36zr"), ]
df<-df[!(df$round_number==16 & df$session_id=="vuqt36zr"), ]

df<-df[!(df$round_number==9 & df$session_id=="e7qed11a"), ]
df<-df[!(df$round_number==22 & df$session_id=="e7qed11a"), ]
df<-df[!(df$round_number<3),]


df$re<-.85*(df$x_t-100)+100
df$inre<-1
df$inre[df$re<92 & df$tre==23]<-0
df$inre[df$re<92 & df$tre==13]<-0
sum(df$inre[df$tre==23])/sum(df$tre==23)
sum(df[df$tre==23,6:15])/sum(df$tre==23)
sum(df[df$tre==13,6:15])/sum(df$tre==13)

df$ids<-df$session*100+df$round_number

save(df,file="inoutdata.Rda")
# Please move this dataset to /data!

dftick<-df[,c("ids","tick","x_t")]
dtick_w<-spread(dftick,ids,x_t)
save(dtick_w,file="datax.Rda")


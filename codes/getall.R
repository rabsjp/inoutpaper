rm(list = ls())
setwd("~/Desktop/jotarepos/inoutrab/data/U")
files = list.files(pattern="*.csv")
library(tidyr)

##We create a new identifier that reads d$uses
### 1 digit: U or O
### 2 digit: high low rf
##  3-4 digits: Number of sesion

df<-NULL

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  #d<-d[,important_names]
  d$tre <- 1
  d$session<-1100+i
  #d<-d[!duplicated(d), ]
  df<-rbind(df,d)
}
df<-df[df$round_number!=15 & df$session_id=="vuqt36zr", ]
df<-df[df$round_number>2 & df$session_id=="vuqt36zr", ]
df<-df[df$round_number!=16 & df$session_id=="vuqt36zr", ]

setwd("~/Desktop/jotarepos/inoutrab/data/O")
files = list.files(pattern="*.csv")


##We create a new identifier that reads d$uses
### 1 digit: U or O
### 2 digit: high low rf
##  3-4 digits: Number of sesion

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  #d<-d[,important_names]
  d$tre <- 2
  d$session<-2100+i
  #d<-d[!duplicated(d), ]
  df<-rbind(df,d)
}

df<-df[-c(df$round_number==9 & df$session_id=="e7qed11a"), ]
df<-df[-c(df$round_number<3 & df$session_id=="e7qed11a"), ]
df<-df[-c(df$round_number==22 & df$session_id=="e7qed11a"), ]

df$re<-.85*df$x_t
df$inre<-1
df$inre[df$re<92]<-0
sum(df$inre[df$tre==2])/sum(df$tre==2)
sum(df[df$tre==2,6:15])/sum(df$tre==2)
sum(df$inre[df$tre==1])/sum(df$tre==1)
sum(df[df$tre==1,6:15])/sum(df$tre==1)


save(df,file="inoutata.Rda")
# Please move this dataset to /data!


g=gc;rm(list = ls())

library(rjson)
library(dplyr)
library(tidyr)
suppressMessages(library(plyr))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(parallelsugar))
suppressMessages(library(doParallel))

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
      string<- paste(url
                     ,"max=",maxrec,"&" #maximum no. of records returned
                     ,"type=",type,"&" #type of trade (c=commodities)
                     ,"freq=",freq,"&" #frequency
                     ,"px=",px,"&" #classification
                     ,"ps=",ps,"&" #time period
                     ,"r=",r,"&" #reporting area
                     ,"p=",p,"&" #partner country
                     ,"rg=",rg,"&" #trade flow
                     ,"cc=",cc,"&" #classification code
                     ,"fmt=",fmt        #Format
                     ,sep = ""
      )
      
      if(fmt == "csv") {
            raw.data<- read.csv(string,header=TRUE)
            return(list(validation=NULL, data=raw.data))
      } else {
            if(fmt == "json" ) {
                  raw.data<- fromJSON(file=string)
                  data<- raw.data$dataset
                  validation<- unlist(raw.data$validation, recursive=TRUE)
                  ndata<- NULL
                  if(length(data)> 0) {
                        var.names<- names(data[[1]])
                        data<- as.data.frame(t( sapply(data,rbind)))
                        ndata<- NULL
                        for(i in 1:ncol(data)){
                              data[sapply(data[,i],is.null),i]<- NA
                              ndata<- cbind(ndata, unlist(data[,i]))
                        }
                        ndata<- as.data.frame(ndata)
                        colnames(ndata)<- var.names
                  }
                  return(list(validation=validation,data =ndata))
            }
      }
}


###reportes
# colnames(reporters)[1]<- "code"
# colnames(reporters)[2]<- "pots"
# reporters[1,]<- NULL
# reporters<- as.data.frame(reporters)
# reporters$code<- as.character(reporters$code)
# str(reporters)


reporters <- data.frame(Code = unlist(reporters$V1), Country = unlist(reporters$V2))
reporters<- filter(reporters, Code!="all")
reporters$Code <- as.numeric(as.character(reporters$Code))
code<- unique(reporters$Code)

### manual

max <- 20
x <- seq_along(code)
code <- split(code, ceiling(x/max))
c1<- code$`1`
c2<- code$`2`
c3<- code$`3`
c4<- code$`4`
c5<- code$`5`
c6<- code$`6`
c7<- code$`7`
c8<- code$`8`
c9<- code$`9`
c10<- code$`10`
c11<- code$`11`
c12<- code$`12`
c13<- code$`13`
c14<- code$`14`
c15<- code$`15`


###1:20-----------
# require(dplyr)
# require(tidyr)
# cores<- detectCores()
# cl<- makeCluster(cores[1]-4)
# registerDoParallel(cl)


# vv<- foreach(i=1:100) %dopar% { 
# stopCluster(cl) 


yr<-c("2017","2010","2005","2000","1995","1990")

rm(i,s,L)
L<- list()


# i=13
# y=1
for(i in 1:length(c1)){
      s <- get.Comtrade(r=paste(c1[i]), p="all",ps="2017",fmt="csv", cc="110814,071410")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[y]]<- s 
            
      }
      cat(paste("Teminado proceso ", c1[i], "!!done\n", sep = "" ))
}


test<- reporters %>% each(Code) %>% 
      do(get.Comtrade(r=reporters$Code, 
                      p="all",ps="2017",fmt="csv", cc="110814,071410"))



cfiles_1_20<- do.call(rbind,L)

#rm(cfiles_1_20, s,L, i)

###21:41-----------
rm(s,L, i)
L<- list()

for(i in 21:41){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_21_41<- do.call(rbind,L)

rm(cfiles, s,L, i)

###42:61-----------
rm(s,L, i)
L<- list()

for(i in 42:61){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_42_61<- do.call(rbind,L)

rm(cfiles, s,L, i)


###62:81-----------
rm(s,L, i)
L<- list()

for(i in 62:81){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_62_81<- do.call(rbind,L)

rm(cfiles, s,L, i)

###82:101-----------
rm(s,L, i)
L<- list()

for(i in 82:101){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_82_101<- do.call(rbind,L)

###102:121-----------
rm(s,L, i)
L<- list()

for(i in 102:121){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_102_121<- do.call(rbind,L)


###122:141-----------
rm(s,L, i)
L<- list()

for(i in 122:141){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_122_141<- do.call(rbind,L)


###142:171-----------
rm(s,L, i)
L<- list()

for(i in 142:171){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Terminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_142_171<- do.call(rbind,L)

###172:201-----------
rm(s,L, i)
L<- list()

for(i in 172:201){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Terminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_172_201<- do.call(rbind,L)

###202:251-----------
rm(s,L, i)
L<- list()

for(i in 202:251){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Terminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_202_251<- do.call(rbind,L)

###252:266-----------
rm(s,L, i)
L<- list()
# i=267
for(i in 252:266){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Terminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_252_266<- do.call(rbind,L)

###301:340-----------
rm(s,L, i)
L<- list()
# i=267
for(i in 267:300){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Terminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_267_300<- do.call(rbind,L)


###267:300-----------
rm(s,L, i)
L<- list()
# i=267
for(i in 267:300){
      s <- get.Comtrade(r=i, p="all",ps="2017,2010,2005,2000", cc="110814", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Terminado proceso ", i, "!!done\n", sep = "" ))
}


cfiles_267_300<- do.call(rbind,L)

#### apilar todos los resultados
append=do.call(rbind, list(cfiles_1_20,cfiles_102_121,cfiles_122_141,cfiles_142_171,
                           cfiles_172_201,cfiles_202_251,cfiles_21_41,cfiles_252_266,cfiles_42_61,cfiles_62_81,cfiles_82_101))

write.csv(append,"./R/appendData.csv")

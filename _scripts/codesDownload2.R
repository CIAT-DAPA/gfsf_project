g=gc;rm(list = ls())


suppressMessages(library(rjson))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))

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



### manual
reporters <- data.frame(Code = unlist(reporters$V1), Country = unlist(reporters$V2))
reporters$Code<- as.character(reporters$Code); reporters$Country<- as.character(reporters$Country)
reporters<- reporters[-1,]
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


### c1
# rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c1)

a<- split(c1, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=1
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c1<- do.call(rbind,L)

#rm(cfiles_1_20, s,L, i)

###21:41-----------c2
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c2)

a<- split(c2, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c2<- do.call(rbind,L)

###42:61-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c3)

a<- split(c3, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c3<- do.call(rbind,L)

###62:81-----------c4
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c4)

a<- split(c4, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=5
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c4<- do.call(rbind,L)


###82:101-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c5)

a<- split(c5, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c5<- do.call(rbind,L)


###102:121-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c6)

a<- split(c6, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=3
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c6<- do.call(rbind,L)


###122:141-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c7)

a<- split(c7, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c7<- do.call(rbind,L)


###142:171-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c8)

a<- split(c8, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c8<- do.call(rbind,L)

###172:201-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c9)

a<- split(c9, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c9<- do.call(rbind,L)

###202:251-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c10)

a<- split(c10, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c10<- do.call(rbind,L)

###252:266-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c11)

a<- split(c11, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

#i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c11<- do.call(rbind,L)

###262:300-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c12)

a<- split(c12, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c12<- do.call(rbind,L)

###267:300-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c13)

a<- split(c13, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c13<- do.call(rbind,L)

###267:300-----------
rm(s,L, i)
L<- list()

max <- 5
x <- seq_along(c14)

a<- split(c14, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")

at<- list(a1,a2,a3,a4)

# i=4
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c14<- do.call(rbind,L)

####c15
rm(s,L, i)
L<- list()

max <-4
x <- seq_along(c15)

a<- split(c15, ceiling(x/max))
a1<- paste(as.character(a$`1`), collapse = ",")
a2<- paste(as.character(a$`2`), collapse = ",")
a3<- paste(as.character(a$`3`), collapse = ",")
a4<- paste(as.character(a$`4`), collapse = ",")



at<- list(a1,a2,a3)

# i=3
for(i in 1:length(at)){
      s <- get.Comtrade(r=at[[i]], p="all",ps="2001,2000,1999,1998,1997", cc="110814,071410", fmt="csv")
      s<- s$data
      
      if(is.null(s)){
            s<- NULL
      }else{
            L[[i]]<- s 
      }
      cat(paste("Teminado proceso ", at[[i]], "!!done\n", sep = "" ))
}


cfiles_c15<- do.call(rbind,L)

#### apilar todos los resultados
# ls()
append=do.call(rbind, list(cfiles_c1,cfiles_c10,cfiles_c11,cfiles_c12,cfiles_c13,cfiles_c14,cfiles_c15,cfiles_c2,cfiles_c3,cfiles_c4,cfiles_c5,cfiles_c6,cfiles_c7,cfiles_c8,cfiles_c9))

#crear copia
write.csv(append,"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/appendData4.csv")
#crear copia RTB folder
rtbFolder<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/") 
write.csv(append,paste(rtbFolder,"appendDataComercioCOMTRADECassavaStarch3.csv", sep = ""))

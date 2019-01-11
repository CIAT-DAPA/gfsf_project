library(statnet) 
library(RColorBrewer)
library(dplyr)
library(tidyr)


## directories
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/NETworksSanCor/redes/DATA")


### sociometrix 
clima<- read.csv("./Clima.csv")
precios<- read.csv("./Precio.csv")
credito<- read.csv("./Credito.csv")
tecnica<- read.csv("./Tecnica.csv")
atrib<- read.csv("./attributesRed.csv")

### nodos agricultores
agr<- c("Agricultores Cereté",
        "Agricultores Chinú", 
       "Agricultores Cotorra",
       "Agricultores Lorica",
       "Agricultores San Andrés", 
       "Agricultores San Pelayo") 

#Type Description Required?
#Nodes: List of nodes in network, along with node labels 
#Ties: List of ties in the network 
#Node: attributes Attributes of the nodes, "Optional"
#Tie: attributes Attributes of the ties, "Optional"
#Metadata: Other information about the entire network Depends

##Convertir formato "Edge list"
# Estructura ***from ----- To****

# netmat2 <- rbind(c(1,2),
#                  c(1,3),
#                  c(2,3),
#                  c(2,4),
#                  c(3,2),
#                  c(5,3))
# net2 <- network(netmat2,matrix.type="edgelist")
# network.vertex.names(net2) <- c("A","B","C","D","E")
# summary(net2)

### climate

cfiles<- list(Climate=clima,Price=precios,Tech=tecnica, Credit=credito)
netWorks<- list()
netWorks<- lapply(1: length(cfiles),function(i){
      xdata<- cfiles[[i]]
      Rtemp<- xdata %>% filter(., !X %in%  agr)
      Rtemp[is.na(Rtemp)]<- 0
      Rtemp$X<- as.character(Rtemp$X)
      Rtemp<- Rtemp %>% gather(Report, Link, 2:ncol(Rtemp))
      colnames(Rtemp)[1]<- "Parnert"
      Rtemp<- Rtemp[c("Report" ,"Parnert", "Link")]
      Rtemp<- filter(Rtemp, Link!=0)
      
      climaRed <- network(Rtemp,matrix.type="edgelist")
#       network.vertex.names(net2) <- c("A","B","C","D","E")
#       netWorks[[i]]<- climaRed
} )

## graph
gplot(climaRed,vertex.col = 2, displaylabels = TRUE)

## 
list.vertex.attributes(climaRed)
set.vertex.attribute(climaRed, "gender", c("F", "F", "M",
                                       "F", "M"))



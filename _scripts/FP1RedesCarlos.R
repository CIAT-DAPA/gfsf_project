##### redes datos de jean
g=gc;rm(list = ls())

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/REDCCAFS")


#### librerias----
suppressMessages(library(rjson))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(circlize))
suppressMessages(library(sna))
suppressMessages(library(statnet))
suppressMessages(library(igraph))
suppressMessages(library(network))
library(tidyverse)


### open de files extention pajek using igraph
# c<- read_graph("./red coord 1 capac.net",format = c("pajek"))
# gplot(c,gmode = "graph")
## read csv
CSA<- read.csv("./CSA_calification.csv")
otrosAtr<- read.csv("./atributosnodes.csv")


cmatrix <- read.csv("./testH.csv", header = T, row.names = 1) %>% as.matrix
diag(cmatrix) <- NA

# Matriz de tensiones
tensiones <- cmatrix
tensiones[!upper.tri(cmatrix)] <- NA
tensiones[lower.tri(tensiones)] <- t(tensiones)[lower.tri(t(tensiones))]
diag(tensiones) <- 0

# Matriz de intercambio y coordinacion
interacambio <- cmatrix
interacambio[!lower.tri(cmatrix)] <- NA
interacambio[upper.tri(interacambio)] <- t(interacambio)[upper.tri(t(interacambio))]
diag(interacambio) <- 0


tensionTest<- network(tensiones,,matrix.type="adjacency", directed = F ) 

plot(tensionTest)
categorias<- tensiones + interacambio
###  5<- tension + coord ,  4<- tensions + intercam, 2<- solo cooperac, 1<- solo intercambio

gg<- categorias ### solo intercambio
gg[gg==c(5)]<-0
gg[gg==c(4)]<-0
gg[gg==c(2)]<-0
gg[gg==c(3)]<-0


gg1<- categorias ### solo cooperacion
gg1[gg1==c(5)]<-0
gg1[gg1==c(4)]<-0
gg1[gg1==c(3)]<-0
gg1[gg1==c(1)]<-0
gg1[gg1==c(2)]<-1


gg2<- categorias ### tension e Intercambio
gg2[gg2==c(5)]<-0
gg2[gg2==c(2)]<-0
gg2[gg2==c(3)]<-0
gg2[gg2==c(1)]<-0
gg2[gg2==c(4)]<-1

gg3<- categorias ### tension y coordinacio
gg3[gg3==c(4)]<-0
gg3[gg3==c(3)]<-0
gg3[gg3==c(2)]<-0
gg3[gg3==c(1)]<-0
gg3[gg3==c(5)]<-1


SoloInter<- network(gg,matrix.type="adjacency", directed = F )
SoloCoope<- network(gg1,matrix.type="adjacency", directed = F )
SoloTenInter<- network(gg2,matrix.type="adjacency", directed = F )
SoloTenCoord<- network(gg3,matrix.type="adjacency", directed = F )



### Funciones
#### rescale function ------------
rescale <- function(nchar,low,high) {
      min_d <- min(nchar)
      max_d <- max(nchar)
      rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
      rscl
}




### explorar modelacion--------------- 
oddsratios <- function (mem) #based on Harris, 2014
{  
      or <- exp( x = mem$coef )
      ste <- sqrt( diag( mem$covar ) ) 
      lci <- exp( mem$coef-1.96*ste ) 
      uci <- exp( mem$coef+1.96*ste )
      ors <- rbind( round( lci,digits = 4 ),round( or,digits = 4),round( uci,digits = 4 ) ) 
      ors <- t(ors)
      colnames(ors) <- c( "Lower","OR","Upper" ) 
      return(ors)
}

gof_pt.ego <- function(x) # x = ergm.gof object 
{
      m <- x$summary.deg
      selm <- m[, "obs"] == 0 & m[, "min"] == 0 & m[, "max"] == 0 
      degcount<- x$pobs.deg[!selm] >= x$bds.deg[1,!selm] & x$pobs.deg[!selm] <= x$bds.deg[2,!selm]
      goftab <- rbind(c(sum(degcount)), c(length(degcount)), c(sum(degcount/length(degcount))))
      colnames(goftab) <- c("degree")
      return(goftab)
}

coord_capa<- read.paj("./red coord 1 capac.net" )
# plot(coord_capa)
infor<- read.paj("./red info.net" )
# plot(infor)
ten<- read.paj("./" )
# plot(infor)


### summary
tempC<- as.sociomatrix(coord_capa)
tempI<- as.sociomatrix(infor)
tempT<- as.sociomatrix(ten)


t<- tempI +tempC +tempT
t[t == 2] <- 1
t[t == 3] <- 1


### creacion matrix base
relaciones<- network(t, matrix.type="adjacency", directed = F)


### creaciones matrices adicionales 
tempoCor<- network(tempC,matrix.type="adjacency", directed = F)
tempoInf<- network(tempI,matrix.type="adjacency", directed = F)
tempoTen<- network(tempT,matrix.type="adjacency", directed = F)
tempoCat<- network(categorias,matrix.type="adjacency", directed = F)


n<- relaciones$gal$n
Nodes<- lapply(1:n, function(v){
      relaciones$val[[v]]$vertex.names
})

##### apilamiento de los nodos y agregando atributos
nodesList<- as.data.frame(do.call(rbind,Nodes))
colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
CSA$nodes<- as.character(CSA$nodes)
j<- dplyr::left_join(nodesList, CSA, by = "nodes")
network::set.vertex.attribute(relaciones, "CSA_SegAli", c(j[,2]))
network::set.vertex.attribute(relaciones, "CSA_Adap", c(j[,3]))
network::set.vertex.attribute(relaciones, "CSA_Miti", c(j[,4]))


#### agregando mas atributos
colnames(otrosAtr)[1]<- "nodes"
otrosAtr$nodes<- as.character(otrosAtr$nodes)
g<- dplyr::left_join(nodesList, otrosAtr, by = "nodes")
network::set.vertex.attribute(relaciones, "tipo", c(g[,2]))
network::set.vertex.attribute(relaciones, "sector", c(g[,3]))
network::set.vertex.attribute(relaciones, "mandCC", c(g[,4]))
network::set.vertex.attribute(relaciones, "nivel", c(g[,5]))


relaciones %v% "Informacion_degree" <- sna::degree(tempoInf, diag = F) #, cmode="indegree"
relaciones %v% "tension_degree" <- sna::degree(tensionTest, diag = F) # cmode="indegree"
relaciones %v% "coopera_degree" <- sna::degree(tempoCor, diag = F) # cmode="indegree"
relaciones %v% "SoloInter_degree" <- sna::degree(SoloInter, diag = F) # cmode="indegree"


### resumen informacion de la red
summary(relaciones,print.adj=FALSE)


### calculando las metricas
deg <- sna::degree(relaciones,gmode="graph")
cls <-  sna::closeness(relaciones,gmode="graph") # la relacion tiene direccion 
bet <-  sna::betweenness(relaciones,gmode="graph")

##### inicio de metricas
detach(package:igraph)
components(relaciones)
suppressMessages(library(statnet))
CentraBet<-centralization(relaciones,betweenness)
CentraDeg<- centralization(relaciones,degree)
vinculos<- relaciones$gal$mnext
jugadores<- relaciones$gal$n

detach(package:statnet)
suppressMessages(library(igraph))
suppressMessages(library(intergraph))
i <- asIgraph(relaciones)### convert network file to igraph
#### eliminando los shapes
idensi<- graph.density(i)
degi<- igraph::degree(i, mode = "total")

#### analisis de cluster usando weighted 
cw <- cluster_walktrap(i) # The idea is that short random walks tend to stay in the same community.
clusMember<- as.vector(membership(cw))

##### profundizacion en  la modularidad #### debe ser emplementada en graficos sin direccion. 
mod<- modularity(cw)
png(paste("./pic/ModularidadCluster_walks_Network.png", sep = ""),width = 500, height = 500)
V(i)$size=(rescale(degi,1,10))*2
E(i)$arrow.size <- .4
plot(cw,i,vertex.label=V(i)$vertex.names, layout=layout.fruchterman.reingold,
     edge.color="grey", main="Modularity de la red de relaciones")

dev.off()

##### demas indicadores
meanDeg<- mean(degree(i))
trans<- transitivity(i)
transLocal<- transitivity(i, type = "local")
assor<- assortativity.degree(i, directed = T)


###### data
dataSUM<- data.frame(nodes= nodesList, 
                     degree= deg,
                     density= idensi,
                     between= bet, 
                     closeness=cls,
                     centraBetw= CentraBet,
                     centraDeg= CentraDeg,
                     ClusWalk= clusMember,
                     modelaridad= mod,
#                      graphdens= gden,
                     meanDegree= meanDeg,
                     transitiv= trans,
                     transitivLocal=transLocal,
                     assorta= assor, 
                     ties= vinculos,
                     nodesTotal= jugadores)  
write.csv(dataSUM,paste("./data/Relaciones_metricas.csv", sep = ""))



#### representacion grafica de la red
V(i)$color<- FALSE
V(i)[V(i)$tipo==1]$color <- "red"
V(i)[V(i)$tipo==2]$color<- "green"
V(i)[V(i)$tipo==3]$color<- "purple"
V(i)[V(i)$tipo==4]$color<- "lightblue"
V(i)[V(i)$tipo==5]$color<- "orange"
V(i)[V(i)$tipo==6]$color<- "black"





png(paste("./pic/relaciones_Network.png", sep = ""), 
    width = 600, height = 600)

op <- par(mfrow=c(1,1)) #c(1,0,1,1),
V(i)$size=(rescale(degi,1,10))*2.5
E(i)$arrow.size <- .9
# E(i)$width <- degi
plot(i,vertex.label=V(i)$vertex.names, layout=layout.fruchterman.reingold,
     edge.color="grey", vertex.label.dist=0.5,vertex.label.font=2,
     vertex.label.cex=0.9, vertex.frame.color="#ffffff", 
     main="Network de relaciones" )#,edge.width=(degi/5)*2 ) #, main=paste("Visualización de ",ti, sep = "" ))
legend(x=-1.2, y=1.2,c("Publico", "Privado", "ONG", "Aso.Prod", "Coop.Inter", "Otro"), pch=21,
       col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
       title="Tipos", pt.bg=c("red","green","purple", "lightblue", "orange","black")) # pt.bg=unique(V(i)$color)

par(op)
dev.off()
      

##### modelacion 
###PART 3: ergm.ego modelling
suppressMessages(library(statnet)) #V 2016.9
suppressMessages(library(ergm)) #V 0.3.0
suppressMessages(library(coda))

Mnull <- ergm(relaciones ~ edges,
                  control=control.ergm(seed=40))

summary(Mnull)



###### modelo probabilidad de conecten los i y j  de iguales o diferentes sectores
MmainA <- ergm(relaciones ~ edges+ 
                         nodematch('sector'),
                   control=control.ergm(seed=40))

summary(MmainA)



###### modelo probabilidad de conecten los i y j  differencial 
MmainA1 <- ergm(relaciones ~ edges+ 
                      nodemix('sector', diff=T),
              control=control.ergm(seed=40))

summary(MmainA1)

###### modelo principal extensiones 
MmainA2 <- ergm(relaciones ~ edges+ 
                      nodemix('sector')+
                control=control.ergm(seed=40))

summary(MmainA2)


###### modelo principal extensiones 
MmainB <- ergm(relaciones ~ edges+ 
                     nodematch('sector')+
                     nodematch('tipo', diff=T),
                control=control.ergm(seed=40))

summary(MmainB)

####
MmainC <- ergm(relaciones ~ edges+ 
                     nodematch('sector')+
                     nodematch('tipo')+
                     nodecov('Informacion_degree')+
                     nodecov('tension_degree')+
                     nodecov('coopera_degree')+
                     nodecov('SoloInter_degree'),
               control=control.ergm(seed=40))

summary(MmainC)



####  sector ::::1 AGRI / 2 ENV / 3 SAN / 4 BOSQUE / 5 INTERSECTORIAL
####  tipo ::::1 PUBLICO / 2 PRIVADO / 3 ONG / 4 ASSO PROD / 5 COOPERACION INT / 6 OTRO 





#### obtener parametros
Bcoefstab <- rbind(
      cbind(summary(DSmod0)$coef, oddsratios(DSmod0), model="NULO", RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod1)$coef, oddsratios(DSmod1),model="1",RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod2)$coef, oddsratios(DSmod2),model="2", RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod3)$coef, oddsratios(DSmod3),model="3", RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod3a)$coef, oddsratios(DSmod3a),model="3a", RiceT=unique(riceTpes[[i]]$Commodity))
      
)
Bcoefstab$Year<- periododsList[[y]]


#### C. Exploring clustering  usando modularidad

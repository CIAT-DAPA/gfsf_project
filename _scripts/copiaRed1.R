##### redes datos de jean

g=gc;rm(list = ls())


setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1")


#### librerias----
suppressMessages(library(rjson))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(circlize))
suppressMessages(library(sna))
suppressMessages(library(statnet))
suppressMessages(library(igraph))
suppressMessages(library(network))


#### rescale function ------------
rescale <- function(nchar,low,high) {
      min_d <- min(nchar)
      max_d <- max(nchar)
      rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
      rscl
}





#### explorar modelacion--------------- 
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

### open de files extention pajek using igraph
# c<- read_graph("./red coord 1 capac.net",format = c("pajek"))
# gplot(c,gmode = "graph")
## read csv
CSA<- read.csv("./CSA_calification.csv")
otrosAtr<- read.csv("./atributosnodes.csv")
intensidad<- read.csv("./intesidadRelaciones.csv")
otrosAtr$NOMBRE.ORGANIZACION<- as.character(otrosAtr$NOMBRE.ORGANIZACION)
otrosAtr$MANDATO.sobre.CC<- as.character(otrosAtr$MANDATO.sobre.CC)


labelsNodes<- read.csv("./nodesinsumo2.csv")
labelsNodes$nodes<- as.character(labelsNodes$nodes)

informCoord<- read.csv("./insumo1.csv")
tension<- read.csv("./TenInsumo2.csv")

###########################################
aa<- informCoord
aa$Actor.1<- as.factor(aa$Actor.1)
aa$Actor.1<- revalue(aa$Actor.1, c("1"= "SAG",
                                   "2"= "DICTA",
                                   "3"= "FIDA",
                                   "4"= "FAO",
                                   "5"= "USAID",
                                   "6"= "MiAmbiente",
                                   "7"="ICF",
                                   "8"= "GIZ",
                                   "9"= "UTSAN",
                                   "10"= "UE",
                                   "11"= "SEDIS",
                                   "12"="PNUD",
                                   "13"= "Clima+",
                                   "14"= "BID",
                                   "15"= "ANAFAE",
                                   "16"= "RedComal",
                                   "17"= "FIPAH",
                                   "18"= "Heifer",
                                   "19"= "IHCafé",
                                   "20"= "Fundacion Puca",
                                   "21"= "ADROH"))

aa$Actor.2<- as.factor(aa$Actor.2)
aa$Actor.2<- revalue(aa$Actor.2, c("1"= "SAG",
                                   "2"= "DICTA",
                                   "3"= "FIDA",
                                   "4"= "FAO",
                                   "5"= "USAID",
                                   "6"= "MiAmbiente",
                                   "7"="ICF",
                                   "8"= "GIZ",
                                   "9"= "UTSAN",
                                   "10"= "UE",
                                   "11"= "SEDIS",
                                   "12"="PNUD",
                                   "13"= "Clima+",
                                   "14"= "BID",
                                   "15"= "ANAFAE",
                                   "16"= "RedComal",
                                   "17"= "FIPAH",
                                   "18"= "Heifer",
                                   "19"= "IHCafé",
                                   "20"= "Fundacion Puca",
                                   "21"= "ADROH"))

##############################################33
bb<- tension
bb$Actor.1<- as.factor(bb$Actor.1)
bb$Actor.1<- revalue(bb$Actor.1, c("1"= "SAG",
                                   "2"= "DICTA",
                                   "3"= "FIDA",
                                   "4"= "FAO",
                                   "5"= "USAID",
                                   "6"= "MiAmbiente",
                                   "7"="ICF",
                                   "8"= "GIZ",
                                   "9"= "UTSAN",
                                   "10"= "UE",
                                   "11"= "SEDIS",
                                   "12"="PNUD",
                                   "13"= "Clima+",
                                   "14"= "BID",
                                   "15"= "ANAFAE",
                                   "16"= "RedComal",
                                   "17"= "FIPAH",
                                   "18"= "Heifer",
                                   "19"= "IHCafé",
                                   "20"= "Fundacion Puca",
                                   "21"= "ADROH"))

bb$Actor.2<- as.factor(bb$Actor.2)
bb$Actor.2<- revalue(bb$Actor.2, c("1"= "SAG",
                                   "2"= "DICTA",
                                   "3"= "FIDA",
                                   "4"= "FAO",
                                   "5"= "USAID",
                                   "6"= "MiAmbiente",
                                   "7"="ICF",
                                   "8"= "GIZ",
                                   "9"= "UTSAN",
                                   "10"= "UE",
                                   "11"= "SEDIS",
                                   "12"="PNUD",
                                   "13"= "Clima+",
                                   "14"= "BID",
                                   "15"= "ANAFAE",
                                   "16"= "RedComal",
                                   "17"= "FIPAH",
                                   "18"= "Heifer",
                                   "19"= "IHCafé",
                                   "20"= "Fundacion Puca",
                                   "21"= "ADROH"))


################################ networks intercambio, y coordinacion #####################

aaCoord<- aa %>%  dplyr::select( Actor.1,Actor.2,tipoRelacion)
aaCoord$Actor.1<- as.character(aaCoord$Actor.1)
aaCoord$Actor.2<- as.character(aaCoord$Actor.2)
aaCoord$tipoRelacion[aaCoord$tipoRelacion==1]<- 0
aaCoord$tipoRelacion[aaCoord$tipoRelacion==2]<- 1

# coord<- network(p2wide,matrix.type="edgelist", directed = F) #### numero 1


aainter<- aa %>% dplyr::select( Actor.1,Actor.2,tipoRelacion)
aainter$Actor.1<- as.character(aainter$Actor.1)
aainter$Actor.2<- as.character(aainter$Actor.2)
aainter$tipoRelacion[aainter$tipoRelacion==1]<- 1
aainter$tipoRelacion[aainter$tipoRelacion==2]<- 0
colnames(aainter)[3]<- "intercambio"
# inter<- network(aainter,matrix.type="edgelist", directed = F) #### numero 2

aaCoInt<- aa %>% dplyr::select( Actor.1,Actor.2)
aaCoInt$Actor.1<- as.character(aaCoInt$Actor.1)
aaCoInt$Actor.2<- as.character(aaCoInt$Actor.2)

coord_inter<- network(aaCoInt,matrix.type="edgelist", directed = F) #### numero 2
matrixcoord_inter<- as.sociomatrix(coord_inter)
write.csv(matrixcoord_inter,"./experiments/matrixcoor_interTercera.csv")

############################ networks intensidad ############################################
aaIntensidad<- aa %>% dplyr::filter(.,tipoRelacion==2) %>% dplyr::select( Actor.1,Actor.2,intensidad)
aaIntensidad$Actor.1<- as.character(aaIntensidad$Actor.1)
aaIntensidad$Actor.2<- as.character(aaIntensidad$Actor.2)
# aaIntensidad$tipoRelacion[aaCoord$tipoRelacion==2]<- 1
colnames(aaIntensidad)[3]<- "intensidad"
intensidad<- network(aaIntensidad,matrix.type="edgelist", directed = F) #### numero 1

############################ networks tension ############################################
bbten<- bb  #%>% dplyr::select( Actor.1,Actor.2,ten)
bbten$Actor.1<- as.character(bbten$Actor.1)
bbten$Actor.2<- as.character(bbten$Actor.2)


ten<- network(bbten,matrix.type="edgelist", directed = F) #### numero 2
plot(ten)


j1<- dplyr::left_join(aainter,aaCoord, by = c("Actor.1", "Actor.2"))
j2<- dplyr::left_join(j1,bbten, by = c("Actor.1", "Actor.2"))

jr<- j2
jr$r<- j2$intercambio + j2$tipoRelacion + j2$ten
jr<- jr[c("Actor.1","Actor.2")]

jt<- bbten %>% dplyr::filter(., ten==1)
ji<- aainter %>% dplyr::filter(., intercambio==1)
jc<- aaCoord %>% dplyr::filter(., tipoRelacion==1)



######################## networks de relaciones completas ##############################

re<- network(j2,matrix.type="edgelist", directed = F) #### numero 2
te<- network(jt,matrix.type="edgelist", directed = F) #### numero 2
ie<- network(ji,matrix.type="edgelist", directed = F) #### numero 2
ce<- network(jc,matrix.type="edgelist", directed = F) #### numero 2
plot(te)
matrixtensiones<-as.sociomatrix(te)
write.csv(matrixtensiones, "./matrixtensiontercerintento.csv")

######################### agregando atributos de nodos ########################
n<- re$gal$n
Nodes<- lapply(1:n, function(v){
      re$val[[v]]$vertex.names
})

##### apilamiento de los nodos y agregando atributos
nodesList<- as.data.frame(do.call(rbind,Nodes))
colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
CSA$nodes<- as.character(CSA$nodes)
nodesList$nodes<- as.character(nodesList$nodes)
j<- dplyr::left_join(nodesList, CSA, by = "nodes")
j$Seguridad.alimentaria[is.na(j$Seguridad.alimentaria)]<- 0
j$Adaptacion[is.na(j$Adaptacion)]<- 0
j$Mitigacion[is.na(j$Mitigacion)]<- 3

network::set.vertex.attribute(re, "CSA_SegAli", c(j[,2]))
network::set.vertex.attribute(re, "CSA_Adap", c(j[,3]))
network::set.vertex.attribute(re, "CSA_Miti", c(j[,4]))


#### agregando mas atributos
colnames(otrosAtr)[1]<- "nodes"
otrosAtr$nodes<- as.character(otrosAtr$nodes)
g<- dplyr::left_join(nodesList, otrosAtr, by = "nodes")
g$TIPO[is.na(g$TIPO)]<- 1
g$SECTOR[is.na(g$SECTOR)]<- 2
g$MANDATO.sobre.CC[is.na(g$MANDATO.sobre.CC)]<- "SI"
g$NIVEL.DE.ACCION[is.na(g$NIVEL.DE.ACCION)]<- 1

network::set.vertex.attribute(re, "tipo", c(g[,2]))
network::set.vertex.attribute(re, "sector", c(g[,3]))
network::set.vertex.attribute(re, "mandCC", c(g[,4]))
network::set.vertex.attribute(re, "nivel", c(g[,5]))


re %v% "Informacion_degree" <- sna::degree(ie, diag = F) #, cmode="indegree"
re %v% "tension_degree" <- sna::degree(te, diag = F) # cmode="indegree"
re %v% "coopera_degree" <- sna::degree(ce, diag = F) # cmode="indegree"
re %v% "coor_inter_degree"<- sna::degree(coord_inter, diag = F)

rematrix<- as.sociomatrix(re)
write.csv(rematrix,"./matrixtercerintento.csv")

### resumen informacion de la red
summary(re,print.adj=FALSE)


########################################### calculando las metricas ###########################
deg <- sna::degree(re,gmode="graph")
cls <-  sna::closeness(re,gmode="graph") # la relacion tiene direccion 
bet <-  sna::betweenness(re,gmode="graph")

##### inicio de metricas
detach(package:igraph)
components(re)
suppressMessages(library(statnet))
CentraBet<-centralization(re,betweenness)
CentraDeg<- centralization(re,degree)
vinculos<- re$gal$mnext
jugadores<- re$gal$n

detach(package:statnet)
suppressMessages(library(igraph))
suppressMessages(library(intergraph))
i <- asIgraph(re)### convert network file to igraph
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

##### demas indicadores------------
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



#### representacion grafica de la red-------------
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
legend(x=-1.8, y=1.2,c("Publico", "Privado", "ONG", "Aso.Prod", "Coop.Inter", "Otro"), pch=21,
       col="#777777", pt.cex=2, cex=0.5, bty="n", ncol=1,
       title="Tipos", pt.bg=c("red","green","purple", "lightblue", "orange","black")) # pt.bg=unique(V(i)$color)

par(op)
dev.off()





######################################### Modelacion  ########################################33
###PART 3: ergm.ego modelling
suppressMessages(library(statnet)) #V 2016.9
suppressMessages(library(ergm)) #V 0.3.0
suppressMessages(library(coda))

m0 <- ergm(re ~ edges,
           control=control.ergm(seed=40))

summary(m0)



###### modelo probabilidad de conecten los i y j  de iguales o diferentes sectores
m1 <- ergm(re ~ edges+ 
                 nodematch('sector'),
           control=control.ergm(seed=40))

summary(m1)



###### modelo probabilidad de conecten los i y j  differencial 
m2 <- ergm(re ~ edges+ 
                 nodemix('sector'),
           control=control.ergm(seed=40))

summary(m2)

###### modelo principal extensiones 
m3 <- ergm(re ~ edges+ 
                 nodematch('sector')+
                 nodematch('tipo', diff=T),
           control=control.ergm(seed=40))

summary(m3)

####
m4 <- ergm(re ~ edges+ 
                 nodematch('sector')+
                 nodematch('tipo')+
                 # nodecov('Informacion_degree')+
                 nodecov('tension_degree')+
                 # nodecov('coopera_degree')+
                 nodecov('coor_inter_degree')+
                 edgecov(intensidad),
           control=control.ergm(seed=40))

summary(m4)




#### obtener parametros
Bcoefstab <- rbind(
      cbind(summary(m0)$coef, oddsratios(m0), model="NULO"),
      cbind(summary(m1)$coef, oddsratios(m1),model="1"),
      cbind(summary(m2)$coef, oddsratios(m2),model="2"),
      cbind(summary(m3)$coef, oddsratios(m3),model="3"),
      cbind(summary(m4)$coef, oddsratios(m4),model="4")
      
      
)

write.csv(Bcoefstab,"./data/resultsmodels.csv")


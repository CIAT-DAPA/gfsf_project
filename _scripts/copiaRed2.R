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

CSA<- read.csv("./CSA_calification.csv")
otrosAtr<- read.csv("./atributosnodes.csv")
intensidad<- read.csv("./intesidadRelaciones.csv")
otrosAtr$NOMBRE.ORGANIZACION<- as.character(otrosAtr$NOMBRE.ORGANIZACION)
otrosAtr$MANDATO.sobre.CC<- as.character(otrosAtr$MANDATO.sobre.CC)

ma<- read.csv("./experiments/matrixtercerintentoNewnodes.csv", header = T, row.names = 1) %>% as.matrix()
colnames(ma)<- row.names(ma)
redN<- network(ma, matrix.type="adjacency", directed = F )
plot(redN)



######################### agregando atributos de nodos ########################
n<- redN$gal$n
Nodes<- lapply(1:n, function(v){
      redN$val[[v]]$vertex.names
})

##### apilamiento de los nodos y agregando atributos
nodesList<- as.data.frame(do.call(rbind,Nodes))
colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
CSA$nodes<- as.character(CSA$nodes)
nodesList$nodes<- as.character(nodesList$nodes)

j<- dplyr::left_join(nodesList, CSA, by = "nodes")

j[which(j$nodes=="MiAmbiente"),][,2]<-0
j[which(j$nodes=="MiAmbiente"),][,3]<-0
j[which(j$nodes=="MiAmbiente"),][,4]<-3

j$Seguridad.alimentaria[is.na(j$Seguridad.alimentaria)]<- 3
j$Adaptacion[is.na(j$Adaptacion)]<- 2
j$Mitigacion[is.na(j$Mitigacion)]<- 1

network::set.vertex.attribute(redN, "CSA_SegAli", c(j[,2]))
network::set.vertex.attribute(redN, "CSA_Adap", c(j[,3]))
network::set.vertex.attribute(redN, "CSA_Miti", c(j[,4]))


#### agregando mas atributos
colnames(otrosAtr)[1]<- "nodes"
otrosAtr$nodes<- as.character(otrosAtr$nodes)
g<- dplyr::left_join(nodesList, otrosAtr, by = "nodes")


g[which(g$nodes=="MiAmbiente"),][,2]<-1
g[which(g$nodes=="MiAmbiente"),][,3]<-2
g[which(g$nodes=="MiAmbiente"),][,4]<-"SI"
g[which(g$nodes=="MiAmbiente"),][,5]<-1


g$TIPO[is.na(g$TIPO)]<- 4
g$SECTOR[is.na(g$SECTOR)]<- 1
g$MANDATO.sobre.CC[is.na(g$MANDATO.sobre.CC)]<- "NO"
g$NIVEL.DE.ACCION[is.na(g$NIVEL.DE.ACCION)]<- 2

network::set.vertex.attribute(redN, "tipo", c(g[,2]))
network::set.vertex.attribute(redN, "sector", c(g[,3]))
network::set.vertex.attribute(redN, "mandCC", c(g[,4]))
network::set.vertex.attribute(redN, "nivel", c(g[,5]))


ten<-read.csv("./experiments/matrixtensiontercerintento.csv",header = T, row.names = 1) %>% as.matrix()
rten<- network(ten, matrix.type="adjacency", directed = F )
plot(rten)

coord_inter<- read.csv("./experiments/inter_coordTercer.csv", header = T, row.names = 1) %>% as.matrix()
rcoord_inter<- network(coord_inter, matrix.type="adjacency", directed = F )
plot(rcoord_inter)

redN %v% "tension_degree" <- sna::degree(rten, diag = F) # cmode="indegree"
redN %v% "coor_inter_degree"<- sna::degree(rcoord_inter, diag = F)

# rematrix<- as.sociomatrix(re)
# write.csv(rematrix,"./matrixtercerintento.csv")
# 
# ### resumen informacion de la red
# summary(re,print.adj=FALSE)
# 

########################################### calculando las metricas ###########################
deg <- sna::degree(redN,gmode="graph")
cls <-  sna::closeness(redN,gmode="graph") # la relacion tiene direccion 
bet <-  sna::betweenness(redN,gmode="graph")

##### inicio de metricas
detach(package:igraph)
components(redN)
suppressMessages(library(statnet))
CentraBet<-centralization(redN,betweenness)
CentraDeg<- centralization(redN,degree)
vinculos<- redN$gal$mnext
jugadores<- redN$gal$n

detach(package:statnet)
suppressMessages(library(igraph))
suppressMessages(library(intergraph))
i <- asIgraph(redN)### convert network file to igraph
#### eliminando los shapes
idensi<- graph.density(i)
degi<- igraph::degree(i, mode = "total")

#### analisis de cluster usando weighted 
cw <- cluster_walktrap(i) # The idea is that short random walks tend to stay in the same community.
clusMember<- as.vector(membership(cw))

##### profundizacion en  la modularidad #### debe ser emplementada en graficos sin direccion. 
mod<- modularity(cw)
png(paste("./pic/ModularidadCluster_walks_NodesNew.png", sep = ""),width = 500, height = 500)
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
write.csv(dataSUM,paste("./data/Relaciones_metricasExper.csv", sep = ""))



#### representacion grafica de la red-------------
V(i)$color<- FALSE
V(i)[V(i)$tipo==1]$color <- "red"
V(i)[V(i)$tipo==2]$color<- "green"
V(i)[V(i)$tipo==3]$color<- "purple"
V(i)[V(i)$tipo==4]$color<- "lightblue"
V(i)[V(i)$tipo==5]$color<- "orange"
V(i)[V(i)$tipo==6]$color<- "black"


png(paste("./pic/Experirelaciones_Network.png", sep = ""), 
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


m0 <- ergm(redN ~ edges,
           control=control.ergm(seed=40))

summary(m0)



###### modelo probabilidad de conecten los i y j  de iguales o diferentes sectores
m1 <- ergm(redN ~ edges+ 
                 nodematch('sector'),
           control=control.ergm(seed=40))

summary(m1)



###### modelo probabilidad de conecten los i y j  differencial 
m2 <- ergm(redN ~ edges+ 
                 nodemix('sector'),
           control=control.ergm(seed=40))

summary(m2)

###### modelo principal extensiones 
m3 <- ergm(redN ~ edges+ 
                 nodematch('sector')+
                 nodematch('tipo', diff=T),
           control=control.ergm(seed=40))

summary(m3)

####
m4 <- ergm(redN ~ edges+ 
                 nodematch('sector')+
                 nodematch('tipo')+
                 # nodecov('Informacion_degree')+
                 nodecov('tension_degree')+
                 # nodecov('coopera_degree')+
                 nodecov('coor_inter_degree'),
           # edgecov(intensidad),
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

##################################### diffussion analisys #############


#### difusion analisis
suppressMessages(library(netdiffuseR))
suppressMessages(library(readr))
s<-20000
set.seed(s)
diffnet_ran <- rdiffnet(200, 20, "random", seed.p.adopt = .1,
                        seed.graph = "small-world",
                        rgraph.args = list(undirected=FALSE, k=4, p=.5),
                        threshold.dist = function(x) 0.3)

plot(diffnet_ran)


tt<- floor(runif(76, min = 1, max = 20))
V(iDHHS)$toa<- tt
pp<- igraph_to_diffnet(graph = iDHHS,toavar = "toa", t0 = 1,t1 = 20)

plot(pp)
summary(pp)
plot_diffnet(pp)
plot_adopters(pp, add=TRUE, what="cumadopt")
plot_infectsuscep(pp, bins=15, K=3, 
                  main = "Distribution of Infectiousness and\nSusceptibility (Random)")


plot_threshold(pp)

ig_net<- barabasi.game(10)
V(ig_net)$toa<- c(1,1,1,1,3,3,3,3,6,6)
d<-igraph_to_diffnet(ig_net,toavar = "toa")
plot(d)



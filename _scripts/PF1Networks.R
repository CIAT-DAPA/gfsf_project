##### redes datos de jean
g=gc;rm(list=ls())
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


### open de files extention pajek using igraph
# c<- read_graph("./red coord 1 capac.net",format = c("pajek"))
# gplot(c,gmode = "graph")

coord_capa<- read.paj("./red coord 1 capac.net" )
# plot(coord_capa)
infor<- read.paj("./red info.net" )
# plot(infor)
ten<- read.paj("./red tension.net" )
# plot(infor)


#### Agrupando los datos de las redes en una sola lista
redes<- list( Information=infor, Coordination=coord_capa, Tension=ten)

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

###ideas agregar las calificaciones deacuerdo a CSA 
CSA<- read.csv("./CSA_calification.csv")
otrosAtr<- read.csv("./atributosnodes.csv")
otrosAtr$TIPO<- as.character(otrosAtr$TIPO)
otrosAtr$SECTOR<- as.character(otrosAtr$SECTOR)
otrosAtr$NIVEL.DE.ACCION<- as.character(otrosAtr$NIVEL.DE.ACCION)
otrosAtr$TIPO<- revalue(otrosAtr$TIPO, c("1"="Publico", "2"="Privado", "3"="ONG", "4"="Aso.Prod", "5"="Coop.Inter", "6"="Otro"))
otrosAtr$SECTOR<- revalue(otrosAtr$SECTOR, c("1"="Agricultura", "2"="Medio.Ambiente", 
                                           "3"="SAN", "4"="Bosque", "5"="Intersectorial"))
otrosAtr$NIVEL.DE.ACCION<- revalue(otrosAtr$NIVEL.DE.ACCION, c("1"="Nacional", "2"="Regional", "3"="Local"))



#############################################  Runniggggggggggggggg ################################################
#### procesamiento inicial 
### agregando attributos nodes

# i=2

lapply(1:length(redes), function(i){
      
      n<- redes[[i]]$gal$n
      Nodes<- lapply(1:n, function(v){
            redes[[i]]$val[[v]]$vertex.names
            })

            ##### apilamiento de los nodos
            nodesList<- as.data.frame(do.call(rbind,Nodes))
            colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
            j<- dplyr::left_join(nodesList, CSA, by = "nodes")
            network::set.vertex.attribute(redes[[i]], "CSA_SegAli", c(j[,2]))
            network::set.vertex.attribute(redes[[i]], "CSA_Adap", c(j[,3]))
            network::set.vertex.attribute(redes[[i]], "CSA_Miti", c(j[,4]))
            
            
            
            ##### agregar otros tipos
            ##### tipo de organizacion 1 PUBLICO / 2 PRIVADO / 3 ONG / 4 ASSO PROD / 5 COOPERACION INT / 6 OTRO 
            ##### sector 1 AGRI / 2 ENV / 3 SAN / 4 BOSQUE / 5 INTERSECTORIAL
            ##### MANDATO sobre CC SI/NO
            ##### NIVEL DE ACCION= 1 Nacional, 2 Regional, 3 Local
            
            colnames(otrosAtr)[1]<- "nodes"
            g<- dplyr::left_join(nodesList, otrosAtr, by = "nodes")
            network::set.vertex.attribute(redes[[i]], "tipo", c(g[,2]))
            network::set.vertex.attribute(redes[[i]], "sector", c(g[,3]))
            network::set.vertex.attribute(redes[[i]], "mandCC", c(g[,4]))
            network::set.vertex.attribute(redes[[i]], "nivel", c(g[,5]))
            
            
            ## titulo graficos
            ti<- redes[[i]]$gal$title
            ## detection and deleting nodos isolates
            nodosSolos<- isolates(redes[[i]])
            network::delete.vertices(redes[[i]],isolates(redes[[i]]))
            
            n2<- redes[[i]]$gal$n
            Nodes2<- lapply(1:n2, function(v){
                  redes[[i]]$val[[v]]$vertex.names
            })
            ##### apilamiento de los nodos
            nodesList2<- as.data.frame(do.call(rbind,Nodes2))
            colnames(nodesList2)[1]<- "nodes" ## renombrando la columna 
            
            
            ### calculando las metricas
            deg <- sna::degree(redes[[i]],gmode="graph")
            cls <-  sna::closeness(redes[[i]],gmode="graph") # la relacion tiene direccion 
            bet <-  sna::betweenness(redes[[i]],gmode="graph")

            ##### inicio de metricas
            detach(package:igraph)
            components(redes[[i]])
            suppressMessages(library(statnet))
            CentraBet<-centralization(redes[[i]],betweenness)
            CentraDeg<- centralization(redes[[i]],degree)
            vinculos<- redes[[i]]$gal$mnext
            jugadores<- redes[[i]]$gal$n

            detach(package:statnet)
            suppressMessages(library(igraph))
            suppressMessages(library(intergraph))
            i <- asIgraph(redes[[i]])### convert network file to igraph
            #### eliminando los shapes
            i<- delete_vertex_attr(i,"shape") # liminar atributo de shape
            idensi<- graph.density(i)
            degi<- igraph::degree(i, mode = "total")
      
            #### analisis de cluster usando weighted 
            cw <- cluster_walktrap(i, weights = E(i)$red.info) # The idea is that short random walks tend to stay in the same community.
            clusMember<- as.vector(membership(cw))
      
            ##### profundizacion en  la modularidad #### debe ser emplementada en graficos sin direccion. 
            mod<- modularity(cw)
            png(paste("./pic/", ti, "_ModularidadCluster_walks_Network.png", sep = ""),width = 500, height = 500)
            V(i)$size=(rescale(degi,1,10))*2
            E(i)$arrow.size <- .4
            plot(cw,i,vertex.label=V(i)$vertex.names, layout=layout.fruchterman.reingold,
                      edge.color="grey", main=paste("Modularity: ",ti, sep = "" ))
            
            dev.off()
            
            ##### demas indicadores
            meanDeg<- mean(degree(i))
            trans<- transitivity(i)
            transLocal<- transitivity(i, type = "local")
            assor<- assortativity.degree(i, directed = T)
            
            
            ###### data
            dataSUM<- data.frame(nodes= nodesList2, 
                                 degree= deg,
                                 density= idensi,
                                 between= bet, 
                                 closeness=cls,
                                 centraBetw= CentraBet,
                                 centraDeg= CentraDeg,
                                 ClusWalk= clusMember,
                                 modelaridad= mod,
                                 graphdens= gden,
                                 meanDegree= meanDeg,
                                 transitiv= trans,
                                 transitivLocal=transLocal,
                                 assorta= assor, 
                                 ties= vinculos,
                                 nodesTotal= jugadores)  
            write.csv(dataSUM,paste("./data/", ti,"_Informacion_metricas.csv", sep = ""))
            
            
            #### representacion grafica de la red
           
            V(i)$color<- FALSE
            V(i)[V(i)$tipo=="Publico"]$color <- "red"
            V(i)[V(i)$tipo=="Privado"]$color<- "green"
            V(i)[V(i)$tipo=="ONG"]$color<- "purple"
            V(i)[V(i)$tipo=="Aso.Prod"]$color<- "lightblue"
            V(i)[V(i)$tipo=="Coop.Inter"]$color<- "orange"
            V(i)[V(i)$tipo=="Otro"]$color<- "black"
            
            
         
            V(i)$shape<- FALSE
            V(i)[V(i)$sector=="Agricultura"]$shape <- "square"
            V(i)[V(i)$sector=="Medio.Ambiente"]$shape<- "square"
            V(i)[V(i)$sector=="SAN"]$shape<- "square"
            V(i)[V(i)$sector=="Bosque"]$shape<- "square"
            V(i)[V(i)$sector=="Intersectorial"]$shape <- "square"
            
            V(i)$shape
            
            

#             degi<- degree(graph = i,mode = "total")
            
            png(paste("./pic/",ti,"_Network.png", sep = ""), 
                width = 600, height = 600)
            
            op <- par(mfrow=c(1,1)) #c(1,0,1,1),
            V(i)$size=(rescale(degi,1,10))*1.5
            E(i)$arrow.size <- .9
            E(i)$width <- degi*1.5
            plot(i,vertex.label=V(i)$vertex.names, layout=layout.fruchterman.reingold,
                 edge.color="grey", vertex.label.dist=0.5,vertex.label.font=2,
                 vertex.label.cex=0.9, vertex.frame.color="#ffffff", 
                 main=paste("Network: ",ti, sep = "" ))#,edge.width=(degi/5)*2 ) #, main=paste("Visualización de ",ti, sep = "" ))
            legend(x=-1.2, y=1.2,c("Publico", "Privado", "ONG", "Aso.Prod", "Coop.Inter", "Otro"), pch=21,
                   col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
                   title="Tipos", pt.bg=c("red","green","purple", "lightblue", "orange","black")) # pt.bg=unique(V(i)$color)
            
            par(op)
            dev.off()
            
            
            
            
      
})



###PART 3: ergm.ego modelling
suppressMessages(library(statnet)) #V 2016.9
suppressMessages(library(ergm.ego)) #V 0.3.0
suppressMessages(library(coda))
suppressMessages(library(ergm))




#                  Directed  Valued	      Loops	  Name
#Informacion	      Yes	    No	 No	  *In     
#coordi&Capa	      No	    No       No	  *CC
#tension	           Yes	    No	 Yes	  *Te

redes$Information %v% "Informa_degree" <- sna::degree(redes$Information, gmode="graph")
redes$Coordination %v% "CC_degree" <- sna::degree(redes$Coordination, gmode="graph")
redes$Tension %v% "Ten_degree" <- sna::degree(redes$Tension, gmode="graph")



Mod0 <- ergm(redes$Information ~ edges, 
              control = control.ergm(seed=40))

summary(Mod0)

Mod1 <- ergm(redes$Information ~ edges+ 
                   nodecov('Informa_degree'), 
             control = control.ergm(seed=40))

summary(Mod1)

+ nodecov( 'knowledge_degree' )








# 
# Vinformacion.ego <- as.egodata.network(redes[[1]])
# Vcoordinacion.ego <- as.egodata(redes[[2]])
# Vtension.ego <- as.egodata(redes[[3]])
# 
# set.seed(0)

Bnull <- ergm(redes[[1]] ~ edges, 
                      control = control.ergm(seed=40))

summary(Bnull)


#### modelo nullo
Bmain1 <- ergm(redes[[1]] ~ edges+
                     nodematch('sector')  , 
              control = control.ergm(seed=40))

summary(Bmain1)


#### modelo relaciones entre sectores
Bmain2 <- ergm(redes[[1]] ~ edges+
                     nodematch('sector'),
               control = control.ergm(seed=40))

summary(Bmain2)


Bmain3 <- ergm(redes[[1]] ~ edges+
                     nodematch('sector', diff=TRUE) +
                     nodecov('CSA_Adap')+
                     nodecov('CSA_Miti')+
                     nodecov('CSA_SegAli')+
                     nodematch('tipo', diff=TRUE), 
               control = control.ergm(seed=40))

summary(Bmain3)



Bmain1_ego <- ergm.ego(Vinformacion.ego ~ edges + degree(1)
                       + nodematch('sector') , # + nodecov('influence_indegree')
                       control = control.ergm.ego(ppopsize = 1000, 
                                                  ergm.control = control.ergm(MCMC.samplesize=100000, 
                                                                              MCMC.burnin=1000000, 
                                                                              MCMC.interval=1000)))
summary(Bmain1_ego)


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

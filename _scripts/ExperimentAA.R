#### ensayo
##### redes datos de jean
g=gc;rm(list = ls())
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1")


# #### libreries----
suppressMessages(library(rjson))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(circlize))
suppressMessages(library(sna))
suppressMessages(library(statnet))
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(netdiffuseR))
suppressMessages(library(readr))

### netowork multilevel 
ma<- read.csv("./informacionCarlos .csv", header = T, row.names = 1) %>% as.matrix()
colnames(ma)<- row.names(ma)
redN<- network(ma, matrix.type="adjacency", directed = F )
plot(redN)

red<- intergraph::asIgraph(redN)
nodesname<- colnames(ma)
V(red)$name<- nodesname[1:vcount(red)]
plot(red)



### creating matrix Recreating three sectors rice, maize, bean
na<- c(paste0("a",seq(1:20))) 
nb<- c(paste0("b",seq(1:20)))
nc<- c(paste0("c",seq(1:20)))

g1<- sample_gnp(20, 3/10) # number nodes and p probability of ties between nodes
g2<- sample_gnp(20, 3/10)
g3<- sample_gnp(20, 3/10)
# as_adjacency_matrix(g1)
V(g1)$name<- na[1:vcount(g1)]
V(g2)$name<- nb[1:vcount(g2)]
V(g3)$name<- nc[1:vcount(g3)]
# as_adjacency_matrix(g1)
E(g1)$weight <- runif(ecount(g1))
E(g2)$weight <- runif(ecount(g2))
E(g3)$weight <- runif(ecount(g3))
as_adjacency_matrix(g1, attr="weight")
as_adjacency_matrix(g2, attr="weight")
as_adjacency_matrix(g3, attr="weight")

plot(g1)
u<-union(g1,g2,g3)
j<- union(u,red) # integrating segegration producers networks and Institutional networks 
plot(j,edge.arrow.size=.4,vertex.label=V(j)$name,vertex.size=4)

# jt<- add_vertices(u,nv = 3,attr=list("EA","EB","EC")) %>% add_edges(c("RedComal","a19",
#                                                                       "ADROH", "b5",
#                                                                       "IHCafé", "c7"))
# create new nodes like Asociation Producers
j<- j + vertices("Rice", "Maize", "Sugar Cane")

plot(j,edge.arrow.size=.4,vertex.label=V(j)$name,vertex.size=4)

j<- add_edges(j,edges = c("Rice","a1",
                          "Maize","b1",
                          "Sugar Cane","c1",
                          "Rice","SAG",
                          "Maize","SAG",
                          "Sugar Cane","SAG"))

plot(j,edge.arrow.size=.4,vertex.label=V(j)$name,vertex.size=4)


########### diffusion
tt<- floor(runif(84, min = 10, max = 60))
V(j)$toa<- tt
pp<- igraph_to_diffnet(graph = j, toavar = "toa", t0 = 1,t1 = 20)
dj<- rdiffnet(threshold.dist = 0.3,seed.graph = pp)
plot(dj)

summary(dj)
plot_diffnet(dj, slices = c(1,5,10,15,20))
plot_adopters(dj,add=TRUE, what="cumadopt")
plot_adopters(dj)




tt<- floor(runif(76, min = 1, max = 20))
V(iDHHS)$toa<- tt
pp<- igraph_to_diffnet(graph = iDHHS,toavar = "toa", t0 = 1,t1 = 20)


plot(pp)
summary(pp)
plot_diffnet(pp)
plot_adopters(pp, add=TRUE, what="cumadopt")
plot_infectsuscep(pp, bins=15, K=3, 
                  main = "Distribution of Infectiousness and\nSusceptibility (Random)")








# 
# 
# 
# 
# #### one way
# 
# rownames <- c(paste0("n",seq(1:100)))
# colnames <- c(paste0("n",seq(1:100)))
# 
# mm<- matrix(0,100,100,byrow = T, dimnames = list(rownames, colnames)) # create matrix cuadrada
# mm<- apply(mm, c(1,2), function(x)sample(c(0,1),1)) #  fill up with zeros and ones
# diag(mm)<-0
# 
# ### second way
# m<-matrix(rbinom(400,1,0.2),20)
# diag(m)<-0
# 
# detach(package:statnet)
# library(igraph)
# 
# ### third way
# # In G(n,p) graphs, the graph has 'n' vertices and for each edge the probability that it is present in the graph is 'p'.
# # In G(n,m) graphs, the graph has 'n' vertices and 'm' edges, and the 'm' edges are chosen uniformly randomly from the set of all possible edges.
# r1<- erdos.renyi.game(n=20, p.or.m = 0.5,type = "gnp", directed = F, loops = F)
# r2<- erdos.renyi.game(n=20, p.or.m = 0.5,type = "gnp", directed = F, loops = F)
# r3<- erdos.renyi.game(n=20, p.or.m = 0.5,type = "gnp", directed = F, loops = F)
# na<- c(paste0("a",seq(1:20)))
# nb<- c(paste0("b",seq(1:20)))
# nc<- c(paste0("c",seq(1:20)))
# 
# V(r1)$label<- na
# V(r2)$label<- nb
# V(r3)$label<- nc
# 
# r<- union(r1,r2,r3,byname = F)
# V(r)
# plot(r2)
# 
# as_edgelist(r1)
# as_adjacency_matrix(r1,type = "both",attr = "label") #,names ="label")
# 
# vertex_attr_names(r1)
# 
# 
# 
# 
# #link the 2 graphs
# regions <- add_edges(regions, which(V(regions)$name %in% c("1", "1")), color="red")
# V(regions)$label <- substr(V(regions)$name, 1, 1)
# 
# 
# #link the 2 graphs
# net <- add_edges(net, which(V(net)$name %in% c("A1", "A2")), color="red")
# 
# plot(regions)
# plot(s)
# ## recreate red
# b<- network(mm, matrix.type="adjacency", directed = F)
# plot(b)
# ## recreate producers regiones
# ## recreate adopcion and diffusion of policy 
# 
# 
# 
# # 
# # #### librerias----
# # suppressMessages(library(rjson))
# # suppressMessages(library(plyr))
# # suppressMessages(library(tidyr))
# # suppressMessages(library(circlize))
# # suppressMessages(library(sna))
# # suppressMessages(library(statnet))
# # suppressMessages(library(igraph))
# # suppressMessages(library(network))
# # 
# # 
# # #### rescale function ------------
# # rescale <- function(nchar,low,high) {
# #       min_d <- min(nchar)
# #       max_d <- max(nchar)
# #       rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
# #       rscl
# # }
# # 
# # 
# # 
# # 
# # 
# # #### explorar modelacion--------------- 
# # oddsratios <- function (mem) #based on Harris, 2014
# # {  
# #       or <- exp( x = mem$coef )
# #       ste <- sqrt( diag( mem$covar ) ) 
# #       lci <- exp( mem$coef-1.96*ste ) 
# #       uci <- exp( mem$coef+1.96*ste )
# #       ors <- rbind( round( lci,digits = 4 ),round( or,digits = 4),round( uci,digits = 4 ) ) 
# #       ors <- t(ors)
# #       colnames(ors) <- c( "Lower","OR","Upper" ) 
# #       return(ors)
# # }
# # 
# # gof_pt.ego <- function(x) # x = ergm.gof object 
# # {
# #       m <- x$summary.deg
# #       selm <- m[, "obs"] == 0 & m[, "min"] == 0 & m[, "max"] == 0 
# #       degcount<- x$pobs.deg[!selm] >= x$bds.deg[1,!selm] & x$pobs.deg[!selm] <= x$bds.deg[2,!selm]
# #       goftab <- rbind(c(sum(degcount)), c(length(degcount)), c(sum(degcount/length(degcount))))
# #       colnames(goftab) <- c("degree")
# #       return(goftab)
# # }
# # 
# # CSA<- read.csv("./CSA_calification.csv")
# # otrosAtr<- read.csv("./atributosnodes.csv")
# # intensidad<- read.csv("./intesidadRelaciones.csv")
# # otrosAtr$NOMBRE.ORGANIZACION<- as.character(otrosAtr$NOMBRE.ORGANIZACION)
# # otrosAtr$MANDATO.sobre.CC<- as.character(otrosAtr$MANDATO.sobre.CC)

### Arroz honduras caso de analisis FSP1
###Load libraries
g=gc;rm(list = ls())
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape))
suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools))
suppressMessages(library(sp))
suppressMessages(library(sna))
suppressMessages(library(network))
suppressMessages(library(intergraph))

### Repositorio https://github.com/caosmax/SNA_FSP1.git

### Directorios
path.root<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1/real/")
pic<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1/real/pic/")
path<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1/")


#### matriz institucional
ma<- read.csv(paste(path,"informacionCarlos .csv",sep = ""), header = T, row.names = 1) %>% as.matrix()
colnames(ma)<- row.names(ma)
redN<- network(ma, matrix.type="adjacency", directed = F )
plot(redN)


library(intergraph)
red<- intergraph::asIgraph(redN)
require(igraph)
nodesname<- colnames(ma)
V(red)$name<- nodesname[1:vcount(red)]
plot(red)


#### Parametros de difusion 
transmission_rate = 0.4
coins = c(rep(1, transmission_rate*100), rep(0,(1-transmission_rate)*100))
n = length(coins)

###method 1
toss = function(freq) {   # toss the coins
      tossing = NULL
      for (i in 1:freq ) tossing[i] = sample(coins, 1, replace=TRUE, prob=rep(1/n, times=n))
      tossing = sum(tossing)
      return (tossing)
}



#### Update graphs
update_diffusers = function(diffusers){
      nearest_neighbors = data.frame(table(unlist(neighborhood(j, 1, diffusers))))
      nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
      keep = unlist(lapply(nearest_neighbors[,2], toss))
      new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
      class(new_infected) <- "igraph.vs"
      diffusers = unique(c(diffusers, new_infected))
      return(diffusers)
}


#### generate networks (Part 1)

# library(igraph)

node_number = 100
g = barabasi.game(node_number, directed =F,power = 1) 
graph_name = "Scale-free network"
plot(g,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)



node_number12 = 71
g12 = barabasi.game(node_number12,directed = F,power = 1) 
graph_name = "Scale-free network"
plot(g12,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)

node_number13 = 820
g13 = barabasi.game(node_number13, directed = F,power = 1) 
graph_name = "Scale-free network"
plot(g13,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)


node_number14 = 84
g14 = barabasi.game(node_number14, directed = F,power = 1) 
graph_name = "Scale-free network"
plot(g14,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)

png(paste("./real/networksByRegion.png", sep = ""),width = 500, height = 500)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(g03,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)
plot(g12,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)
plot(g13,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)
plot(g14,vertex.size=6, vertex.label=NA,edge.arrow.size=.1)
dev.off()

### label nodes 
dr03<- c(paste0("r03",seq(1:321)))
dr12<- c(paste0("r12",seq(1:77)))
dr13<- c(paste0("r13",seq(1:820)))
dr14<- c(paste0("r14",seq(1:84)))

# gq03<- sample_gnp(321, 5/321,loops = F) # number nodes and p probability of ties between nodes
# gq12<- sample_gnp(77, 4/77,loops = F)
# gq13<- sample_gnp(820, 3/820,loops = F)
# gq14<- sample_gnp(84, 3/84,loops = F)

# degree_distribution(g12)
# test<-sample_correlated_gnp_pair(50, 0.3, 0.5)
# plot(test)

# as_adjacency_matrix(g1)
V(g03)$name<- dr03[1:vcount(g03)]
V(g12)$name<- dr12[1:vcount(g12)]
V(g13)$name<- dr13[1:vcount(g13)]
V(g14)$name<- dr14[1:vcount(g14)]

# as_adjacency_matrix(g1)
E(g03)$weight <- runif(ecount(g03))
E(g12)$weight <- runif(ecount(g12))
E(g13)$weight <- runif(ecount(g13))
E(g14)$weight <- runif(ecount(g14))

as_adjacency_matrix(g03, attr="weight")
as_adjacency_matrix(g12, attr="weight")
as_adjacency_matrix(g13, attr="weight")
as_adjacency_matrix(g14, attr="weight")


# require(igraph)
u<- union(g03,g12,g13,g14)
# u<- graph.union(g03,g12)
# u<- graph.union(g03,g12,g13,g14)

plot(u,vertex.label=NA,edge.arrow.size=.1, vertex.size=6)
j<- union(u,red) # integrating segegration producers networks and Institutional networks 

# V(j)$frame.color <- "white"
# V(j)$color <- "orange"
# E(j)$arrow.mode <- 0
plot(j,vertex.size=5,vertex.label=NA) #vertex.label=V(j)$name,vertex.size=4)


j<- j + vertices("dr03", "dr12", "dr13", "dr14")
plot(j,vertex.size=5,vertex.label=NA) #vertex.label=V(j)$name,vertex.size=4)
j<- igraph::add_edges(j,edges = c("dr03","r031",
                                  "dr12","r121",
                                  "dr13","r13815",
                                  "dr14", "r1484",
                                  "dr03","SAG",
                                  "dr12","SAG",
                                  "dr13", "SAG",
                                  "dr14","SAG"))
png(paste("./real/networksUnion.png", sep = ""),width = 500, height = 500)
plot(j,vertex.size=5,vertex.label=NA) #vertex.label=V(j)$name,vertex.size=4)
dev.off()


#################################### method manual ###############################
suppressMessages(library(netdiffuseR))
#### difussion method 1
# tt<- floor(runif(1321, min = 10, max = 60))
# V(j)$toa<- tt
# pp<- igraph_to_diffnet(graph = j, toavar = "toa", t0 = 1,t1 = 20)
# dj<- rdiffnet(threshold.dist = 0.3,seed.graph = pp)
# plot(dj)
### visualizar todos los 
# nodes<- (V(j))
### difusion method 2
seed_num = 1
set.seed(20140301)
node_number=1321
#diffusers = sample(V(j),seed_num) ### aca se elige el difusor inicial
diffusers = V(j)["SAG"]
infected =list()
infected[[1]]= diffusers
# set the color
E(j)$color = "grey"
V(j)$color = "white"


### Initiate the diffusers (Part 2)
set.seed(2014); layout.old = layout.fruchterman.reingold(j, niter = 1000)
V(j)$color[V(j)%in% diffusers] = "red"
plot(j, layout =layout.old, vertex.size=2,vertex.label=NA)
plot(j, layout =layout.old, vertex.size=2,vertex.label = ifelse(V(j) %in% diffusers,V(j),NA))


### Start the contagion !
total_time = 1
while(length(infected[[total_time]]) < node_number){ 
      infected[[total_time+1]] = sort(update_diffusers(infected[[total_time]]))
      cat(length(infected[[total_time+1]]), "-->")
      total_time = total_time + 1
}


### Save as the animation (Part 1)
plot_time_series = function(infected, m){
      num_cum = unlist(lapply(1:m, 
                              function(x) length(infected[[x]]) ))
      p_cum = num_cum/node_number
      p = diff(c(0, p_cum))
      time = 1:m
      plot(p_cum~time, type = "b", 
           ylab = "CDF", xlab = "Time",
           xlim = c(0,total_time), ylim =c(0,1))
      plot(p~time, type = "h", frame.plot = FALSE,
           ylab = "PDF", xlab = "Time",
           xlim = c(0,total_time), ylim =c(0,1))
}


plot_time_series(infected,25)


#### Save as the animation (Part 2)
plot_gif = function(infected){
      m = 1
      while(m <= length(infected)){
            layout(matrix(c(1, 2, 1, 3), 2,2, byrow = TRUE), widths=c(3,1), heights=c(1, 1))
            V(j)$color = "white"
            V(j)$color[V(g)%in%infected[[m]]] = "red"
            plot(j, layout =layout.old, edge.arrow.size=0.2,vertex.size=5,vertex.label=NA)
            title(paste(graph_name, "\n Transmission Rate =", transmission_rate, ", Day", m))
            plot_time_series(infected, m)
            m = m + 1}
}


library(animation)

saveGIF({
      ani.options(interval = 0.5, convert = 
                        shQuote("C:/Program Files/ImageMagick-7.0.4-Q16/convert.exe"))
      # start the plot
      plot_gif(infected)
}, movie.name = "testRiceSAG.gif",ani.width = 800, ani.height = 500)




### table tiempo y nodos adoptantes
# length(infected)
# i=5
# pp<- list()
# for(i in 1:length(infected)){
#       # node<- as.character(infected[[i]])
#       table(infected[[i]])
#       node<- as_ids(infected[[i]])
#       pp[[i]]<- data.frame(node,year=i, total=node_number)
# }
# infected[[1]]
# rr<- rbind(infected)


diffusers = V(j)["SAG"]

nearest_neighbors = data.frame(table(unlist(neighborhood(j, 1, diffusers))))

# nei<- neighborhood(graph = j, order = 1, nodes = diffusers)
# plot(induced_subgraph(j, ego(j, 1, diffusers)[[1]]))
# plot(induced_subgraph(j, ego(j, 1, diffusers)[[1]]))
# 
# plot(induced.subgraph(j,ego(graph = j,order = 1,nodes = diffusers)[[1]]))
# plot(induced.subgraph(j,neighborhood(graph = j,order = 1,nodes = diffusers)[[1]]))


#### One diffuser
png(paste("./real/NeighborhoodAZeroPatient.png", sep = ""),width = 500, height = 500)
plot(induced.subgraph(j,neighborhood(graph = j,order = 1,nodes = diffusers)[[1]]), 
     vertex.label=NA, vertex.size=10)
dev.off()

### two diffuser 
diffusers = V(j)["RedComal", "PRONADERS"]
# png(paste("./real/NeighborhoodBZeroPatient.png", sep = ""),width = 500, height = 500)
# plot(induced.subgraph(j,neighborhood(graph = j,order = 1,nodes = V(j)[c("PRONADERS","RedComal")])[[2]]), 
#      vertex.size=10) #vertex.label=NA, 
# 
# dev.off()

# test<- adjacent_vertices(j, c("RedComal", "PRONADERS")) # crear neighborhoods

test<- adjacent_vertices(j, c("RedComal", "PRONADERS"))
subgraphs = make_ego_graph(j, order=1, nodes=c("RedComal","PRONADERS"), mode = "in")
plot(subgraphs[[1]])
plot(subgraphs[[2]])
y<- union(subgraphs[[1]], subgraphs[[2]])

# set the color
diffusers = V(y)["RedComal", "PRONADERS"]
E(y)$color = "grey"
V(y)$color = "white"
V(y)$color[V(y)%in% diffusers] = "red"

png(paste("./real/NeighborhoodBZeroPatient.png", sep = ""),width = 500, height = 500)
plot(y, vertex.size=10, vertex.label=NA)
dev.off()


#### Update graphs
update_diffusers = function(diffusers){
      nearest_neighbors = data.frame(table(unlist(neighborhood(j, 1, diffusers))))
      nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
      keep = unlist(lapply(nearest_neighbors[,2], toss))
      new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
      class(new_infected) <- "igraph.vs"
      diffusers = unique(c(diffusers, new_infected))
      return(diffusers)
}

#################################### method automatic ###############################
# https://github.com/USCCANA/netdiffuseR 
tt<- floor(runif(node_number, min = 10, max = 60))
V(j)$toa<- tt
pp<- igraph_to_diffnet(graph = j, toavar = "toa", t0 = 1,t1 = 20)
dj<- rdiffnet(threshold.dist = 0.3,seed.graph = pp)
plot(dj)

summary(dj)
plot_diffnet(dj, slices = c(1,5,10,15,20))
plot_adopters(dj,add=TRUE, what="cumadopt")
plot_adopters(dj)



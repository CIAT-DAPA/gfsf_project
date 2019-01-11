suppressMessages(library(igraph))
suppressMessages(library(animation))
suppressMessages(library(dplyr))

# https://chengjunwang.com/network-diffusion/#/6

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

###Method 2
# transmission_rate = 0.4
# coins = c(1, 0) 
# probabilities = c(transmission_rate, 1-transmission_rate )         
# # sample(coins, 1, rep=TRUE, prob=probabilities) # Generate a sequence
# # toss the coins
# toss = function(freq) {
#       tossing = NULL
#       for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities)
#       tossing = sum(tossing)
#       return (tossing)
# }

#### Update graphs
update_diffusers = function(diffusers){
      nearest_neighbors = data.frame(table(unlist(neighborhood(g, 1, diffusers))))
      nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
      keep = unlist(lapply(nearest_neighbors[,2], toss))
      new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
      class(new_infected) <- "igraph.vs"
      diffusers = unique(c(diffusers, new_infected))
      return(diffusers)
}


#### generate networks (Part 1)
node_number = 100
library(igraph)
node_number

# ### generate regular networks (Part 1)
# g = graph.tree(node_number, children = 2)
# g = graph.star(node_number)
# g = graph.full(node_number)
# g = graph.ring(node_number)
# g = connect.neighborhood(graph.ring(node_number), 2)
# 
# ### generate regular networks (Part 2)
# g = erdos.renyi.game(node_number, 0.1)
# g = rewire.edges( graph.ring(node_number), prob = 0.8 ) 
# g = watts.strogatz.game(1,node_number,3,0.2) 

g = barabasi.game(node_number) 
graph_name = "Scale-free network"
plot(g)

####Initiate the diffusers (Part 1)
seed_num = 1
set.seed(20140301)

diffusers = sample(V(g),seed_num) ### aca se elige el difusor inicial
infected =list()
infected[[1]]= diffusers
# set the color
E(g)$color = "grey"
V(g)$color = "white"

### Initiate the diffusers (Part 2)
set.seed(2014); layout.old = layout.fruchterman.reingold(g, niter = 1000)
V(g)$color[V(g)%in% diffusers] = "red"
plot(g, layout =layout.old)

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


plot_time_series(infected,10)

#### Save as the animation (Part 2)
plot_gif = function(infected){
      m = 1
      while(m <= length(infected)){
            layout(matrix(c(1, 2, 1, 3), 2,2, byrow = TRUE), widths=c(3,1), heights=c(1, 1))
            V(g)$color = "white"
            V(g)$color[V(g)%in%infected[[m]]] = "red"
            plot(g, layout =layout.old, edge.arrow.size=0.2)
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
}, movie.name = "test.gif",ani.width = 800, ani.height = 500)


infected[2]












### Understanding neighborhood (Part 1)
g<- graph.ring(100)
s<- graph.tree(1000)
# n1<- neighborhood(graph = g,order = 0,nodes = 1:3)
# n1<- neighborhood(graph = g,order = 1,nodes = 1:3)
V(g)$name<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
gn1<- graph.neighborhood(g,2,1:3)

veci<- data.frame(table(unlist(neighborhood(s,1,1000))))
veci = subset(veci, !(veci[,1]%in% 1000))
keep = unlist(lapply(veci[,2], toss))




#### my understanding 
cc<- rep(x = 1,transmission_rate*1000)
bb<- rep(x = 0, (1-transmission_rate)*1000)
coins2<- c(cc,bb)
n2<- length(coins2)

barplot(sample(x = c(1,0),1000,replace =TRUE,prob =rep(c(0.4,0.6), times=1000)))
barplot(table(sample(x=c(1,0),size=200, replace = TRUE, prob=c(0.4,0.6))))
barplot(table(sample(x = c(0,1),replace=TRUE,size = 5000, prob =c(0.3,0.7))))

### http://www.rexamples.com/14/Sample()

tossing<- NULL
for(i in 1:1000)tossing[i]<- sample(coins2,1,replace =TRUE, prob = rep(1/1000,times=1000))
tossing=sum(tossing)

### crear una una red 
### definir una tecnologia  y un tiempo de adopcion
### usar un camino de influencia en la ponderación (relational, positional and central)


# Simulating diffusion networks


# Introduction

# Before we start, a review of the concepts we will be using here
# 
# 1. Exposure: Proportion/number of neighbors that has adopted an innovation at each point in time.
# 2. Threshold: The proportion/number of your neighbors who had adopted at or one time period before ego (the focal individual) adopted.
# 3. Infectiousness: How much $i$'s adoption affects her alters.
# 4. Susceptibility: How much $i$'s alters' adoption affects her.
# 5. Structural equivalence: How similar are $i$ and $j$ in terms of position in the network.
# 

# We will simulate a diffusion network with the following parameters:
#       
# 1.  Will have 1,000 vertices,
# 2.  Will span 20 time periods,
# 3.  The initial adopters (seeds) will be selected random,
# 4.  Seeds will be a 10\% of the network,
# 5.  The graph (network) will be small-world,
# 6.  Will use the WS algorithmwith $p=.2$ (probability of rewire).
# 7.  Threshold levels will be uniformly distributed between [0.3, 0.7\]

library(netdiffuseR)
knitr::opts_chunk$set(comment = "#")

set.seed(1213) # generating random graph 
net <- rdiffnet(
      n              = 1e3,                         # 1.
      t              = 20,                          # 2.
      seed.nodes     = "random",                    # 3.
      seed.p.adopt   = .1,                          # 4. proportion for the early adopters
      seed.graph     = "small-world",               # 5.
      rgraph.args    = list(p=.2),                  # 6.
      threshold.dist = function(x) runif(1, .3, .7) # 7. proportion of neighbors that leads you to adopt a particular behavior
)

net2 <- rdiffnet(
      n              = 1e3,                         # 1.
      t              = 20,                          # 2.
      seed.nodes     = "random",                    # 3.
      seed.p.adopt   = .1,                          # 4. proportion for the early adopters
      seed.graph     = "small-world",               # 5.
      rgraph.args    = list(p=.2),
      threshold.dist = 0.1 # 7. proportion of neighbors that leads you to adopt a particular behavior
)

summary(net); summary(net2)
plot_diffnet2(net);plot_diffnet2(net2)
plot_adopters(net2)
plot_adopters(net)

a1<-exposure(graph = net)
a2<-exposure(graph = net2)


plot(net)
plot_diffnet(net)
plot_diffnet2(net)
plot_adopters(net)
plot_threshold(net)
plot_infectsuscep(net2, K=2)
plot_hazard(net)

ex1<- exposure(net)

rdiffnet(rgraph.args = )

### Scripts to get prototype model diffusion 
### Carlos Eduardo Gonzalez 

g=gc;rm(list = ls())

### Repositorio https://github.com/caosmax/SNA_FSP1.git

############### Load libraries ###############
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape))
suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools))
suppressMessages(library(sp))
suppressMessages(library(sna))
suppressMessages(library(network))
suppressMessages(library(intergraph))
suppressMessages(library(FinCal))


############### Directories ####################
path.root<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1/real/")
pic<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1/real/pic/")
path<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/FP1/")


############### Load data #######################
ma<- read.csv(paste(path,"informacionCarlos .csv",sep = ""), header = T, row.names = 1) %>% as.matrix()
colnames(ma)<- row.names(ma)
redN<- network(ma, matrix.type="adjacency", directed = F ) # became 

#### Become file from network to igraph extention 
library(intergraph)
red<- intergraph::asIgraph(redN)
require(igraph)
nodesname<- colnames(ma)
V(red)$name<- nodesname[1:vcount(red)]

###################### comparing random models to empirical  ##################
#data de la red institucional
graph.density(red)
c<- mean(igraph::degree(red))
# data de la red aleatoria con los datos de la red real
r_e.r_red <- erdos.renyi.game(21,p.or.m = c/(vcount(red)-1),type='gnp') # numero de nodos, P= c/n-1 
r_s.w_red <- watts.strogatz.game(dim=1,size=21,nei=3,p=.25)
r_s.f_red <- barabasi.game(21,out.dist=c(0,.7,.3),
                          directed=FALSE,zero.appeal=2)

d<- graph.density(r_e.r_red)
dd<- graph.density(r_s.w_red)
ddd<- graph.density(r_s.f_red)
dddd<- graph.density(red)
de<- list(d,dd,ddd,dddd)

q<- mean(degree(r_e.r_red))
qq<- mean(degree(r_s.w_red))
qqq<- mean(degree(r_s.f_red))
qqqq<- mean(degree(red))
avg<- list(q,qq,qqq,qqqq)

y<- transitivity(r_e.r_red)
yy<- transitivity(r_s.w_red)
yyy<- transitivity(r_s.f_red)
yyyy<- transitivity(red)
tra<- list(y,yy,yyy,yyyy)

rm(d,dd,ddd,dddd,q,qq,qqq,qqqq,y,yy,yyy,yyyy) #eliminar archivos

### Creating table with comparison results 
models<- c("Erdos-Renyi", "Small World", "Preferencial Attachment", "Institutional Network")
t<- data.frame(name=models,Size="21",Density=unlist(de),Avg.degree=unlist(avg), Transitivity=unlist(tra))

### Write table and export as .csv
write.csv(t,paste(path,"/ComparisonModelsRandom.csv", sep = ""))

#### Plotting comparison graph
png(paste(path,"/comparasionModels.png", sep = ""),width = 800, height = 800)
par(mfrow=c(2,2),mar=c(0,0,5,3))
par(mgp=c(1,0.5,0)) # distancia de texto y area
plot(r_e.r_red, vertex.size=7, vertex.label=NA, edge.arroz.size=.1,
     main="Erdos-Renyi",vertex.color=2)
plot(r_s.w_red, vertex.size=7, vertex.label=NA, edge.arroz.size=.1,
     vertex.color=3, main="Small World")
plot(r_s.f_red, vertex.size=7, vertex.label=NA, edge.arroz.size=.1,
     vertex.color=4, main="Preferencial Attachment")
plot(red, vertex.size=7, vertex.label=NA, edge.arroz.size=.1,
     vertex.color=5, main="Institutional Network")
dev.off()


##################### Simulation networks and difussion of technology ################
node_number = 100 ### Representing number of farmes  

#### generate networks 
g<- igraph::barabasi.game(n = node_number) ## scale free
h<- igraph::erdos.renyi.game(n = node_number,p.or.m = 0.05,directed = TRUE) # random graph

### graph of random graph with seed node 
png(paste(path,"/Difussion_rg_sf.png", sep = ""),width = 800, height = 800)
par(mfrow=c(1,2),mar=c(8,0,5,0))
par(mgp=c(0,0,0)) # distancia de texto y area
plot(h, vertex.size=7, vertex.label=NA, edge.arrow.size=.1,
     main="Erdos-Renyi",vertex.color=2)
plot(g, vertex.size=7, vertex.label=NA, edge.arrow.size=.1,
     vertex.color=3, main="Scale Free")
dev.off()


#### Difussion parameters 
transmission_rate = 0.4 # tasa de transmision para ambos scale free and random graph 
coins<-c(1,0)
probabilities = c(transmission_rate, 1-transmission_rate)

### toss the coins
toss = function(freq) {
      tossing = NULL
      for (i in 1:freq ) tossing[i] = 
                  sample(coins, 1, rep=TRUE, prob=probabilities)
      tossing = sum(tossing)
      return (tossing)
}

######################################### Scale Free ####################################
graph_name = "Scale-free network"

####Initiate the diffusers
seed_num = 1
set.seed(20140301)

### Selection of nodes as seed inside of network
diffusers = sample(V(g),seed_num) 
infected =list()
infected[[1]]= diffusers
### set the color
E(g)$color = "grey"
V(g)$color = "white"

# ### visualization graphs
# png(paste(path,"/Dif_seed_model_sf.png", sep = ""),width = 800, height = 500)
# par(mfrow=c(2,2),mar=c(0.5,2,0.5,2),omi=c(0,0.5,0.5,0.1)) # mar order;bottom, left, top and right
# layout.old = layout.fruchterman.reingold(g, niter = 1000)
# plot(g, layout =layout.old,vertex.size=6, 
#      vertex.label=NA, 
#      edge.arrow.size=0.2,
#      vertex.color=4, main="Scale free")
# 
# V(g)$color[V(g)%in% diffusers] = "red"
# plot(g, layout =layout.old,vertex.size=6, 
#      vertex.label=NA, edge.arrow.size=0.2,
#      main="With patient zero, random (red node)")
# 
# layout.old = layout.fruchterman.reingold(h, niter = 1000)
# plot(h, layout =layout.old,vertex.size=6, 
#      vertex.label=NA, 
#      edge.arrow.size=0.1,
#      vertex.color=4, main="Random Graph")
# 
# V(h)$color[V(h)%in% diffusers_rg] = "red"
# plot(h, layout =layout.old,vertex.size=6, 
#      vertex.label=NA, edge.arrow.size=0.1,
#      main="With patient zero, random (red node)")
# 
# dev.off()


### Diffusers 
update_diffusers = function(diffusers){
      nearest_neighbors = data.frame(table(unlist(neighborhood(g, 1, diffusers))))
      nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
      keep = unlist(lapply(nearest_neighbors[,2], toss))
      new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
      class(new_infected) <- "igraph.vs"
      diffusers = unique(c(diffusers, new_infected))
      return(diffusers)
}

## Another representation of network and diffuser 
set.seed(20140301); 
layout.old = layout.fruchterman.reingold(g, niter = 1000)
V(g)$color[V(g)%in%diffusers] = "red"
plot(g,vertex.size=4, layout =layout.old,edge.arrow.size=0.2,vertex.label=NA )

### Start the contagions !!
total_time = 1
while(length(infected[[total_time]]) < node_number){ 
      infected[[total_time+1]] = sort(update_diffusers(infected[[total_time]]))
      cat(length(infected[[total_time+1]]), "-->")
      total_time = total_time + 1
}

### Save as imagen to documents 
plot_time_series_graph = function(infected, m){
      num_cum = unlist(lapply(1:m, 
                              function(x) length(infected[[x]]) ))
      p_cum = num_cum/node_number
      p = diff(c(0, p_cum))
      time = 1:m
      par(mfrow=c(1,2),mar=c(4,5,0.5,3),oma=c(0,0,2,0))#,mar=c(1,1,3,1)) # mar order;bottom, left, top and right
      # par(mgp=c(3,2,2)) # distancia de texto y area
      plot(p_cum~time, type = "b", 
           ylab = "CDF", xlab = "Time",
           xlim = c(0,total_time), ylim =c(0,1))
      plot(p~time, type = "h", frame.plot = FALSE,
           ylab = "PDF", xlab = "Time",
           xlim = c(0,total_time), ylim =c(0,1))
      title(main = "Scale Free", outer = T,line = 0) #outer=TRUE,
      
}

### Export graph
png(paste(path,"/infected_sf.png", sep = ""),width = 800, height = 600)
plot_time_series_graph(infected,30)
dev.off()

#### Save as the animation (Part 2)
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

### Plotting gif
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


### Save animation
library(animation)

saveGIF({
      ani.options(interval = 0.5, convert = 
                        shQuote("C:/Program Files/ImageMagick-7.0.4-Q16/convert.exe"))
      # start the plot
      plot_gif(infected)
}, movie.name = "Scale_free.gif",ani.width = 800, ani.height = 500)



rm(total_time, layout.old, seed_num)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################################### Random Graph ####################################
graph_name_rg= "Random Graph"
seed_num=1
### Diffuser
diffusers_rg = sample(V(h),seed_num) ### aca se elige el difusor inicial
infected_rg =list()
infected_rg[[1]]= diffusers_rg
# set the color
E(h)$color = "grey"
V(h)$color = "white"


update_diffusers_rg = function(diffusers_rg){
      nearest_neighbors_rg = data.frame(table(unlist(neighborhood(h, 1, diffusers_rg))))
      nearest_neighbors_rg = subset(nearest_neighbors_rg, !(nearest_neighbors_rg[,1]%in%diffusers_rg))
      keep_rg = unlist(lapply(nearest_neighbors_rg[,2], toss))
      new_infected_rg = as.numeric(as.character(nearest_neighbors_rg[,1][keep_rg >= 1]))
      class(new_infected_rg) <- "igraph.vs"
      diffusers_rg = unique(c(diffusers_rg, new_infected_rg))
      return(diffusers_rg)
}

## Random  Graph 
set.seed(20140301) 
layout.old = layout.fruchterman.reingold(h, niter = 1000)
V(h)$color[V(h)%in%diffusers_rg] = "red"
plot(h,vertex.size=4, layout =layout.old,edge.arrow.size=0.2,vertex.label=NA )
dev.off()

total_time_rg = 1
while(length(infected_rg[[total_time_rg]]) < node_number){ 
      infected_rg[[total_time_rg+1]] = sort(update_diffusers_rg(infected_rg[[total_time_rg]]))
      cat(length(infected_rg[[total_time_rg+1]]), "-->")
      total_time_rg = total_time_rg + 1
}


### Save as imagen for documents
plot_time_series_rg = function(infected_rg, m){
      num_cum = unlist(lapply(1:m, 
                              function(x) length(infected_rg[[x]]) ))
      p_cum = num_cum/node_number
      p = diff(c(0, p_cum))
      time = 1:m
      par(mfrow=c(1,2),mar=c(4,5,0.5,3),oma=c(0,0,2,0))#,mar=c(1,1,3,1)) # mar order;bottom, left, top and right
      plot(p_cum~time, type = "b", 
           ylab = "CDF", xlab = "Time",
           xlim = c(0,total_time_rg), ylim =c(0,1))
      plot(p~time, type = "h", frame.plot = FALSE,
           ylab = "PDF", xlab = "Time",
           xlim = c(0,total_time_rg), ylim =c(0,1))
      title(main = "Random Graph", outer = T,line = 0) #outer=TRUE,
}
### exporting graph to documents
png(paste(path,"/infected_rg.png", sep = ""),width = 800, height = 600)
plot_time_series_rg(infected_rg,7)
dev.off()


## graph for animation
plot_time_series = function(infected_rg, m){
      num_cum = unlist(lapply(1:m, 
                              function(x) length(infected_rg[[x]]) ))
      p_cum = num_cum/node_number
      p = diff(c(0, p_cum))
      time = 1:m
      plot(p_cum~time, type = "b", 
           ylab = "CDF", xlab = "Time",
           xlim = c(0,total_time_rg), ylim =c(0,1))
      plot(p~time, type = "h", frame.plot = FALSE,
           ylab = "PDF", xlab = "Time",
           xlim = c(0,total_time_rg), ylim =c(0,1))
}


## Random graph 
plot_gif_rg = function(infected_rg){
      m = 1
      while(m <= length(infected_rg)){
            layout(matrix(c(1, 2, 1, 3), 2,2, byrow = TRUE), widths=c(3,1), heights=c(1, 1))
            V(h)$color = "white"
            V(h)$color[V(h)%in%infected_rg[[m]]] = "red"
            plot(h, layout =layout.old, edge.arrow.size=0.2)
            title(paste(graph_name, "\n Transmission Rate =", transmission_rate, ", Day", m))
            plot_time_series(infected_rg, m)
            m = m + 1}
}


## random graph 
saveGIF({
      ani.options(interval = 0.5, convert = 
                        shQuote("C:/Program Files/ImageMagick-7.0.4-Q16/convert.exe"))
      # start the plot
      plot_gif_rg(infected_rg)
}, movie.name = "Random_graph.gif",ani.width = 800, ani.height = 500)

########################### Extract to data ################################333
yr<- length(infected)
nodes<-list()
for(i in 1:length(infected)){
      a<- do.call(rbind,infected[i]) %>% as.data.frame(.)
      a<- as.data.frame(a)
      acum_nodes<- ncol(a)
      nodes[i]<- acum_nodes
  
}
n<- rbind(nodes) %>% as.data.frame()
n<- unlist(nodes)
cfiles<- data.frame(yr=1:30, nodesAcum=n)
cfiles$rate_adop<- (cfiles$nodesAcum/node_number)*100
write.csv(cfiles, paste(path,"/datos_adopt_sf.csv", sep = ""))


########################################Surplus Excendent model ########################
# R options
options(warn = -1)
options(scipen = 999)

a<- cfiles[,2]
y<- cfiles[,1]

range_costos<- rep(x =0.3:0.6,1)

range_yields<- (seq(10,40,by=1))/100
range_costos<- (seq(30,60,by=1))/100

### funcion 
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/funcionSurplusEconomicModel.R")

### Creacion de diferentes escenarios de costos y rendimientos 
cm<- expand.grid(range_costos,range_yields)
colnames(cm)[1]<- "Costos"
colnames(cm)[2]<- "Rendimientos"

num_comb<- nrow(cm)


####################################### loops surplus economic #####################################
resul_sce<- list()
i=1
for(i in 1:nrow(cm)){
      #combinacion de rendimientos y costos
      combinations<- cm[i,]
      
      #runing model 
      m<- list(modeltest(prob_success =0.8 ,
                    elas_supply =0.3,
                    change_yield = combinations$Rendimientos,
                    elas_demand =-1.1 ,
                    prod_prices =300 ,
                    input_cost_changes =combinations$Costos,
                    depre_rate =1 ,
                    discount_rate =10 ,
                    cre_exo =0.06 ,
                    q =1800 ,
                    yr = y ,
                    adop= a,
                    time_inves =6 ,
                    r_d_budget =5000000 ))
      
      resul_sce[[i]]<- list(m)
      
}


View(resul_sce[1][[1]][[1]])



# 
# 
# m<- modeltest(prob_success =0.8 ,
#               elas_supply =0.3,
#               change_yield = 0.2,
#               elas_demand =-1.1 ,
#               prod_prices =300 ,
#               input_cost_changes =0.1 ,
#               depre_rate =1 ,
#               discount_rate =10 ,
#               cre_exo =0.06 ,
#               q =1800 ,
#               yr = y ,
#               adop= a,
#               time_inves =6 ,
#               r_d_budget =5000000 )
#                         



# 
# ### parameters 
# q= (4*100)*4.5  ## total production 
# r_d_budget=5000000 # development technology
# time_inves=5
# cfiles$rate_adop<-(cfiles$nodesAcum/100)*100
# adop<- (cfiles$rate_adop)
# 
# parameters<- list(
#       prob_success=0.8,
#       elas_supply=0.2,
#       elas_demand=-1.4,
#       change_yield=0.2,
#       prod_prices=300, # dolares per ton
#       time=Periodo,
#       input_cost_changes=0.50, #porcentage (%)
#       depre_rate=1, #porcentage (%)
#       discount_rate=0.1, #porcentage (%)
#       # Cantidades= q, # rendimiento promedio * ha * numero de agricultores
#       cre_exo=0.06
# )
# 
# m<- data.frame(Yrs=1:yr,
#            E_ofer= parameters$elas_supply, 
#            E_demand= parameters$elas_demand,
#            Change_yield= parameters$change_yield,
#            Change_equi_yield= parameters$change_yield/parameters$elas_supply, # cambios rendimientos/elas productor
#            Change_cost_he= parameters$input_cost_changes,
#            Change_equi_cost= parameters$input_cost_changes/(1+ parameters$change_yield),
#            Change_net= (parameters$change_yield/parameters$elas_supply)-(parameters$input_cost_changes/(1+ parameters$change_yield)),
#            Prob_exito=parameters$prob_success,
#            Tasa_adop=adop,
#            Tasa_dep=parameters$depre_rate,
#            Cantidades= q
# )
# 
# require(dplyr)
# m<- m %>% mutate(K= Change_net*Prob_exito*Tasa_adop*Tasa_dep,
#                  Z= (E_ofer*K)/(E_ofer+E_demand),
#                  precio=parameters$prod_prices,
#                  Tasa_exter_creci=parameters$cre_exo,
#                  Change_exc_total=(K*precio*Cantidades*(1+0.5*(Z*E_demand))),
#                  Change_exc_pro= precio*Cantidades*(K-Z)*(1+0.5*(Z*E_demand)),
#                  Change_exc_consu=precio*Cantidades*Z*(1+(0.5*Z*E_demand)),
#                  Change_exc_consu=precio*Cantidades*Z*(1+(0.5*Z*E_demand)),
#                  Costos_R_I=0)
#                 
# ### data R&D
# rd<- data.frame(Yrs=1:time_inves,E_ofer=0,E_demand=0,Change_yield=0,Change_equi_yield=0,Change_cost_he=0,
#                 Change_equi_cost=0,Change_net=0, Prob_exito=0,Tasa_adop=0,Tasa_dep=0,
#                 Cantidades=0 ,K=0, Z=0, precio=0,Tasa_exter_creci=0, Change_exc_total=0,
#                 Change_exc_pro=0,Change_exc_consu=0,Costos_R_I=r_d_budget/time_inves)
# 
# ###Agregate tiempo para agregar costos y tiempos de inversion
# xfiles<- rbind(rd,m) # aplir las bases de datos
# xfiles<- xfiles %>% mutate(bef_net=Change_exc_total-Costos_R_I)
# ## valor presente neto 
# npv(r = parameters$discount_rate, xfiles[,"bef_net"])
# irr(cf = xfiles[,"bef_net"])   
# xfiles<- xfiles %>% mutate(totalB=sum(bef_net), totalC=sum(Costos_R_I),Rela_Bef_Cost=totalB/totalC ) 
# 
# ### calculate parameters 
# 
# at0<- cfiles[which(cfiles$yr==1),] %>% select(rate_adop) %>% .[,1] # Adoption level at year of release (At0)
# Atmax=100 # 50% of Atmax (At3)
# at3<- Atmax/2 # Maximum adoption rate (Atmax)
# t0<- 1 # Year of beginning of adoption (t0) 
# t3<- cfiles[which(cfiles$nodesAcum>+50 & cfiles$nodesAcum<=Atmax),] %>% nrow(.)# Years after 50% of Atmax is reached (t3)     
# t0_max<- cfiles[which(cfiles$nodesAcum>=at0 & cfiles$nodesAcum<=Atmax),] %>% nrow(.) 
# 
# 
# #beta
# beta<- (log(at0/Atmax)- log(at3/Atmax))/(t0-t3)
# #alpha
# alpha<- (log(at3/Atmax))-(t3*beta)
# 
# adop_for<- list()
# #i=1
# for(i in 1:length(cfiles$yr)){
#       adop_for[i]<- Atmax/(1+ exp(-(alpha+(beta*i))))
# 
# }
# 
# ### estimate graph 
# Curve_adop<- do.call(rbind,adop_for) %>% data.frame
# colnames(Curve_adop)<- "Rate_adop"
# Curve_adop$Rate_adop<- round(Curve_adop$Rate_adop)
# Curve_adop$yr<- 1:length(cfiles$yr) 
# plot(Curve_adop$yr, Curve_adop$Rate_adop, type="b", xlab="Time", ylab="Rate Adoption", main="Scale Free")
# 
# ########################## several combinatins ##################################
# ### Evaluated range of change in yields min 5% to max 80%
# 

a<- c(1,   1 ,  2 ,  2 ,  3 ,  4 ,  6,   7 ,  7 ,  9,  12,  14 , 24,  30,  42,
      54,  69 , 77 , 84 , 90 , 95 , 95 , 98 , 99,  99,  99 , 99 , 99,  99, 100)

Curve_adop[,1]


range_yields<- rep(x =5:80, 1)


# R options
options(warn = -1)
options(scipen = 999)

# 
# ### parameters 
# q= (4*100)*4.5  ## total production 
# r_d_budget=5000000 # development technology
# time_inves=5
# cfiles$rate_adop<-(cfiles$nodesAcum/100)*100
# adop<- (cfiles$rate_adop)
# 
# parameters<- list(
#       prob_success=0.8,
#       elas_supply=0.2,
#       elas_demand=-1.4,
#       change_yield=0.2,
#       prod_prices=300, # dolares per ton
#       time=Periodo,
#       input_cost_changes=0.50, #porcentage (%)
#       depre_rate=1, #porcentage (%)
#       discount_rate=0.1, #porcentage (%)
#       # Cantidades= q, # rendimiento promedio * ha * numero de agricultores
#       cre_exo=0.06
# )
# 
# m<- data.frame(Yrs=1:yr,
#                E_ofer= parameters$elas_supply, 
#                E_demand= parameters$elas_demand,
#                Change_yield= parameters$change_yield,
#                Change_equi_yield= parameters$change_yield/parameters$elas_supply, # cambios rendimientos/elas productor
#                Change_cost_he= parameters$input_cost_changes,
#                Change_equi_cost= parameters$input_cost_changes/(1+ parameters$change_yield),
#                Change_net= (parameters$change_yield/parameters$elas_supply)-(parameters$input_cost_changes/(1+ parameters$change_yield)),
#                Prob_exito=parameters$prob_success,
#                Tasa_adop=adop,
#                Tasa_dep=parameters$depre_rate,
#                Cantidades= q
# )
# 
# require(dplyr)
# m<- m %>% mutate(K= Change_net*Prob_exito*Tasa_adop*Tasa_dep,
#                  Z= (E_ofer*K)/(E_ofer+E_demand),
#                  precio=parameters$prod_prices,
#                  Tasa_exter_creci=parameters$cre_exo,
#                  Change_exc_total=(K*precio*Cantidades*(1+0.5*(Z*E_demand))),
#                  Change_exc_pro= precio*Cantidades*(K-Z)*(1+0.5*(Z*E_demand)),
#                  Change_exc_consu=precio*Cantidades*Z*(1+(0.5*Z*E_demand)),
#                  Change_exc_consu=precio*Cantidades*Z*(1+(0.5*Z*E_demand)),
#                  Costos_R_I=0)
# 
# ### data R&D
# rd<- data.frame(Yrs=1:time_inves,E_ofer=0,E_demand=0,Change_yield=0,Change_equi_yield=0,Change_cost_he=0,
#                 Change_equi_cost=0,Change_net=0, Prob_exito=0,Tasa_adop=0,Tasa_dep=0,
#                 Cantidades=0 ,K=0, Z=0, precio=0,Tasa_exter_creci=0, Change_exc_total=0,
#                 Change_exc_pro=0,Change_exc_consu=0,Costos_R_I=r_d_budget/time_inves)
# 
# ###Agregate tiempo para agregar costos y tiempos de inversion
# xfiles<- rbind(rd,m) # aplir las bases de datos
# xfiles<- xfiles %>% mutate(bef_net=Change_exc_total-Costos_R_I)
# ## valor presente neto 
# npv(r = parameters$discount_rate, xfiles[,"bef_net"])
# irr(cf = xfiles[,"bef_net"])   
# xfiles<- xfiles %>% mutate(totalB=sum(bef_net), totalC=sum(Costos_R_I),Rela_Bef_Cost=totalB/totalC ) 

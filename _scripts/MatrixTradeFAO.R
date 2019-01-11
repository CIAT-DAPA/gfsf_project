
#libraries

g=gc;rm(list = ls())
# R options

options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))
suppressMessages(library(curl))
suppressMessages(library(shiny))
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(parallelsugar))
suppressMessages(library(doParallel))
suppressMessages(library(qgraph))





### matrix international trade

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")

faoStat<- read.csv(file = "./Trade_DetailedTradeMatrix_E_All_Data_(Norm).csv", header = T)

cfiles<- faoStat
cfiles$Year.Code<- NULL
# cfiles$Flag<- NULL
cfiles$Element<- as.character(cfiles$Element)
cfiles$Reporter<- as.character(cfiles$Reporter)
cfiles$Partner.Countries<- as.character(cfiles$Partner.Countries)
cfiles$Item<- as.character(cfiles$Item)

cfiles$Element.Code<- NULL
cfiles$Unit<- NULL
cfiles$Item.Code<- NULL

# Just exports
cfiles<- filter(cfiles, Element=="Export Quantity")

# Just majors grains
# pat<- c("Rice", "Drybean", "Maize", "Soybean")

cf<- cfiles[grep(pattern ="(Rice milled equivalent)",x = cfiles$Item, ignore.case = T),] #|Drybean|Maize|Soybean"
rownames(cf)<- 1:nrow(cf)


cf$Reporter<- plyr::revalue(cf$Reporter, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                                               "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                               "Côte d'Ivoire"="Ivory Coast",
                                                               "China, Hong Kong SAR"='Hong Kong',
                                                               "China, Macao SAR"='Macao',
                                                               "China, mainland"='China',
                                                               "China, Taiwan Province of"='Taiwan',
                                                               "RÃ©union"='Reunion',
                                                               "Sudan (former)"="Sudan former",
                                                               "Netherlands Antilles (former)"="Netherlands Antilles",
                                                               "Bolivia (Plurinational State of)"="Bolivia",
                                                               "Venezuela (Bolivarian Republic of)"="Venezuela",
                                                               "Iran (Islamic Republic of)"="Iran",
                                                               "Falkland Islands (Malvinas)"="Falkland Islands" ))

cf$Partner.Countries<- plyr::revalue(cf$Partner.Countries, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                                             "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                             "Côte d'Ivoire"="Ivory Coast",
                                                             "China, Hong Kong SAR"='Hong Kong',
                                                             "China, Macao SAR"='Macao',
                                                             "China, mainland"='China',
                                                             "China, Taiwan Province of"='Taiwan',
                                                             "RÃ©union"='Reunion',
                                                             "Sudan (former)"="Sudan former",
                                                             "Netherlands Antilles (former)"="Netherlands Antilles",
                                                             "Bolivia (Plurinational State of)"="Bolivia",
                                                             "Venezuela (Bolivarian Republic of)"="Venezuela",
                                                             "Iran (Islamic Republic of)"="Iran",
                                                              "Falkland Islands (Malvinas)"="Falkland Islands" ))


#---------------------------------------------------------------------------#
caribbean<- c("Cuba", "Dominican Republic", "Haiti", "Jamaica", "Dominica")      
Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico","Nicaragua","Panama","El Salvador", "Trinidad and Tobago")
Australia_and_New_Zealand<- c( "New Zealand", "Australia")
Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")
Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar","Mozambique","Malawi","Rwanda","Somalia", "Tanzania",
                   "Uganda","Zambia","Zimbabwe")
Eastern_Asia<- c("China","Japan","South Korea","Mongolia","North Korea", "Taiwan")
Eastern_Europe<- c("Bulgaria","Belarus","Czech Republic","Hungary","Moldova","Other Balkans","Poland",
                   "Romania","Russia","Slovakia","Ukraine", "Russian Federation")
Melanesia<- c("Fiji", "Papua New Guinea","Solomon Islands","Vanuatu")
Middle_Africa<- c("Angola","Central African Rep.","Cameroon","DRC","Congo","Gabon","Equatorial Guinea","Chad")
Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Sudan","Tunisia")
Northern_America<-c("Canada","Greenland","USA")
Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","UK")
South_America<- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guyanas","Peru","Paraguay","Uruguay","Venezuela","Suriname")
South_Eastern_Asia<- c("Indonesia","Cambodia","Laos","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "Timor LEste","Vietnam")
Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")
Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Iran","Sri Lanka","Nepal","Other Indian Ocean","Pakistan")
Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia")
Western_Africa<- c("Benin","Burkina Faso","Ivory Coast","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leon","Togo")
Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", "Lebanon","Palestine", "Rest of Arabia","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates")
Western_Europe<- c("Austria","Belgium-Luxembourg","Switzerland","Germany","France","Netherlands","Other Atlantic")
#----------------------------------------------------------------#


##filter by crop
y1<- cf
yc <- y1[c("Reporter.Countries","Partner.Countries", "Year", "Value" )] #circule graph


##ajustar los nucles para correr en paralelo
# cores<- detectCores()
# cl<- makeCluster(cores[1]-2)
# registerDoParallel(cl)

p<- unique(yc$Year)
p<- sort(p)
# x=1

############################ circulos graficos ???##################################
foreach(x=1:length(p)) %dopar%{ 
      require(dplyr)
      require(tidyr)
      require(circlize)
#       cat(paste(">>>>>>>>>>>>> start with ", p[x], " Done!!!!\n ", sep = ""))
      temp<- yc %>% filter(Year==p[x]) %>% select(Reporter,Partner.Countries,Value)
#     
    

      auxCarlos<- expand.grid(unique(temp$Reporter), unique(temp$Partner.Countries))
      colnames(auxCarlos)[1]<-"Reporter"
      colnames(auxCarlos)[2]<- "Partner.Countries"
      auxCarlos2 <- temp[,1:2]
            
      require(sqldf)
      auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
      dim(auxCarlos2)
      auxCarlos2$Value <- 0
      p1wide <- rbind(temp, auxCarlos2)
      
      p1wide <- as.data.frame(p1wide)
      p1wide$Reporter <- as.character(p1wide$Reporter)
      p1wide$Partner.Countries <- as.character(p1wide$Partner.Countries)
      p1wide$Value <- as.numeric(p1wide$Value)
      
      
      treal1<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal2<- p1wide %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- p1wide %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      treal4<- p1wide %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      treal5<- p1wide %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal7<- p1wide %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      treal8<- p1wide %>% filter(Reporter %in% Melanesia) %>% mutate(., zone="Melanesia")
      treal9<- p1wide %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      treal10<- p1wide %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal11<- p1wide %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal12<- p1wide %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal13<- p1wide %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- p1wide %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal15<- p1wide %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal16<- p1wide %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal17<- p1wide %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal18<- p1wide %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      treal19<- p1wide %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal20<- p1wide %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")
      
      r<- rbind(treal1, treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal19,treal20)
      
      rjoin<- left_join(r, p1wide, by = c("Reporter", "Partner.Countries", "Value"))
      
      #   tempcolor<- unique(rjoin$zone)
      colorstemp<- c("##f1eef6", "#d0d1e6","#a6bddb", "#74a9cf", "#3690c0","#0570b0", "#034e7b" )
      rjoin$color<- NA
      rjoin$color[rjoin$zone=="Western_Europe"]<-  "#40004b"
      rjoin$color[rjoin$zone=="South_America"]<-   "#f0027f"
      rjoin$color[rjoin$zone=="Eastern_Asia"]<-    "#33a02c"
      rjoin$color[rjoin$zone=="Northern_Europe"]<- "#74a9cf"
      rjoin$color[rjoin$zone=="South_Eastern_Asia"]<- "#3690c0"
      rjoin$color[rjoin$zone=="Central_America"]<- "#ff7f00"
      rjoin$color[rjoin$zone=="Southern_Europe"]<-  "#034e7b"
      rjoin$color[rjoin$zone=="Australia_and_New_Zealand"]<- "#00441b"
      rjoin$color[rjoin$zone=="Southern_Asia"]<- "#cab2d6"
      rjoin$color[rjoin$zone=="Eastern_Africa"]<- "#fb9a99"
      rjoin$color[rjoin$zone=="Northern_America"]<- "#543005"
      rjoin$color[rjoin$zone=="Melanesia"]<- "#b15928"
      
      q2<- quantile(temp$Value,probs = 0.25)
#       temp<- temp %>% filter(Value>=q2)
      
      color<- rjoin %>% filter(., Value>=q2) %>% select("Reporter","Partner.Countries" , "color")  
      colorsss<- unique(rjoin$color)
      
      rjoin$color<- as.factor(rjoin$color)
      rjoin<- rjoin %>% select("Reporter","Partner.Countries" , "Value","color")
      rdata<- rjoin %>% select("Reporter","Partner.Countries" , "Value")
      
      # grafico codes
      circos.clear()
      circos.par(start.degree = 10, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      par(mar = rep(0, 4))
  
      
      png(paste("./graficosArroz/",p[x],'Expors_Rice.png',sep = ""), width=10, height=10, units='in', res=300)
      
         chordDiagram(x = rdata[which(rdata$Value>=q2),],col=color$color,  transparency = 0.25, #col=color$color #[rdata$mean>0.7,]
                   directional = 1,
                   direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                   annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            xplot = get.cell.meta.data("xplot")
            ylim = get.cell.meta.data("ylim")
            sector.name = get.cell.meta.data("sector.index")
            if(abs(xplot[2] - xplot[1]) < 20) {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                              niceFacing = TRUE, adj = c(0, 0.5))
            } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                              niceFacing = TRUE, adj = c(0.5, 0))
            }
      }, bg.border = NA)
      
      dev.off()
      
      
      dev.off()
      circos.clear()
      
      
}

stopCluster(cl)




#
# #     
# ######## forceNetwork and others graphs
# qu<- quantile(superflow$Value, probs = 0.25)
# forceNetwork(Links= superflow[which(superflow$Value>=qu),], Nodes= nodesmix, Source ="Reporter" ,Target = "Partner",
#              Value ="Value" ,NodeID = "name",Group ="name",zoom = T,opacityNoHover=0.4) 
# 
# simpleNetwork(superflow[which(superflow$Value>=qu),], fontFamily = "fantasy")
# simpleNetwork(superflow[which(superflow$Value>=qu),], fontFamily = "sans-serif")
# 
# 
# # net<- graph_from_data_frame(d=sankeyList$links, vertices = sankeyList$nodes, directed = T)
# # 

############################ networks analysis ##################################

crops<- unique(cf$Item)
# y1<- cf[grep(pattern ="Rice milled",x = cf$Item, ignore.case = T),]
# y1 <- y1[c("Reporter","Partner.Countries", "Year","Value" )]

periodlist<- unique(yc$Year)
periodlist<- sort(periodlist, decreasing = F)

# Creating directories 
main_dir<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/pic"
sub_dir<- "Rice"
out.dir <- file.path(main_dir, sub_dir)

if(!dir.exists(out.dir)){
dir.create(out.dir)
      }else{print("Dir already exists!")
}

# data
colnames(yc)[1]<-"Reporter" 
colnames(yc)[2]<- "Partner"
#j=1

##ajustar los nucles para correr en paralelo
cores<- detectCores()
cl<- makeCluster(cores[1]-2)
registerDoParallel(cl) 

#j=28
foreach(j=1:length(periodlist)) %dopar%{ 
# lapply(1:length(periodlist), function(j){
      
      require(dplyr)
      require(igraph)
      
      cat(paste(">>>>>>>>>>>>> start with ", periodlist[j], " Done!!!!\n ", sep = ""))
      
      # filter data by period
      flows <-  filter(yc, Year==periodlist[[j]]) %>%
            filter(., Partner!="Unspecified") %>% filter(., Value!=0)
      flows$Year <- NULL
      
      auxCarlos<- expand.grid(unique(flows$Reporter), unique(flows$Partner))
      colnames(auxCarlos)[1]<-"Reporter"
      colnames(auxCarlos)[2]<- "Partner"
      auxCarlos2 <- flows[,1:2]
      
      require(sqldf)
      auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
      dim(auxCarlos2)
      auxCarlos2$Value <- 0
      p1wide <- rbind(flows, auxCarlos2)
      
      p1wide <- as.data.frame(p1wide)
      p1wide$Reporter <- as.character(p1wide$Reporter)
      p1wide$Partner<- as.character(p1wide$Partner)
      p1wide$Value <- as.numeric(p1wide$Value)
  
      virtCountryList <- unique(c(p1wide$Reporter, p1wide$Partner)) # sort

      
      # order  data
      p1wide <- p1wide[order(p1wide$Value, decreasing = T),]
      rownames(p1wide) <- 1:nrow(p1wide)
      
      treal1<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal2<- p1wide %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- p1wide %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      treal4<- p1wide %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      treal5<- p1wide %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal7<- p1wide %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      treal8<- p1wide %>% filter(Reporter %in% Melanesia) %>% mutate(., zone="Melanesia")
      treal9<- p1wide %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      treal10<- p1wide %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal11<- p1wide %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal12<- p1wide %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal13<- p1wide %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- p1wide %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal15<- p1wide %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal16<- p1wide %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal17<- p1wide %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal18<- p1wide %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      treal19<- p1wide %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal20<- p1wide %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")
      
      r<- rbind(treal1, treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal19,treal20)
      
      rjoin<- left_join(r, p1wide, by = c("Reporter", "Partner", "Value"))
      
  
      #creation formate more appropiate for our analysis
       dataigraph<- graph_from_data_frame(p1wide[which(p1wide$Value!=0),])
       bad<- V(dataigraph)[igraph::degree(dataigraph)<1] # identify those vertices part of less three edges
       dataigraph<- delete_vertices(dataigraph,bad) # eliminando los nodos con menos de 4 conecciones 
       
       LAC<- as.vector(rbind(South_America,Central_America,caribbean))
       
       #highlight one node
       V(dataigraph)$color<- ifelse(V(dataigraph)$name %in% LAC,"green", "yellow")
       # plot(dataigraph)
    
       #ponderaciones  Weight
#         E(dataigraph)$width<- E(dataigraph)$weight
#         plot(dataigraph,edge.width=E(dataigraph)$weight)

       #### grafica1
       png(paste(main_dir,"/",periodlist[j], "Networkplot1.png", sep = ""), width = 800, height =800, units = "px")
       par(mfrow=c(2,3))
       
       plot(dataigraph, layout=layout_with_fr, edge.arrow.size=0,vertex.size=8, vertex.label=NA)
       mtext("layout_with_fr", side=1)
       
       layout<- layout.fruchterman.reingold(dataigraph)
       plot(dataigraph, layout=layout,edge.arrow.size=0, vertex.size=8, vertex.label=NA)
       mtext("fruchtermanreingold", side=1)
       
       layout<- layout_randomly(dataigraph)
       plot(dataigraph, layout=layout,edge.arrow.size=0, vertex.size=8, vertex.label=NA)
       mtext("layout_randomly", side=1)
       
       layout<- layout_in_circle(dataigraph)
       plot(dataigraph, layout=layout,edge.arrow.size=0, vertex.size=8, vertex.label=NA)
       mtext("layout_in_circle", side=1)
       
       layout<- layout_on_sphere(dataigraph)
       plot(dataigraph, layout=layout,edge.arrow.size=0, vertex.size=8, vertex.label=NA)
       mtext("layout_on_sphere", side=1)
       
       layout<- layout_with_kk(dataigraph,)
       plot(dataigraph, layout=layout,edge.arrow.size=0, vertex.size=8, vertex.label=NA)
       mtext("layout_with_kk", side=1)
       
       dev.off()
       
       # grafica 2
       png(paste(main_dir,"/",periodlist[j], "Networkplot2.png", sep = ""), width = 800, height =800, units = "px")
       layout<- layout.fruchterman.reingold(dataigraph)
       plot(dataigraph, layout=layout,edge.arrow.size=0, vertex.size=8, vertex.label=NA)
       mtext("fruchtermanreingold", side=1)
       
       dev.off()
       #centrality
       cent.eigen<- eigen_centrality(graph = dataigraph)
       #degree
       d<- igraph:: degree(dataigraph, mode="total")
       #degree distribution
       ed_d<- igraph::degree.distribution(dataigraph)
       #Global clustering
       gloClus<- transitivity(dataigraph, type="global")
       #Local
       locaClus<- transitivity(dataigraph, type= "local")
       #cluster_walktrap
       cl_w<- cluster_walktrap(dataigraph) 
       #Betweeness centrality
       betw<- betweenness.estimate(dataigraph, cutoff = 10)
       #eigen centrality
       ec<- eigen_centrality(dataigraph)
       #closeness centrality
       cloCen<-estimate_closeness(dataigraph, cutoff = 10)
       #assortativy
       assor<- assortativity_degree(dataigraph)
       #modularidad
       modula<- modularity(dataigraph, membership(cl_w))
       #nodes and Ver
       conex<- (E(dataigraph))
      
       #Base datos 
       data.centr<-data.frame(centralidad= cent.eigen$vector, deg=d,ClusGlobal=gloClus, 
                              ClusLocal=locaClus,betwNess=betw,
                              eigenCen=ec$vector,CloseCent=cloCen,
                              assort=assor,
                              modularidad=modula,
                              cl_wModu=cl_w$modularity,
                              member=cl_w$membership,
                              year=periodlist[[j]])  
       
       data.centr$country<- row.names(data.centr)
       row.names(data.centr)<- NULL
       data.centr<- data.centr[c("year","country","centralidad", "deg","ClusGlobal","ClusLocal", "betwNess","eigenCen","CloseCent", "assort",
                                 "modularidad" ,"cl_wModu" ,"member" )]
       write.csv(x = data.centr, file = paste(main_dir,"/RiceCent_",periodlist[[j]],".csv", sep = ""))
       
     
       
}
stopCluster(cl)
      

      
d<- list.files(path = paste(main_dir,"/", sep = ""),pattern = "RiceCent_",full.names = T)
d<- lapply(d,read.csv,header=T)
d<- do.call(rbind,d)
d$X<-NULL

write.csv(d, paste(main_dir, "/", "dataCompleta.csv", sep = ""))

# graficos 
### centrality
temp<- d %>% select("country","year" ,"centralidad")

anos<- c( "1986", "1995", "2005", "2013")
temp<- filter(temp, year %in% anos ) %>% spread(year, centralidad)

png(filename =paste(main_dir, "/", sub_dir,"centra86_95.png", sep = "") )
plot(temp$`1986`, temp$`1995`, main="Centrality", xlab = " centrality 1986", 
     ylab = "centrality 1995", pch=19)
abline(lm(temp$`1995`~temp$`1986`), col="red")
dev.off()

png(filename =paste(main_dir, "/", sub_dir,"centra95_05.png", sep = "") )
plot(temp$`1995`, temp$`2005`, main="Centrality", xlab = " centrality 1995", 
     ylab = "centrality 2005", pch=19)
abline(lm(temp$`2005`~temp$`1995`), col="red")
dev.off()


png(filename =paste(main_dir, "/", sub_dir,"centra05_13.png", sep = "") )
plot(temp$`2005`, temp$`2013`, main="Centrality", xlab = " centrality 2005", 
     ylab = "centrality 2013", pch=19)
abline(lm(temp$`2005`~temp$`1995`), col="red")
dev.off()


### global clustering
g<- d %>% select("year", "ClusGlobal") 
Cl<- table(g$ClusGlobal)
yr<- unique(g$year)
data.clus<- data.frame(year=yr, clus=Cl)
colnames(data.clus)[2]<-"ClusGlobal" 
data.clus$clus.Freq<- NULL
data.clus$ClusGlobal<- as.character(data.clus$ClusGlobal)
data.clus$ClusGlobal<- as.numeric(data.clus$ClusGlobal)

png(filename =paste(main_dir, "/", sub_dir,"ClusteringGlobal.png", sep = "") )
plot(data.clus$year, data.clus$ClusGlobal, type = "l")
se.clus<- ts(data = data.clus[2], start =c(1986) )
plot.ts(se.clus,  col = "blue")
dev.off()

###local clustering
Lc<- d %>% group_by(year) %>% mutate(., aveDegree=mean(deg)) %>% select(year,aveDegree)

Cl<- table(Lc$aveDegree)
yr<- unique(Lc$year)
data.clus<- data.frame(year=yr, clus=Cl)
colnames(data.clus)[2]<-"AveDegree" 
data.clus$clus.Freq<- NULL
data.clus$AveDegree<- as.character(data.clus$AveDegree)
data.clus$AveDegree<- as.numeric(data.clus$AveDegree)
# data.clus$AveDegree<- round(data.clus$AveDegree, digits = 0)
png(filename =paste(main_dir, "/", sub_dir,"AverageDegree.png", sep = ""))
se.clus<- ts(data = data.clus[2], start =c(1986) )
plot.ts(se.clus,  col = "blue")
dev.off()


#### total trade
total<- yc %>% group_by(Year) %>% mutate(.,Xtotal=sum(Value)/1000000) %>% select(Year, Xtotal)
total<- total[order(total$Year),]

Cl<- table(total$Xtotal)
yr<- unique(total$Year)
data.clus<- data.frame(year= yr,clus=Cl)
colnames(data.clus)[2]<-"Total Exports" 
data.clus$clus.Freq<- NULL
data.clus$`Total Exports`<- as.character(data.clus$`Total Exports`)
data.clus$`Total Exports`<- as.numeric(data.clus$`Total Exports`)
data.clus$year<- as.numeric(data.clus$year)

se.clus<- ts(data = data.clus[2], start =c(1986))
png(filename =paste(main_dir, "/", sub_dir,"TotalExports.png", sep = ""))
plot.ts(se.clus, pch = ".", col = "blue")
dev.off()


############################################## data densidades #################################
lapply(1:length(crops), function(c){
      # Creating directories 
      main_dir<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/pic"
      sub_dir<- crops[c]
      out.dir <- file.path(main_dir, sub_dir)
      
      if(!dir.exists(out.dir)){
            dir.create(out.dir)
      }else{print("Dir already exists!")
            
      }   
      
      d<- list.files(path = paste(out.dir, "/","igraph/", sep = ""),pattern = ".csv",full.names = T)
      d<- lapply(d,read.csv,header=T)
      d<- do.call(rbind,d)
      d$X<-NULL
      
      write.csv(d, paste(main_dir, "/", sub_dir,"_dataDensity.csv", sep = ""))
})



dd<- list.files(path=main_dir,pattern= ".csv",full.names = T)
dd<- lapply(dd,read.csv,header=F)
dd<- do.call(rbind,dd)
dd<- na.omit(dd)
dd$V1<-NULL
colnames(dd)<-c( "denN", "yr" ,"crop","vcount","ecount" )
      

############################################## codes deleted ##################################

#we can also color the connecting edges differently depending on the grade



#       # creacion listas
#       sankeyList <- list(links = flows,
#                          nodes = virtNodes)
#       
#     
#       # make and save each plot
#       s<- simpleNetwork(sankeyList$links, fontFamily = "fantasy",linkDistance =300,fontSize =20,
#                         opacity = 0.6,linkColour ="#104e8b", nodeColour = "#0008", zoom = T) #charge = 0.5
#       
#       saveNetwork(s, paste(out.dir,"/SimpleNetwork",periodlist[j],".html", sep = ""), selfcontained = T) 
#       
#       
#      




#       # to get the more appropiate for igraph
#       net<- graph_from_data_frame(d=sankeyList$links, vertices = sankeyList$nodes, directed = T)
#       class(net)
#       E(net) # print the list of edges per vertex
#       V(net) # list of vertices(people)
#       
#       # plot 
#       plot(net)
#       net<- simplify(net,remove.loops = T, remove.multiple = F)
#       
#       
#       # subxet that data. If we  want to exclude pleople  who are in the network only tangentially(participate 1 or 2)
#       # relationship
#       
#       bad<- V(dataigraph)[degree(dataigraph)<3] # identify those vertices part of less three edges
#       # network layouts 
#       # hist(flows$Value)
#       mean(flows$Value)
#       sd(flows$Value)
#       E(net)
#       
#       
#       ## calculate centrality measures for social 
#       # indegree centrality measures how many people direct social
#       indegree_social<- degree(dat = s, mode='in')
#       degree(dat = sankeyList$links,g = 1,nodes = sankeyList$nodes)
#       # degree(dat = as.matrix(sankeyList$flows), g=1, nodes = sankeyList$nodes)
#       
#       # Obteniendo la mediana
#       cut.off<- quantile(flows$Value, probs = 0.5) # mean(flows$Value)
#       
#       # eliminar vertices y lazos por debajo de la mediana y sin ningun tipo de conexion 
#       net<-  delete_edges(net, E(net)[Value<=cut.off])
#       net<- delete_vertices(net,V(net)[igraph::degree(net)==0])
#       
#       lac<- c( "Argentina", "Mexico", "Venezuela","Paraguay", "Peru", "Brazil","Chile", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago",
#               "Barbados","Panama", "El Salvador", "Suriname", "Uruguay", "Honduras","Martinique", "Nicaragua", "Haiti","Colombia", "Costa Rica", 
#               "Aruba", "Belize","Ecuador", "Guatemala","Chile", "Bolivia (Plurinational State of)")
#       
#       #grafico 1
#       #layout type
#       l<- layout.star(net) #layout.mds
#       l<- norm_coords(l, xmin = -1, xmax = 1, ymin = -1, ymax = 1)
#       # color specific nodes
#       
#       V(net)$color<- "gray"
#       V(net)$color[V(net)$name %in% lac]<- "red"
#       
#       
#       
#       # we use degree as measure of how many point are conectec to each vertex
#       V(net)$vertex_degree<- igraph::degree(net)
#       
#       jpeg(filename = paste(out.dir, "/","igraph/", cul, "_igraph", periodlist[j], ".jpeg", sep = ""), width =6000, height = 6000, res=600 )
#       plot(net, layout=l,#vertex.label=V(net)$name,
#           # margin=0.05, 
#           arrow.mode=3,
#           vertex.label.cex=1,
#           edge.label.family="Times",
#           edge.arrow.width=0.5,
#           edge.width=0.5, 
#           vertex.size=V(net)$vertex_degree,
#           vertex.label.dist=1,
#           vertex.shape="square",
#           vertex.color= V(net)$color,
#           main=paste("Exports: Network Rice \n Year: ",periodlist[j], ", over median by ", cut.off, " tonnes", sep = ""))
#       
#       dev.off()
#       
#       # grafico 2
#       
#       #layout type
#       l<- layout_nicely(net) #layout.mds
#       # l<- norm_coords(l, xmin = -1, xmax = 1, ymin = -1, ymax = 1)
#       # color specific nodes
#       
#       
#       
#       #Density
#       denN<- edge_density(net, loops = F) # The density of a graph is the ratio of the number of edges and the number of possible edges.
#       denN<- as.data.frame(denN)
#       denN$yr<- periodlist[j]
#       denN$crop<- cul
#       denN$vcount<- vcount(net) # Order (number of vertices) of a graph
#       denN$ecount<- ecount(net) #The size of the graph (number of edges)
#       
#       write.csv(denN, paste(out.dir, "/","igraph/", cul, "_ratioDensityGraph_", periodlist[j], ".csv", sep = ""))
#       
#       ego_size(net, order = 1, nodes = V(net))
#       neir<- make_ego_graph(graph = net,order = 1,nodes = V(net), mode = c("all")) #  is creates (sub)graphs from all neighborhoods of the given vertices with the given order parameter. This function preserves the vertex, edge and graph attributes.
#       


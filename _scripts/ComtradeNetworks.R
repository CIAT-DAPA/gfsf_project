
# Code for create networks analysis and plotting diferents linkages
# Carlos Edo
################ library networks 

#packages
# install.packages("igraph") 
# install.packages("network") 
# install.packages("sna")
# install.packages("visNetwork")
# install.packages("threejs")
# install.packages("networkD3")
# install.packages("ndtv")
# 

#load libraries
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(networkD3))
suppressMessages(library(ndtv))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape))
suppressMessages(library(reshape2))

#defined directory
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")

d<- readRDS(file = "./datatotal.rds")

export<- filter(d, Trade.Flow=="Export") %>% filter(., Year==2014) %>% filter(., Commodity.Code==1006)

# datos para crear links=="from","to","weight","type" 
# datos para crear nodes=="id","media","media.type","type.label","audience.size"
namesReport<- export[,c("Reporter.Code","Reporter")]
namesReport<- namesReport[!duplicated(namesReport),]
row.names(namesReport)<- 1:nrow(namesReport)
nodesExport<- namesReport
linksExport<- export[,c("Reporter.Code", "Partner.Code","Alt.Qty.Unit","Commodity.Code")]


# exploration 
head(nodesExport)
head(linksExport)
nrow(nodesExport); length(unique(nodesExport$Reporter.Code))
nrow(linksExport); nrow(unique(links[,c("Reporter.Code", "Partner.Code")]))

net <- graph_from_data_frame(d=linksExport, vertices=nodesExport, directed=T) 
                              
##### redes datos de jean
g=gc;rm(list = ls())

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDSNA")


######################################### librerias #####################################
suppressMessages(library(rjson))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(circlize))
suppressMessages(library(sna))
suppressMessages(library(statnet))
suppressMessages(library(igraph))
suppressMessages(library(network))


######################################## rescale function ###############################
rescale <- function(nchar,low,high) {
      min_d <- min(nchar)
      max_d <- max(nchar)
      rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
      rscl
}

######################################## datos ##########################################

cmatrix <- read.csv("./sna.csv", header = T, row.names = 1) %>% as.matrix
diag(cmatrix) <- NA
# Matriz de intercambio y coordinacion
intercambio <- cmatrix
intercambio[!lower.tri(cmatrix)] <- NA
intercambio[upper.tri(intercambio)] <- t(intercambio)[upper.tri(t(intercambio))]
diag(intercambio) <- 0



############################# graph ###################################################
interTest<- network(intercambio,matrix.type="adjacency", directed = F ) 
detach(package:statnet)
suppressMessages(library(igraph))
suppressMessages(library(intergraph))
i <- asIgraph(interTest)
degi<- igraph::degree(i, mode = "total")

png(paste("./PPA_SNA.png", sep = ""), 
    width = 600, height = 600)

op <- par(mfrow=c(1,1)) #c(1,0,1,1),
V(i)$size=(rescale(degi,1,10))*2.5
E(i)$arrow.size <- .9
# E(i)$width <- degi
plot(interTest,vertex.label=V(interTest)$vertex.names, layout=layout.fruchterman.reingold,
     edge.color="grey", vertex.label.dist=0.5,vertex.label.font=2,
     vertex.label.cex=0.9, vertex.frame.color="#ffffff", 
     main="Network de relaciones" )#,edge.width=(degi/5)*2 ) #, main=paste("Visualización de ",ti, sep = "" ))
legend(x=-1.2, y=1.2,c("Publico", "Privado", "ONG", "Aso.Prod", "Coop.Inter", "Otro"), pch=21,
       col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
       title="Tipos", pt.bg=c("red","green","purple", "lightblue", "orange","black")) # pt.bg=unique(V(i)$color)

par(op)
dev.off()


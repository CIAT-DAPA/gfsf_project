#### sce3 Vs sce1
#### Codigo para procesar resultados de WB

### libraries 
## limpiar consola
g=gc;rm(list = ls())

### librerias 
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(rgdal))
suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools))
suppressMessages(library(sp))
suppressMessages(library(maps))
suppressMessages(library(raster))
suppressMessages(library(cumplyr))
suppressMessages(library(scales))
suppressMessages(library(ff))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))


### directories
#Definir directorio de trabajo
# setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB")
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/RTB2/")
cropsin<- c("R&T-Other Roots","R&T-Yams","R&T-Sweet Potato","R&T-Potato","R&T-Cassava", "F&V-Banana" )

cores<- detectCores()
cl<- makeCluster(cores[1]-4)
registerDoParallel(cl)

yuca<- data.table::fread(file = "./yuca.csv")
# ssp<- split(yuca,yuca$scenario) %>% saveRDS(object=., paste(copy,unique(.$scenarios),".rds"))

yuca<- yuca %>% filter(commodity %in% cropsin)
saveRDS(yuca, file =paste(copy,"Y.rds",sep = ""))





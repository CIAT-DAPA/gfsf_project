#### modelo para TCN. Carlos Eduardo Gonzalez R. 
g=gc;rm(list = ls())

### paquetes-----

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(sf))
suppressMessages(library(rgdal))
options(digits=2)

### directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/TNC project/TNC")


### cargar los datos area------------
xx<- list.files( pattern = "AREAX0", full.names = T)
### extrayendo nombres de los escenarios
sce<- lapply(1:length(xx), function(i){
      xx[i]<- gsub("./AREAX0_",replacement = "",  xx[i])
      xx[i]<- gsub("_CC_SSP2.csv",replacement = "",  xx[i])
})

### convirtiendo en archivo .csv
xx<- lapply(xx, read.csv)
# i=1
zz<- lapply(1:length(xx), function(i){
      
      test<- xx[[i]]
      # test$sce<- sce[[i]]
      x<- as.data.frame(names(test))
      x<- names(test)
      cc<- rbind(x, test)
      colnames(cc)[1]<- "fpu"
      colnames(cc)[2]<- "crop"
      colnames(cc)[3]<- "system"
      colnames(cc)[4]<- "year"
      colnames(cc)[5]<- "Val"
      
      cc$year <- gsub("x", "", cc$year, ignore.case = T)
      cc$Val <- gsub("x", "", cc$Val, ignore.case = T)
      cc$fpu<- as.character(cc$fpu)
      cc$crop<- as.character(cc$crop)
      cc$system<- as.character(cc$system)
      cc$year<- as.integer(cc$year)
      cc$year<- as.numeric(cc$year)
      
      
      cc$Val<- as.numeric(as.character(cc$Val))
      cc$Val<- as.numeric(cc$Val)
      cc$sce<- sce[[i]]
      
      return(cc)
      
})

area<- do.call(rbind,zz)

rm(sce,xx,zz)


### cargar los datos GDPFPUHX0------------
xx<- list.files( pattern = "GDPFPUHX0", full.names = T)
### extrayendo nombres de los escenarios
sce<- lapply(1:length(xx), function(i){
      xx[i]<- gsub("./GDPFPUHX0_",replacement = "",  xx[i])
      xx[i]<- gsub("_CC_SSP2.csv",replacement = "",  xx[i])
})

### convirtiendo en archivo .csv
xx<- lapply(xx, read.csv)
# i=1
zz<- lapply(1:length(xx), function(i){
      
      test<- xx[[i]]
      # test$sce<- sce[[i]]
      x<- as.data.frame(names(test))
      x<- names(test)
      cc<- rbind(x, test)
      colnames(cc)[1]<- "fpu"
      colnames(cc)[2]<- "zone"
      colnames(cc)[3]<- "year"
      colnames(cc)[4]<- "Val"
      
      cc$year <- gsub("x", "", cc$year, ignore.case = T)
      cc$Val <- gsub("x", "", cc$Val, ignore.case = T)
      cc$fpu<- as.character(cc$fpu)
      cc$zone<- as.character(cc$zone)
      cc$year<- as.integer(cc$year)
      cc$year<- as.numeric(cc$year)
      
      
      cc$Val<- as.numeric(as.character(cc$Val))
      cc$Val<- as.numeric(cc$Val)
      cc$sce<- sce[[i]]
      
      return(cc)
      
})

gdp<- do.call(rbind,zz)

rm(sce,xx,zz)


### cargar los datos PopFPUHX0------------
xx<- list.files( pattern = "PopFPUHX0", full.names = T)
### extrayendo nombres de los escenarios
sce<- lapply(1:length(xx), function(i){
      xx[i]<- gsub("./PopFPUHX0_",replacement = "",  xx[i])
      xx[i]<- gsub("_CC_SSP2.csv",replacement = "",  xx[i])
})

### convirtiendo en archivo .csv
xx<- lapply(xx, read.csv)
# i=1
zz<- lapply(1:length(xx), function(i){
      
      test<- xx[[i]]
      # test$sce<- sce[[i]]
      x<- as.data.frame(names(test))
      x<- names(test)
      cc<- rbind(x, test)
      colnames(cc)[1]<- "fpu"
      colnames(cc)[2]<- "zone"
      colnames(cc)[3]<- "year"
      colnames(cc)[4]<- "Val"
      
      cc$year <- gsub("x", "", cc$year, ignore.case = T)
      cc$Val <- gsub("x", "", cc$Val, ignore.case = T)
      cc$fpu<- as.character(cc$fpu)
      cc$zone<- as.character(cc$zone)
      cc$year<- as.integer(cc$year)
      cc$year<- as.numeric(cc$year)
      
      
      cc$Val<- as.numeric(as.character(cc$Val))
      cc$Val<- as.numeric(cc$Val)
      cc$sce<- sce[[i]]
      
      return(cc)
      
})

pop<- do.call(rbind,zz)

rm(sce,xx,zz)


### cargar los datos QFSX0------------
xx<- list.files( pattern = "QFSX0", full.names = T)
### extrayendo nombres de los escenarios
sce<- lapply(1:length(xx), function(i){
      xx[i]<- gsub("./QFSX0_",replacement = "",  xx[i])
      xx[i]<- gsub("_CC_SSP2.csv",replacement = "",  xx[i])
})

### convirtiendo en archivo .csv
xx<- lapply(xx, read.csv)
# i=1
zz<- lapply(1:length(xx), function(i){
      
      cc<- xx[[i]]
      # test$sce<- sce[[i]]
      # x<- as.data.frame(names(test))
      # x<- names(test)
      # cc<- rbind(x, test)
      colnames(cc)[1]<- "fpu"
      colnames(cc)[2]<- "year"
      cc<- cc %>% tidyr::gather(sys,Val, 3:ncol(cc))
      
      # cc$year <- gsub("x", "", cc$year, ignore.case = T)
      # cc$Val <- gsub("x", "", cc$Val, ignore.case = T)
      cc$fpu<- as.character(cc$fpu)
      # cc$crop<- as.character(cc$crop)
      cc$sys<- as.character(cc$sys)
      cc$year<- as.integer(cc$year)
      cc$year<- as.numeric(cc$year)
      
      
      cc$Val<- as.numeric(as.character(cc$Val))
      cc$Val<- as.numeric(cc$Val)
      cc$sce<- sce[[i]]
      
      return(cc)
      
})

qfs<- do.call(rbind,zz)

rm(sce,xx,zz)


### cargar los datos YLDX0------------
xx<- list.files( pattern = "YLDX0_", full.names = T)
### extrayendo nombres de los escenarios
sce<- lapply(1:length(xx), function(i){
      xx[i]<- gsub("./YLDX0_",replacement = "",  xx[i])
      xx[i]<- gsub("_CC_SSP2.csv",replacement = "",  xx[i])
})

### convirtiendo en archivo .csv
xx<- lapply(xx, read.csv)
# i=1
zz<- lapply(1:length(xx), function(i){
      
      test<- xx[[i]]
      # test$sce<- sce[[i]]
      x<- as.data.frame(names(test))
      x<- names(test)
      cc<- rbind(x, test)
      colnames(cc)[1]<- "fpu"
      colnames(cc)[3]<- "crop"
      colnames(cc)[2]<- "system"
      colnames(cc)[4]<- "year"
      colnames(cc)[5]<- "Val"
      
      cc$year <- gsub("x", "", cc$year, ignore.case = T)
      cc$Val <- gsub("x", "", cc$Val, ignore.case = T)
      cc$fpu<- as.character(cc$fpu)
      cc$crop<- as.character(cc$crop)
      cc$system<- as.character(cc$system)
      cc$year<- as.integer(cc$year)
      cc$year<- as.numeric(cc$year)
      
      
      cc$Val<- as.numeric(as.character(cc$Val))
      cc$Val<- as.numeric(cc$Val)
      cc$sce<- sce[[i]]
      
      return(cc)
      
})

yield<- do.call(rbind,zz)

rm(sce,xx,zz)


## cargar los datos animales----------
xx<- list.files( pattern = "ANIMAL_", full.names = T)
### extrayendo nombres de los escenarios
sce<- lapply(1:length(xx), function(i){
      xx[i]<- gsub("./YLDX0_",replacement = "",  xx[i])
      xx[i]<- gsub("_CC_SSP2.csv",replacement = "",  xx[i])
})

### convirtiendo en archivo .csv
xx<- lapply(xx, read.csv)
# i=1
zz<- lapply(1:length(xx), function(i){
      
      test<- as.data.frame(xx[[i]])
      test$FPU<- as.character(test$FPU)
      test$AnimalNumber<- as.character(test$AnimalNumber)
      
      test<- test %>% dplyr::select(FPU, AnimalNumber, year, Urban)
      
      colnames(test)[1]<- "fpu"
      colnames(test)[4]<- "Val"
      
      cc<- test
      cc$year<- as.integer(cc$year)
      cc$year<- as.numeric(cc$year)
      cc$sce<- sce[[i]]
      
      return(cc)
      
})

animal<- do.call(rbind,zz)
animal$AnimalNumber<- plyr::revalue(animal$AnimalNumber,c("jbeef"="Cattle ranch",
                                                    "jlamb"="Sheep, lamb, goat production",
                                                    "jpoul"="Poultry",
                                                    "jeggs"="Egg production",
                                                    "jmilk"="Dairy production",
                                                    "jpork"="Pigs"))

rm(sce,xx,zz)


##### cargar shape file #####
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(Hmisc))
suppressMessages(library(raster))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape))
suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools))
suppressMessages(library(gridExtra))
suppressMessages(library(sp))
suppressMessages(library(maptools))
suppressMessages(library(maps))
suppressMessages(library(raster))
suppressMessages(library(sf))

alc <- shapefile("./Shape_files/FPU_Latinoamerica.shp")
dataShp<- alc@data
fpuAlc<-unique(dataShp$New_FPU)

#### cambios en porcentuales #####
options(digits=2)
crops<- unique(area$crop)
cultivos<- c("jsoyb","jmaiz", "jrice","jpalm")
ani<- unique(animal$AnimalNumber)
scenarios<- unique(area$sce)

#1) area
areaF<- area %>% dplyr::filter(.,crop %in% cultivos)
areaF$crop<- plyr::revalue(areaF$crop,c("jsoyb"="Soybean",
                                        "jmaiz"="Maize",
                                        "jrice"="Rice",
                                        "jpalm"="Palm Fruit"))

areaF$system<- plyr::revalue(areaF$system,c("air"="Irrigated",
                                            "arf"="Rainfed"))

areaF<- areaF %>% group_by(system) %>% spread("year", "Val")
areaF$Change<- ((areaF$`2050`-areaF$`2010`)/areaF$`2010`)*100 
areaF<- areaF %>% dplyr::select(fpu,crop,system,sce,Change) %>% dplyr::filter(.,fpu %in% fpuAlc)
scenarios<- unique(areaF$sce)
sistemas<- unique(areaF$system)
areatest<- areaF

### areaF resultado
Map_LatinAmerica<- fortify(alc)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

areaF<- areaF %>% spread(sce,Change)
areaF$NOCC<- NULL
areaF$CC_mean<- apply(areaF[,4:ncol(areaF)],1, mean, na.rm = TRUE)
areaF<- areaF %>% dplyr::select(fpu, crop, system, CC_mean)


## analisis para Area
#################################  RICE ####################################### 
# t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(t in 1:length(sistemas)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>% filter(., crop=="Rice") #%>% filter(., sce==scenarios[s])%>%
      colnames(proof)[1]<-"New_FPU"
      require(sf)
      require(maptools)
      require(rgdal)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Rice_", sistemas[t],sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Rice",sistemas[t],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[t]]<- ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC_mean)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, na.value="grey",breaks=seq(-50,50,10)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Rice ", sistemas[t], sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[t]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Rice_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], labels = c("A", "B"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  MAIZE ####################################### 
# t=1 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(t in 1:length(sistemas)){
      proof<- areaF %>% filter(.,system==sistemas[t]) %>% filter(., crop=="Maize") #filter(., sce==scenarios[s])%>%
      colnames(proof)[1]<-"New_FPU"
      

      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Maize_", sistemas[t], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Maize",sistemas[t],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[t]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC_mean)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-20,160,30)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Maize", sistemas[t],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[t]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Maize_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], labels = c("A", "B"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  SOYBEAN ####################################### 
t=1 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(sistemas)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>% filter(., crop=="Soybean") #filter(., sce==scenarios[s])%>% 
           
      colnames(proof)[1]<-"New_FPU"
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Soybean_", sistemas[t],sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Soybean",sistemas[t],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC_mean)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-20,100,20)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Soybean", sistemas[t], sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Soybean_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], labels = c("A", "B"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  Palm Fruit ####################################### 
# t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(t in 1:length(sistemas)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>%filter(., crop=="Palm Fruit") # filter(., sce==scenarios[s])%>% 
            
      colnames(proof)[1]<-"New_FPU"
      
#       upperlimit<- max(proof$Change)
#       summary(proof$Change)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_PalmFruit_", sistemas[t],sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Palm Fruit",sistemas[t],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC_mean)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-10,110,20)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Palm Fruit", sistemas[t],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Palm Fruit_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], labels = c("A", "B"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()



### tabla de impacto-------- 
mdwide<- data.frame(areatest,"Cat"=ifelse(areatest$sce=="NOCC","NOCC","CC"))

areaw<- mdwide %>% spread(sce,Change)
areaw$median<- apply(areaw[,5:8],1, median, na.rm = TRUE)
areaw<- as.data.frame(areaw)
areaw<- areaw[c( "fpu", "crop", "system" , "Cat", "median" )]
areaw<- areaw %>% spread(Cat, median)
areaw$pp<- areaw$CC-areaw$NOCC
areaw<- areaw %>% gather(Sce, Val, 4:ncol(areaw))

write.csv(areaw, "./olddata/ReportArea.csv")


# hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
# png(filename= paste("./oldpic/","AREA_HeapMap.png",sep=""), 
#     width = 9, height = 11, units = 'in', res = 300)
# 
# 
# n<- ggplot(data =areaw, aes(Sce, fpu)) + 
#       geom_tile(aes(fill = Val), colour = "white")+  facet_grid(.~crop, drop = T)+
#       labs(x=NULL, y=NULL, title=paste("Area Changes by scenario\n from 2010 to 2050",sep = "")) +
#       scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") +  #  theme_grey() 
#       scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(91/100)+ 
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme_grey() + labs(x = "",y = "")+
#       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
#       theme(axis.text.y = element_text(hjust = 1, size = 11))+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#       theme(strip.text=element_text(size=8))+
#       theme(strip.text.y = element_text(angle = 0,size = 11)) 
# 
# 
# plot(n)
# dev.off()  

#2) Yield----------------
yieldF<- yield %>% dplyr::filter(.,crop %in% cultivos)
yieldF$crop<- plyr::revalue(yieldF$crop,c("jsoyb"="Soybean",
                                          "jmaiz"="Maize",
                                          "jrice"="Rice",
                                          "jpalm"="Palm Fruit"))

yieldF$system<- plyr::revalue(yieldF$system,c("air"="Irrigated",
                                              "arf"="Rainfed"))

yieldF<- yieldF %>% group_by(system) %>% spread("year", "Val")
yieldF$Change<- ((yieldF$`2050`-yieldF$`2010`)/yieldF$`2010`)*100 
yieldF<- yieldF %>% dplyr::select(fpu,crop,system,sce,Change) %>% dplyr::filter(.,fpu %in% fpuAlc)
scenarios<- unique(yieldF$sce)
sistemas<- unique(yieldF$system)


#################################  RICE ####################################### 
t=2 ### definicion del sistema irrigado==1 secano==2

pic<- list()
for(s in 1:length(scenarios)){
      proof<- yieldF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% filter(., crop=="Rice")
      colnames(proof)[1]<-"New_FPU"
      summary(proof$Change)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Yield_Rice_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Yield_Rice",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, na.value="grey",breaks=seq(-10,80,20)) +
            labs(x=NULL, y=NULL, title= paste("Rice Yield ", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Yield_Rice",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  MAIZE ####################################### 
#t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- yieldF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% 
            filter(., crop=="Maize")
      colnames(proof)[1]<-"New_FPU"
      summary(proof$Change)
      upperlimit<- max(proof$Change)
      
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Yield_Maize_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Yield_Maize",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-10,110,30)) +
            labs(x=NULL, y=NULL, title= paste("Maize Yield ", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Yield_Maize",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  SOYBEAN ####################################### 
#t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- yieldF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% 
            filter(., crop=="Soybean")
      colnames(proof)[1]<-"New_FPU"
      summary(proof$Change)
      upperlimit<- max(proof$Change)
      summary(proof$Change)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Yield_Soybean_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Yield_Soybean",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-40,100,20)) +
            labs(x=NULL, y=NULL, title= paste("Soybean yield ", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Yield_Soybean",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  Palm Fruit ####################################### 
#t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- yieldF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% 
            filter(., crop=="Palm Fruit")
      colnames(proof)[1]<-"New_FPU"
      summary(proof$Change)
      upperlimit<- max(proof$Change)
      summary(proof$Change)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Yield_PalmFruit_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Yield_Palm Fruit",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-20,100,20)) +
            labs(x=NULL, y=NULL, title= paste("Palm Fruit Yield", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Yield_Palm Fruit",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#### Tabla de impacto-------- 
mdwidey<- data.frame(yieldF,"Cat"=ifelse(yieldF$sce=="NOCC","NOCC","CC"))

yieldw<- mdwidey %>% spread(sce,Change)
yieldw$median<- apply(yieldw[,5:8],1, median, na.rm = TRUE)
yieldw<- as.data.frame(yieldw)
yieldw<- yieldw[c( "fpu", "crop", "system" , "Cat", "median" )]
yieldw<- yieldw %>% spread(Cat, median)
yieldw$pp<- yieldw$CC-yieldw$NOCC
yieldw<- yieldw %>% gather(Sce, Val, 4:ncol(yieldw))

write.csv(yieldw, "./olddata/ReportYield.csv")


# hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
# png(filename= paste("./oldpic/","YIELD_HeapMap.png",sep=""), 
#     width = 9, height = 11, units = 'in', res = 300)
# labs2 = 'Percentage change(%)/diff(pp) \n2010 to 2050'
# 
# n<- ggplot(data =yieldw, aes(Sce, fpu)) + 
#       geom_tile(aes(fill = Val), colour = "white")+  facet_grid(.~crop, drop = T)+
#       labs(x=NULL, y=NULL, title=paste("Yields comparision by scenarios \n from 2010 to 2050",sep = "")) +
#       scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
#       scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(91/100)+ 
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme_grey() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
#       theme(axis.text.y = element_text(hjust = 1, size = 11))+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#       theme(strip.text=element_text(size=8))+
#       theme(strip.text.y = element_text(angle = 0,size = 11)) 
# 
# 
# plot(n)
# dev.off()  


#### Combinando Yields and Area-------
areaw$var<- "Area"
yieldw$var<- "Yield"

pots<- c("ORI_VEN","RVE_VEN","URU_URY","AMA_PER",
         "PEC_PER", "PAR_PRY","PAN_PAN",
         "CRB_CRB",  "NIC_NIC", "MIM_MEX",
         "RIG_MEX",  "UME_MEX","YUC_MEX",
         "JAM_JAM", "HND_HND",
         "HTI_HTI","GSA_GSA", "GTM_GTM",
         "SLV_SLV", "AMA_ECU",
         "NWS_ECU", "DOM_DOM",
         "CUB_CUB", "CRI_CRI", "AMA_COL",
         "NWS_COL", "ORI_COL", "CHC_CHL",
         "AMA_BRA", "NEB_BRA", "PAR_BRA",
         "SAN_BRA", "TOC_BRA", "URU_BRA",
         "AMA_BOL","PAR_BOL","BLZ_BLZ",
         "PAR_ARG", "RIC_ARG", "SAL_ARG","TIE_ARG")


ay<- rbind(areaw,yieldw)
ay$fpu <- factor(ay$fpu, levels = c(pots))

# s=1
# for(s in 1:length(sistemas)){
#       
#       hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
#       png(filename= paste("./oldpic/", sistemas[s],"_AREA_YIELD_HeapMap.png",sep=""), 
#           width = 10, height = 11, units = 'in', res = 300)
#       labs2 = '% & diff(pp)\n2010 to 2050'
#       
#       
#       n<- ggplot(data =ay[which(ay$system==sistemas[s]),], aes(Sce, fpu)) + 
#             geom_tile(aes(fill = Val), colour = "white")+  facet_wrap(var~crop, nrow = 1)+
#             labs(x=NULL, y=NULL, title=paste(sistemas[s],", area and yield impacts by scenarios",sep = "")) +
#             scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
#             scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + #coord_equal()+ 
#             theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#             theme_grey() +
#             theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
#             theme(axis.text.y = element_text(hjust = 1, size = 11))+
#             theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#             theme(strip.text=element_text(size=8))+
#             theme(strip.text.y = element_text(angle = 0,size = 11)) 
#       
#       
#       plot(n)
#       dev.off()  
# }


######################### Supply land  ##################---------------
# qfs$sys<- plyr::revalue(qfs$sys,c("air"="Irrigated","arf"="Rainfed", "gir"="Intermediated"))

qfsTotal<- qfs %>% spread(sys, Val)
qfsTotal$total<- rowSums(qfsTotal[4:ncol(qfsTotal)],na.rm=T)
qfsTotal<- qfsTotal %>% dplyr::select(fpu, year, sce, total)
qfsTotal<- qfsTotal %>% spread(year, total)
qfsTotal$Change<- ((qfsTotal$`2050`-qfsTotal$`2010`)/qfsTotal$`2010`)*100 
qfsTotal<- qfsTotal %>% dplyr::select(fpu, sce, Change)%>% dplyr::filter(.,fpu %in% fpuAlc)
qfsTotal<- data.frame(qfsTotal,"Cat"=ifelse(qfsTotal$sce=="NOCC","NOCC","CC"))
qfsTotal<- qfsTotal %>% spread(sce, Change)
qfsTotal$median<- apply(qfsTotal[,3:6],1, median, na.rm = TRUE)
qfsTotal<- as.data.frame(qfsTotal)
qfsTotal<- qfsTotal[c( "fpu", "Cat", "median")]
qfsTotal<- qfsTotal %>% spread(Cat, median)
write.csv(qfsTotal,"./olddata/ChangeSupplyLand.csv")

colnames(qfsTotal)[1]<-"New_FPU"
require(sf)
require(maptools)
alc.sf <- st_as_sf(alc)
alc.sf <- left_join(alc.sf, qfsTotal, by = c('New_FPU'))
alc.sh <- as(alc.sf, 'Spatial')
shp<- alc.sh
writeOGR(alc.sh, dsn = './oldpic/shapes', layer = "AreExpansion", driver = 'ESRI Shapefile')
ftfy <- fortify(shp, region = 'New_FPU')
tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))

#Coropleta 
png(filename = paste("./","Expansion_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

labs2 = 'Percentage Change\nfrom 2010 to 2050'
pic<- ggplot() +
      geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC)) +
      geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
      coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
      scale_fill_gradient2(low="#ca0020", mid="white", high="blue", midpoint = 0, na.value="grey",breaks=seq(0,50,10)) +
      labs(x=NULL, y=NULL, title="Expansion Land supply")+
      theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))



plot(pic)
dev.off()
      


## pp
qfsTotal$pp<- qfsTotal$CC-qfsTotal$NOCC
qfsTotal<- qfsTotal %>% gather(Sce, Val, 2:ncol(qfsTotal))
qfsTotalw<- qfsTotal
write.csv(qfsTotal,"./olddata/SupplyLand.csv")

pots<- c("ORI_VEN","RVE_VEN","URU_URY","AMA_PER",
         "PEC_PER", "PAR_PRY","PAN_PAN",
         "CRB_CRB",  "NIC_NIC", "MIM_MEX",
         "RIG_MEX",  "UME_MEX","YUC_MEX",
         "JAM_JAM", "HND_HND",
         "HTI_HTI","GSA_GSA", "GTM_GTM",
         "SLV_SLV", "AMA_ECU",
         "NWS_ECU", "DOM_DOM",
         "CUB_CUB", "CRI_CRI", "AMA_COL",
         "NWS_COL", "ORI_COL", "CHC_CHL",
         "AMA_BRA", "NEB_BRA", "PAR_BRA",
         "SAN_BRA", "TOC_BRA", "URU_BRA",
         "AMA_BOL","PAR_BOL","BLZ_BLZ",
         "PAR_ARG", "RIC_ARG", "SAL_ARG","TIE_ARG")

# qfsTotal<- qfsTotal %>% gather(sce,val, 2:ncol(qfsTotal))
# qfsTotal$fpu <- factor(qfsTotal$fpu, levels = c(pots))

# hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
# png(filename= paste("./oldpic/SupplyLand_HeapMap.png",sep=""), 
#     width = 10, height = 11, units = 'in', res = 300)
# labs2 = 'Percentage change'
# 
# 
# n<- ggplot(data =qfsTotal, aes(sce, fpu)) + 
#       geom_tile(aes(fill = val), colour = "white")+
#       labs(x=NULL, y=NULL, title="Supply land percentage change \nfrom 2010 to 2050") +
#       scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
#       scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(80/100)+ 
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme_grey() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
#       theme(axis.text.y = element_text(hjust = 1, size = 11))+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#       theme(strip.text=element_text(size=8))+
#       theme(strip.text.y = element_text(angle = 0,size = 11)) 
# 
# 
# plot(n)
# dev.off()  

###### logica los impactos cultivos-----------

#### analisis
#1) filtrar systemas
sis<- unique(ay$system)
p<- list()
# s=1
for(s in 1:length(sis)){
      irri<- ay %>% dplyr::filter(.,system==sis[s]) %>% dplyr::filter(Sce=="CC")%>% spread(var, Val)
      AnYn<- which(irri$Area<0 & irri$Yield<0)
      ApYp<- which(irri$Area>0 & irri$Yield>0)
      AnYp<- which(irri$Area<0 & irri$Yield>0)
      ApYn<- which(irri$Area>0 & irri$Yield<0)
      
      negativo<-  c(AnYn)
      positivo<- c(ApYp)
      anegaypos<- c(AnYp) 
      aposynega<- c(ApYn) 
      
      # copia
      tanz<- irri
      tanz$trend<- NA
      tanz$trend[AnYn]<- "Negative"
      tanz$trend[ApYp]<- "Positive"
      tanz$trend[AnYp]<- "Negative in Area and positive in yield"
      tanz$trend[ApYn]<- "Positive in Area and negative in yield"
      
      p[[s]]<- as.data.frame(tanz)
      
#       write.csv(tanz,paste("./olddata/", sis[s],"_TendenciasAreaYield.csv",sep = ""), row.names = FALSE)
      
}

pp<- do.call(rbind,p)
write.csv(pp,paste("./olddata/Total_TendenciasAreaYield.csv",sep = ""), row.names = FALSE)

####### maps ---------------

ttpp<- pp %>% dplyr::select(fpu, crop, system, trend)
cultivos<- unique(ttpp$crop)
x=2 # sistemas 1= irri 2=rainfed
# c=1 # 
pic<- list()

for(c in 1:length(cultivos)){
      
      proof<- ttpp %>% filter(., system==sistemas[x]) %>% filter(., crop==cultivos[c])
      colnames(proof)[1]<-"New_FPU"
      proof$New_FPU<- as.character(proof$New_FPU)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("CambiosCropsTrends_", cultivos[c],"_",sistemas[x], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Trends_", cultivos[c],"_",sistemas[x],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Trends from Impacts'
      pic[[c]]<- ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = trend)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            labs(x=NULL, y=NULL, title= paste("Trends for ", cultivos[c],", system: ", sistemas[x],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[c]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","CambiosCrops_",sistemas[x],"_coropletaAllScenarios.png", sep=""), width = 10, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D" ), ncol = 2, nrow = 2,common.legend = F,widths = c(2,2))

plot(gg)
dev.off()

###### logica los impactos animales-----------
animal$sce<- gsub("./ANIMAL_",replacement = "",  animal$sce)
animal<- animal %>% filter(., AnimalNumber!="Egg production")


animalW<- aggregate(animal["Val"],by=list(animal$year,
                                       animal$sce,
                                       animal$fpu),FUN=sum)

colnames(animalW)[1]<- "year"
colnames(animalW)[2]<- "sce"
colnames(animalW)[3]<- "fpu"



animalF<- animalW %>% spread("year", "Val")
animalF$Change<- ((animalF$`2050`-animalF$`2010`)/animalF$`2010`)*100 
animalF<- animalF %>% dplyr::select(fpu,sce,Change) %>% dplyr::filter(.,fpu %in% fpuAlc)

animalF<- data.frame(animalF,"Cat"=ifelse(animalF$sce=="NOCC","NOCC","CC"))
animalF<- animalF %>% spread(sce,Change)
animalF$median<- apply(animalF[,3:6],1, median, na.rm = TRUE)
animalF<- animalF[c( "fpu", "Cat", "median" )]
animalF<- animalF %>% spread(Cat, median)
animalEx<- animalF
# animalF$NOCC<- NULL

write.csv(animalF,paste("./olddata/TotalAnimalNumbers.csv",sep = ""), row.names = FALSE)

colnames(animalF)[1]<-"New_FPU"

require(sf)
require(maptools)
alc.sf <- st_as_sf(alc)
alc.sf <- left_join(alc.sf, animalF, by = c('New_FPU'))
alc.sh <- as(alc.sf, 'Spatial')
shp<- alc.sh
writeOGR(alc.sh, dsn = './oldpic/shapes', layer = "CambiosAnimal", driver = 'ESRI Shapefile')
ftfy <- fortify(shp, region = 'New_FPU')
tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))

#Coropleta 
png(filename = paste("./CambiosAnimalNumber_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

labs2 = 'Percentage Change % \n2010 to 2050'
pic<- ggplot() +
      geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC)) +
      geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
      coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
      scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, na.value="grey",breaks=seq(-40,80,20)) +
      labs(x=NULL, y=NULL, title= "Changes of animal number")+
      theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))

      
      
plot(pic)
dev.off()
      



# pic<- list()
# for(b in 1:length(buu)){
#       proof<- animalF %>% filter(., AnimalNumber==buu[b]) 
#       colnames(proof)[1]<-"New_FPU"
#       require(sf)
#       require(maptools)
#       alc.sf <- st_as_sf(alc)
#       alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
#       alc.sh <- as(alc.sf, 'Spatial')
#       shp<- alc.sh
#        writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("CambiosAnimalNumber_", buu[b], sep = ""), driver = 'ESRI Shapefile')
#       ftfy <- fortify(shp, region = 'New_FPU')
#       tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
#       
#       #Coropleta 
#       png(filename = paste("./","CambiosAnimalNumber_", buu[b],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
#       
#       labs2 = 'Percentage Change % \n2010 to 2050'
#       pic[[b]]<- ggplot() +
#             geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = CC)) +
#             geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
#             coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
#             scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, na.value="grey",breaks=seq(-40,80,20)) +
#             labs(x=NULL, y=NULL, title= paste("Changes of animal number for ", buu[b],sep = ""))+
#             theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
#             theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
#             theme(axis.text=element_text(size=14),
#                   axis.title=element_text(size=14,face="bold"))
#       
#       
#       
#       plot(pic[[b]])
#       dev.off()
#       
# }
library(ggpubr)
#Coropleta 
png(filename = paste("./","CambiosAnimalNumber_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]],pic[[5]],pic[[6]], labels = c("A", "B","C", "D","E", "F" ), ncol = 3, nrow = 3,common.legend = T)

plot(gg)
dev.off()

################# analisis discriminante multivariado #############

library(stats)

### crear datos completos
# pp # rendimientos, eliminar pp
# qfsTotal # oferta de tierra, eliminar NoCC
# animalEx # esta listo para agregar.
colnames(animalEx)[2]<- "Animal"
pp$Sce<- NULL
pp$trend<- NULL

ppi<- pp[which(pp$system=="Irrigated"),]
ppi$Yield<- NULL
ppr<- pp[which(pp$system=="Rainfed"),]
ppr$Yield<- NULL

tti <- ppi %>% select(fpu, crop, Area) %>% spread(crop, Area) 
colnames(tti)[2:ncol(tti)]<- paste0("Irr_", "Area_",colnames(tti[c(2:ncol(tti))]))
tti$fpu<- as.character(tti$fpu)
ttr <- ppr %>% select(fpu, crop, Area) %>% spread(crop, Area) 
colnames(ttr)[2:ncol(ttr)]<- paste0("Rain_", "Area_",colnames(ttr[c(2:ncol(ttr))]))
ttr$fpu<- as.character(ttr$fpu)
dt<- left_join(ttr,tti, by=c("fpu"))
dt<- left_join(dt,animalEx, by=c("fpu"))


dt<- dt %>% gather(Var, Change,2:ncol(dt))

dt$fpu <- factor(dt$fpu, levels = c(pots))

png(filename= paste("./oldpic/AllVariables_HeapMap.png",sep=""), 
    width = 7, height = 10, units = 'in', res = 300)
labs2 = 'Percentage \nchange (%)'


n<- ggplot(data =dt, aes(Var, fpu)) + 
      geom_tile(aes(fill = Change), colour = "white")+
      labs(x=NULL, y=NULL) + labs(fill=labs2)+
      scale_fill_gradient2(low="red", mid="white", high="blue", midpoint = 0, na.value="grey",breaks=seq(-140,140,30))+
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(45/100)+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  


#### analisis 
dw<- dt %>% spread(Var,Change)
write.csv(dw,paste("./olddata/summaryCrops.csv",sep = ""), row.names = FALSE)



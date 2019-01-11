#### modelo para TCN. Carlos Eduardo Gonzalez R. 
g=gc;rm(list = ls())

### paquetes
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(sf))
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
scenarios<- unique(areaF$sce)

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


Map_LatinAmerica<- fortify(alc)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
 

## analisis para Area
#################################  RICE ####################################### 
t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% filter(., crop=="Rice")
      colnames(proof)[1]<-"New_FPU"
      require(sf)
      require(maptools)
      require(rgdal)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Rice_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Rice",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, na.value="grey",breaks=seq(-50,50,10)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Rice ", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Rice",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  MAIZE ####################################### 
# t=1 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% 
            filter(., crop=="Maize")
      colnames(proof)[1]<-"New_FPU"
      
      upperlimit<- max(proof$Change)
      
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Maize_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Maize",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-20,160,30)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Maize", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Maize",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  SOYBEAN ####################################### 
# t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% 
            filter(., crop=="Soybean")
      colnames(proof)[1]<-"New_FPU"
      
      upperlimit<- max(proof$Change)
      summary(proof$Change)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
      writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Soybean_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Soybean",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-20,100,20)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Soybean", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Soybean",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

#################################  Palm Fruit ####################################### 
# t=2 ### definicion del sistema irrigado==1 secano==2
# s=1
pic<- list()
for(s in 1:length(scenarios)){
      proof<- areaF %>% filter(., system==sistemas[t]) %>% filter(., sce==scenarios[s])%>% 
            filter(., crop=="Palm Fruit")
      colnames(proof)[1]<-"New_FPU"
      
      upperlimit<- max(proof$Change)
      summary(proof$Change)
      require(sf)
      require(maptools)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('New_FPU'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
            writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_PalmFruit_", sistemas[t],"_",scenarios[s], sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'New_FPU')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'New_FPU'))
      
      #Coropleta 
      png(filename = paste("./","Palm Fruit",sistemas[t],"_",scenarios[s],"coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change % \n2010 to 2050'
      pic[[s]]<-ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Change)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
                                 na.value="grey",breaks=seq(-10,110,20)) +
            labs(x=NULL, y=NULL, title= paste("Area Harvest of Palm Fruit", sistemas[t],  "\nscenario ", scenarios[s],sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[s]])
      dev.off()
      
}
library(ggpubr)
#Coropleta 
png(filename = paste("./","Palm Fruit",sistemas[t],"_coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], pic[[3]], pic[[4]], labels = c("A", "B","C", "D"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

### tabla de impacto-------- 
mdwide<- data.frame(areaF,"Cat"=ifelse(areaF$sce=="NOCC","NOCC","CC"))

areaw<- mdwide %>% spread(sce,Change)
areaw$median<- apply(areaw[,5:8],1, median, na.rm = TRUE)
areaw<- as.data.frame(areaw)
areaw<- areaw[c( "fpu", "crop", "system" , "Cat", "median" )]
areaw<- areaw %>% spread(Cat, median)
areaw$pp<- areaw$CC-areaw$NOCC
areaw<- areaw %>% gather(Sce, Val, 4:ncol(areaw))

write.csv(areaw, "./olddata/ReportArea.csv")


hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename= paste("./oldpic/","AREA_HeapMap.png",sep=""), 
    width = 9, height = 11, units = 'in', res = 300)


n<- ggplot(data =areaw, aes(Sce, fpu)) + 
      geom_tile(aes(fill = Val), colour = "white")+  facet_grid(.~crop, drop = T)+
      labs(x=NULL, y=NULL, title=paste("Area Changes by scenario\n from 2010 to 2050",sep = "")) +
      scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(91/100)+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

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

write.csv(areaw, "./olddata/ReportYield.csv")


hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename= paste("./oldpic/","YIELD_HeapMap.png",sep=""), 
    width = 9, height = 11, units = 'in', res = 300)
labs2 = 'Percentage change(%)/diff(pp) \n2010 to 2050'

n<- ggplot(data =yieldw, aes(Sce, fpu)) + 
      geom_tile(aes(fill = Val), colour = "white")+  facet_grid(.~crop, drop = T)+
      labs(x=NULL, y=NULL, title=paste("Yields comparision by scenarios \n from 2010 to 2050",sep = "")) +
      scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(91/100)+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  


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

s=1
for(s in 1:length(sistemas)){
      
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename= paste("./oldpic/", sistemas[s],"_AREA_YIELD_HeapMap.png",sep=""), 
          width = 10, height = 11, units = 'in', res = 300)
      labs2 = '% & diff(pp)\n2010 to 2050'
      
      
      n<- ggplot(data =ay[which(ay$system==sistemas[s]),], aes(Sce, fpu)) + 
            geom_tile(aes(fill = Val), colour = "white")+  facet_wrap(var~crop, nrow = 1)+
            labs(x=NULL, y=NULL, title=paste(sistemas[s],", area and yield impacts by scenarios",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + #coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_grey() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) 
      
      
      plot(n)
      dev.off()  
}


######################### Supply land  ##################
qfs$sys<- plyr::revalue(qfs$sys,c("air"="Irrigated","arf"="Rainfed", "gir"="Intermediated"))

qfsTotal<- qfs %>% spread(sys, Val)
qfsTotal$total<- rowSums(qfsTotal[4:ncol(qfsTotal)],na.rm=T)
qfsTotal<- qfsTotal %>% dplyr::select(fpu, year, sce, total)
qfsTotal<- qfsTotal %>% spread(year, total)
      
qfsTotal$Change<- ((qfsTotal$`2050`-qfsTotal$`2010`)/qfsTotal$`2010`)*100 
qfsTotal<- qfsTotal %>% dplyr::select(fpu, sce, Change)%>% dplyr::filter(.,fpu %in% fpuAlc)
qfsTotal<- qfsTotal %>% spread(., sce, Change)
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

qfsTotal<- qfsTotal %>% gather(sce,val, 2:ncol(qfsTotal))

qfsTotal$fpu <- factor(qfsTotal$fpu, levels = c(pots))

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename= paste("./oldpic/SupplyLand_HeapMap.png",sep=""), 
    width = 10, height = 11, units = 'in', res = 300)
labs2 = 'Percentage change'


n<- ggplot(data =qfsTotal, aes(sce, fpu)) + 
      geom_tile(aes(fill = val), colour = "white")+
      labs(x=NULL, y=NULL, title="Supply land percentage change \nfrom 2010 to 2050") +
      scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal(80/100)+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

#### combine area and supply land
areaF<- area %>% dplyr::filter(.,crop %in% cultivos)
areaF$crop<- plyr::revalue(areaF$crop,c("jsoyb"="Soybean",
                                        "jmaiz"="Maize",
                                        "jrice"="Rice",
                                        "jpalm"="Palm Fruit"))

areaF$system<- plyr::revalue(areaF$system,c("air"="Irrigated",
                                            "arf"="Rainfed"))
areaF<- area %>% dplyr::filter(.,crop %in% cultivos)

areaF$crop<- plyr::revalue(areaF$crop,c("jsoyb"="Soybean",
                                        "jmaiz"="Maize",
                                        "jrice"="Rice",
                                        "jpalm"="Palm Fruit"))

areaF$system<- plyr::revalue(areaF$system,c("air"="Irrigated",
                                            "arf"="Rainfed"))


mdwide<- data.frame(areaF,"Cat"=ifelse(areaF$sce=="NOCC","NOCC","CC"))
colnames(mdwide)[3]<- "sys"

#### supply land
as<-  qfs %>% dplyr::filter(.,fpu %in% fpuAlc)
colnames(as)[4]<- "supply"
aa<- left_join(mdwide, as,by = c("fpu", "year", "sce", "sys"))
aa$rate<- (aa$Val/aa$supply)*100
aa<- aa %>% dplyr::select(fpu, crop, sys, year, sce, Cat, rate)
write.csv(aa,"./olddata/RateSupplyLand.csv")



#Evolution of Net Trade by crop and by region
aa<- aa %>% spread(.,sce, rate)
aa<- aa %>% dplyr::filter(.,fpu %in% fpuAlc)
aa$mean<-apply(aa[,6:ncol(aa)],1, mean, na.rm = TRUE)

aa<- aa %>% select(fpu, crop, sys, year, Cat, mean)
# 
# datmin<- aggregate(aa[,11:ncol(aa)],by=list(aa$fpu,aa$crop,aa$Cat,aa$sys),FUN=min)
# 
# datmin<- datmin %>% gather("time","datmin", 5:ncol(datmin))
# names(datmin)<-c("fpu","Crop","Cat","sys","time","datmin")
# 
# 
# datmed<- aggregate(aa[,11:ncol(aa)],by=list(aa$fpu,aa$crop,aa$Cat,aa$sys),FUN=median)
# datmed<- datmed %>% gather("time","datmed", 5:ncol(datmed))
# names(datmed)<-c("fpu","Crop","Cat","sys","time","datmed")
# 
# 
# datmax<- aggregate(aa[,11:ncol(aa)],by=list(aa$fpu,aa$crop,aa$Cat,aa$sys),FUN=max)
# datmax<- datmax %>% gather("time","datmax", 5:ncol(datmax))
# names(datmax)<-c("fpu","Crop","Cat","sys", "time","datmax")
# 
# 
# 
# extremos<-merge(datmin,datmax)
# 
# datost<- merge(extremos,datmed)
pots<- unique(aa$fpu)

py<- NULL
sp<- unique(datost$sys)
i=1 ## lugar
s=1
for (i in 1:length(pots)) {
      
      aa1<- aa %>%  filter(.,sys==sp[s]) %>% filter(.,fpu==pots[i])
      aa1$Cat<- as.character(aa1$Cat)
      tiff(filename=paste(grd,"Test_",pots[i],"_net_tradeNew.tiff",sep=""), 
           width = 10, height = 7, units = 'in', res = 100)
      
      ggplot(data=aa1, aes(x=year,y=mean,group=Cat))+ geom_line(aes(linetype=Cat, color=crop))+ theme(legend.position="bottom")
            
            
                           geom_line(linetype=aa1$Cat,color=aa1$fpu,size=1)+ geom_point()+
                           labs(y="Rate crop of supply land",x="Year")+
                           theme(legend.position="bottom")
                     
      
      dev.off()
      print(i)
}  

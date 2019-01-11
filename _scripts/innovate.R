### competicion 

#Load libraries
library(dplyr)
library(plyr)
library(tidyr)
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(maptools)
library(gridExtra)
library(sp)
library(maptools)
library(maps)
library(raster)
library(lattice)
library(latticeExtra)
library(dplyr)
library(xlsx)

work<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF"
path.root<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/Innocentive 9933872/Innocentive 9933872/"

#cargamos grilla
pic2<- shapefile(paste0(path.root,"Grid_ETRS89_LAEA5210_50KEEA15975I/","Grid_LAEA5210_50K_polygons.shp"))
plot(pic2)

#shape grilla 
pic <- shapefile(paste0(path.root,"Shape_50km_Grid_EU/Shape_50km_Grid_EU/","grid50_etrs.shp"))
plot(pic)

#shape EU
eu <- shapefile(paste0(work,"/NUTS_2013_01M_SH/NUTS_2013_01M_SH/data/","NUTS_RG_01M_2013.shp"))
plot(eu)

extent(eu) #extension de spam
extent(pic) #extension de crop modeling ciat

pic_geo <- spTransform(x = pic, CRSobj = crs(eu))

# 
# crs(eu) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# crs(pic) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


#shape EU
eu2 <- shapefile(paste0(work,"/NUTS_2013_01M_SH/NUTS_2013_01M_SH/data/","NUTS_BN_01M_2013.shp"))
plot(eu2)
#data
cfiles1<- read.xlsx(paste(work,"/20170726_AMU_CrowdEx_DatasetWithDiistributions (1).xlsx", sep = ""), sheetIndex = "Final")
cfiles1$NA.<- NULL
# colnames(cfiles1)[1]<- "GRID_CODE"
cfiles1$Cell.ID<- as.character(cfiles1$Cell.ID)
cfiles1$Cell.ID<- as.numeric(cfiles1$Cell.ID)


# #ajustes shape file
# pic@data$GRID_CODE<- as.character(pic@data$GRID_CODE)
# pic@data$GRID_CODE<- as.numeric(pic@data$GRID_CODE)
pic_geo@data$id<- row.names(pic_geo@data)
# pic@data$GRID_CODE<- row.names(pic@data)
 
r1<- cfiles1[c("Longitude","Latitude","Mean")]
ra<- rasterFromXYZ(xyz = r1,res = c(0.01,0.01), crs ="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  )
ra.c<- rasterFromCells(ra)

coordinates(r1)
gridded(r1)<- TRUE




r <- raster(ncols=418, nrows=418)
cells <- c(1:3, 210)
r <- rasterFromCells(r, cells)
cbind(1:ncell(r), getValues(r))

pic.df<- fortify(pic_geo)
pic.df<- join(pic.df, pic_geo@data, by="id")
pic.df<- merge(pic.df, cfiles1, by.x="GRID_CODE", by.y="Cell.ID", all.x=T, a..ly=F)

eu.f<- fortify(eu)

y<- ggplot(data = pic.df, aes(x=long, y=lat, group=group)) + 
            geom_raster(aes(x=long, y=lat,fill=Mean))+ 
            geom_path( colour="gray", linestyle=2)+
      
      
      ggplot(data=eu.f, aes(x=long, y=lat, group = group)) +
      geom_polygon(data=eu.f, aes(x=long, y=lat, group = group),colour="white", fill="gray" )+
      coord_equal() +
      geom_path(data=eu.f,aes(x=long, y=lat, group=group), colour="red", size=0.25)+
      geom_raster(data= pic.df, aes(x=long, y=lat,fill=Mean))


      facet_grid(sys~crop)+
      theme(strip.text.x = element_text(angle = 0,size = 18),strip.background = element_rect(colour="white", fill="white")) + 
      theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
      scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
      labs(fill=labs2)+ 
      theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size=14),
            legend.title = element_text(face="bold",size=14),
            legend.background = element_blank(),
            legend.key = element_blank(),
            strip.text.y = element_text(size=16, face="bold"),
            plot.title = element_text(face="bold", size=22),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(colour="white", fill="white")) 


plot(y)

## Kenya raster
g=gc;rm(list = ls())

#Load libraries---
library(dplyr)
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


#directories---

dir<-"C:/Users/CEGONZALEZ/Documents/GFSF"
#cargamos shape ALC
# Kenya <- shapefile(paste0(dir,"/","Kenya.shp"))
fpu<-  shapefile("C:/Users/CEGONZALEZ/Desktop/BID/BID/newBID/ImpactResults/Shapefiles mundo/fpu_shp/fpu_shp/fpu.shp")
atest <- raster(paste0(dir,"/","spam2005v2r0_yield_bean_total.tiff"))

# filter FPU kenya
fpu <- fpu[fpu@data$Region_Nam=="Kenya",]

# 
# maskKenya<- mask(atest, fpu)
# projection(maskKenya) <- "+proj=utm +zone=48 +datum=WGS84"
# plot(maskKenya)



# crop using Extent
crs(atest)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(fpu)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
#Recorte primero por extent y luego mascara
cr <- crop(atest, extent(fpu))
rr <- raster::mask(cr,fpu)

# ploting easier
plot(rr)
plot(fpu,add=T)


# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
r.spdf <- as(rr, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
head(r.df)
colnames(r.df)[1]<- "Val"
r.df<- filter(r.df, Val!=0)
# g <- ggplot(r.df, aes(x=x, y=y)) + geom_tile(aes(fill = Val)) + coord_equal()

#fortify fpu
fpu<- fortify(fpu)


color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
labs2 = 'Yield\n(Kg/ha)'
png(filename = paste(dir,"KenyaArea.png", sep=""), 
    width = 20, height = 12, units = 'in', res = 100)

ggplot() +
      geom_polygon(data=fpu, aes(x=long, y=lat, group = group),colour="white", fill="white")+
      geom_path(data=fpu, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      geom_raster(data= r.df, aes(x=x, y=y, fill=Val))+
      theme(strip.text.x = element_text(angle = 0,size = 18),strip.background = element_rect(colour="white", fill="white")) + 
      theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
      scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
      # labs(fill=labs2)+ 
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
dev.off()

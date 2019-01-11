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

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files")
grd<-"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/BID2version/"

f<- shapefile("./AdjuCarEdo.shp")
plot(f)

dat<- f@data
# colnames(dataShp)[7]<- "NAME"
# dataShp<- dataShp[c("NAME", "New_FPU", "NFPU_INT",  "SUM_SUM_GA" ,"New_Basin" , "Basin_Name" ,"New_Region",     
#                     "Rural_UN20", "Urban_UN20", "Total_UN20" ,"Rural_Pop_", "Urban_Pop_" ,"FPU_PopSha" ,"FPU_PopS_1",
#                     "Shape_Leng", "Shape_Area", "Region_BID")]

# dataA<- a@data
# dataA$NAME<- plyr::revalue(dataA$NAME,c("Guyana"="Guyanas South America",
#                                  "Suriname" ="Guyanas South America"))

require(sf)
require(maptools)

alc.sf <- st_as_sf(f)
alc.sf <- left_join(alc.sf, dat, by = c("OBJECTID_1", "OBJECTID", "NAME",  "ISO3", "ISO2","FIPS", "COUNTRY","ENGLISH",   
                                        "FRENCH", "SPANISH",  "LOCAL", "FAO",  "WAS_ISO", "EU",  "SQKM","Shape_Leng",
                                        "Shape_Area","Region_BID"))
alc.sh <- as(alc.sf, 'Spatial')
shp<- alc.sh

# writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Maize_", sistemas[t], sep = ""), driver = 'ESRI Shapefile')
ftfy <- fortify(shp, region = 'NAME')
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
tst <- left_join(ftfy, shp@data, by = c('id'='NAME'))


#Coropleta 
png(filename = paste(grd,"LAC_REGION_BID_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

labs2 = 'BID Regions'
pic<- ggplot() +
      geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = Region_BID)) +
      geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
     labs(fill=labs2)+  coord_fixed()+ theme()+ coord_equal()+
#       coord_cartesian(xlim = c(-100, -25), ylim = c(10,-60)) +
#       scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, 
#                            na.value="grey",breaks=seq(-20,100,20)) +
#       labs(x=NULL, y=NULL, title= paste("Area Harvest of Soybean", sistemas[t], sep = ""))+
      theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))



plot(pic)
dev.off()






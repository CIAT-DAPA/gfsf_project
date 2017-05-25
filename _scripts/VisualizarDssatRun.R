#Visualize DSSAT runs

#Load libraries
library(dplyr)
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(maptools)
library(sp)
library(maptools)
library(maps)
# library(ncdf)

#Directorios
path.root<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

#cargamos shape ALC
map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
plot(alc)

grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
copyall<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/")
shp<- c ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/")
sis.en<- c("irrigated", "rainfed")
sys<- c("IRR","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m", "WFD")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

# raste de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

c=s=g=1
pic<-list()
for( c in 1:length(crops)){
      
      dataF<-list.files(path = grd1,pattern =paste("YieldMax_",crops[c],"_", sep =""),full.names = T)
      dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
      DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL
      gcm<- unique(DataFiles$sce) 
      
      #calcular promedio por pixel
      DataFiles$mean<-apply(DataFiles[,8:ncol(DataFiles)],1,function(x){mean(x,na.rm = T)})
      DataFiles<- DataFiles[,-c(8:35)]
      
      for(s in 1:length(sys)){
            for(g in 1:length(gcm){
                  require(dplyr)
                  DataFiles<- filter(DataFiles, sys==sys[s] & sce==gcm[g] ) 
                  # Analisis grafico
                  Map_LatinAmerica1<- fortify(alc)
                  
                  color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
                  labs2 = 'Yield Max (ave) \n(Kg/ha)'
                  pic[[g]]<-ggplot() +
                        geom_polygon(data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                        geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
                        geom_raster(data=DataFiles, aes(x=long, y=lat,fill=mean))+
                        labs(title=paste(crops[c], " Raster, Maximus yields by System= ",sys[s], ", Average from 2020 to 2050\n  GCM=",gcm[g], sep = ""))+
                        theme_bw()+ 
                        scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
                        theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
                        labs(fill=labs2)
                  
#                   ggsave(filename=paste(copyall,crops[c],"_",sys[s],"YieldsMeanMax",".png", sep=""), plot=y, width=20, height=20, dpi=500,scale=1.5)
                  
                  
                  plot(pic[1])  
                  
            }
         
            
      }
      
      
}

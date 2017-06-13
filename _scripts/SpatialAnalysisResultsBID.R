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
sys<- c("IRR","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m", "WFD")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

# raste de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")


for( c in 1:length(crops)){
      
      dataF<-list.files(path = grd1,pattern =paste("YieldMax_",crops[c],"_", sep =""),full.names = T)
      dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
      DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL
      gcm<- unique(DataFiles$sce) 

      #calcular promedio por pixel
      DataFiles$mean<-apply(DataFiles[,8:ncol(DataFiles)],1,function(x){mean(x,na.rm = T)})
      DataFiles<- DataFiles[,-c(8:35)]
     
      for(s in 1:length(sys)){
            require(dplyr)
            DataFiles<- filter(DataFiles, sys==sys[s]) 
            # Analisis grafico
            Map_LatinAmerica1<- fortify(alc)
            
            color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
            labs2 = 'Yield Max (ave) \n(Kg/ha)'
            y=ggplot() +
                  geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                  geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
                  geom_raster(data=DataFiles, aes(x=long, y=lat,fill=mean))+
                  facet_wrap(~sce,labeller =label_parsed,scales = "free")+
                  theme(strip.text.x = element_text(angle = 0,size = 16),strip.background = element_rect(colour="white", fill="white")) + 
                  labs(title=paste( crops[c], " Raster, Maximum yields selected by System= ",sys[s], " Average \n All scenarios",sep = ""))+
                  theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
                  scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
                  labs(fill=labs2)+
                  theme(panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor.y = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.text = element_text(size=18),
                        legend.title = element_text(face="bold",size=18),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        plot.title = element_text(face="bold", size=18),
                        panel.border = element_blank(),
                        axis.ticks = element_blank())
            
            ggsave(filename=paste(copyall,crops[c],"_",sys[s],"YieldsMeanMax",".png", sep=""), plot=y, width=20, height=20, dpi=400,scale=1.5)
            
           
            print(c)
            
      }
      
     
 }

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%% Coropletas con datos de los rendimientos ponderados

### graficas para los rendimientos ponderados y agregados
#maps with yield weight
library(sp)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library(gridExtra)
library(ggthemes)
library(broom)
library(dplyr)
library(tidyr)
library(reshape)
library(ggplot2)
library(RColorBrewer)


fpuMaps <- shapefile(paste0(map,"FPU_Latinoamerica.shp"))
grd<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/YieldsWeight/")


plotList<-list()
allg<- list()
#Rice

avgCrops <- lapply(1:length(crops), function(c){
      
      dataF <- list.files(path = grd, pattern = crops[c],full.names = T)
      dataF <- lapply(dataF, read.csv, stringsAsFactors = F)
      DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL
      DataFiles$mean<-apply(DataFiles[,5:ncol(DataFiles)],1,function(x){mean(x,na.rm = T)})
      DataFiles<- DataFiles[,-c(5:32)]
      DataFiles$crop<- crops[c]
      return(DataFiles)
      
})
avgCrops <- do.call(rbind, avgCrops)

# gg <- avgCrops %>% filter(crop == "Rice" & sce == "bcc_csm1_1") %>% ggplot() + geom_bar(aes(x = FPU, y = mean), stat = "identity") + facet_wrap(~sys, ncol = 1)
# gg <- gg + theme_bw()
# gg


hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
names(avgCrops)[5] <- "Yields_weighted"


for(c in 1:length(crops)){
      
      
      png(filename = paste(copyall,crops[c],"_YieldsWeight",".png",sep=""), 
          width = 20, height = 12, units = 'in', res = 100)
      
      gg <- ggplot(data =avgCrops[avgCrops$crop ==crops[c],], aes(y = FPU, x = sce)) + 
            geom_tile(aes(fill = Yields_weighted), colour = "white")+ 
            labs(x=NULL, y=NULL, title="") +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + 
            labs(x = "",y = "") +
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
            theme(axis.text.y = element_text(hjust = 1, size = 12))+
            theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
            facet_wrap(~ sys, scales = "free") + theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 12))+
            labs(title=paste( crops[c], "Rendimientos Ponderados y agregados por\n FPU (promedio 2050-2020)",sep = ""))
            

      plot(gg)
      dev.off() 
      
}





      
#       DataFiles2 <- DataFiles %>% group_by(FPU, sce, sys, mean) %>% do(tidy(merge(x = fpuMaps, y = ., by.x = "New_FPU", by.y = "FPU")))
#       newobj <- fortify(DataFiles2, region = "FPU")
#       
#       test <- DataFiles2 %>% filter(sce == "bcc_csm1_1" & sys == "IRRI")
#       test_newobj <- fortify(test, region = "FPU")
#       
#       gg<- ggplot() + geom_map(data = test, aes(map_id = FPU, fill = mean), colour = "black", map = test_newobj) +
#             expand_limits(x = test_newobj$long, y = test_newobj$lat) +
#             geom_path(data = test_newobj, colour = "black", aes(long, lat, group = id), size = 0.5) +
#             coord_map() + scale_fill_viridis()
#       
      #######
      
     
      
c=1
i=1
      
for(i in 1:length(gcm)){

      dataF<-list.files(path = grd,pattern =crops[c],full.names = T)
      dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
      DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL
      gcm<- unique(DataFiles$sce) 
      
      DataFiles$mean<-apply(DataFiles[,5:ncol(DataFiles)],1,function(x){mean(x,na.rm = T)})
      DataFiles<- DataFiles[,-c(5:32)]
      
      require(dplyr)
      DataFiles<- filter(DataFiles, sys=="IRRI" & sce==gcm[i]) 
      rownames(DataFiles)<- 1:nrow(DataFiles)
      
       newobj <- merge(fpuMaps, DataFiles, by.x="New_FPU", by.y="FPU")
       newobj <- fortify(newobj, region = "New_FPU")
       
             
       p<-ggplot() + geom_map(data = DataFiles, aes(map_id =FPU, fill = mean),colour="black", map = newobj) +
             expand_limits(x = newobj$long, y = newobj$lat)+
             geom_path(data = newobj,colour="black", aes(long, lat, group=group), size=0.5)+
             coord_map()+ scale_fill_viridis() + 
             labs(title=paste( crops[c], " promedio de los rendimientos ponderados y agregados\n", gcm[i]," ,sistema irrigado\n (2050-2020)" ,sep = ""))
       p
     
              plotList[[i]]<-p 
   
 } 

### prueba grafica para un solo systema

require(dplyr)
test<-DataFiles


proof <- test %>% group_by(sce,sys) %>% do(., merge(fpuMaps, ., by.x="New_FPU", by.y="FPU") 
                                               %>% fortify(., region = "New_FPU"))


                                        

p<-ggplot() + geom_map(data = test, aes(map_id =FPU, fill = mean),colour="black", map = proof) +
      expand_limits(x = proof$long, y = proof$lat)+
      geom_path(data = proof,colour="black", aes(long, lat, group=group), size=0.5)+
      coord_map()+ scale_fill_viridis() + 
      labs(title=paste( crops[c], " promedio de los rendimientos ponderados y agregados\n", gcm[i]," ,sistema irrigado\n (2050-2020)" ,sep = ""))
p



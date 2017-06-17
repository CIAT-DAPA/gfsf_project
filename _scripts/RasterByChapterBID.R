#Visualize DSSAT runs

#Load libraries
library(dplyr)
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
treat<- c("IRRI","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m", "WFD")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

# raster de plantilla
tmpRaster <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Rendimientos Raster/Arroz/1971-1998/_WFD_Riego_1971.asc")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      Rendimientos Maximos        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

y<- list()
w<- list()
for(c in 1:length(crops)){

      dataF<-list.files(path = grd1,pattern= crops[c],full.names = T)
      dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
      dataF<- do.call(rbind,dataF); dataF$X<- NULL
      dataF<- filter(dataF, sce=="WFD")
      
            for(t in 1:length(treat)){
                  w[[t]]<- filter(dataF, sys==treat[t])
                  w[[t]]$mean<- apply( w[[t]][,8:ncol( w[[t]])],1,function(x){mean(x,na.rm = T)})
                  w[[t]]<-  w[[t]][,-c(8:35)]

                   w[[t]]<- aggregate( w[[t]]$mean, by=list( w[[t]]$long, w[[t]]$lat, w[[t]]$FPU),FUN=mean)
                  colnames( w[[t]])<- c( "long","lat", "FPU", "mean" )
                  Map_LatinAmerica1<- fortify(alc)
                  color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
                  labs2 = 'Yield\n(Kg/ha)'
                  
                  png(filename = paste(copyall,crops[c],"_",treat[t],"_meanMaxYieldWFD.png", sep=""), 
                      width = 20, height = 12, units = 'in', res = 100)
                  y[[t]]<- ggplot() +
                        geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                        geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
                        geom_raster(data= w[[t]], aes(x=long, y=lat,fill=mean))+
                        theme(strip.text.x = element_text(angle = 0,size = 16),strip.background = element_rect(colour="white", fill="white")) + 
                        theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
                        scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
                        labs(fill=labs2)+ 
                        labs(title=paste(crops[c], " \n ",treat[t], sep = ""))+
                        theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              legend.text = element_text(size=10),
                              legend.title = element_text(face="bold",size=10),
                              legend.background = element_blank(),
                              legend.key = element_blank(),
                              plot.title = element_text(face="bold", size=18),
                              panel.border = element_blank(),
                              axis.ticks = element_blank())
                  
                  plot(y[[t]])
                  dev.off()
                  

    
      }
            graph <- arrangeGrob(y[[1]],y[[2]], nrow=1)
            ggsave(file= paste(copyall,crops[c],"figureMaxYield.png", sep = ""), graph, width=10, height=10.5, units='in') 

      
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   rendimientos mean      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Se calcula los rendimientos promedios entre variables para probar diferentes tipos de raster. Option A. Max yield B. Ave Yield

########################################################## GCMs -------------------------------

#directorios
gdr1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
grdw<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/")

########### PARTE B
#cargar files
sys<- c("IRRI","RA")


for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running select to max value for yield: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(gdr1,"/", crops[c],sep = ""),pattern = sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            c_files$X<- NULL
            #reshape corregir año
            require(plyr)
            require(tidyr)
            c_files<- c_files %>% 
                  gather(year,val, 9:36) 
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            
            # rendimientos por pixel/variedad/año
            c_files<- c_files %>% 
                  spread(v,val) 
            c_files[,"ymean"] <- apply(c_files[, 9:ncol(c_files)], 1, mean)
            #Eliminar columnas de variedades
            c_files<- c_files[ ,c(1:8,ncol(c_files):ncol(c_files))]
            is.na(c_files)
            #reshape
            c_files<- c_files %>% 
                  spread(year,ymean) 
            write.csv(c_files,paste(copy,"YieldMean_",crops[c],"_",sys[s],"_", ".csv",sep = "")) 
            
      }
}




########################################################### Datos historicos -------------------------------

for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running select to max value for yield: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(grdw,crops[c],sep = ""),pattern = sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            c_files$X<- NULL
            #reshape corregir año
            require(plyr)
            require(tidyr)
            c_files<- c_files %>% 
                  gather(year,val, 9:36) 
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            
            # rendimeintos por pixel/variedad/año
            c_files<- c_files %>% 
                  spread(v,val) 
            c_files[,"ymean"] <- apply(c_files[, 9:ncol(c_files)], 1, mean)

            #Eliminar columnas de variedades
            c_files<- c_files[ ,c(1:8,ncol(c_files):ncol(c_files))]
            #reshape
            c_files<- c_files %>% 
                  spread(year,ymean) 
            write.csv(c_files,paste(copy,"YieldMean_",crops[c],"_",sys[s],"_WFD", ".csv",sep = "")) 
            
      }
}




treat<- c("IRRI","RA")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Rendimientos means  WFD   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

y<- list()
w<- list()
Map_LatinAmerica1<- fortify(alc)
color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
labs2 = 'Yield\n(Kg/ha)'


##crops
y<- list()

for (c in 1:length(crops)){
      dataF<- list.files(path = grd1,pattern= paste("YieldMean_",crops[c], sep = ""),full.names = T)
      dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
      dataF<- do.call(rbind,dataF); dataF$X<- NULL
      dataF<- filter(dataF, sce=="WFD")
      cfiles<- dataF 
      cfiles$mean<- apply( cfiles[,8:ncol( cfiles)],1,function(x){mean(x,na.rm = T)})
      cfiles<-  cfiles[,-c(8:35)]    
      
      
      cfiles<- aggregate(cfiles$mean, by=list( cfiles$long, cfiles$lat, cfiles$FPU, cfiles$sys),FUN=mean)
      colnames( cfiles)<- c( "long","lat", "FPU","sys","mean" )
      cfiles$sys<-revalue(cfiles$sys, c("IRRI"="Irrigated",
                                        "RA"="Rain-fed"))
      
      png(filename = paste(copyall,crops[c],"_MeanYield__meanWFD.png", sep=""), 
          width = 20, height = 12, units = 'in', res = 100)
      y[[c]]<- ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            geom_raster(data= cfiles, aes(x=long, y=lat,fill=mean))+ facet_wrap(~sys)+
            theme(strip.text.x = element_text(angle = 0,size = 16),strip.background = element_rect(colour="white", fill="white")) + 
            theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            labs(fill=labs2)+ 
            labs(title=crops[c])+
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.text = element_text(size=10),
                  legend.title = element_text(face="bold",size=10),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text.y = element_text(size=12, face="bold"),
                  plot.title = element_text(face="bold", size=10),
                  panel.border = element_blank(),
                  axis.ticks = element_blank(),
                  strip.background = element_rect(colour="white", fill="white")) 
      
      plot(y[[c]])
      dev.off()
      
}



graph <- arrangeGrob(y[[1]],y[[2]],y[[3]],y[[4]],y[[5]], nrow=3)
ggsave(file= paste(copyall,"figura_MeanYield_figure.png", sep = ""), graph, width=25, height=15, units='in') 

         
 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Rendimientos means  GCM     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
##crops
p<- list()

for (c in 1:length(crops)){
      dataF<- list.files(path = grd1,pattern= paste("YieldMean_",crops[c],sep = ""),full.names = T)
      dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
      dataF<- do.call(rbind,dataF); dataF$X<- NULL
      dataF<- filter(dataF, sce!="WFD")
      cfiles<- dataF 
      

      #Find areas where consistently failing and discard these from aggregation
      zeros.wfd.r = apply(cfiles[,8:ncol(cfiles)],1,function(x) sum(x==0,na.rm=T))
      ind.falla = which(zeros.wfd.r>=12)  
      
      #Descartar pixeles con más de 13 años con fallas en la linea base
      if(sum(ind.falla) == 0)
      {
            cfiles<-cfiles
      } else {
            cfiles<-cfiles[-ind.falla,]
      }
     
       
# filtros para obtener recuperar la mayor cantidad de informacion posible 
cfiles$change<- NA
for(i in 1:nrow(cfiles)){

      if(((cfiles$X2022[i]>0)+(cfiles$X2049[i]>0))>=2) {
            cfiles$change[i]<- ((cfiles$X2049[i]-cfiles$X2022[i])/cfiles$X2022[i])*100}else{}
      
      
      if(((cfiles$X2022[i]==0)+(cfiles$X2049[i]>0))>=2) {
            cfiles$change[i]<- ((cfiles$X2049[i]-cfiles$X2023[i])/cfiles$X2023[i])*100}else{}
      
      if(((cfiles$X2022[i]==0)+(cfiles$X2049[i]>0)+ (cfiles$X2023[i]==0))>=3){
            cfiles$change[i]<- ((cfiles$X2049[i]-cfiles$X2024[i])/cfiles$X2024[i])*100}else{}
      
      if(((cfiles$X2022[i]==0)+(cfiles$X2049[i]==0))>=2){
            cfiles$change[i]<- ((cfiles$X2048[i]-cfiles$X2023[i])/cfiles$X2023[i])*100}else{}
      
      if(((cfiles$X2022[i]==0)+(cfiles$X2049[i]==0)+ (cfiles$X2023[i]==0))>=3){
            cfiles$change[i]<- ((cfiles$X2048[i]-cfiles$X2024[i])/cfiles$X2024[i])*100}else{}
      
      if(((cfiles$X2022[i]>0)+(cfiles$X2049[i]==0))>=2) {
            cfiles$change[i]<- ((cfiles$X2048[i]-cfiles$X2022[i])/cfiles$X2022[i])*100}else{}
      
      if(((cfiles$X2022[i]>0)+(cfiles$X2049[i]==0)+(cfiles$X2048[i]==0))>=3) {
            cfiles[-c(i),]} else{}      
      
      if(((cfiles$X2022[i]>0)+(cfiles$X2049[i]==0)+(cfiles$X2048[i]==0) + (cfiles$X2047[i]==0))>=4) {
            cfiles[-c(i), ]}else{} 
      
      if(((cfiles$X2022[i]>0)+(cfiles$X2049[i]==0)+(cfiles$X2048[i]==0) + (cfiles$X2047[i]==0)+ (cfiles$X2046[i]==0))>=5) {
            cfiles[-c(i), ]}else{}
      
      if(((cfiles$X2022[i]>0)+(cfiles$X2049[i]==0)+(cfiles$X2048[i]==0) + (cfiles$X2047[i]==0)+ (cfiles$X2046[i]==0)+ (cfiles$X2045[i]==0))>=6) {
            cfiles[-c(i), ]}else{}
      
      if(((cfiles$X2022[i]>0)+ (cfiles$X2023[i]>0)+(cfiles$X2049[i]==0)+(cfiles$X2048[i]==0) + (cfiles$X2047[i]==0)+ (cfiles$X2046[i]==0)+ (cfiles$X2045[i]==0)+ (cfiles$X2024[i]==0))>=8) {
            cfiles[-c(i), ]}else{}
      }


      ## Poner ceros en valores infinitos  y -100   
      ## e infinitos y cambios no recuperables
       if(length(which(cfiles$change == Inf)) > 0){
                  cfiles$change[which(cfiles$change == Inf)] <- 0}else{}
      if(length(which(cfiles$change==-100)) > 0){
            cfiles$change[which(cfiles$change==-100)] <- 0}else{}
  
      cfiles[is.na(cfiles)==TRUE]<- 0
      
      ### Eliminar ceros
      cfiles<-cfiles[!(cfiles$change==0),]
      ### eliminar datos extremos
      
      q95<- quantile(cfiles$change, 0.95)
      cfiles <- cfiles[-which(cfiles$change > q95),]; rownames(cfiles) <- 1:nrow(cfiles)


      cfiles<- aggregate(cfiles$change, by=list( cfiles$long, cfiles$lat, cfiles$FPU, cfiles$sys),FUN=mean)
      colnames( cfiles)<- c( "long","lat", "FPU","sys","mean" )
      cfiles$sys<- revalue(cfiles$sys, c("IRRI"="Irrigated",
                                        "RA"="Rain-fed"))
      
      # saveRDS(object = cfiles, file = paste(copyall, "soya.RDS", sep=""))
      
      png(filename = paste(copyall,crops[c],"_MeanYield__meanGCM.png", sep=""), 
          width = 20, height = 12, units = 'in', res = 100)
      p[[c]]<- ggplot() +
            geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
            geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
            geom_raster(data= cfiles, aes(x=long, y=lat,fill=mean))+ facet_wrap(~sys)+
            theme(strip.text.x = element_text(angle = 0,size = 16),strip.background = element_rect(colour="white", fill="white")) + 
            theme_bw()+ coord_equal() + theme(aspect.ratio = 1) +
            scale_fill_gradientn(colours=color_scale,na.value = "grey50")+ 
            labs(fill=labs2)+ 
            labs(title=crops[c])+
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.text = element_text(size=10),
                  legend.title = element_text(face="bold",size=10),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text.y = element_text(size=12, face="bold"),
                  plot.title = element_text(face="bold", size=10),
                  panel.border = element_blank(),
                  axis.ticks = element_blank(),
                  strip.background = element_rect(colour="white", fill="white")) 
      
      plot(p[[c]])
      dev.off()
      
}


g=gc;rm(list = ls())






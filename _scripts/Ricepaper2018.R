############################ maps and bars ############################################################################

#Load libraries-----------------
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

setwd("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BID_2/RicePaper/")
map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
fpuMaps<- shapefile(paste0(map,"FPU_Latinoamerica.shp"))

grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/Test")
grdmaps<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/BID_2/RicePaper/"
treat<- c("Irrigated","Rainfed")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
grdw<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/YieldsWeight/Test/")
#objetos
sys<- c( "IRRI", "RA")

Map_LatinAmerica1<- fortify(alc)
cfpuData<- (fpuMaps@data); cfpuData<- cfpuData[,c("New_FPU" , "Region_Nam")]
cfpeCen<-  (fpuMaps@data); cfpeCen<- cfpeCen[,c("New_FPU" , "Region_BID")]
colnames(cfpuData)<- c("FPU", "Country")
colnames(cfpeCen)<- c("FPU", "BIDregions")

p<- list()

## Start grafico de barras------------
dataF<- list.files(path = grdw,pattern= ".csv",full.names = T)
dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
dataF<- do.call(rbind,dataF); dataF$X<- NULL
cfiles<- dataF 

#### continue modeling datos
cfiles<- cfiles %>% spread("year", "Area_Wgtd_Mean")

cfiles[,"ymean"] <- apply(cfiles[, 5:ncol(cfiles)], 1, mean)
cfiles<- cfiles[,c("crop", "FPU", "sys", "sce","ymean")]
cfiles<- cfiles %>% spread(sce, ymean)
cfiles[,"meanGCM"] <- apply(cfiles[, 4:12], 1, mean)
cfiles<- cfiles[,c("crop","FPU","sys","WFD","meanGCM")]

cfiles$change<- ((cfiles$meanGCM-cfiles$WFD)/cfiles$WFD)*100

if(length(which(cfiles$change == Inf)) > 0){
      cfiles$change[which(cfiles$change == Inf)] <- 0}else{}
cfiles$change[is.na(cfiles$change)==TRUE]<- 0
### Eliminar ceros
cfiles<-cfiles[!(cfiles$change==0),]

# define a function to remove outliers
FindOutliers <- function(data) {
      lowerq = quantile(data)[2]
      upperq = quantile(data)[4]
      iqr = upperq - lowerq 
      #Or use IQR(data)
      # we identify extreme outliers
      extreme.threshold.upper = (iqr * 3) + upperq
      extreme.threshold.lower = lowerq - (iqr * 3)
      result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}


# use the function to identify outliers
temp <- FindOutliers(cfiles$change)
cfOut<- cfiles[temp,]
cfilesNEt<- cfiles[-temp,]
cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rain-fed"))

cfilesJoin<- left_join(cfilesNEt, cfpuData, by=("FPU"))
pots<- unique(cfilesJoin$Country)


cfilesJoin<- filter(cfilesJoin,crop=="Rice")
cfilesProof<- cfilesJoin
cfilesJoin<- cfilesJoin %>% gather(sce,val,4:5)

### grafico de barras
labs2 = 'Type'
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 

png(filename="//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/HetmapGCMYields.png", 
     width = 9, height = 6, units = 'in', res = 300)
#### grafico heatmap
n<- ggplot(data =cfilesJoin, aes(FPU,sys)) + 
      geom_tile(aes(fill = change), colour = "white")+  #facet_grid(sys~FPU,drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100), limits=c(-60,60))+ labs(x = "",y = "") +  
      coord_equal()+ 
      theme(aspect.ratio = 1)+
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



#### grafico de barras

png(filename= "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/bar_Facet.png", 
    width = 9, height = 6, units = 'in', res = 300)

pic2<- ggplot(data=cfilesProof, aes(x=crop, y=change,fill=sys)) + 
      facet_grid(.~sys)+
      geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.7))+
      scale_fill_brewer(palette = "Dark2") +
      labs(x="Crops",y="Percentage Change in Yield")+
      theme(legend.position = "none")+
      theme(axis.text.x=element_text(size=12))+
      theme(axis.text.y=element_text(size=12))+
      theme(strip.text.x = element_text(angle = 0,size = 14, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 14, face = "bold.italic"))+
      theme(aspect.ratio = 1)+
      theme(legend.text = element_text(size=11),
            legend.title = element_text(face="bold",size=12),
            legend.background = element_blank(),
            legend.key = element_blank(),
            strip.text.y = element_text(size=12, face="bold"),
            plot.title = element_text(face="bold", size=20),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(colour="white", fill="white")) 


plot(pic2)
dev.off()

      

## Start grafico de barras regions------------
dataF<- list.files(path = grdw,pattern= ".csv",full.names = T)
dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
dataF<- do.call(rbind,dataF); dataF$X<- NULL
cfiles<- dataF 
cfiles<- cfiles %>% spread("year", "Area_Wgtd_Mean")

cfiles[,"ymean"] <- apply(cfiles[, 5:ncol(cfiles)], 1, mean)
cfiles<- cfiles[,c("crop", "FPU", "sys", "sce","ymean")]
cfiles<- cfiles %>% spread(sce, ymean)
cfiles[,"meanGCM"] <- apply(cfiles[, 4:12], 1, mean)
cfiles<- cfiles[,c("crop","FPU","sys","WFD","meanGCM")]

cfiles$change<- ((cfiles$meanGCM-cfiles$WFD)/cfiles$WFD)*100

if(length(which(cfiles$change == Inf)) > 0){
      cfiles$change[which(cfiles$change == Inf)] <- 0}else{}
cfiles$change[is.na(cfiles$change)==TRUE]<- 0
### Eliminar ceros
cfiles<-cfiles[!(cfiles$change==0),]

# define a function to remove outliers
FindOutliers <- function(data) {
      lowerq = quantile(data)[2]
      upperq = quantile(data)[4]
      iqr = upperq - lowerq 
      #Or use IQR(data)
      # we identify extreme outliers
      extreme.threshold.upper = (iqr * 3) + upperq
      extreme.threshold.lower = lowerq - (iqr * 3)
      result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}


# use the function to identify outliers
temp <- FindOutliers(cfiles$change)
cfOut<- cfiles[temp,]
cfilesNEt<- cfiles[-temp,]
cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rain-fed"))

cfilesJoin<- left_join(cfilesNEt, cfpeCen, by=("FPU"))
pots<- unique(cfilesJoin$BIDregions)

################# coropletas----------
dataF<- list.files(path = grd1,pattern= "YieldMax_",full.names = T)
dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
dataF<- do.call(rbind,dataF); dataF$X<- NULL

cfiles<- dataF 
cfiles[,3:4]<- NULL
# cfiles<- cfiles %>% filter(., sce!="WFD")%>% group_by(long,lat,FPU,sys,crop,year) %>% summarise(val=mean(ymax, na.rm=T))
# cfiles<- as.data.frame(cfiles)    
#       
# cfiles<- cfiles %>% spread("year", "val")

cfiles<- cfiles %>% spread("year", "ymax")

cfiles[,"ymean"] <- apply(cfiles[, 7:ncol(cfiles)], 1, mean)
cfiles<- cfiles[,c("long","lat","FPU","sce","sys","crop","ymean")]
cfiles<- cfiles %>% spread(sce, ymean)
cfiles[,"meanGCM"] <- apply(cfiles[, 6:14], 1, mean)
cfiles<- cfiles[,c("long","lat","FPU","sys","crop","WFD","meanGCM")]


cfiles$change<- ((cfiles$meanGCM-cfiles$WFD)/cfiles$WFD)*100
cfiles<- cfiles[,c("long", "lat","FPU","sys","crop","change")]
cfiles$sys<- plyr::revalue(cfiles$sys, c("IRRI"="Irrigated","RA"="Rainfed"))


if(length(which(cfiles$change == Inf)) > 0){
      cfiles$change[which(cfiles$change == Inf)] <- 0}else{}
cfiles$change[is.na(cfiles$change)==TRUE]<- 0
### Eliminar ceros
cfiles<-cfiles[!(cfiles$change==0),]

# define a function to remove outliers
FindOutliers <- function(data) {
      lowerq = quantile(data)[2]
      upperq = quantile(data)[4]
      iqr = upperq - lowerq 
      #Or use IQR(data)
      # we identify extreme outliers
      extreme.threshold.upper = (iqr * 3) + upperq
      extreme.threshold.lower = lowerq - (iqr * 3)
      result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

cfilesJoin<- left_join(cfiles, cfpuData, by=("FPU"))

# # use the function to identify outliers
# temp <- FindOutliers(cfiles$change)
# cfOut<- cfiles[temp,]
# cfilesNEt<- cfiles[-temp,]
# 


lugares<- c("Bolivia","Argentina","Ecuador","Nicaragua","El Salvador","Guatemala","Dominican Republic","Honduras", "Jamaica")
# l=3 #pais
# c=1 #crops
# t=1 #treat

# allo<- alc[alc@data$COUNTRY==lugares[l],]
map_allo<- fortify(alc)
labs2 = 'Percentage Change\n (%)'
bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

for(t in 1:length(treat)){
            
            proof<- cfilesJoin %>% filter(., crop=="Rice")%>% 
                  filter(., sys==treat[t])%>% filter(., change<=150)%>% 
                  filter(.,change>=-150)

            
            
            #Coropleta 
            png(filename =paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/",treat[t],"_LACcoropleta.png", sep = ""), 
                width = 9, height = 6, units = 'in', res = 400)
            
            
            coro2<- ggplot() +
                  geom_polygon(data=map_allo, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                  geom_path(data=map_allo, aes(x=long, y=lat, group=group), colour="black", size=0.5)+
                  geom_raster(data= proof, aes(x=long, y=lat,fill=change), alpha=0.8)+
                  theme()+ coord_equal() + 
                  # scale_fill_gradient2(low="#FF0000FF", mid="white", high="#0000FFFF") + #, midpoint = 0,breaks=seq(-150,150,20)
                  # scale_fill_gradientn(colours = c("#FF0000FF","#FFFFFFFF","#0000FFFF"),breaks=seq(-150,150,20))+
                  scale_fill_gradientn(colours=c(bl,"white", re), na.value = "white",
                                       limits = c(-150,150)) +
            
                  labs(fill=labs2)+ labs(x=NULL, y=NULL, title= paste(treat[t]," ",crops[c]," (% change)", sep = ""))+
                  theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
                  theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
                  theme(axis.text=element_text(size=14),
                        axis.title=element_text(size=14,face="bold"))+
                  theme(legend.title=element_blank())+
                  theme(legend.text=element_text(size=14))
            
#                   theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.3))
            
            plot(coro2)
            dev.off() 
            
            cat(paste("terminado ",treat[t], " it's done\n", sep = ""))
      }
  



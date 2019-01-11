### Codigos para generar los graficos y procesar información del proyecto BID
### 09-13-2018
### Autor Carlos Edo Gonzalez

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
suppressMessages(library(gridExtra))
suppressMessages(library(sp))
suppressMessages(library(maptools))
suppressMessages(library(maps))
suppressMessages(library(raster))


#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

#Dirección graficos
grd<-"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/BID2version/"


################################ Cargando datos ######################################

#Cargar marco de datos principal
md<-read.csv("Phase2/V2_allRegions.csv",header=T)


#Hacer un subconjunto que sólo contenga las variables de mi interés
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                    md$impactparameter=="TAreaXAgg -- Total Area" |
                    md$impactparameter== "QNXAgg -- Net Trade" | 
                    md$impactparameter== "QDXAgg -- Total Demand" |
                    md$impactparameter=="TYldXAgg -- Total Yield")

mdsub$impactparameter<- as.character(mdsub$impactparameter)
mdsub$scenario<- as.character(mdsub$scenario)
mdsub$commodity<- as.character(mdsub$commodity)
mdsub$region<- as.character(mdsub$region)
mdsub$productiontype<- as.character(mdsub$productiontype)

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand",
                                                        "QNXAgg -- Net Trade"="Net Trade",
                                                        "QSXAgg -- Total Production"="Total Production",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"="Total Yield"))

#Hacer un subconjunto que sólo contenga los cinco cultivos analizados
mdsubcrop<-subset(mdsub,mdsub$commodity=="jbean"| mdsub$commodity=="jmaiz" |
                        mdsub$commodity=="jrice" | mdsub$commodity=="cs" |
                        mdsub$commodity=="jwhea" | mdsub$commodity=="cbean" |
                        mdsub$commodity=="cmaiz" | mdsub$commodity=="crice" |
                        mdsub$commodity=="js" | mdsub$commodity=="cwhea")


alc<- mdsubcrop[grep(pattern = "LAC-",x = mdsubcrop$region, ignore.case = T),]
alc$region<-  gsub("^LAC-", "",alc$region)

#reshape
mdwide<- alc %>% spread(year, Val)
mdwide$commodity<- revalue(mdwide$commodity, c("cbean"= "Bean",
                                               "cmaiz"="Maize",
                                               "crice"="Rice",
                                               "cs"="Soybean",
                                               "cwhea"="Wheat",
                                               "jbean"="Bean",
                                               "jmaiz"="Maize",
                                               "jrice"="Rice",
                                               "js"="Soybean",
                                               "jwhea"="Wheat"))

mdwide<-data.frame(mdwide,"Cat"=ifelse(mdwide$scenario=="NoCC","NoCC","CC"))

rend_all<- mdwide[,-c(6:20)]
rend_all$Percentage_Change<-((rend_all$X2050-rend_all$X2020)/rend_all$X2020)*100
write.csv(rend_all,paste(grd, "Data.csv", sep = ""))

#Mediana de los cambios porcentuales por categorias.
anal_datag<- aggregate(rend_all[,"Percentage_Change"],
                       by=list(rend_all$region,rend_all$impactparameter,
                               rend_all$commodity,rend_all$Cat),
                       FUN=median)
colnames(anal_datag)<- c("Region", "Parameter", "Crop", "Sce", "Val")
anal_datag$Sce<- as.character(anal_datag$Sce)
write.csv(anal_datag,paste(grd, "DataMedian.csv", sep = ""))
write.csv(anal_datag,paste(grd, "NolabelsDataMedian.csv", sep = ""))

# croput<- c("Soya", "Trigo") ### Cultivos para eliminar 
# anal_datag<- anal_datag %>% dplyr::filter(.,!Crop %in% croput)

################################ Graficos de IMPACT model ###############################

pl<-NULL
# i=1
pots<- unique(anal_datag$Region)

for (i in 1:length(pots)){
      
      tiff(filename=paste(grd,"Test_",pots[i],"_country_reportNew.tiff",sep=""), 
           width = 14, height = 7, units = 'in', res = 100)
      

      pl[[i]]<- print(ggplot(data=(subset(anal_datag,anal_datag$Region==pots[i]
                                          & anal_datag$Parameter!="Net Trade" 
                                          & anal_datag$Sce != "NoCC")),aes(x=Crop,y=Val
                                                                           ,fill=Parameter))  +
                            facet_wrap( ~Parameter,ncol=4)+
                            geom_bar(stat="identity")+
                            geom_point(aes(shape=Sce),data=(subset(anal_datag,anal_datag$Region== pots[i]
                                                                   & anal_datag$Parameter !="Net Trade" 
                                                                   & anal_datag$Sce == "NoCC")),
                                       alpha = 0.4,size=4) +
                            guides(fill=FALSE)+
                            theme(#axis.text.x = element_text(angle = 0, hjust = 1),
                                  legend.title=element_blank(),
                                  axis.text= element_text(face = "bold", size = 12),
                                  axis.title=element_text(face = "bold", size = 12))+
                            theme(legend.position="bottom",legend.text=element_text(size=14),
                                  strip.text.x = element_text(size = 16, angle = 0))+
                            labs(y="percentage change",x="")
      )
      
      
      dev.off()
      print(i)
}              

################################ Graficos de IMPACT model Net Trade ###############################

datos<- subset(rend_all,rend_all$impactparameter=="Net Trade")
# datos<- datos %>% dplyr::filter(.,!commodity %in% croput )
datmin<- aggregate(datos[,paste("X20",20:50,sep="")],
                   by=list(datos$region,datos$commodity),FUN=min)
datmin<- datmin %>% gather("time","datmin", 3:ncol(datmin))
names(datmin)<-c("Region","Crop","time","datmin")
datmin$time<-  gsub("X", "",datmin$time)
datmin$time<- as.numeric(datmin$time)


datmed<- aggregate(datos[,paste("X20",20:50,sep="")],
                   by=list(datos$region,datos$commodity),FUN=median)
datmed<- datmed %>% gather("time","datmed", 3:ncol(datmed))
names(datmed)<-c("Region","Crop","time","datmed")
datmed$time<-  gsub("X", "",datmed$time)
datmed$time<- as.numeric(datmed$time)


datmax<-aggregate(datos[,paste("X20",20:50,sep="")],
                  by=list(datos$region,datos$commodity),FUN=max)
datmax<- datmax %>% gather("time","datmax", 3:ncol(datmax))
names(datmax)<-c("Region","Crop","time","datmax")
datmax$time<-  gsub("X", "",datmax$time)
datmax$time<- as.numeric(datmax$time)

extremos<-merge(datmin,datmax)
datost<-merge(extremos,datmed)

py<- NULL

##### croput<- c("Soybean", "Wheat")
# i=9
for (i in 1:length(pots)) {
      
      tiff(filename=paste(grd,"Test_",pots[i],"_net_tradeNew.tiff",sep=""), 
           width = 10, height = 7, units = 'in', res = 100)
      
      py[[i]]<-print(ggplot(data=subset(datost,datost$Region==pots[i]),
                            aes(time,datmed,group=Crop,color=Crop)) + 
                           geom_line(linetype="dashed",size=1)+
                           geom_ribbon(aes(ymin=datmin,ymax=datmax,
                                           fill=Crop,colour=Crop,
                                           linetype=NA),
                                       alpha=0.1) +
                           labs(y="Net Trade (000 tm)",x="Year")+ 
                           theme(legend.position="bottom", 
                                 legend.text=element_text(size=20),
                                 axis.text= element_text(face = "bold.italic", size = 20),
                                 axis.title=element_text(face = "bold.italic", size = 20),
                                 legend.title=element_blank())
      )
      
      
      dev.off()
      print(i)
}  

# g=gc;rm(list = ls())

################################ Graficos de DSSAT por país ############################################################################

map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
fpuMaps<- shapefile(paste0(map,"FPU_Latinoamerica.shp"))

grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/Test")
treat<- c("Irrigated","Rainfed")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
grdw<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/YieldsWeight/Test/")
#objetos
sys<- c( "IRRI", "RA")

Map_LatinAmerica1<- fortify(alc)
cfpuData<- (fpuMaps@data); cfpuData<- cfpuData[,c("New_FPU", "Region_Nam")]
cfpeCen<-  (fpuMaps@data); cfpeCen<- cfpeCen[,c("New_FPU", "Region_BID")]
colnames(cfpuData)<- c("FPU", "Country")
colnames(cfpeCen)<- c("FPU", "BIDregions")

cfpeCenV2<-  (fpuMaps@data)
cfpeCenV2<- cfpeCenV2[,c("New_FPU","Region_Nam", "Region_BID")]


p<- list()


## Start grafico de barras
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
q<- quantile(cfiles[,("change")],prob=0.98,na.rm=TRUE)

cfiles[which(cfiles[,6]>q),6]=q
# temp <- FindOutliers(cfiles$change)

# cfilesNEt<- cfiles[-temp,]
cfilesNEt<- cfiles
cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rainfed"))
cfilesJoin<- left_join(cfilesNEt, cfpuData, by=("FPU"))

# cfilesNEt<- cfiles
# cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rainfed"))
# cfilesJoin<- left_join(cfilesNEt, cfpuData, by=("FPU"))
pots<- unique(cfilesJoin$Country)

p=5


# Create shape by country
for(p in 1:length(pots)){
      
      
      cfilesProof<- cfilesJoin %>% filter(., Country==pots[p])
      labs2 = 'Type'

      png(filename= paste(grd,"Super",pots[p],"_bar_Facet.png", sep = ""), 
          width = 10, height = 7, units = 'in', res = 300)
      
      pic2<- ggplot(data=cfilesProof, 
                    aes(x=crop, y=change,fill=sys)) + 
            facet_grid(.~sys)+
            geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.7))+
            scale_fill_brewer(palette = "Dark2") +
            labs(x="Crops",y="Percentage Change in Yields")+
            theme(legend.position = "none")+
            theme(axis.text.x=element_text(size=12))+
            theme(axis.text.y=element_text(size=12))+
            theme(strip.text.x = element_text(angle = 0,size = 12, face = "bold"))+
            theme(strip.text.y = element_text(angle = 0,size = 12, face = "bold"))+
            labs(fill=labs2)+ theme(aspect.ratio = 1)+
            theme(legend.text = element_text(size=11),
                  legend.title = element_text(face="bold",size=12),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text.y = element_text(size=12, face="bold"),
                  plot.title = element_text(face="bold", size=20),
                  panel.border = element_blank(),
                  axis.ticks = element_blank(),
                  strip.background = element_rect(colour="white", fill="white")) 
#       +
#             ylim(-40, 10)
      
      
      plot(pic2)
      dev.off()
      
      
      
      cat(paste(pots[p], " ha sido terminado\n great!!!", sep = ""))
      
}

################################ Graficos de DSSAT Regiones #####################
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
cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rainfed"))
cfilesNEt$crop<- plyr::revalue(cfilesNEt$crop, c("Maize"="Maize"))
colnames(cfpeCenV2)[1]<- "FPU"
cfilesJoin<- left_join(cfilesNEt, cfpeCenV2, by=("FPU"))
pots<- unique(cfilesJoin$Region_BID)
lugares<- c("Colombia", "Peru", "Uruguay", "Costa Rica", "Mexico", "Panama")


### ajuste CEN
# croput<- c("Soybean", "Wheat")
# cfilesJoin<- cfilesJoin %>% dplyr::filter(.,!crop %in% croput)
### Analisis por pais:seleccionar el pais y la region para excluirlo del analisis.
### Analisis regional deshabilitar p y l y correr el codigo deshabilitando el segundo filtro
p=4 # Region_BID
l=3 # pais

# Create shape by RegionsBID
for(p in 1:length(pots)){
      
      cfilesProof<- cfilesJoin %>% filter(., Region_BID==pots[p])%>% 
            filter(.,Region_Nam!=lugares[l]) # analisis regional deshabilitar el segundo filtro

      png(filename= paste(grd,"Super",pots[p],"_Regional_bar_Facet.png", sep = ""), 
          width = 10, height = 6, units = 'in', res = 300)
      
      pic2<- ggplot(data=cfilesProof, aes(x=crop, y=change,fill=sys)) + 
            facet_grid(.~sys)+
            geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.7))+
            scale_fill_brewer(palette = "Dark2") +
            labs(x="Crops",y="Percentage change \nin Yields")+
            #             theme(legend.position = "none")+
            #             theme(axis.text.x=element_text(size=12))+
            #             theme(axis.text.y=element_text(size=12))+
            #             theme(strip.text.x = element_text(angle = 0,size = 14, face = "bold.italic"))+
            #             theme(strip.text.y = element_text(angle = 0,size = 14, face = "bold.italic"))+
            theme(aspect.ratio = 1)+
            theme(legend.text = element_text(size=11),
                  legend.title = element_text(face="bold",size=12),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text.y = element_text(size=12, face="bold"),
                  plot.title = element_text(face="bold", size=20),
                  panel.border = element_blank(),
                  axis.ticks = element_blank(),
                  strip.background = element_rect(colour="white", fill="white"),
                  axis.text= element_text(face = "bold", size = 12),
                  axis.title=element_text(face = "bold", size = 12)) +
            ylim(-40, 10)+ theme(legend.position="bottom",legend.text=element_text(size=20),
                                 strip.text.x = element_text(size = 16, angle = 0))+
            theme(legend.position = "none")
      
      
      
      plot(pic2)
      dev.off()
      
      
      
      cat(paste(pots[p], " ha sido terminado\n great!!!", sep = ""))
      
}

################################ Graficos de coropletas ###################
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


lugares<- c("Colombia", "Peru", "Uruguay", "Costa Rica", "Mexico", "Panama")

#### Selección por pais y cultivo
l=2 #pais
c=1 #crops
t=2 #treat 1=irri 2=rainf

allo<- alc[alc@data$COUNTRY==lugares[l],]
map_allo<- fortify(allo)
labs2 = 'Percentage Change\n (%)'

for(c in 1:length(crops)){
      for(t in 1:length(treat)){
            
            proof<- cfilesJoin %>% filter(., Country==lugares[l] & crop==crops[c] & sys==treat[t])
            proof<- filter(proof, change<=100) %>% filter(., change>=-100)
            
            
            #Coropleta 
            png(filename = paste(grd,lugares[l],"_",crops[c],"-",treat[t],"_","coropleta.png", sep=""), width = 10, height = 6, units = 'in', res = 400)
            
            coro2<- ggplot() +
                  geom_polygon(data=map_allo, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                  geom_path(data=map_allo, aes(x=long, y=lat, group=group), colour="black", size=0.5)+
                  geom_raster(data= proof, aes(x=long, y=lat,fill=change), alpha=0.8)+
                  theme()+ coord_equal() + 
                  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint = 0,breaks=seq(-4,12,2)) +
                  labs(fill=labs2)+ labs(x=NULL, y=NULL, title= paste(treat[t]," ",crops[c]," (% change)", sep = ""))+
                  theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold"))+
                  theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold"))+
                  theme(axis.text=element_text(size=14),
                        axis.title=element_text(size=14,face="bold"))+
                  theme(legend.title=element_blank())+
                  theme(legend.key.width = unit(0.5,"cm"),
                        legend.box.spacing = unit(0.5,"cm"),legend.key.height = unit(1,"cm"))+
                  theme(legend.text=element_text(size=12))+
                  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.3))
            
            plot(coro2)
            
            dev.off() 
            
            cat(paste("terminado ", crops[c]," ",treat[t], " it's done\n", sep = ""))
      }
      cat(paste("terminado ", crops[c], " it's done\n  is completed", sep = ""))
      
}

################################ Graficos de Suitability #################################

#Limitar numero de decimales
options(digits=3) 
options(scipen = 999)

#load data 
grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2")
grd2<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/")
grdmaps<-"C://dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/"
treat<- c("Irrigated","Rainfed")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 
sui<- read.csv(paste(grd1,"/","summary_table_countriesv2.csv", sep = ""))
sui[,1]<- NULL
colnames(sui)<- c("Crop","Region", "Mean suitability change (%)",	"SD (%)","Current area (km2)","Future area (km2)")
sui$Crop<- as.character(sui$Crop)
sui$Region<- as.character(sui$Region)
pots<- unique(sui$Region)
# 7: costarica
#15: Honduras
#reshape
sui<- sui %>% gather(variables, val, 3:ncol(sui))
### Eliminar ceros
sui$val[is.na(sui$val)==TRUE]<- 0
sui<-sui[!(sui$val==0),]


# p=14
for(p in 1:length(pots)){
      
      suifiles<- dplyr::filter(sui,Region==pots[p]) %>% 
            dplyr::filter(., variables=="Mean suitability change (%)")
            # %>% 
            # filter(.,Crop!="Coffee Robusta") %>% filter(.,Crop!="Coffee Arabica")
      
      labs2 = 'Type'
      png(filename= paste(grd,"/Suitability_",pots[p],"_bar_Interactions.png", sep = ""), 
          width = 12, height = 6, units = 'in', res = 300)
      
      pic3<- ggplot(data=suifiles, aes(x=Crop, y=val,fill=Crop)) + 
            geom_bar(stat="identity",position=position_dodge(width=.7))+
            scale_fill_brewer(palette = "Dark2") +
            labs(x="Crops",y="Percentage Change in Suitability")+
            theme(legend.position = "none")+ 
            #             theme(axis.text.x=element_text(size=30))+
            #             theme(axis.text.y=element_text(size=30))+
            #             theme(strip.text.x = element_text(angle = 90,size = 30, face = "bold.italic"))+
            #             theme(strip.text.y = element_text(angle = 0,size = 30, face = "bold.italic"))+
            labs(fill=labs2)+ theme(aspect.ratio = 1)+
            theme(#legend.text = element_text(size=11),
                  axis.text =(element_text(face = "bold.italic", size = 24)),
                  #                   legend.title =element_text(face="bold",size=14),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  #                   axis.text.x=element_text(size=30),
                  #                   strip.text.y = element_text(size=14, face="bold"),
                  #                   plot.title = element_text(face="bold", size=20),
                  panel.border = element_blank(),
                  axis.ticks = element_blank(),
                  strip.background = element_rect(colour="white", fill="white")) +
            theme_light()+theme(legend.position = "none",
                                axis.text =(element_text(face = "bold", size = 12)),
                                axis.title =element_text(face = "bold", size = 12))
      
      
      
      plot(pic3)
      dev.off()
      
      
      
      cat(paste(pots[p], " ha sido terminado\n great!!!", sep = "")) 
}

################################ Mapas de Suitability ###############

#### Mapas EcoCrop 16 Mayo 2016 
#### Estos Graficos son utilizando ya los mapas categorizados creados por Patricia
recategorizar <- function(data){ 
      # color <- 0
      if(data == 5){
            data <- "Convierte en idóneo"#'Becomes Suitable'
            # color <- 'Blue'
      }
      
      if(data == 4){
            data <-  "Más idóneo"#'More Suitable'
            # color <- 'Green'
      }
      
      if(data == 3){
            data <- "Mantiene idóneo" #'Remains Suitable'
            # color <- 'burlywood1'
      }
      
      if(data == 2){
            data <- "Menos,pero todavia idóneo"#'Less but Still Suitable'
            # color <- 'Orange'
      }
      
      if(data == 1){
            data <-"Convierte en No idóneo" #'Becomes Unsuitable'
            # color <- 'Red'
      }
      
      if(data == 0){
            data <- 'NA'
      }
      
      # return(data.frame(categoria = data, color = color))
      return(data)
}

path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/'
names_crop <- c('yam', 'sugarcane', 'potato','cassava', 'banana')  # 'coffea_robusta'
names_crop_to_graph <- c('Yam', 'Sugarcane', 'Potato', 'Cassava', 'Banana')  #'Coffee Robusta'
# by_country <- c("Colombia", "Peru", "Uruguay", "Costa Rica", "Mexico", "Panama")
by_country <- c("Honduras")

shape <- readOGR(paste0(path, '03-Map_LatinAmerica/Latino_America1.shp'), 'Latino_America1')
path_data <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Raster visualizador Ecocrop/Future/'


#### Para generar los graficos se selecciona el pais y el cultivo
i=1  #crop
j=1  #country
for(i in 1:length(names_crop)) {
      
      crop_sC <- raster(paste0(path_data, names_crop[i], '_change_forest_exc_suit_categories.tif'))
      filter_country <- shape[shape$COUNTRY == by_country[j], ]
      crop_sC <- crop( crop_sC, filter_country)
      crop_sC <- raster::mask( crop_sC, filter_country)
      crop_sC <- rasterToPoints(crop_sC) 
      crop_sC_re <- data.frame(crop_sC, categoria = unlist(lapply(crop_sC[, 3], recategorizar)) )
      colors_by_categoria <- data.frame(categoria = c("Convierte en No idóneo",#'Becomes Unsuitable', 
                                                      "Menos,pero todavia idóneo",#'Less but Still Suitable', 
                                                      "Mantiene idóneo",#'Remains Suitable', 
                                                      "Más idóneo",#'More Suitable',
                                                      "Convierte en idóneo"#'Becomes Suitable'
      ), 
      
      color = c('#e41a1c',
                '#ff7f00',
                '#fb9a99',
                '#984ea3',
                '#377eb8'
      ))
      
      
      levels_col_cat <- colors_by_categoria %>%
            filter(categoria %in% unique(crop_sC_re$categoria))
      
      crop_sC_re <- crop_sC_re %>%
            filter(categoria != 'NA' ) 
      
      crop_sC_re$categoria <- factor(crop_sC_re$categoria, levels = as.character(levels_col_cat$categoria))

      x <- crop_sC_re
      filter_country1 <- fortify(filter_country)
      
      png(filename = paste(grd,'EcoCrop_atrr_', names_crop_to_graph[i],"_","BID_Version3_coropleta.png", sep=""), width = 10, height = 6, units = 'in', res = 400)
      
      y <- ggplot() +
            geom_polygon( data = filter_country1, aes(x=long, y=lat, group = group), colour="red", fill="white", alpha = 0.7 )+
            geom_raster(data=x, aes(x, y, fill = categoria)) +
            geom_path(data = filter_country1, aes(x=long, y=lat, group=group), colour="black", size = 0.25)+
            coord_equal() +
            ggtitle(paste(names_crop_to_graph[i])) +
            # ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],'): \n',models[m,],sep=''))+
            # scale_fill_gradientn(colours=color_scale,limits=limits2,na.value = "grey50")+ # limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
            theme_bw()+
            labs(fill='')+
            theme(
                  legend.text = element_text(size=14),
                  legend.title = element_text(face="bold",size=14),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())+
            scale_fill_manual(values = as.character(levels_col_cat$color))
      
      
      plot(y)
      dev.off() 
      
      
}


dev.off()

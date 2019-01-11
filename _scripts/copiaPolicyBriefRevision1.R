# Actualizado Programa para formatear los resultados del modelo IMPACT y realizar los analisis graficos-----
#Por: ajustado por Carlos Edo
library(reshape)
library(ggplot2)
library(plyr)
library(tidyr)
library(grid)
library(gridExtra)


#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

#Dirreción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/"


#Cargar marco de datos principal
# md<-read.csv("Resultados_Ciat_StCty_31_08_16_new.csv",header=T)
md<-read.csv("Phase2/V2_allRegions.csv",header=T)
# md<-read.csv("Phase2/TestAllRegionsBID.csv",header=T)

zones<- c("Argentina", "Bolivia", 
          "Ecuador", "Guatemala", "Nicaragua",
          "Republica Dominicana")

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
mdwide$commodity<- revalue(mdwide$commodity, c("cbean"= "Fríjol",
                                              "cmaiz"="Maíz",
                                              "crice"="Arroz",
                                              "cs"="Soya",
                                              "cwhea"="Trigo",
                                              "jbean"="Fríjol",
                                              "jmaiz"="Maíz",
                                              "jrice"="Arroz",
                                              "js"="Soya",
                                              "jwhea"="Trigo"))

mdwide<-data.frame(mdwide,"Cat"=ifelse(mdwide$scenario=="NoCC","NoCC","CC"))

rend_all<- mdwide[,-c(6:20)]
rend_all$Percentage_Change<-((rend_all$X2050-rend_all$X2020)/rend_all$X2020)*100
write.csv(rend_all,paste(grd, "Data.csv", sep = ""))
# write.csv(rend_all,paste(grd, "NolabelsData.csv", sep = ""))

#Mediana de los cambios porcentuales por categorias.

anal_datag<- aggregate(rend_all[,"Percentage_Change"],
                      by=list(rend_all$region,rend_all$impactparameter,
                              rend_all$commodity,rend_all$Cat),
                      FUN=median)
colnames(anal_datag)<- c("Region", "Parameter", "Crop", "Sce", "Val")
anal_datag$Sce<- as.character(anal_datag$Sce)
write.csv(anal_datag,paste(grd, "DataMedian.csv", sep = ""))
# write.csv(anal_datag,paste(grd, "NolabelsDataMedian.csv", sep = ""))
croput<- c("Soya", "Trigo")
anal_datag<- anal_datag %>% dplyr::filter(.,!Crop %in% croput)
#Realizar proceso para graficar todas las variables por regiones.

pl<-NULL
# i=9
pots<- unique(anal_datag$Region)

for (i in 1:length(pots)){
      
      tiff(filename=paste(grd,"Test_",pots[i],"_country_reportNew.tiff",sep=""), 
           width = 12, height = 7, units = 'in', res = 100)
      
      anal_datag$Parameter<- revalue(anal_datag$Parameter, c("Net Trade"="Comercio Neto",
                                                             "Total Area"="Área total",
                                                             "Total Demand"="Demanda Total",
                                                             "Total Production"="Producción Total",
                                                             "Total Yield"="Rendimiento Total"))
      
      pl[[i]]<- print(ggplot(data=(subset(anal_datag,anal_datag$Region==pots[i]
                                                   & anal_datag$Parameter!="Comercio Neto" 
                                                   & anal_datag$Sce != "NoCC")),aes(x=Crop,y=Val
                                                                                        ,fill=Parameter))  +
                           facet_wrap( ~Parameter,ncol=4)+
                           geom_bar(stat="identity")+
                           geom_point(aes(shape=Sce),data=(subset(anal_datag,anal_datag$Region== pots[i]
                                                                                & anal_datag$Parameter !="Comercio Neto" 
                                                                                & anal_datag$Sce == "NoCC")),
                                      alpha = 0.4,size=4) +
                           guides(fill=FALSE)+
                           theme(#axis.text.x = element_text(angle = 0, hjust = 1),
                                 legend.title=element_blank(),
                                       axis.text= element_text(face = "bold.italic", size = 20),
                                       axis.title=element_text(face = "bold.italic", size = 20))+
                           theme(legend.position="bottom",legend.text=element_text(size=20),
                                 strip.text.x = element_text(size = 16, angle = 0))+
                           labs(y="Cambio porcentual",x="")
      )
      
      
      dev.off()
      print(i)
}              


#Evolution of Net Trade by crop and by region
rend_all$impactparameter<- revalue(rend_all$impactparameter, c("Net Trade"="Comercio Neto",
                                                       "Total Area"="Área total",
                                                       "Total Demand"="Demanda Total",
                                                       "Total Production"="Producción Total",
                                                       "Total Yield"="Rendimiento Total"))
datos<- subset(rend_all,rend_all$impactparameter=="Comercio Neto")
# datos<- datos %>% dplyr::filter(.,!commodity %in% croput )
datmin<- aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=min)
datmin<- datmin %>% gather("time","datmin", 3:ncol(datmin))
names(datmin)<-c("Region","Crop","time","datmin")
datmin$time<-  gsub("X", "",datmin$time)
datmin$time<- as.numeric(datmin$time)


datmed<- aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=median)
datmed<- datmed %>% gather("time","datmed", 3:ncol(datmed))
names(datmed)<-c("Region","Crop","time","datmed")
datmed$time<-  gsub("X", "",datmed$time)
datmed$time<- as.numeric(datmed$time)


datmax<-aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=max)
datmax<- datmax %>% gather("time","datmax", 3:ncol(datmax))
names(datmax)<-c("Region","Crop","time","datmax")
datmax$time<-  gsub("X", "",datmax$time)
datmax$time<- as.numeric(datmax$time)

extremos<-merge(datmin,datmax)

datost<-merge(extremos,datmed)

py<- NULL

# croput<- c("Soybean", "Wheat")
i=9
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
                           labs(y="Comercio neto (000 tm)",x="Año")+ 
                           theme(legend.position="bottom", 
                                 legend.text=element_text(size=20),
                                 axis.text= element_text(face = "bold.italic", size = 20),
                                 axis.title=element_text(face = "bold.italic", size = 20),
                                 legend.title=element_blank())
      )
      
      
      dev.off()
      print(i)
}  

g=gc;rm(list = ls())

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

map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
fpuMaps<- shapefile(paste0(map,"FPU_Latinoamerica.shp"))

grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/Test")
grdmaps<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/policyBriefNew/policyBriefTest/"
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

cfilesJoin<- left_join(cfilesNEt, cfpuData, by=("FPU"))
pots<- unique(cfilesJoin$Country)

p=11


# Create shape by country-----------
for(p in 1:length(pots)){

              
               cfilesProof<- cfilesJoin %>% filter(., Country==pots[p])
               labs2 = 'Type'
#                  png(filename= paste(grdmaps,"Super",pots[p],"_bar_Interactions.png", sep = ""), 
#                 width = 9, height = 6, units = 'in', res = 300)
#                pic1<- ggplot(cfilesProof, aes(interaction(crop, sys),change, fill=sys)) + 
#                      geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.60), na.rm = T)+
#                      scale_fill_brewer(palette = "BrBG") +
#                      labs(x="Cultivos",y="Cambio porcentual en rendimientos")+
#                      labs(fill=labs2)+
#                      theme(axis.text.x=element_text(size=12, angle = 90))+
#                      theme(axis.text.y=element_text(size=12))
#                
#                
#                plot(pic1)
#                
#                dev.off()
               png(filename= paste(grdmaps,"Super",pots[p],"_bar_Facet.png", sep = ""), 
                   width = 9, height = 6, units = 'in', res = 300)
               
               pic2<- ggplot(data=cfilesProof[which(cfilesProof$crop!="Bean") & which(cfilesProof$sys!="Irrigated"),], 
                             aes(x=crop, y=change,fill=sys)) + 
                     facet_grid(.~sys)+
                     geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.7))+
                     scale_fill_brewer(palette = "Dark2") +
                     labs(x="Cultivos",y="Percentage Change in Yields")+
                     theme(legend.position = "none")+
                     theme(axis.text.x=element_text(size=12))+
                     theme(axis.text.y=element_text(size=12))+
                     theme(strip.text.x = element_text(angle = 0,size = 14, face = "bold.italic"))+
                     theme(strip.text.y = element_text(angle = 0,size = 14, face = "bold.italic"))+
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
               
               
               plot(pic2)
               dev.off()
               
               
               
               cat(paste(pots[p], " ha sido terminado\n great!!!", sep = ""))

}


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
cfilesNEt$sys<- plyr::revalue(cfilesNEt$sys, c("IRRI"="Irrigated","RA"="Rainfed"))
cfilesNEt$crop<- plyr::revalue(cfilesNEt$crop, c("Maize"="Maize"))

cfilesJoin<- left_join(cfilesNEt, cfpeCen, by=("FPU"))
pots<- unique(cfilesJoin$BIDregions)

### ajuste CEN
croput<- c("Soybean", "Wheat")
cfilesJoin<- cfilesJoin %>% dplyr::filter(.,!crop %in% croput)
p=3


# Create shape by RegionsBID-----------
for(p in 1:length(pots)){
      
      
      cfilesProof<- cfilesJoin %>% filter(., BIDregions==pots[p])
      
      cfilesProof$crop<- revalue(cfilesProof$crop,c("Maize"="Maíz",
                                                    "Bean"="Fríjol",
                                                    "Rice"="Arroz"))
      
      
      cfilesProof$sys<- revalue(cfilesProof$sys,c("Rainfed"="Secano",
                                                  "Irrigated"="Irrigado")) 
      
#       labs2 = 'Type'
#       png(filename= paste(grdmaps,"Super",pots[p],"_bar_Interactions.png", sep = ""), 
#           width = 9, height = 6, units = 'in', res = 300)
#       pic1<- ggplot(cfilesProof, aes(interaction(crop, sys),change, fill=sys)) + 
#             geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.60), na.rm = T)+
#             scale_fill_brewer(palette = "Dark2") +
#             labs(x="Crops",y="Percentage Change in Yield")+
#             labs(fill=labs2)+
#             theme(axis.text.x=element_text(size=12, angle = 90))+
#             theme(axis.text.y=element_text(size=12))
#       
#       
#       plot(pic1)
#       
#       dev.off()
      png(filename= paste(grdmaps,"Super",pots[p],"_bar_Facet.png", sep = ""), 
          width = 9, height = 6, units = 'in', res = 300)
      
      pic2<- ggplot(data=cfilesProof, aes(x=crop, y=change,fill=sys)) + 
            facet_grid(.~sys)+
            geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.7))+
            scale_fill_brewer(palette = "Dark2") +
            labs(x="Cultivos",y="Cambio porcentual \nen rendimientos")+
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
                  axis.text= element_text(face = "bold.italic", size = 24),
                  axis.title=element_text(face = "bold.italic", size = 20)) +
            ylim(-40, 10)+ theme(legend.position="bottom",legend.text=element_text(size=20),
                                 strip.text.x = element_text(size = 16, angle = 0))+
            theme(legend.position = "none")
      
      
      
      plot(pic2)
      dev.off()
      
      
      
      cat(paste(pots[p], " ha sido terminado\n great!!!", sep = ""))
      
}

################# coropletas ###################
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



lugares<- c("Bolivia","Argentina","Ecuador","Nicaragua","El Salvador","Guatemala","Dominican Republic","Honduras", "Jamaica", "Colombia")
l=10 #pais
c=4 #crops
t=2 #treat

allo<- alc[alc@data$COUNTRY==lugares[l],]
map_allo<- fortify(allo)
labs2 = 'Percentage Change\n (%)'

for(c in 1:length(crops)){
      for(t in 1:length(treat)){
            
            proof<- cfilesJoin %>% filter(., Country==lugares[l] & crop==crops[c] & sys==treat[t])
            proof<- filter(proof, change<=100) %>% filter(., change>=-100)
                           
 
            
            #Coropleta 
            png(filename = paste(grd,lugares[l],"_",crops[c],"-",treat[t],"_","coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

       
             coro2<- ggplot() +
                   geom_polygon(data=map_allo, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
                   geom_path(data=map_allo, aes(x=long, y=lat, group=group), colour="black", size=0.5)+
                   geom_raster(data= proof, aes(x=long, y=lat,fill=change), alpha=0.8)+
                   theme()+ coord_equal() + 
                   scale_fill_gradient2(low="red", mid="white", high="blue", midpoint = 0,breaks=seq(-100,100,20)) +
                   labs(fill=labs2)+ labs(x=NULL, y=NULL, title= paste(treat[t]," ",crops[c]," (% change)", sep = ""))+
                   theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
                   theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
                   theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))+
                   theme(legend.title=element_blank())+
                   theme(legend.text=element_text(size=14))+
                   theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.3))
             
             plot(coro2)
            dev.off() 
             
       cat(paste("terminado ", crops[c]," ",treat[t], " it's done\n", sep = ""))
      }
      cat(paste("terminado ", crops[c], " it's done\n  is completed", sep = ""))
      
}


################################### Suitability #################################

#Limitar numero de decimales-----
options(digits=3) 
options(scipen = 999)

#load data 
grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2")
grd2<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/")
grdmaps<-"C://dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/"
treat<- c("Irrigated","Rainfed")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 
sui<- read.csv(paste(grd1,"/","summary_table_countries.csv", sep = ""))
sui[,1]<- NULL
colnames(sui)<- c("Crop","Region", "Mean suitability change (%)",	"SD (%)","Current area (km2)","Future area (km2)")
sui$Crop<- as.character(sui$Crop)
sui$Region<- as.character(sui$Region)
pots<- unique(sui$Region)
#reshape
sui<- sui %>% gather(variables, val, 3:ncol(sui))
### Eliminar ceros
sui$val[is.na(sui$val)==TRUE]<- 0
sui<-sui[!(sui$val==0),]


p=14
for(p in 1:length(pots)){
      
      suifiles<- dplyr::filter(sui,Region==pots[p]) %>% 
            dplyr::filter(., variables=="Mean suitability change (%)") %>% 
            filter(.,Crop!="Wheat") %>% filter(.,Crop!="Yam")%>% filter(.,Crop!="Coffee Robusta")
#       suifiles$Crop<- revalue(suifiles$Crop,c("Banana"="Banano",
#                                               "Cassava"="Yuca",
#                                               "Sugarcane"="Caña de Azúcar",
#                                               "Yam"="Ñame"))
#       %>% 
#             filter(.,Crop!="Yam")
      labs2 = 'Type'
      png(filename= paste(grd1,"/Suitability_",pots[p],"_bar_Interactions.png", sep = ""), 
          width = 9, height = 6, units = 'in', res = 300)
     
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
                                axis.text =(element_text(face = "bold.italic", size = 12)),
                                axis.title =element_text(face = "bold.italic", size = 12))
      
   
      
      plot(pic3)
      dev.off()
      
      
      
      cat(paste(pots[p], " ha sido terminado\n great!!!", sep = "")) 
}


############################### regions ##############################################################################
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

map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
fpuMaps<- shapefile(paste0(map,"FPU_Latinoamerica.shp"))

grd1<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/Test")
grdmaps<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/policyBriefNew/policyBriefTest/"
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

cfilesJoin<- left_join(cfilesNEt, cfpuData, by=("FPU"))
pots<- unique(cfilesJoin$Country)

p=9

# Create shape by country-----------
for(p in 1:length(pots)){
      
      
      cfilesProof<- cfilesJoin %>% filter(., Country==pots[p])%>% 
            filter(., !crop %in% croput)
      labs2 = 'Type'

      cfilesProof$crop<- revalue(cfilesProof$crop,c("Maize"="Maíz",
                                              "Bean"="Fríjol",
                                              "Rice"="Arroz"))
      
      
      cfilesProof$sys<- revalue(cfilesProof$sys,c("Rainfed"="Secano",
                                                    "Irrigated"="Irrigado")) 
                             
#       png(filename= paste(grdmaps,"Super",pots[p],"_bar_Interactions.png", sep = ""), 
#           width = 9, height = 6, units = 'in', res = 300)
#       pic1<- ggplot(cfilesProof, aes(interaction(crop, sys),change, fill=sys)) + 
#             geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.60))+
#             scale_fill_brewer(palette = "Dark2") +
#             labs(x="Crops",y="Percentage Change in Yield")+
#             labs(fill=labs2)+
#             theme(axis.text.x=element_text(size=12, angle = 90))+
#             theme(axis.text.y=element_text(size=12))
#       
#       
#       plot(pic1)
#       
#       dev.off()
      png(filename= paste(grdmaps,"Super",pots[p],"_bar_Facet.png", sep = ""), 
          width = 9, height = 6, units = 'in', res = 300)
      
      pic2<- 
      ggplot(data=cfilesProof, aes(x=crop, y=change,fill=sys)) + 
            facet_grid(.~sys)+
            geom_bar(stat= "summary", fun.y = "mean",position=position_dodge(width=.7),na.rm = T)+
            scale_fill_brewer(palette = "Dark2") +
            labs(x="Cultivos",y="Cambio porcentual\n en rendimiento")+
            theme(legend.position = "none")+
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
                  axis.text= element_text(face = "bold.italic", size = 24),
                  axis.title=element_text(face = "bold.italic", size = 20)) +
            ylim(-40, 10)+ theme(legend.position="bottom",legend.text=element_text(size=20),
                                 strip.text.x = element_text(size = 16, angle = 0))+
            theme(legend.position = "none")
            
      
      plot(pic2)
      dev.off()
      
      
      
      cat(paste(pots[p], " ha sido terminado\n great!!!", sep = ""))
      
}



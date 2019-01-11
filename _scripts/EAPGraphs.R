

##########################################################################################################################%
#############################  PARTE 1. codigo para cargar datos por grupos y dejarlos listos para el procesamiento. #####%
##########################################################################################################################%
g=gc;rm(list = ls())

# directories-------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/USAIDForGFSF")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/Graphs/")
grp<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/CSVfiles/")
rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")

# librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(dplyr)
library(RColorBrewer)

#################################################### load files ###########################################


# Datos por sistema de produccion  riego y secano---------------
datasys<- c("YldXAgg","AreaXAgg")
clist<-list()
for( i in 1:length(datasys)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",datasys[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- datasys[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Sys"    
      names(clist[[i]])[5]<- "Year"
      names(clist[[i]])[6]<- "Val"
      cat(paste("Running the impactparameter ",datasys[i],  " it's done\n", sep = "" ))
      
}
datasys<- do.call(rbind, clist)
saveRDS(datasys, file = paste(rdsFiles, "datasys.rds",sep = ""))
rm(clist,datasys)

# Datos categorias totales-------------
datatotal<- c("TAreaXAgg", "TYldXAgg") # agregado TYldXAgg, "QSupXAgg"
clist<-list()
for( i in 1:length(datatotal)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",datatotal[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- datatotal[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",datatotal[i],  " it's done\n", sep = "" ))
      
}
datatotal<- do.call(rbind, clist)
saveRDS(datatotal, file = paste(rdsFiles, "datatotal.rds",sep = ""))
rm(clist,datatotal)


# Datos categorias agregados---------- 
dataagg<- c("QINTXAgg","QBFXAgg","QFXAgg", "QLXAgg","QOTHRXAgg" )
clist<-list()
for( i in 1:length(dataagg)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",dataagg[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- dataagg[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",dataagg[i],  " it's done\n", sep = "" ))
      
}
dataagg<- do.call(rbind, clist)
saveRDS(dataagg, file = paste(rdsFiles, "dataagg.rds",sep = ""))
rm(clist,dataagg)

# datos categorias animales--------
dataanimal<- c("AnmlNumXAgg" , "AnmlYldXAgg")
clist<-list()
for( i in 1:length(dataanimal)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",dataanimal[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- dataanimal[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",dataanimal[i],  " it's done\n", sep = "" ))
      
}
dataanimal<- do.call(rbind, clist)
saveRDS(dataanimal, file = paste(rdsFiles, "dataanimal.rds",sep = ""))
rm(clist,dataanimal)

# data net trade & food -------
TradeFood<- c("QNXAgg", "FoodAvailXAgg","PerCapKCalCXAgg", "QDXAgg","QSupXAgg")#"TYldXAgg"
clist<-list()
#i=3
for( i in 1:length(TradeFood)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",TradeFood[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- TradeFood[i]
      names(clist[[i]])[1]<- "Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<- "Regions"
      names(clist[[i]])[4]<- "Year"    
      names(clist[[i]])[5]<- "Val"
      cat(paste("Running the impactparameter ",TradeFood[i],  " it's done\n", sep = "" ))
      
}
TradeFood<- do.call(rbind, clist)
saveRDS(TradeFood, file = paste(rdsFiles, "TradeFood.rds",sep = ""))
rm(clist,TradeFood)



# data socieconomic-------
EcoFood<- c("PopulationAtRiskXagg","PopXAgg","TotalMalnourishedXagg")
clist<-list()
for( i in 1:length(EcoFood)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",EcoFood[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- EcoFood[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Regions"
      names(clist[[i]])[3]<-"Year"    
      names(clist[[i]])[4]<-"Val"
      cat(paste("Running the impactparameter ",EcoFood[i],  " it's done\n", sep = "" ))
}
EcoFood<- do.call(rbind, clist)
saveRDS(EcoFood, file = paste(rdsFiles, "EcoFood.rds",sep = ""))
rm(clist,EcoFood)


# data socieconomic2-------
EcoFood2<- c("GDPXAgg","pcGDPXAgg")
clist<-list()
for( i in 1:length(EcoFood2)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",EcoFood2[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- EcoFood2[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Regions"
      names(clist[[i]])[3]<-"Year"    
      names(clist[[i]])[4]<-"Val"
      cat(paste("Running the impactparameter ",EcoFood2[i],  " it's done\n", sep = "" ))
}
EcoFood2<- do.call(rbind, clist)
saveRDS(EcoFood2, file = paste(rdsFiles, "EcoFood2.rds",sep = ""))
rm(clist,EcoFood2)


# data socieconomic3-------
EcoFood3<- c("ShareAtRiskXagg")
clist<-list()
for( i in 1:length(EcoFood3)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",EcoFood3[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- EcoFood3[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Regions"
      names(clist[[i]])[3]<-"Year"    
      names(clist[[i]])[4]<-"Val"
      cat(paste("Running the impactparameter ",EcoFood3[i],  " it's done\n", sep = "" ))
}
EcoFood3<- do.call(rbind, clist)
saveRDS(EcoFood3, file = paste(rdsFiles, "EcoFood3.rds",sep = ""))
rm(clist,EcoFood3)


#Data parameters Water-------
green<- c("")
clist<-list()

for( i in 1:length(green)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",green[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- green[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<-"Commodity"
      names(clist[[i]])[3]<-"Regions"    
      names(clist[[i]])[4]<-"Sys"
      names(clist[[i]])[5]<-"Year"
      names(clist[[i]])[6]<-"Val"
      cat(paste("Running the impactparameter ",green[i],  " it's done\n", sep = "" ))
}
green<- do.call(rbind, clist)
saveRDS(green, file = paste(rdsFiles, "green.rds",sep = ""))
rm(clist,green)


#Data parameter shock climatico-----------
Blue<- c("BlueWatXAgg")
clist<-list()

for( i in 1:length(Blue)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",Blue[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- Blue[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<-"Commodity"
      names(clist[[i]])[3]<-"Regions"    
      names(clist[[i]])[4]<-"Year"
      names(clist[[i]])[5]<-"Val"
      cat(paste("Running the impactparameter ",Blue[i],  " it's done\n", sep = "" ))
}
Blue<- do.call(rbind, clist)
saveRDS(Blue, file = paste(rdsFiles, "Blue.rds",sep = ""))
rm(clist,Blue)

#Data parameter shock climatico--------------
shock<- c("YldCliShkXAgg")
clist<-list()
for( i in 1:length(shock)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",shock[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- shock[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<-"Regions" 
      names(clist[[i]])[4]<-"Sys"
      names(clist[[i]])[5]<-"Year"
      names(clist[[i]])[6]<-"Val"
      cat(paste("Running the impactparameter ",shock[i],  " it's done\n", sep = "" ))
}
shock<- do.call(rbind, clist)
saveRDS(shock, file = paste(rdsFiles, "shock.rds",sep = ""))
rm(clist,shock)

# Precios --------------
precios<- c("PPXAgg","PCXAgg")
clist<-list()

for( i in 1:length(precios)){
      #load files
      clist[i]<- list.files(path = grp,pattern= paste("^",precios[i], sep=""),full.names = T)
      clist[i]<- lapply(clist[i],read.csv,header=F)
      clist[[i]]$parameter<- precios[i]
      names(clist[[i]])[1]<-"Scenarios"
      names(clist[[i]])[2]<- "Commodity"
      names(clist[[i]])[3]<-"Regions" 
      names(clist[[i]])[4]<-"Year"
      names(clist[[i]])[5]<-"Val"
      cat(paste("Running the impactparameter ",precios[i],  " it's done\n", sep = "" ))
}
precios<- do.call(rbind, clist)
saveRDS(precios, file = paste(rdsFiles, "precios.rds",sep = ""))
rm(clist,precios)



##########################################################################################################################%
############################# PARTE 2. Codigo para realizar graficas,  EAP Carlos Edo.####################################%
##########################################################################################################################%

g=gc;rm(list = ls())


### Carlos Eduardo Gonzalez R. 
# librerias------------
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse)) 
suppressMessages(library(modelr)) 
suppressMessages(library(purrr)) 
suppressMessages(library(broom)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(corrplot)) 
suppressMessages(library(FactoMineR)) 
suppressMessages(library(factoextra)) 
suppressMessages(library(cluster)) 
suppressMessages(library(RCurl)) 
suppressMessages(library(ggthemes)) 
suppressMessages(library(tidyquant))
suppressMessages(library(devtools))
suppressMessages(library(mvoutlier))
suppressMessages(library(R.utils))
suppressMessages(library(RColorBrewer))



# Big Regions
r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA")
nocc<- c("REF_NoCC","NoCC_IX+WUE","NoCC_IX","NoCC_COMP","NoCC_ISW")

# Scenarios 
sce_ref<- c("REF_HGEM", "REF_IPSL", "REF_NoCC")
sce_wat<- c("IX+WUE","IX", "ISW","REF_IPSL" )
sce_prod<- c("HGEM_COMP","HGEM_ISW","HGEM_IX","HGEM_IX+WUE" ,"HIGH","HIGH+NARS","HIGH+RE","IPSL_ISW","IPSL_IX",
             "IPSL_IX+WUE", "ISPL_COMP","MED","REF_IPSL","REF_HGEM")
sceBase<- c("REF_IPSL","REF_HGEM", "REF_NoCC")
sceCC<- c("REF_IPSL","REF_HGEM")

#  All Countries.
eap<- c("EAP", "EAP-Cambodia", "EAP-Indonesia","EAP-Malaysia","EAP-Philippines","EAP-Thailand","EAP-Vietnam", "EAP-Laos", "EAP-Myanmar")

# filter crops
nocrops<- c("SGC","FOR","AOT","AMT","CER","F&V","R&T","COT","PUL", "OLS","SGC", "MLS", "OIL")
nograph<- c("AIIA","All","AllC","AllA")
no_crops<- c(nocrops,nograph)
nocropsRe<- c("AOT" ,"AMT", "CER", "F&V", "R&T", "COT", "PUL", "SGR", "MLS", "OIL","OLS")

# paleta de color
color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab') 

abbreviate("percentage") 
options(warn = -1); options(scipen = 999) 
options(digits=3) 



# cargando datos para ASEAN
cfiles<-list.files(path = rdsFiles, pattern = "Blue.rds|dataagg.rds|datatotal.rds|precios.rds|TradeFood.rds|green.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles



############### Supply&Demand--------------
cdata<- cfiles
cdata[[5]]$Scenarios<-  gsub("'",'', cdata[[5]]$Scenarios)
s<-unique(cdata[[5]]$Scenarios)
cdata[[5]]$Commodity<-  gsub("'", '', cdata[[5]]$Commodity)
cdata[[5]]$Regions<-  gsub("'", '', cdata[[5]]$Regions)
cdata[[5]]$Year<-  gsub("'",'', cdata[[5]]$Year)

cdata[[5]]$Scenarios<- as.character(cdata[[5]]$Scenarios)
cdata[[5]]$Commodity<- as.character(cdata[[5]]$Commodity)
cdata[[5]]$Regions<- as.character(cdata[[5]]$Regions) 
cdata[[5]]$Year<- as.numeric(cdata[[5]]$Year)

# ajuste de los scenarios
cdata[[5]]$Scenarios<- plyr::revalue(cdata[[5]]$Scenarios, c("SSP2-HGEM2"="REF_HGEM",
                                                             "SSP2-HGEM-HiNARS2"="HIGH+NARS",
                                                             "SSP2-HGEM-HiREFF2"="HIGH+RE",
                                                             "SSP2-HGEM-HiYld2"="HIGH",
                                                             "SSP2-HGEM-IRREXP2"="HGEM_IX",
                                                             "SSP2-HGEM-IRREXP-WUE2"="HGEM_IX+WUE",
                                                             "SSP2-HGEM-LoYld2"="MED",
                                                             "SSP2-HGEM-MMEFF2"="RMM",
                                                             "SSP2-HGEM-Pangloss2"="HGEM_COMP",
                                                             "SSP2-HGEM-RegYld2"="REGION",
                                                             "SSP2-HGEM-SWHC2"="HGEM_ISW",
                                                             "SSP2-IPSL2"="REF_IPSL",
                                                             "SSP2-IPSL-IRREXP2"="IPSL_IX",
                                                             "SSP2-IPSL-IRREXP-WUE2"="IPSL_IX+WUE",
                                                             "SSP2-IPSL-Pangloss2"="ISPL_COMP",
                                                             "SSP2-IPSL-SWHC2"="IPSL_ISW",
                                                             "SSP2-NoCC"="REF_NoCC",
                                                             "SSP2-NoCC-IRREXP2"="NoCC_IX",
                                                             "SSP2-NoCC-IRREXP-WUE2"="NoCC_IX+WUE",
                                                             "SSP2-NoCC-Pangloss2"="NoCC_COMP",
                                                             "SSP2-NoCC-SWHC2"="NoCC_ISW"))



# subset all crops
allcrops<- filter(cdata[[5]], Commodity=="All")

# filter cultivos 
cdata[[5]]<- filter(cdata[[5]], Commodity %in% nocropsRe) 
# Select regiones 
cdata[[5]]<-  cdata[[5]][grep(pattern= "EAP",x =  cdata[[5]]$Regions, ignore.case = T),]
# cdata[[1]]<- filter(cdata[[1]], Scenarios %in% s)
cdata[[5]]<- filter(cdata[[5]], Regions %in% eap) 

rario_para<- c("QSupXAgg","QDXAgg")
cdata[[5]]<- filter(cdata[[5]], parameter %in% rario_para)
cdata[[5]]<-  cdata[[5]]%>% spread(parameter, Val)
cdata[[5]]$ratio<- (cdata[[5]]$QSupXAgg/cdata[[5]]$QDXAgg)

# vector con los parametros
parameter<-unique(cdata[[5]]$parameter)

# firts example
pro<- filter(cdata[[5]], Scenarios %in% sce_prod)
pro<- pro %>% gather(variable, Val, 5: ncol(pro)) %>% filter(., variable=="ratio") 
pro$Val[is.na(pro$Val)==TRUE]<- 0
### Eliminar ceros
pro<-pro[!(pro$Val==0),]
pro<- pro[c("Scenarios", "Commodity", "Regions","variable","Year","Val" )]

pro$Commodity<- plyr::revalue(pro$Commodity, c("AMT"="Meat",
                                               "AOT"="Dairy",
                                               "CER"="Cereals",
                                               "COT"="Other Crops",
                                               "F&V"="Fruits and Vegetables",
                                               "FOR"="Other Crops",
                                               "MLS"="Oilmeals",
                                               "OIL"="Processed Oils",
                                               "OLS"="Traded Oilseeds",
                                               "PUL"="Pulses",
                                               "R&T"="Root and Tubers",
                                               "SGC"="Sugar Crops*",
                                               "SGR"="Sugar Crops**"))


pro<- pro %>% spread(Year, Val)
n1<-match("2010",names(pro))

datmin<- filter(pro,!Scenarios %in% sceCC)
datmin<- aggregate(datmin[,n1:ncol(datmin)],by=list(datmin$Regions,datmin$Commodity),FUN=min)
datmin<- datmin %>% gather("time","datmin", 3:ncol(datmin))
names(datmin)<-c("Regions","Commodity","time","datmin")

## valor medio solo para NoCC, escenarios
datmed<- filter(pro, Scenarios %in% sceCC)
datmed<- aggregate(datmed[,n1:ncol(datmed)],by=list(datmed$Regions,datmed$Commodity),FUN=median)
datmed<- datmed %>% gather("time","datmed", 3:ncol(datmed))
names(datmed)<-c("Regions","Commodity","time","datmed")

datmax<- filter(pro, !Scenarios %in% sceCC)
datmax<-aggregate(datmax[,n1:ncol(datmax)],by=list(datmax$Regions,datmax$Commodity),FUN=max)
datmax<- datmax %>% gather("time","datmax", 3:ncol(datmax))
names(datmax)<-c("Regions","Commodity","time","datmax")

#join data
extremos<- merge(datmin,datmax)
datost<- merge(extremos,datmed)
datost$time<- as.numeric(datost$time)


#grafico1
wk_dir <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/ASEAN" 

tiff(filename=paste(wk_dir, "/","RAtiosTrends.tiff", sep = ""), 
     width = 14, height = 10, units = 'in', res = 100)

pic<- ggplot(data=datost,aes(time,datmed,group=Commodity,color=Commodity)) + 
      facet_wrap(~Regions,nrow = 4,scales = "free")+
      geom_line(linetype="dashed",size=1)+ 
      geom_ribbon(aes(ymin=datmin,ymax=datmax,fill=Commodity,colour=Commodity,linetype=NA),alpha=0.1) +
      labs(y="Ratio",x="Year", title= "Ratio")+ 
      coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + geom_hline(yintercept = 1,size = 1)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
      theme(axis.text.y = element_text(hjust = 1, size = 8))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 8))+
      # theme(legend.position="bottom")+ 
      scale_x_continuous( breaks=seq(2010,2050,5))

plot(pic)
dev.off() 


tiff(filename=paste(wk_dir, "/","EAP_RAtiosTrends.tiff", sep = ""), 
     width = 14, height = 10, units = 'in', res = 100)

pic<- ggplot(data=datost[which(datost$Regions=="EAP"),],aes(time,datmed,group=Commodity,color=Commodity)) + 
      facet_wrap(~Commodity,nrow = 4,scales = "free")+
      geom_line(linetype="dashed",size=1)+ 
      geom_ribbon(aes(ymin=datmin,ymax=datmax,fill=Commodity,colour=Commodity,linetype=NA),alpha=0.1) +
      labs(y="Ratio",x="Year", title= "Ratio")+ 
      coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + geom_hline(yintercept = 1,size = 1)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
      theme(axis.text.y = element_text(hjust = 1, size = 8))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 8))+
      # theme(legend.position="bottom")+ 
      scale_x_continuous( breaks=seq(2010,2050,5))

plot(pic)
dev.off() 

write.csv(pro,paste(wk_dir, "/","ratios.csv", sep = ""))

# grafico 2

# Select regiones 
allcrops<-  allcrops[grep(pattern= "EAP",x =  allcrops$Regions, ignore.case = T),]
allcrops<- filter(allcrops, Regions %in% eap) 
allcrops<- filter(allcrops, parameter %in% rario_para)
allcrops<-  allcrops%>% spread(Year, Val) 
allcrops$Change<- ((allcrops$`2050`-allcrops$`2020`)/allcrops$`2020`)*100
allcrops<- allcrops[c("Scenarios","Commodity", "Regions","parameter","Change")]


sceBase<-c("REF_IPSL","REF_NoCC","REF_HGEM")
allcrops<- filter(allcrops,Scenarios %in% sceBase ) 
allcrops<- filter(allcrops, parameter %in% rario_para)


# ajuste de los scenarios
allcrops$parameter<- plyr::revalue(allcrops$parameter, c("QDXAgg"="Demand","QSupXAgg"="Supply"))
allcrops$Regions<-  gsub("EAP-", '', allcrops$Regions)
allcrops$Regions <- factor(allcrops$Regions, levels = c("EAP","Cambodia","Indonesia", "Laos","Malaysia", "Myanmar","Philippines", "Thailand","Vietnam"))



tiff(filename=paste(wk_dir, "/","ChangeSuppl&Demand1.tiff", sep = ""), 
     width = 8, height = 8, units = 'in', res = 100)

b1 <- ggplot(allcrops, aes(interaction(Regions, parameter),Change, fill=Scenarios)) + 
      theme_bw() + geom_bar(stat="identity", position=position_dodge(width=.60)) +
      scale_fill_brewer(palette = "Dark2")+ 
      labs(x="Parameters & countries\n by All crops",y="Percentage Change\n from 2020 to 2050")+
      coord_flip()+ 
      theme(axis.text.y=element_text(size=1))+ theme(axis.text.y=element_text(size=12))+ 
      theme(axis.title.x=element_text(size=12, face='bold'))+ 
      theme(plot.title=element_text(size=15, face = 'bold'))
plot(b1)
dev.off() 

write.csv(allcrops, paste(wk_dir, "/","AllCommoditiesChange.csv", sep=""))

tiff(filename=paste(wk_dir, "/","ChangeSuppl&Demand2.tiff", sep = ""), 
     width = 8, height = 8, units = 'in', res = 100)

b2 <- ggplot(allcrops,aes(interaction(Regions, parameter),Change, fill=Scenarios)) + 
      theme_bw() + geom_bar(stat="identity", position=position_dodge(width=.60)) + facet_wrap(~parameter)+
      scale_fill_brewer(palette = "Dark2")+ 
      labs(x="Parameters & countries\n by All crops",y="Percentage Change\n from 2020 to 2050")+
      coord_flip()+
      theme(axis.text.y=element_text(size=1))+ theme(axis.text.y=element_text(size=12))+ 
      theme(axis.title.x=element_text(size=12, face='bold'))+ 
      theme(plot.title=element_text(size=15, face = 'bold'))
plot(b2)
dev.off() 


############### BlueWater--------------
cdata<- cfiles
cdata[[4]]$Scenarios<-  gsub("'",'', cdata[[4]]$Scenarios)
s<-unique(cdata[[4]]$Scenarios)
cdata[[4]]$Commodity<-  gsub("'", '', cdata[[4]]$Commodity)
cdata[[4]]$Regions<-  gsub("'", '', cdata[[4]]$Regions)
cdata[[4]]$Year<-  gsub("'",'', cdata[[4]]$Year)

cdata[[4]]$Scenarios<- as.character(cdata[[4]]$Scenarios)
cdata[[4]]$Commodity<- as.character(cdata[[4]]$Commodity)
cdata[[4]]$Regions<- as.character(cdata[[4]]$Regions) 
cdata[[4]]$Year<- as.numeric(cdata[[4]]$Year)

# ajuste de los scenarios
cdata[[1]]$Scenarios<- plyr::revalue(cdata[[1]]$Scenarios, c("SSP2-HGEM2"="REF_HGEM",
                                                             "SSP2-HGEM-HiNARS2"="HIGH+NARS",
                                                             "SSP2-HGEM-HiREFF2"="HIGH+RE",
                                                             "SSP2-HGEM-HiYld2"="HIGH",
                                                             "SSP2-HGEM-IRREXP2"="HGEM_IX",
                                                             "SSP2-HGEM-IRREXP-WUE2"="HGEM_IX+WUE",
                                                             "SSP2-HGEM-LoYld2"="MED",
                                                             "SSP2-HGEM-MMEFF2"="RMM",
                                                             "SSP2-HGEM-Pangloss2"="HGEM_COMP",
                                                             "SSP2-HGEM-RegYld2"="REGION",
                                                             "SSP2-HGEM-SWHC2"="HGEM_ISW",
                                                             "SSP2-IPSL2"="REF_IPSL",
                                                             "SSP2-IPSL-IRREXP2"="IPSL_IX",
                                                             "SSP2-IPSL-IRREXP-WUE2"="IPSL_IX+WUE",
                                                             "SSP2-IPSL-Pangloss2"="ISPL_COMP",
                                                             "SSP2-IPSL-SWHC2"="IPSL_ISW",
                                                             "SSP2-NoCC"="REF_NoCC",
                                                             "SSP2-NoCC-IRREXP2"="NoCC_IX",
                                                             "SSP2-NoCC-IRREXP-WUE2"="NoCC_IX+WUE",
                                                             "SSP2-NoCC-Pangloss2"="NoCC_COMP",
                                                             "SSP2-NoCC-SWHC2"="NoCC_ISW"))



# subset all crops
allcrops<- filter(cdata[[1]], Commodity=="All")

# filter cultivos 
cdata[[1]]<- filter(cdata[[1]], Commodity %in% nocropsRe) 
# Select regiones 
cdata[[1]]<-  cdata[[1]][grep(pattern= "EAP",x =  cdata[[1]]$Regions, ignore.case = T),]
# cdata[[1]]<- filter(cdata[[1]], Scenarios %in% s)
cdata[[1]]<- filter(cdata[[1]], Regions %in% eap) %>% spread(., Year, Val) %>% filter(., Scenarios %in% sceBase)
cdata[[1]]$Change<- ((cdata[[1]]$`2050`-cdata[[1]]$`2020`)/cdata[[1]]$`2020`)*100

cdata[[1]]<- cdata[[1]][c("Scenarios","Commodity","Regions","parameter","Change")]

cdata[[1]]$Change[is.na(cdata[[1]]$Change)==TRUE]<- 0
### Eliminar ceros
cdata[[1]]<-cdata[[1]][!(cdata[[1]]$Change==0),]


cdata[[1]]$Commodity<- plyr::revalue(cdata[[1]]$Commodity, c("AMT"="Meat",
                                                             "AOT"="Dairy",
                                                             "CER"="Cereals",
                                                             "COT"="Other Crops",
                                                             "F&V"="Fruits&Vegetables",
                                                             "FOR"="Other Crops",
                                                             "MLS"="Oilmeals",
                                                             "OIL"="Processed Oils",
                                                             "OLS"="Traded Oilseeds",
                                                             "PUL"="Pulses",
                                                             "R&T"="Root&Tubers",
                                                             "SGC"="Sugar Crops*",
                                                             "SGR"="Sugar Crops**"))


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
temp <- FindOutliers(cdata[[1]]$Change)
cfOut<- cdata[[1]][temp,]
cfilesNEt<- cdata[[1]][-temp,]

cfilesNEt$Regions<-  gsub("EAP-", '', cfilesNEt$Regions)




# definiendo limites para hacer graficos
breaks <- seq(from=min(range(cfilesNEt$Change)), to=max(range(cfilesNEt$Change)), length.out=100)
lit<- tail(breaks, n=1)


png(filename=paste(wk_dir, "/", "Bluewater","_HeapMap.png", sep = ""), 
    width = 10, height = 8, units = 'in', res = 100)

w1<- ggplot(data =cfilesNEt, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Change), colour = "white")+  facet_wrap(~Commodity,nrow = nfilas,ncol=7,drop = T)+
      labs(x=NULL, y=NULL,title= "Blue Water", fill="Percentage\nChange(%)" ) +
      scale_fill_gradientn(colours = hm.palette(100),limits=c(-lit,lit))+   
      coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "", title="Blue water (irrigated water applied to crops)")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9,face = "bold.italic"))+
      theme(strip.text=element_text(size=10))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 

plot(w1)
dev.off() 

############### GreenWater--------
cdata<- cfiles
cdata[[4]]$Scenarios<-  gsub("'",'', cdata[[4]]$Scenarios)
cdata[[4]]$Commodity<-  gsub("'", '', cdata[[4]]$Commodity)
cdata[[4]]$Regions<-  gsub("'", '', cdata[[4]]$Regions)
cdata[[4]]$Year<-  gsub("'",'', cdata[[4]]$Year)
cdata[[4]]$Sys<-  gsub("'",'', cdata[[4]]$Sys)

cdata[[4]]$Scenarios<- as.character(cdata[[4]]$Scenarios)
cdata[[4]]$Commodity<- as.character(cdata[[4]]$Commodity)
cdata[[4]]$Regions<- as.character(cdata[[4]]$Regions) 
cdata[[4]]$Year<- as.numeric(cdata[[4]]$Year)
cdata[[4]]<- filter(cdata[[4]], Commodity %in% nocropsRe )

# Select regiones 
cdata[[4]]<-  cdata[[4]][grep(pattern= "EAP",x =  cdata[[4]]$Regions, ignore.case = T),]
# x<- filter(x, Scenarios %in% s)
cdata[[4]]<- filter(cdata[[4]], Regions %in% eap) 
cdata[[4]]<-  cdata[[4]]%>% spread(Year, Val) 
cdata[[4]]<-  filter(cdata[[4]],Sys!="gir")

# ajuste de los scenarios
cdata[[4]]$Scenarios<- plyr::revalue(cdata[[4]]$Scenarios, c("SSP2-HGEM2"="REF_HGEM",
                                                             "SSP2-HGEM-HiNARS2"="HIGH+NARS",
                                                             "SSP2-HGEM-HiREFF2"="HIGH+RE",
                                                             "SSP2-HGEM-HiYld2"="HIGH",
                                                             "SSP2-HGEM-IRREXP2"="HGEM_IX",
                                                             "SSP2-HGEM-IRREXP-WUE2"="HGEM_IX+WUE",
                                                             "SSP2-HGEM-LoYld2"="MED",
                                                             "SSP2-HGEM-MMEFF2"="RMM",
                                                             "SSP2-HGEM-Pangloss2"="HGEM_COMP",
                                                             "SSP2-HGEM-RegYld2"="REGION",
                                                             "SSP2-HGEM-SWHC2"="HGEM_ISW",
                                                             "SSP2-IPSL2"="REF_IPSL",
                                                             "SSP2-IPSL-IRREXP2"="IPSL_IX",
                                                             "SSP2-IPSL-IRREXP-WUE2"="IPSL_IX+WUE",
                                                             "SSP2-IPSL-Pangloss2"="ISPL_COMP",
                                                             "SSP2-IPSL-SWHC2"="IPSL_ISW",
                                                             "SSP2-NoCC"="REF_NoCC",
                                                             "SSP2-NoCC-IRREXP2"="NoCC_IX",
                                                             "SSP2-NoCC-IRREXP-WUE2"="NoCC_IX+WUE",
                                                             "SSP2-NoCC-Pangloss2"="NoCC_COMP",
                                                             "SSP2-NoCC-SWHC2"="NoCC_ISW"))



# Select regiones 
cdata[[4]]$Commodity<- plyr::revalue(cdata[[4]]$Commodity, c("AMT"="Meat",
                                                             "AOT"="Dairy",
                                                             "CER"="Cereals",
                                                             "COT"="Other Crops",
                                                             "F&V"="Fruits&Vegetables",
                                                             "FOR"="Other Crops",
                                                             "MLS"="Oilmeals",
                                                             "OIL"="Processed Oils",
                                                             "OLS"="Traded Oilseeds",
                                                             "PUL"="Pulses",
                                                             "R&T"="Root&Tubers",
                                                             "SGC"="Sugar Crops*",
                                                             "SGR"="Sugar Crops**"))

cdata[[4]]<-  cdata[[4]][grep(pattern= "EAP",x =  cdata[[4]]$Regions, ignore.case = T),]
cdata[[4]]<- filter(cdata[[4]], Scenarios %in% sceBase)
cdata[[4]]$Change<- ((cdata[[4]]$`2050`-cdata[[4]]$`2020`)/cdata[[4]]$`2020`)*100
cdata[[4]]$Sys<- plyr::revalue(cdata[[4]]$Sys,c("air"="Irrigated", "arf"="Rainfed"))
cdata[[4]]<- cdata[[4]][c("Scenarios","Commodity","Regions","Sys","parameter","Change")]

# cdata[[4]]$Change[is.na(cdata[[4]]$Change)==TRUE]<- 0
### Eliminar ceros
# cdata[[4]]<-cdata[[1]][!(cdata[[4]]$Change==0),]




cfilesNEt<-cdata[[4]]
cfilesNEt$Regions<-  gsub("EAP-", '', cfilesNEt$Regions)




# definiendo limites para hacer graficos
breaks <- seq(from=min(range(cfilesNEt$Change)), to=max(range(cfilesNEt$Change)), length.out=100)
lit<- tail(breaks, n=1)


png(filename=paste(wk_dir, "/", "GreenWater","_HeapMap.png", sep = ""), 
    width = 12, height = 10, units = 'in', res = 100)

w2<- ggplot(data =cfilesNEt, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Change), colour = "white")+  facet_grid(Sys~Commodity,drop = T)+
      labs(x=NULL, y=NULL,title= "", fill="Percentage\nChange(%)" ) +
      scale_fill_gradientn(colours = hm.palette(100),limits=c(-lit,lit))+   
      coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "", title="Green water (precipitation on crop area)")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9,face = "bold.italic"))+
      theme(strip.text=element_text(size=10))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 

plot(w2)
dev.off() 


water <- arrangeGrob(w1, w2, nrow=2) 
ggsave(file=paste(wk_dir, "/", "Water","_HeapMap.png",sep = ""), water, width=16, height=16, units='in') 

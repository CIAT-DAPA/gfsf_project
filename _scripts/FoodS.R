### Paper SAN

### Directorio de bases de datos
setwd("C:/Users/User/Dropbox/SAN")

## decimales
options(warn = -1)
options(scipen = 999)

### Librerias
library(ggplot2)
library(dplyr)
library(tidyr)

### Crear archivo madre
cfiles <- read.csv("lamp_native_20180613-163318.csv")
### Crear un archivo de respaldo para trabajar llamado fs (food security)
fs <- cfiles

### Revisar tipo de variables
# length(fs) ## N° de columnas
# ls(fs) ## Nombre columnas
# str(fs) ## Estructura del objeto

### Convertir variables factor a caracter
fs$MODEL<-as.character(fs$MODEL)
fs$SCENARIO<- as.character(fs$SCENARIO)
fs$REGION<- as.character(fs$REGION)
fs$VARIABLE<- as.character(fs$VARIABLE)
fs$UNIT<- as.character(fs$UNIT)
# str(fs) ## revisar

### Crar una variable con solo la columna VARIABLE
var<-unique(fs$VARIABLE)

### Corregir y unificar nombres
fs$REGION<- plyr::revalue(fs$REGION, c("Colombia"="COL", "clm"="COL",
                                       "Brazil"="BR", "BRA"="BR","bra"="BR",
                                       "Central America and Caribbean"="CAC", "cac"="CAC",
                                       "Argentina"="ARG",
                                       "Mexico"="MEX", "mex"="MEX",
                                       "South America_Northern"="SUR_AN",
                                       "South America_Southern"="SUR_AS",
                                       "XLM"="RestLAC_ADAGE",
                                       "ola"="OtherLAC_PHOX",
                                       "ven"="VEN"))

### Crear una variable con solo la variable REGION
reg<- unique(fs$REGION) 
# length(reg)

## Crear un objeto con los países de LAC
lac<- c("ARG", "BR", "CAC", "COL", "MEX", "SUR_AN", "SUR_AS",
        "OtherLAC_PHOX","RestLAC_ADAGE", "VEN")

### Eliminar países innecesarios
fsa<- fs %>% dplyr::filter(REGION%in%lac)
# unique(fsa$REGION) ## Verificar los países

### Eliminar columnas innecesarias o seleccionar variables
# fsa$X1990<-NULL ## Elimina columnas
# fsa$X2004<-NULL
# fsa$X2012<-NULL

fsa<- fsa %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables

### Cambiar nombre a variables de seg. alimentaria
# unique(fsa$VARIABLE) ## Listar nombres
fsa$VARIABLE<-plyr::revalue(fsa$VARIABLE, c("Food Security|Dietary Adequacy|Share US 2010 cal/cap/day"="Dietary Adequacy",
                                            "Food Security|Food Expenditure Share Income"="Food Expenditure Share Income",
                                            "Food Security|Food Import Share Merchandise Export"="Food Import Share Merch Export",
                                            "Food Security|Food Production Value|Agricultural Sector"="Food Production Value",
                                            "Food Security|Grain Import Ratio"="Grain Import Ratio",
                                            "Food Security|Protein Animal"="Protein Animal",
                                            "Food Security|Protein Total"="Protein Total",
                                            "Food Security|Share Staple Calories"="Share Staple Calories",
                                            "GDP|PPP"="GDP PPP"
))

### Crear objeto con variables de seg. alim.
foods<- c("Dietary Adequacy","Food Expenditure Share Income",
          "Food Import Share Merch Export","Food Production Value",
          "Grain Import Ratio","Protein Animal","Protein Total",
          "Share Staple Calories","GDP PPP")

### Eliminar variables de seguridad alimentaria
food<- fsa %>% dplyr::filter(VARIABLE%in%foods)

### Cambiar nombre de variables
names(food)<-c("Model","Scenario","Region","Variable","2005","2010","2015",
               "2020","2025","2030","2035","2040","2045","2050")

### Subsets por variable
#Variables con unidad pct
diet_adeq<- food[grep("Dietary Adequacy", food$Variable),]
food_exp_share<- food[grep("Food Expenditure Share Income", food$Variable),]
share_staples<- food[grep("Share Staple Calories", food$Variable),]
grain_imp_ratio<- food[grep("Grain Import Ratio", food$Variable),]
food_imp_merch_exp<- food[grep("Food Import Share Merch Export", food$Variable),]
# Variables con unidad g_pcap_pday
protein_total<- food[grep("Protein Total", food$Variable),]
protein_animal<- food[grep("Protein Animal", food$Variable),]
# Variables con unidad mil_2005USD
food_prod_value<- food[grep("Food Production Value", food$Variable),]
# Variables con unidad thous_2005USD
gdp_cap_ppp<- food[grep("GDP PPP", food$Variable),]

### Homogeneizar valores para diet en GCAM
# Separar modelo GCAM
#diet_GCAM<- diet_adeq %>%filter(Model=="GCAM_LAMP")
#diet_GCAM$X2010<- (diet_GCAM$X2010)*100
#diet_others<- food %>%filter(!Model=="GCAM_LAMP")

######### Exploración gráficos combinación política y CFE ############
### No pol + CFE ###
# ccsm_4p5_cfe_nopol, ccsm_8p5_cfe_nopol, gfdl_4p5_cfe_nopol,
# gfdl_8p5_cfe_nopol, hadgem_4p5_cfe_nopol, hadgem_8p5_cfe_nopol

### Crear objeto con escenario 4p5 no pol y CFE
sce45<-c("ccsm_4p5_cfe_nopol", "gfdl_4p5_cfe_nopol",
         "hadgem_4p5_cfe_nopol")

### Crear objeto con escenario 8p5 no pol y CFE
sce85<-c("ccsm_8p5_cfe_nopol", "gfdl_8p5_cfe_nopol",
         "hadgem_8p5_cfe_nopol")

### Crear objetos con las variable de seg alim solo los sce 4p5
diet_adeq45<- diet_adeq %>% filter(.,Scenario %in% sce45)
food_exp_share45<- food_exp_share %>% filter(.,Scenario %in% sce45)
food_prod_value45<- food_prod_value %>% filter(.,Scenario %in% sce45)
grain_imp_ratio45<- grain_imp_ratio %>% filter(.,Scenario %in% sce45)
protein_total45<- protein_total %>% filter(.,Scenario %in% sce45)
share_staples45<- share_staples %>% filter(.,Scenario %in% sce45)
protein_animal45<- protein_animal %>% filter(.,Scenario %in% sce45)
gdp_cap_ppp45<- gdp_cap_ppp %>% filter(.,Scenario %in% sce45)
food_imp_merch_exp45<- food_imp_merch_exp %>% filter(.,Scenario %in% sce45)

### Calcular los datos mínimos, medianos y máximos
## food_exp_share45 ####
## Mínimos
datamin<-aggregate(food_exp_share45[,c(5:ncol(food_exp_share45))],
                   by=list(food_exp_share45$Model,
                           food_exp_share45$Region,
                           food_exp_share45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(food_exp_share45[,c(5:ncol(food_exp_share45))],
                   by=list(food_exp_share45$Model,
                           food_exp_share45$Region,
                           food_exp_share45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(food_exp_share45[,c(5:ncol(food_exp_share45))],
                   by=list(food_exp_share45$Model,
                           food_exp_share45$Region,
                           food_exp_share45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/food_exp_share45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("C:/Users/User/Dropbox/SAN/pic","FS_",pots[i],"_TrendModels.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="%",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
} 

## food_prod_value45 ####

## Mínimos
datamin<-aggregate(food_prod_value45[,c(5:ncol(food_prod_value45))],
                   by=list(food_prod_value45$Model,
                           food_prod_value45$Region,
                           food_prod_value45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(food_prod_value45[,c(5:ncol(food_prod_value45))],
                   by=list(food_prod_value45$Model,
                           food_prod_value45$Region,
                           food_prod_value45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(food_prod_value45[,c(5:ncol(food_prod_value45))],
                   by=list(food_prod_value45$Model,
                           food_prod_value45$Region,
                           food_prod_value45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/food_prod_value45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("C:/Users/User/Dropbox/SAN/pic","FS_",pots[i],"_TrendModels.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="mil_2005USD",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
} 

## grain_imp_ratio45 ####
## Mínimos
datamin<-aggregate(grain_imp_ratio45[,c(5:ncol(grain_imp_ratio45))],
                   by=list(grain_imp_ratio45$Model,
                           grain_imp_ratio45$Region,
                           grain_imp_ratio45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(grain_imp_ratio45[,c(5:ncol(grain_imp_ratio45))],
                   by=list(grain_imp_ratio45$Model,
                           grain_imp_ratio45$Region,
                           grain_imp_ratio45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(grain_imp_ratio45[,c(5:ncol(grain_imp_ratio45))],
                   by=list(grain_imp_ratio45$Model,
                           grain_imp_ratio45$Region,
                           grain_imp_ratio45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/grain_imp_ratio45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("./pic","FS_",pots[i],"_grain_imp.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="%",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
} 

## protein_total45 ####
## Mínimos
datamin<-aggregate(protein_total45[,c(5:ncol(protein_total45))],
                   by=list(protein_total45$Model,
                           protein_total45$Region,
                           protein_total45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(protein_total45[,c(5:ncol(protein_total45))],
                   by=list(protein_total45$Model,
                           protein_total45$Region,
                           protein_total45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(protein_total45[,c(5:ncol(protein_total45))],
                   by=list(protein_total45$Model,
                           protein_total45$Region,
                           protein_total45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/protein_total45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("./pic","FS_",pots[i],"_proteintot.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="g_pcap_pday",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
} 

## share_staples45 ####
## Mínimos
datamin<-aggregate(share_staples45[,c(5:ncol(share_staples45))],
                   by=list(share_staples45$Model,
                           share_staples45$Region,
                           share_staples45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(share_staples45[,c(5:ncol(share_staples45))],
                   by=list(share_staples45$Model,
                           share_staples45$Region,
                           share_staples45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(share_staples45[,c(5:ncol(share_staples45))],
                   by=list(share_staples45$Model,
                           share_staples45$Region,
                           share_staples45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/share_staples45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("./pic","FS_",pots[i],"_proteintot.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="g_pcap_pday",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
} 

## protein_animal45 ####
## Mínimos
datamin<-aggregate(protein_animal45[,c(5:ncol(protein_animal45))],
                   by=list(protein_animal45$Model,
                           protein_animal45$Region,
                           protein_animal45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(protein_animal45[,c(5:ncol(protein_animal45))],
                   by=list(protein_animal45$Model,
                           protein_animal45$Region,
                           protein_animal45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(protein_animal45[,c(5:ncol(protein_animal45))],
                   by=list(protein_animal45$Model,
                           protein_animal45$Region,
                           protein_animal45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/protein_animal45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("./pic","FS_",pots[i],"_proteintot.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="g_pcap_pday",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
} 

## gdp_cap_ppp45 ####
## Mínimos
datamin<-aggregate(gdp_cap_ppp45[,c(5:ncol(gdp_cap_ppp45))],
                   by=list(gdp_cap_ppp45$Model,
                           gdp_cap_ppp45$Region,
                           gdp_cap_ppp45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(gdp_cap_ppp45[,c(5:ncol(gdp_cap_ppp45))],
                   by=list(gdp_cap_ppp45$Model,
                           gdp_cap_ppp45$Region,
                           gdp_cap_ppp45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(gdp_cap_ppp45[,c(5:ncol(gdp_cap_ppp45))],
                   by=list(gdp_cap_ppp45$Model,
                           gdp_cap_ppp45$Region,
                           gdp_cap_ppp45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/gdp_cap_ppp45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("./pic","FS_",pots[i],"_proteintot.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="g_pcap_pday",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
}

## food_imp_merch_exp45 ####
## Mínimos
datamin<-aggregate(food_imp_merch_exp45[,c(5:ncol(food_imp_merch_exp45))],
                   by=list(food_imp_merch_exp45$Model,
                           food_imp_merch_exp45$Region,
                           food_imp_merch_exp45$Variable),
                   FUN=min,rm.na=F)

datamin<-datamin %>% gather("time","datamin", 4:ncol(datamin))
names(datamin)<-c("Model","Region","Variable","time","datamin")
datamin$time<- sub(pattern = "X",replacement = "", x = datamin$time)
datamin$time<- as.integer(datamin$time)
datamin$time<- as.numeric(datamin$time) 

## Medianas
datamed<-aggregate(food_imp_merch_exp45[,c(5:ncol(food_imp_merch_exp45))],
                   by=list(food_imp_merch_exp45$Model,
                           food_imp_merch_exp45$Region,
                           food_imp_merch_exp45$Variable),
                   FUN=median,rm.na=F)

datamed<-datamed %>% gather("time","datamed", 4:ncol(datamed))
names(datamed)<-c("Model","Region","Variable","time","datamed")
datamed$time<- sub(pattern = "X",replacement = "", x = datamed$time)
datamed$time<- as.integer(datamed$time)
datamed$time<- as.numeric(datamed$time) 

## Máximos
datamax<-aggregate(food_imp_merch_exp45[,c(5:ncol(food_imp_merch_exp45))],
                   by=list(food_imp_merch_exp45$Model,
                           food_imp_merch_exp45$Region,
                           food_imp_merch_exp45$Variable),
                   FUN=max,rm.na=F)

datamax<-datamax %>% gather("time","datamax", 4:ncol(datamax))
names(datamax)<-c("Model","Region","Variable","time","datamax")
datamax$time<- sub(pattern = "X",replacement = "", x = datamax$time)
datamax$time<- as.integer(datamax$time)
datamax$time<- as.numeric(datamax$time) 

extremos<- merge(datamin,datamax)
datost<- merge(extremos,datamed)

write.csv(datost,"./Results/food_imp_merch_exp45.csv")

pots<-unique(food$Region)
py<-NULL
i=1
for(i in 1:length(pots)){
  
  data<- datost %>% filter(Region==pots[i]) #%>%filter(.,Model=="GCAM_LAMP")
  
  tiff(filename=paste("./pic","FS_",pots[i],"_proteintot.tiff",sep=""), 
       width = 10, height = 7, units = 'in', res = 100)
  
  py[[i]]<-print(ggplot(data, aes(time,datamed,group=Variable,color=Variable)) + 
                   geom_line(linetype="dashed",size=1)+ facet_grid(~Model)+ 
                   geom_ribbon(aes(ymin=datamin,ymax=datamax,
                                   fill=Variable,colour=Variable,linetype=NA),
                               alpha=0.1) + 
                   labs(title=paste("Region", pots[i],sep = "") )+
                   labs(y="g_pcap_pday",x="Year")+
                   theme(legend.position="bottom")
  )
  
  dev.off()
  print(i)
}
### Paper SAN

### Directorio de bases de datos
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/Paper SAN")

## decimales
options(warn = -1)
options(scipen = 999)

### Librerias
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)

### Crear archivo madre
cfiles<- read.csv("lamp_native_20180926-212748.csv")
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
# Variables con unidad mil_2005USD
food_prod_value<- food[grep("Food Production Value", food$Variable),]
#Variables con unidad pct
diet_adeq<- food[grep("Dietary Adequacy", food$Variable),]
food_exp_share<- food[grep("Food Expenditure Share Income", food$Variable),]
share_staples<- food[grep("Share Staple Calories", food$Variable),]
grain_imp_ratio<- food[grep("Grain Import Ratio", food$Variable),]
food_imp_merch_exp<- food[grep("Food Import Share Merch Export", food$Variable),]
# Variables con unidad g_pcap_pday
protein_total<- food[grep("Protein Total", food$Variable),]
protein_animal<- food[grep("Protein Animal", food$Variable),]
# Variables con unidad thous_2005USD
gdp_cap_ppp<- food[grep("GDP PPP", food$Variable),]

### Homogeneizar valores ####
## diet_adeq
# Separar modelo GCAM
diet_GCAM<- diet_adeq %>%filter(Model=="GCAM_LAMP")
diet_others<- diet_adeq %>%filter(!Model=="GCAM_LAMP")
# Multiplicar por 100 para %
diet_GCAM$`2005`<- (diet_GCAM$`2005`)*100
diet_GCAM$`2010`<- (diet_GCAM$`2010`)*100
diet_GCAM$`2015`<- (diet_GCAM$`2015`)*100
diet_GCAM$`2020`<- (diet_GCAM$`2020`)*100
diet_GCAM$`2025`<- (diet_GCAM$`2025`)*100
diet_GCAM$`2030`<- (diet_GCAM$`2030`)*100
diet_GCAM$`2035`<- (diet_GCAM$`2035`)*100
diet_GCAM$`2040`<- (diet_GCAM$`2040`)*100
diet_GCAM$`2045`<- (diet_GCAM$`2045`)*100
diet_GCAM$`2050`<- (diet_GCAM$`2050`)*100
# Combinar bases
diet_adeq<- rbind(diet_GCAM, diet_others)

## diet_adeq
# Separar modelo MEG4C
diet_GCAM<- diet_adeq %>%filter(Model=="MEG4C")
diet_others<- diet_adeq %>%filter(!Model=="MEG4C")
# Multiplicar por 100 para %
diet_GCAM$`2010`<- (diet_GCAM$`2010`)*100
diet_GCAM$`2015`<- (diet_GCAM$`2015`)*100
diet_GCAM$`2020`<- (diet_GCAM$`2020`)*100
diet_GCAM$`2025`<- (diet_GCAM$`2025`)*100
diet_GCAM$`2030`<- (diet_GCAM$`2030`)*100
diet_GCAM$`2035`<- (diet_GCAM$`2035`)*100
diet_GCAM$`2040`<- (diet_GCAM$`2040`)*100
diet_GCAM$`2045`<- (diet_GCAM$`2045`)*100
diet_GCAM$`2050`<- (diet_GCAM$`2050`)*100
# Combinar bases
diet_adeq<- rbind(diet_GCAM, diet_others)

## share_staples
# Separar modelo GCAM
share_staples_GCAM<- share_staples %>%filter(Model=="GCAM_LAMP")
share_staples_others<- share_staples %>%filter(!Model=="GCAM_LAMP")
# Multiplicar por 100 para %
share_staples_GCAM$`2005`<- (share_staples_GCAM$`2005`)*100
share_staples_GCAM$`2010`<- (share_staples_GCAM$`2010`)*100
share_staples_GCAM$`2015`<- (share_staples_GCAM$`2015`)*100
share_staples_GCAM$`2020`<- (share_staples_GCAM$`2020`)*100
share_staples_GCAM$`2025`<- (share_staples_GCAM$`2025`)*100
share_staples_GCAM$`2030`<- (share_staples_GCAM$`2030`)*100
share_staples_GCAM$`2035`<- (share_staples_GCAM$`2035`)*100
share_staples_GCAM$`2040`<- (share_staples_GCAM$`2040`)*100
share_staples_GCAM$`2045`<- (share_staples_GCAM$`2045`)*100
share_staples_GCAM$`2050`<- (share_staples_GCAM$`2050`)*100
# Combinar bases
share_staples<- rbind(share_staples_GCAM, share_staples_others)

# ## food_prod_value
# # Separar modelo GCAM
# food_prod_value_GCAM<- food_prod_value %>%filter(Model=="GCAM_LAMP")
# food_prod_value_others<- food_prod_value %>%filter(!Model=="GCAM_LAMP")
# # Multiplicar por 100 para %
# food_prod_value_GCAM$`2005`<- (food_prod_value_GCAM$`2005`)*1000
# food_prod_value_GCAM$`2010`<- (food_prod_value_GCAM$`2010`)*1000
# food_prod_value_GCAM$`2015`<- (food_prod_value_GCAM$`2015`)*1000
# food_prod_value_GCAM$`2020`<- (food_prod_value_GCAM$`2020`)*1000
# food_prod_value_GCAM$`2025`<- (food_prod_value_GCAM$`2025`)*1000
# food_prod_value_GCAM$`2030`<- (food_prod_value_GCAM$`2030`)*1000
# food_prod_value_GCAM$`2035`<- (food_prod_value_GCAM$`2035`)*1000
# food_prod_value_GCAM$`2040`<- (food_prod_value_GCAM$`2040`)*1000
# food_prod_value_GCAM$`2045`<- (food_prod_value_GCAM$`2045`)*1000
# food_prod_value_GCAM$`2050`<- (food_prod_value_GCAM$`2050`)*1000
# # Combinar bases
# food_prod_value<- rbind(food_prod_value_GCAM, food_prod_value_others)

######### Exploración gráficos combinación política y CFE ############
### Crear objeto con combinación de escenarios para 4.5
sce_cfe_tx<-c("ccsm_4p5_cfe_ffict",
              "gfdl_4p5_cfe_ffict",
              "hadgem_4p5_cfe_ffict")

sce_cfe_notx<-c("hadgem_4p5_cfe_nopol",
               "ccsm_4p5_cfe_nopol",
               "gfdl_4p5_cfe_nopol",
               "gfdl_8p5_cfe_nopol",
               "hadgem_8p5_cfe_nopol",
               "ccsm_8p5_cfe_nopol")

sce_nocfe_notx<- c("ccsm_8p5_nocfe_nopol",
                   "gfdl_4p5_nocfe_nopol",
                   "hadgem_4p5_nocfe_nopol",
                   "ccsm_4p5_nocfe_nopol",
                   "gfdl_8p5_nocfe_nopol",
                   "hadgem_8p5_nocfe_nopol")

sce_nocfe_tx<- c("gfdl_4p5_nocfe_ffict",
                 "hadgem_4p5_nocfe_ffict",
                 "ccsm_4p5_nocfe_ffict")

## Crear vector y lista con escenarios
run<- c("sce_cfe_notx","sce_cfe_tx", "sce_nocfe_notx", "sce_nocfe_tx")
ran<- list(sce_cfe_notx, sce_cfe_tx, sce_nocfe_notx,sce_nocfe_tx)

### Calcular los datos mínimos, medianos y máximos
## food_prod_value ####

food_prod_value$cat<- ifelse(grepl(pattern ="*_4p5_",
                                     x = food_prod_value$Scenario,ignore.case = T),
                               "RCP4.5", "RCP8.5")

# i=1
gui<- list()

for(i in 1:length(ran)){

  test<- ran[[i]]
  
  ytest45 <- food_prod_value %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  if(i==1){
      datost2$Sce<- run[i]

  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  
  gui[[i]]<- datost2
  
}

xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")
  

p1<- proof %>% filter(., Sce %in% g1)
gf1<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) +
  labs(title="Average value of food production \n by CFE/Pol scenarios", x="mil_2005USD")
plot(gf1)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

## diet_adeq ####
diet_adeq$cat<- ifelse(grepl(pattern ="*_4p5_",x = diet_adeq$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- diet_adeq %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf2<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) + 
  labs(title="Average dietary energy supply adequacy \n by CFE/Pol scenarios", x="pct")
plot(gf2)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

# food_exp_share ####
food_exp_share$cat<- ifelse(grepl(pattern ="*_4p5_",x = food_exp_share$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- food_exp_share %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf3<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) +
  labs(title="Food Expenditure Share Income \n by CFE/Pol scenarios", x="pct")
 plot(gf3)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

# share_staples ####
share_staples$cat<- ifelse(grepl(pattern ="*_4p5_",x = share_staples$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- share_staples %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf4<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) +
  labs(title="Share of dietary energy supply derived from cereals, roots and tubers \n by CFE/Pol scenarios", x="pct")
 plot(gf4)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

# grain_imp_ratio ####
grain_imp_ratio$cat<- ifelse(grepl(pattern ="*_4p5_",x = grain_imp_ratio$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- grain_imp_ratio %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf5<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) + 
  labs(title="Grain Import Ratio \n by CFE/Pol scenarios", x="pct")
# plot(gf5)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

# food_imp_merch_exp####
food_imp_merch_exp$cat<- ifelse(grepl(pattern ="*_4p5_",x = food_imp_merch_exp$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- food_imp_merch_exp %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf6<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) + 
  labs(title="food_imp_merch_exp \n by CFE/Pol scenarios", x="pct")
# plot(gf6)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

# protein_total ####
protein_total$cat<- ifelse(grepl(pattern ="*_4p5_",x = protein_total$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- protein_total %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf7<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) + 
  labs(title="Protein total \n by CFE/Pol scenarios", x="g/cap/day")
# plot(gf7)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)

# protein_animal ####
protein_animal$cat<- ifelse(grepl(pattern ="*_4p5_",x = protein_animal$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- protein_animal %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  dplyr::select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf8<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) + 
  labs(title="Protein animal \n by CFE/Pol scenarios", x="g/cap/day")
plot(gf8)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)
# gdp_cap_ppp ####
gdp_cap_ppp$cat<- ifelse(grepl(pattern ="*_4p5_",x = gdp_cap_ppp$Scenario,ignore.case = T),"RCP4.5", "RCP8.5")

# Loop para los gráficos de densidad
# i=1
gui<- list()

for(i in 1:length(ran)){
  
  test<- ran[[i]]
  
  ytest45 <- gdp_cap_ppp %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
  ytest45$`2005`<-NULL
  
  datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
  datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
  datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))
  
  extremos<- merge(datmin,datmax)
  
  datost2<- merge(extremos,datmed)
  
  if(i==1){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==2){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==3){
    datost2$Sce<- run[i]
    
  }else{}
  if(i==4){
    datost2$Sce<- run[i]
    
  }else{}
  
  gui[[i]]<- datost2
  
}

# Gráficos de densidad
xfiles<-do.call(rbind,gui)

proof<- xfiles %>%  select(Model,Region,Sce,datmed, yr) %>% filter(yr=="2050")
g1<- c("sce_cfe_notx","sce_cfe_tx")
g2<- c("sce_nocfe_notx","sce_nocfe_tx")

p1<- proof %>% filter(., Sce %in% g1)
gf9<- ggplot(proof, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5) + 
  labs(title="GDP CAP PPP \n by CFE/Pol scenarios", x="thous_2005USD")
 plot(gf9)

# p2<- proof %>% filter(., Sce %in% g2)
# gf2<- ggplot(p2, aes(datmed)) + geom_density(aes(color=Sce),alpha = 0.5)
# plot(gf2)
### Combinar gráficos ####
png(filename = paste("./pic/","Imp.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange(gf5, gf6, ncol = 2, nrow = 1,common.legend = T, legend = "bottom")
# labels = c("A", "B")
plot(gg)
dev.off()


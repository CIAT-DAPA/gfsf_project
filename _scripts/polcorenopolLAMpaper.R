### Paper SAN
g=gc;rm(list = ls())
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

### Convertir variables factor a caracter
fs$MODEL<-as.character(fs$MODEL)
fs$SCENARIO<- as.character(fs$SCENARIO)
fs$REGION<- as.character(fs$REGION)
fs$VARIABLE<- as.character(fs$VARIABLE)
fs$UNIT<- as.character(fs$UNIT)

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

################################ scenarios ######################################
sce_cfe_tx_4p5<-c("ccsm_4p5_cfe_ffict",
              "gfdl_4p5_cfe_ffict",
              "hadgem_4p5_cfe_ffict")


sce_cfe_notx_4p5<-c("hadgem_4p5_cfe_nopol",
                "ccsm_4p5_cfe_nopol",
                "gfdl_4p5_cfe_nopol")

sce_cfe_notx_8p5<-c("gfdl_8p5_cfe_nopol",
                  "hadgem_8p5_cfe_nopol",
                  "ccsm_8p5_cfe_nopol")

sce_nocfe_notx_4p5<- c("ccsm_4p5_nocfe_nopol",
                   "gfdl_4p5_nocfe_nopol",
                   "hadgem_4p5_nocfe_nopol")

sce_nocfe_notx_8p5<- c("ccsm_8p5_nocfe_nopol",
                        "gfdl_8p5_nocfe_nopol",
                        "hadgem_8p5_nocfe_nopol")

sce_nocfe_tx_4p5<- c("gfdl_4p5_nocfe_ffict",
                 "hadgem_4p5_nocfe_ffict",
                 "ccsm_4p5_nocfe_ffict")
sce_core_nopol<- c("core_ref_noimpacts_nopol")### se puede usar para la tabla en el paper
sce_core_pol_4p5<- c("core_4p5_noimpacts_ffict")

################################ lista de escenarios ###############################
listSce<- list(sce_cfe_tx,
               sce_cfe_notx,
               sce_nocfe_notx,
               sce_nocfe_tx) 
listSce2<- list(sce_cfe_tx_4p5,
                sce_cfe_notx_4p5,
                sce_cfe_notx_8p5,
                sce_nocfe_notx_4p5,
                sce_nocfe_notx_8p5,
                sce_nocfe_tx_4p5,
                sce_core_nopol,
                sce_core_pol_4p5)
                

#### function 
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

################################ Food security analisys ############################################
fsa<- fsa %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables

### Cambiar nombre a variables de seg. alimentaria
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

### crear una lista por escenario
sl<- split(food,food$Scenario)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(Scenario %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(Scenario %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(Scenario %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(Scenario %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(Scenario %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(Scenario %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(Scenario %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(Scenario %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})

### data completa apilada
bbb<- do.call(rbind,aaa)
qqq<-bbb ###haciendo copia 
qqq$growth<- ((qqq$`2050`-qqq$`2020`)/qqq$`2020`)*100
options(scipen=999)

qqq<- qqq %>% select(Model,Scenario,Region,Variable,sce,growth) %>% 
      group_by(Model,Variable,sce) %>% summarise(mean=mean(growth))
write.csv(qqq,"./pic/VarFoodGrowthtest.csv")

rm(bbb,aaa,qqq)
################################ Carbon Concentration ######################
fge<- fs %>% dplyr::filter(REGION%in%lac)
fge<- fge %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables
allpar<- unique(fge$VARIABLE)

co<- c("Concentration|CO2")

co2<- fge %>% filter( VARIABLE %in% co) 
co2$VARIABLE<- plyr::revalue(co2$VARIABLE,c("Concentration|CO2"="ConcentrationCO2ppm"))


### Eliminar periodos
co2<- co2 %>% select(MODEL, SCENARIO,REGION, VARIABLE,X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
co2<- co2 %>% gather(year,val,5:ncol(co2))
co2$year<- gsub(pattern = "X",replacement = "",x = co2$year,fixed = T)
co2$year<- as.numeric(co2$year)


### calculo de las tasas de participacion de cada categoria sobre el total
co2<- co2 %>% spread(year,val) 
co2$Change<- NA
co2$Change<- ((co2$`2050`- co2$`2010`)/co2$`2010`)*100
co2<- co2 %>% select(MODEL,SCENARIO,REGION,VARIABLE,Change)


### crear una lista por escenario
sl<- split(co2,co2$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                          ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                        ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                               ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                      ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                             ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                    ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})

### data completa apilada
bbb<- do.call(rbind,aaa)
cc<- c("sce_core_nopol","sce_core_pol_4p5")                 #c("sce_corenoimp_notx", "sce_core4.5noimp_tx")
pol4.5<- c("sce_nocfe_notx_4p5","sce_nocfe_tx_4p5")#c("sce_nocfe_notx","sce_nocfe_tx")
### pol sin CFE
qfiles<- bbb %>% dplyr::filter(sce %in% pol4.5) %>% #filter(rcp=="RCP4.5")%>%
      group_by(MODEL,VARIABLE,sce)%>%
      summarise(mean=mean(Change)) 
write.csv(qfiles,paste("./pic/PolCarbonConcentrationRCP4.5NoCFE.csv"))

### CC 
qfiles<- bbb %>% dplyr::filter(sce %in% cc) %>% filter(rcp=="RCP4.5")%>%
      group_by(MODEL,VARIABLE,sce)%>%
      summarise(mean=mean(Change)) 
write.csv(qfiles,paste("./pic/CCCarbonConcentrationRCP4.5.csv"))

rm(bbb,aaa,qqq)
################################ Emissions CO #########################################
fge<- fs %>% dplyr::filter(REGION%in%lac)
fge<- fge %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables
allpar<- unique(fge$VARIABLE)
co<- c( "Emissions|CO2")

co2<- fge %>% filter( VARIABLE %in% co) 
co2$VARIABLE<- plyr::revalue(co2$VARIABLE,c("Emissions|CO2"="Emissions CO2"))


### Eliminar periodos
co2<- co2 %>% select(MODEL, SCENARIO,REGION,VARIABLE,X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
co2<- co2 %>% gather(year,val,5:ncol(co2))
co2$year<- gsub(pattern = "X",replacement = "",x = co2$year,fixed = T)
co2$year<- as.numeric(co2$year)


### definiendo RCP
# co2$rcp<- ifelse(grepl(pattern ="*_4p5_",x = co2$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
co2$rcp<- ifelse(grepl(pattern ="*_8p5_",x = co2$SCENARIO,ignore.case = T),"RCP8.5", "RCP4.5")

### calculo de las tasas de participacion de cada categoria sobre el total
co2<- co2 %>% spread(year,val) 
co2$Change<- NA
co2$Change<- ((co2$`2050`- co2$`2010`)/co2$`2010`)*100
co2<- co2 %>% select(MODEL,SCENARIO,REGION,rcp,VARIABLE,Change)


### crear una lista por escenario
sl<- split(co2,co2$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_nocfe_notx",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_tx",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_core4.5noimp_tx",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_corenoimp_notx","")))))))
      
      return(ee)      
})

### data completa apilada
bbb<- do.call(rbind,aaa)
model<- unique(bbb$MODEL)
cc<-c("sce_corenoimp_notx", "sce_core4.5noimp_tx")
pol<- c("sce_nocfe_notx","sce_nocfe_tx")
### pol sin CFE
qfiles<- bbb %>% dplyr::filter(sce %in% pol) %>% filter(rcp=="RCP4.5")%>%
      group_by(MODEL,VARIABLE,sce)%>%
      summarise(mean=mean(Change)) 
write.csv(qfiles,paste("./pic/PolemmisionRCP4.5NoCFE.csv"))

### CC 
qfiles<- bbb %>% dplyr::filter(sce %in% cc) %>% filter(rcp=="RCP4.5")%>%
      group_by(MODEL,VARIABLE,sce)%>%
      summarise(mean=mean(Change)) 
write.csv(qfiles,paste("./pic/CCPolemmisionRCP4.5.csv"))


labs2 = 'Types'
### RCP=4.5
qfiles<- bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5")%>%
      group_by(MODEL,VARIABLE,sce,UNIT)%>%
      summarise(mean=mean(Change)) 



############################################### Price carbon  ###########################################
fge<- fs %>% dplyr::filter(REGION%in%lac)
fge<- fge %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables
allpar<- unique(fge$VARIABLE)
### filtro sobre la variable de consumo
price<- fge[grep("*Price", fge$VARIABLE),]
price$VARIABLE<- as.character(price$VARIABLE)


###### categorias precios
vv<- c("Price|Carbon")
price<- price %>% filter(VARIABLE %in% vv)


### rename variables
price$VARIABLE<- as.character(price$VARIABLE)
price$VARIABLE<- plyr::revalue(price$VARIABLE,c("Price|Carbon"="Price Carbon"))

### Eliminar periodos
price$X1990<- NULL; price$X2004<- NULL
price<- price %>% select(MODEL, SCENARIO,REGION,VARIABLE, X2010,
                         X2020,X2025,X2030,X2035,X2040,X2045,X2050)
price<- price %>% gather(year,val,5:ncol(price))
price$year<- gsub(pattern = "X",replacement = "",x = price$year,fixed = T)
price$year<- as.numeric(price$year)

xfiles<- price
### definiendo RCP
# xfiles$rcp<- ifelse(grepl(pattern ="*_4p5_",x = xfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
xfiles<- xfiles %>%  group_by(MODEL,SCENARIO,REGION,VARIABLE)%>% 
      spread(year,val) 
jfiles<-xfiles

sl<- split(jfiles,xfiles$SCENARIO)
### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})

### data completa apilada
jjjfiles<- do.call(rbind,aaa)

jjjfiles<- jjjfiles %>% gather(year,val,6:12) %>% group_by(MODEL,VARIABLE,sce,year,REGION)%>% 
      summarise(mean=mean(val)) #%>% filter(.,rcp!="RCP8.5")

write.csv(jjjfiles,paste("./pic/CarbonPrice_RCP4.5_priceNOCFE.csv"))

############################################### Price agricultural commodities ###########################################
fge<- fs %>% dplyr::filter(REGION%in%lac)
fge<- fge %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables
allpar<- unique(fge$VARIABLE)
### filtro sobre la variable de consumo
price<- fge[grep("*Price", fge$VARIABLE),]
price$VARIABLE<- as.character(price$VARIABLE)


###### categorias precios
vv<- c("Price|Agriculture|All Crops")
price<- price %>% filter(VARIABLE %in% vv)


### rename variables
price$VARIABLE<- as.character(price$VARIABLE)
price$VARIABLE<- plyr::revalue(price$VARIABLE,c("Price|Agriculture|All Crops"="Price All Crops"))

### Eliminar periodos
price$X1990<- NULL; price$X2004<- NULL
price<- price %>% select(MODEL, SCENARIO,REGION,VARIABLE,X2010,
                         X2020,X2025,X2030,X2035,X2040,X2045,X2050)
price<- price %>% gather(year,val,5:ncol(price))
price$year<- gsub(pattern = "X",replacement = "",x = price$year,fixed = T)
price$year<- as.numeric(price$year)

xfiles<- price

### definiendo RCP
xfiles$rcp<- ifelse(grepl(pattern ="*_4p5_",x = xfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")


### calculo de las tasas de participacion de cada categoria sobre el total
xfiles<- xfiles %>%  group_by(MODEL,SCENARIO,REGION,rcp, VARIABLE)%>% 
      spread(year,val) 
jfiles<-xfiles

sl<- split(jfiles,xfiles$SCENARIO)
### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})
### data completa apilada
jjjfiles<- do.call(rbind,aaa)

jjjfiles<- jjjfiles %>% gather(year,val,6:13) %>% group_by(MODEL,VARIABLE,sce,rcp,year,REGION)%>% 
      summarise(mean=mean(val))

write.csv(jjjfiles,paste("./pic/AllcropsAllFood_price.csv"))





############################################### Graficas PAPER Comparar ###################
fge<- fs %>% dplyr::filter(REGION%in%lac)
fge<- fge %>% select(MODEL,SCENARIO,REGION,VARIABLE,X2005,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables
allpar<- unique(fge$VARIABLE)
### a) agricutural commodity price (y) carbon price (x)
a1<- c("Price|Carbon","Price|Agriculture|All Crops")
abs<- fge %>% dplyr::filter(., VARIABLE %in% a1)

abs$VARIABLE<- plyr::revalue(abs$VARIABLE,c( "Price|Agriculture|All Crops"="AgriPrice",
                                             "Price|Carbon"="Carbon Price"  ))

### Eliminar periodos
abs$X1990<- NULL; abs$X2004<- NULL
abs<- abs %>% select(MODEL, SCENARIO,REGION, VARIABLE,
                     X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
abs<- abs %>% gather(year,val,5:ncol(abs))
abs$year<- gsub(pattern = "X",replacement = "",x = abs$year,fixed = T)
abs$year<- as.numeric(abs$year)

### definiendo RCP
abs$rcp<- ifelse(grepl(pattern ="*_4p5_",x = abs$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### crear una lista por escenario
sl<- split(abs,abs$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})
##### data completa apilada
bbb<- do.call(rbind,aaa)
model<- unique(bbb$MODEL)
cc<-c("sce_corenoimp_notx", "sce_core4.5noimp_tx")
pol<- c("sce_nocfe_notx","sce_nocfe_tx")
### pol sin CFE
qfiles <- bbb %>% filter(., sce %in% pol )%>% filter(., year!=2010) %>%filter(rcp=="RCP4.5") %>% #
      filter(.,MODEL!="Phoenix")%>%
      spread(year,val)


qfiles$base<-qfiles$`2020`/qfiles$`2020`
qfiles$i_25<-qfiles$`2025`/qfiles$`2020`
qfiles$i_30<-qfiles$`2030`/qfiles$`2020`
qfiles$i_35<-qfiles$`2035`/qfiles$`2020`
qfiles$i_40<-qfiles$`2040`/qfiles$`2020`
qfiles$i_45<-qfiles$`2045`/qfiles$`2020`
qfiles$i_50<-qfiles$`2050`/qfiles$`2020` 

qfiles<- qfiles %>% select(MODEL,SCENARIO,REGION,VARIABLE,rcp,sce,base,i_25,i_30,i_35,
                     i_40,i_45,i_50) %>% gather(index,val,7:ncol(.)) %>% 
      group_by(MODEL,REGION,VARIABLE,rcp,sce,index) %>%
      summarise(ave=mean(val,rm.na=T)) %>% na.omit()%>% 
      group_by(MODEL,REGION,rcp,sce,index) %>%
      spread(VARIABLE,ave)


write.csv(bbb,"./pic/priceCarbon_AgriculturalPOL.csv")

###solo los precios de agricultura
nnn<- qfiles
nnn<- nnn %>% group_by(MODEL, rcp, sce, index) %>%
      summarise(meanCar=mean(`Carbon Price`), meanAgr=mean(AgriPrice))
write.csv(nnn,"./pic/avepriceCarbon_AgriculturalPOL.csv")

###################################### heat map #########################
### filtro sobre la variable de consumo
price<- fge[grep("*Price", fge$VARIABLE),]
price$VARIABLE<- as.character(price$VARIABLE)


###### categorias precios
vv<- c("Price|Agriculture|All Crops")
price<- price %>% filter(VARIABLE %in% vv)


### rename variables
price$VARIABLE<- as.character(price$VARIABLE)
price$VARIABLE<- plyr::revalue(price$VARIABLE,c("Price|Agriculture|All Crops"="Price All Crops"))

### Eliminar periodos
price$X1990<- NULL; price$X2004<- NULL
price<- price %>% select(MODEL, SCENARIO,REGION,VARIABLE,X2010,
                         X2020,X2025,X2030,X2035,X2040,X2045,X2050)
price<- price %>% gather(year,val,5:ncol(price))
price$year<- gsub(pattern = "X",replacement = "",x = price$year,fixed = T)
price$year<- as.numeric(price$year)

xfiles<- price

### definiendo RCP
# xfiles$rcp<- ifelse(grepl(pattern ="*_4p5_",x = xfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")


### calculo de las tasas de participacion de cada categoria sobre el total
xfiles<- xfiles %>%  group_by(MODEL,SCENARIO,REGION, VARIABLE)%>% 
      spread(year,val) 
jfiles<-xfiles

sl<- split(jfiles,xfiles$SCENARIO)
### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})

### data completa apilada
jjjfiles<- do.call(rbind,aaa)
"sce_corenoimp_notx"
jjjfiles<- jjjfiles %>% gather(year,val,5:12) %>% group_by(MODEL,VARIABLE,sce,year,REGION)%>% 
      summarise(mean=mean(val))

write.csv(jjjfiles,paste("./pic/AllcropsAllFood_price.csv"))


########################################## relation price carbon price food ###########################

pfood<- read.csv("./pic/AllcropsAllFood_price.csv")
pcarbon<- read.csv("./pic/CarbonPrice_RCP4.5_priceNOCFE.csv")



p.all<- rbind(pfood,pcarbon)
p.all$X<- NULL
p.all$UNIT<- NULL

p.all$MODEL<- as.character(p.all$MODEL)
p.all$sce<- as.character(p.all$sce)
p.all$rcp<- as.character(p.all$rcp)
p.all$REGION<- as.character(p.all$REGION)
# p.all$UNIT<- as.character(p.all$UNIT)
write.csv(p.all,"./pic/RCP4.5Food&carbonTest.csv")


### CC 
qfiles <- bbb %>% filter(., sce %in% cc )%>% filter(., year!=2010) %>%filter(rcp=="RCP4.5") %>% #
      filter(.,MODEL!="Phoenix")%>%
      spread(year,val)


qfiles$base<-qfiles$`2020`/qfiles$`2020`
qfiles$i_25<-qfiles$`2025`/qfiles$`2020`
qfiles$i_30<-qfiles$`2030`/qfiles$`2020`
qfiles$i_35<-qfiles$`2035`/qfiles$`2020`
qfiles$i_40<-qfiles$`2040`/qfiles$`2020`
qfiles$i_45<-qfiles$`2045`/qfiles$`2020`
qfiles$i_50<-qfiles$`2050`/qfiles$`2020` 

qfiles<- qfiles %>% select(MODEL,SCENARIO,REGION,VARIABLE,rcp,sce,base,i_25,i_30,i_35,
                           i_40,i_45,i_50) %>% gather(index,val,7:ncol(.)) %>% 
      group_by(MODEL,REGION,VARIABLE,rcp,sce,index) %>%
      summarise(ave=mean(val,rm.na=T)) %>% na.omit()%>% 
      group_by(MODEL,REGION,rcp,sce,index) %>%
      spread(VARIABLE,ave)

write.csv(bbb,"./pic/priceCarbon_AgriculturalCC.csv")


###solo los precios de agricultura
nnn<- qfiles
nnn<- nnn %>% group_by(MODEL, rcp, sce, index) %>%
      summarise(meanCar=mean(`Carbon Price`), meanAgr=mean(AgriPrice))
write.csv(nnn,"./pic/avepriceCarbon_AgriculturalCC.csv")


############ reference
##### data completa apilada
### crear una lista por escenario
sl<- split(abs,abs$SCENARIO)

### asignacion de categoria escenario 

aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})


bbb<- do.call(rbind,aaa)
bbb$UNIT<- NULL
# base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")

vvv<- bbb %>% filter(., year==2050)%>%
      spread(year,val) %>% group_by(MODEL,REGION, VARIABLE,rcp, sce) %>%
      summarise(ave=mean(`2050`))
write.csv(vvv,"./pic/coreanalisis.csv")

bbb <- bbb %>% filter(., year==2050)%>%
      spread(year,val) %>% group_by(MODEL,REGION, VARIABLE,rcp, sce) %>%
      summarise(ave=mean(`2050`)) %>% spread(sce, ave) 

write.csv(bbb,"./pic/coreanalisisCompleto.csv")

# %>% 
#       select(MODEL,VARIABLE, REGION,rcp, sce_cfe_tx,sce_core_pol) %>% filter(.,rcp=="RCP4.5")

bbb$index<-  bbb$sce_cfe_tx/bbb$sce_core_pol

bbb<- bbb %>% select(MODEL,VARIABLE,REGION,index,rcp) %>%  
      spread(VARIABLE,index) %>% filter(MODEL!="Phoenix")




# bbb$i_cfe_notx_core_nopol<- bbb$sce_cfe_notx/bbb$sce_core_nopol
# bbb$i_cfe_notx_core_nopol<-  bbb$sce_cfe_notx/bbb$sce_core_nopol




# %>%  group_by(MODEL,SCENARIO,REGION,rcp,sce) %>% 
#       mutate(i_p_2020=`2020`/`2020`,
#          i_p_2025=`2025`/`2020`,
#          i_p_2030=`2030`/`2020`,
#          i_p_2035=`2035`/`2020`,
#          i_p_2040=`2040`/`2020`,
#          i_p_2045=`2045`/`2020`,
#          i_p_2050=`2050`/`2020`) %>% 
#       select(MODEL,SCENARIO,REGION,VARIABLE,rcp,sce,i_p_2020,i_p_2025,i_p_2030,i_p_2035,
#                                             i_p_2040,i_p_2045,i_p_2050) %>% 
#       gather(index,val,7:ncol(.)) %>%  
#       group_by(MODEL,REGION,VARIABLE,rcp,sce,index) %>%
#       summarise(ave=mean(val,rm.na=T)) %>% na.omit()%>% 
#       group_by(MODEL,REGION,rcp,sce,index) %>%
#       spread(VARIABLE,ave)
unique(bbb$sce)
reg<-unique(bbb$REGION)

i=4
for(i in 1:length(reg)){
      
      png(filename= paste("./pic/",reg[[i]],"_pricsCarbonAgriculture.png",sep=""), 
          width = 8, height = 8, units = 'in', res = 300)
      
      mmm<- ggplot(bbb %>% filter(.,sce=="sce_cfe_tx")%>%
                         filter(.,rcp=="RCP4.5") %>%  filter(.,REGION==reg[[i]]),
                   aes(x=`Carbon Price`,y=AgriPrice))+
            geom_point()+ geom_line() + facet_wrap(~MODEL,scales = "free")+
            labs(x="Carbon price\nindex(Base=2020)",y="Agricultural crops price\nindex(Base=2020)",
                 title= paste("REGION= ",reg[[i]],", RCP5.5" ,sep = ""))+
            theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      plot(mmm)
      dev.off()  
      
}



ce<- cc[grep("Price", cc$VARIABLE),]
unique(ce$VARIABLE)
prifood<- c("Price|Agriculture|Maize", 
            "Price|Agriculture|Oil Crops",
            "Price|Agriculture|Other Crops",
            "Price|Agriculture|Other Grain",
            "Price|Agriculture|Sugar Crops",
            "Price|Agriculture|Wheat", 
            "Price|Carbon",
            "Price|Agriculture|Energy Crops",
            "Price|Agriculture|Fiber Crops" ,
            "Price|Agriculture|Rice",
            "Price|Agriculture|Roots Tuber") 


unique(iii$VARIABLE)
iii<- cc %>% dplyr::filter(., VARIABLE %in% prifood)

iii$VARIABLE<- plyr::revalue(iii$VARIABLE,c( "Price|Agriculture|Maize"="Maize", 
                                             "Price|Agriculture|Oil Crops"="Oil_crops",
                                             "Price|Agriculture|Other Crops"="Other_crops",
                                             "Price|Agriculture|Other Grain"="Other_grain",
                                             "Price|Agriculture|Sugar Crops"="Sugar",
                                             "Price|Agriculture|Wheat"="Wheat", 
                                             "Price|Carbon"="Price_Carbon",
                                             "Price|Agriculture|Energy Crops"="Energy_crops",
                                             "Price|Agriculture|Fiber Crops"="Fiber_crops" ,
                                             "Price|Agriculture|Rice"="Rice",
                                             "Price|Agriculture|Roots Tuber"="R&T"))

### Eliminar periodos
iii$X1990<- NULL; iii$X2004<- NULL
iii<- iii %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                     X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
iii<- iii %>% gather(year,val,6:ncol(iii))
iii$year<- gsub(pattern = "X",replacement = "",x = iii$year,fixed = T)
iii$year<- as.numeric(iii$year)

### definiendo RCP
iii$rcp<- ifelse(grepl(pattern ="*_4p5_",x = iii$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### crear una lista por escenario
sl<- split(iii,iii$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% dplyr::mutate(sce= ifelse(SCENARIO %in% listSce2[[1]],"sce_cfe_tx_4p5",
                                                 ifelse(SCENARIO %in% listSce2[[2]],"sce_cfe_notx_4p5",
                                                        ifelse(SCENARIO %in% listSce2[[3]],"sce_cfe_notx_8p5",
                                                               ifelse(SCENARIO %in% listSce2[[4]],"sce_nocfe_notx_4p5",
                                                                      ifelse(SCENARIO %in% listSce2[[5]],"sce_nocfe_notx_8p5",
                                                                             ifelse(SCENARIO %in% listSce2[[6]],"sce_nocfe_tx_4p5",
                                                                                    ifelse(SCENARIO %in% listSce2[[7]],"sce_core_nopol",
                                                                                           ifelse(SCENARIO %in% listSce2[[8]],"sce_core_pol_4p5","")))))))))
      
      return(ee)      
})

### data completa apilada
ppp<- do.call(rbind,aaa)
ppp$UNIT<- NULL
base2<- c("core_ref_noimpacts_nopol")
ppp <- ppp %>% filter(., !SCENARIO %in% base ) %>% filter(., year!=2010)%>%
      spread(year,val)


ppp$base<-ppp$`2020`/ppp$`2020`
ppp$i_25<-ppp$`2025`/ppp$`2020`
ppp$i_30<-ppp$`2030`/ppp$`2020`
ppp$i_35<-ppp$`2035`/ppp$`2020`
ppp$i_40<-ppp$`2040`/ppp$`2020`
ppp$i_45<-ppp$`2045`/ppp$`2020`
ppp$i_50<-ppp$`2050`/ppp$`2020` 

ppp<- ppp %>% select(MODEL,SCENARIO,REGION,VARIABLE,rcp,sce,base,i_25,i_30,i_35,
                     i_40,i_45,i_50) %>% gather(index,val,7:ncol(.)) %>% 
      group_by(MODEL,REGION,VARIABLE,rcp,sce,index) %>%
      summarise(ave=mean(val,rm.na=T)) %>% na.omit()%>% 
      group_by(MODEL,REGION,VARIABLE,rcp,sce,index) %>%
      spread(VARIABLE,ave) %>% filter(.,MODEL!="Phoenix")


unique(ppp$sce)
reg<-unique(ppp$REGION)
cul<- unique(iii$VARIABLE)
climas<- c("sce_cfe_tx","sce_cfe_notx")
i=1
c=2
for(i in 1:length(reg)){
      for(c in 1:length(cul)){
            png(filename= paste("./pic/",reg[[i]],"_",cul[[c]],"_pricCarbonAgriculture.png",sep=""), 
                width = 8, height = 8, units = 'in', res = 300)
            hfiles<- ppp %>% filter(.,sce %in% climas)%>%
                  filter(.,rcp=="RCP4.5") %>%  filter(.,REGION==reg[[i]]) %>% 
                  select(MODEL,REGION,rcp,sce,index,cul[[c]],Price_Carbon) %>% as.data.frame()
            
            mmm<- ggplot(hfiles,aes(x=hfiles$Price_Carbon,y=hfiles[,6]))+
                  geom_point()+ geom_line() + facet_wrap(~MODEL,scales = "free")+
                  labs(x="Carbon price\nindex(Base=2020)",
                       y= paste("Price of ",cul[[c]],"\nindex(Base=2020)",sep = ""),
                       title= paste("REGION= ",reg[[i]],", RCP4.5" ,sep = ""))+
                  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
                  theme(axis.text.y = element_text(hjust = 1, size = 11))+
                  theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
                  theme(strip.text=element_text(size=8))+
                  theme(strip.text.y = element_text(angle = 0,size = 11))+
                  theme(legend.position="bottom")
            plot(mmm)
            dev.off()  
            
            
      }
      
}











#### agricultul foods 



xfiles$base<-xfiles$`2020`/xfiles$`2020`
xfiles$i_25<-xfiles$`2025`/xfiles$`2020`
xfiles$i_30<-xfiles$`2030`/xfiles$`2020`
xfiles$i_35<-xfiles$`2035`/xfiles$`2020`
xfiles$i_40<-xfiles$`2040`/xfiles$`2020`
xfiles$i_45<-xfiles$`2045`/xfiles$`2020`
xfiles$i_50<-xfiles$`2050`/xfiles$`2020` 

xfiles<- xfiles %>% select(MODEL,SCENARIO,REGION,UNIT,VARIABLE,rcp,base,i_25,i_30,i_35,i_40,i_45,i_50)






tesb <- bbb %>% dplyr::filter(.,Scenario %in% test) %>%  filter(.,cat == "RCP4.5") %>% gather(yr,val,6:14) 
ytest45$`2005`<-NULL

datmin<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarise(datamin= min(val,na.rm=T))
datmed<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmed= median(val,na.rm=T))
datmax<- ytest45 %>% dplyr::group_by(Model,Region,Variable,yr,cat) %>% dplyr::summarize(datmax= max(val,na.rm=T))




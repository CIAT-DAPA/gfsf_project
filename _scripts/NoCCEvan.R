#### sce3 Vs sce1
#### Codigo para procesar resultados de WB

### libraries 
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
suppressMessages(library(sp))
suppressMessages(library(maps))
suppressMessages(library(raster))
suppressMessages(library(cumplyr))
suppressMessages(library(scales))



### directories
#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB")
gen<- c("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/")

### Load data
### cargar los datos area
### groups scenarios
g1<- c("_SSP1"); g2<- c("_SSP2"); g3<- c("_SSP3"); g4<- c("_SSP4");g5<- c("_SSP5")
rcp<- list(g1,g2,g3,g4,g5)


### groups of variables 
# datos por sistema de produccion  riego y secano
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )
# datos categorias totales
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area")
# datos categorias agregados 
dataagg<- c("FoodAvailXAgg", "QDXAgg -- Total Demand",
            "QEXAgg -- Export","QINTXAgg -- Intermediate Demand","QMXAgg -- Import",
            "QFXAgg -- Household Demand","QSupXAgg -- Commodity Supply",
            "QSXAgg -- Total Production")


# datos categorias animales
dataanimal<- c("AnmlNumXAgg -- Animal Numbers" , "AnmlYldXAgg -- Animal Yield")
# data tratamiento especial
dataespecial<- c("QNXAgg -- Net Trade")


cropsout<- c("Other", "Other Pulses", "Other meals","Other Cereals", "Other Oilseeds",
             "Other Oils", "Other Roots", "Soybean Meal", "Temperate Fruit" ,"Rapeseed Oil" ,
             "Groundnut Oil", "Groundnut meal",  "Rapeseed Meal", "Soybean Oil","Soybean Meal",  
             "Palm Kernel Oil" ,"Palm Kernel Meal", "Sunflower Meal", "Sugar", "Palm Fruit Oil" )




################### TOPICS ################################################
ca<- c("RCP4.5_SSP2", "RCP8.5_SSP2", "RCP6.0_SSP2","RCP2.6_SSP2")

xx<- list.files( path = "./tables/",pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="NOCC_", x = kk)]
# kk<- list(kk)
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)

# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)
phi$X<- NULL


      
d<- phi %>% group_by(impactparameter,commodity,region,
                   year) %>% summarise(mean=mean(Val))
      
d$region<- revalue(d$region,c("SSA-Ivory Coast"="Ivory Coast",
                                    "SSA-Mali"="Mali"))
     
write.csv(d,paste("./tables/AveNoCC.csv"))
      



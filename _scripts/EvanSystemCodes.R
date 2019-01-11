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

options(digits=3) 
options(scipen = 999)



### directories
#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB/tables/")

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


############################################ datatotal ############################################
i=1
for(i in 1:length(datatotal)){
      
      xx<- list.files( path ="./",pattern ="perc_changeByYear_", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern=datatotal[i], x = kk)]
      

      pp<- lapply(kk,read.csv) ### load data

      ### convirtiendo en archivo data
      xx<- do.call(rbind, pp)
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      yr<- c('2030','2040', '2050')
      xx$change<- ((xx$CC-xx$NoCC)/xx$NoCC)*100
      xx<- xx %>% select(impactparameter,commodity,region,year,perc_change,sce) %>% spread(sce,perc_change) %>%filter(year %in% yr)
      
      write.csv(xx,paste("./appendix/",datatotal[i], "_tableEvan.csv", sep = ""))
      
}

############################################ dataagg ############################################
for(i in 1:length(dataagg)){
      
      xx<- list.files( path ="./",pattern ="perc_changeByYear_", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern=dataagg[i], x = kk)]
      
      
      pp<- lapply(kk,read.csv) ### load data
      
      ### convirtiendo en archivo data
      xx<- do.call(rbind, pp)
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      yr<- c('2030','2040', '2050')
      xx<- xx %>% select(impactparameter,commodity,region,year,perc_change,sce) %>% spread(sce,perc_change) %>%filter(year %in% yr)
      
      write.csv(xx,paste("./appendix/",dataagg[i], "_tableEvan.csv", sep = ""))
      
}



#################################### Sistemas de Produccion ##########################################
i=1
for(i in 1:length(datasys)){
      
      xx<- list.files( path ="./",pattern ="perc_changeByYear_", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern=datasys[i], x = kk)]
      
      
      pp<- lapply(kk,read.csv) ### load data
      
      ### convirtiendo en archivo data
      xx<- do.call(rbind, pp)
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      yr<- c('2030','2040','2050')
      xx<- xx %>% select(impactparameter,commodity,productiontype,region,year,perc_change,sce) %>% spread(sce,perc_change) %>%filter(year %in% yr)
      
      write.csv(xx,paste("./appendix/",datasys[i], "_tableEvan.csv", sep = ""))
      
}

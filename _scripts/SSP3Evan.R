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

xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP3", x = kk)]
# kk<- list(kk)
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$productiontype<- as.character(xx$productiontype)

# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)


### nocc

nocc<- phi$scenario[grep(pattern = "WB_NoCC_SSP", x = phi$scenario)]
nocc<- unique(nocc)

no<- phi %>% filter(scenario %in% nocc)
no <- no[!duplicated(no[c('impactparameter',"scenario", 
                          "commodity","year" ,"Val")]),]
no$year<- as.numeric(no$year)
### calculate 2018 values 

# base<- base %>% spread(year,Val)
write.csv(no,paste("./tables/NOCC_SSP3.csv", sep = ""))


k<- list()
r<- list()
# i=1

for(i in seq_along(datatotal)){
      
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
      # filtro por numero de ref
      ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
      ref<- unique(ref)
      
      
      nocc<- k[[i]] %>% filter(scenario %in% ref)
      df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                    "commodity","year" ,"Val")]),]
      df$year<- as.numeric(df$year)
      ### calculate 2018 values 
      
      # # base<- base %>% spread(year,Val)
      # write.csv(df,paste("./tables/",ref,"_SSP3.csv", sep = ""))
      
      #filtro
      #Eliminando todos los NoCC
      k[[i]]<- k[[i]] %>% dplyr::filter(!scenario %in% ref)
      #apilando solo un NoCC
      k[[i]]<- rbind(k[[i]], df)
      df<- df %>% spread(year,Val)
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,commodity,region)%>%
            spread ("year","Val") %>% as.data.frame()
      
      ##creando variable de scenario
      sce<- unique(k[[i]]$scenario)
      sce<- substring(sce, 9)
      s<- as.data.frame(table(sce))
      # u<- dim(s)[1]
      s<- s %>% filter(.,Freq==5 )
      
      # o=1
      f<-list()
      for(o in 1:NROW(s)){
            tm<- sce[o]
            d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
            d<- rbind(d, df)
            ref<- d$scenario[grep(pattern = "WB_NoCC_SSP", x = d$scenario)]
            ref<- unique(ref)
            d<- data.frame(d,"Cat"=ifelse(d$scenario==ref,"NoCC","CC"))
            d<-d[,-c(5:19)]
            
            

            d<- d %>% gather(year, val,5:35)
            d$year<- gsub(pattern = "X",replacement = "",x = d$year)
            d$year<- as.numeric(d$year)

            d<- d %>% group_by(impactparameter,commodity,region,
                               Cat,year) %>% summarise(mean=mean(val))%>% 
                  as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
                  mutate(perc_change=((CC-NoCC)/NoCC)*100)
            
            d$region<- revalue(d$region,c("SSA-Ivory Coast"="Ivory Coast",
                                          "SSA-Mali"="Mali"))
            ssp<- substring(tm,8)
            d$ssp<- ssp
            rcp<- substring(tm,1,6)
            d$rcp<- rcp
            f[[o]]<- d
      }
      
      ### apilar resultados  
      bb<- do.call(rbind, f)
      bb<- as.data.frame(bb)
      
      write.csv(bb,paste("./tables/perc_changeByYear_", datatotal[i],"_SSP3.csv"))
      
      
      
      print(i)
}  


r<- list()
f<- list()
# i=1
for(i in seq_along(dataagg)){
      
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
      # filtro por numero de ref
      ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
      ref<- unique(ref)
      
      
      nocc<- k[[i]] %>% filter(scenario %in% ref)
      df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                    "commodity","year" ,"Val")]),]
      df$year<- as.numeric(df$year)
      # write.csv(df,paste("./tables/",ref,"_SSP3.csv", sep = ""))
      
      #filtro
      #Eliminando todos los NoCC
      k[[i]]<- k[[i]] %>% dplyr::filter(!scenario %in% ref)
      #apilando solo un NoCC
      k[[i]]<- rbind(k[[i]], df)
      df<- df %>% spread(year,Val)
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,commodity,region)%>%
            spread ("year","Val") %>% as.data.frame()
      
      ##creando variable de scenario
      sce<- unique(k[[i]]$scenario)
      sce<- substring(sce, 9)
      s<- as.data.frame(table(sce))
      # u<- dim(s)[1]
      s<- s %>% filter(.,Freq==5 )
      
      # o=1
      f<-list()
      for(o in 1:NROW(s)){
            tm<- sce[o]
            d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
            d<- rbind(d, df)
            ref<- d$scenario[grep(pattern = "WB_NoCC_SSP", x = d$scenario)]
            ref<- unique(ref)
            d<- data.frame(d,"Cat"=ifelse(d$scenario==ref,"NoCC","CC"))
            d<-d[,-c(5:19)]
            
            
            d<- d %>% gather(year, val,5:35)
            d$year<- gsub(pattern = "X",replacement = "",x = d$year)
            d$year<- as.numeric(d$year)
            
            d<- d %>% group_by(impactparameter,commodity,region,
                               Cat,year) %>% summarise(mean=mean(val))%>% 
                  as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
                  mutate(perc_change=((CC-NoCC)/NoCC)*100)
            
            d$region<- revalue(d$region,c("SSA-Ivory Coast"="Ivory Coast",
                                          "SSA-Mali"="Mali"))
            ssp<- substring(tm,8)
            d$ssp<- ssp
            rcp<- substring(tm,1,6)
            d$rcp<- rcp
            f[[o]]<- d
      }
      
      ### apilar resultados  
      bb<- do.call(rbind, f)
      bb<- as.data.frame(bb)
      
      write.csv(bb,paste("./tables/perc_changeByYear_", dataagg[i],"_SSP3.csv"))
      
      
      
      print(i)
}


# 
# 
####################### indicator additionals ##############################################

xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP3", x = kk)]
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$productiontype<- as.character(xx$productiontype)

# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)


# selecciono el cultivo
k<- phi %>% filter(., impactparameter %in% dataespecial)


# filtro por numero de ref
ref<- k$scenario[grep(pattern = "WB_NoCC_SSP", x = k$scenario)]
ref<- unique(ref)


nocc<- k %>% filter(scenario %in% ref)
df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                              "commodity","year" ,"Val")]),]
df$year<- as.numeric(df$year)
### calculate 2018 values 

# base<- base %>% spread(year,Val)
# write.csv(df,paste("./tables/",ref,"_SSP3.csv", sep = ""))

#filtro
#Eliminando todos los NoCC
k<- k %>% dplyr::filter(!scenario %in% ref)
#apilando solo un NoCC
k<- rbind(k, df)
df<- df %>% spread(year,Val)
#reordeno los datos
rownames(k)<- 1: nrow(k)
#reshape a lo ancho  
k<- k %>% group_by(impactparameter,scenario,commodity,region)%>%
      spread ("year","Val") %>% as.data.frame()

##creando variable de scenario
sce<- unique(k$scenario)
sce<- substring(sce, 9)
s<- as.data.frame(table(sce))
# u<- dim(s)[1]
s<- s %>% filter(.,Freq==5 )

# o=1
f<-list()
for(o in 1:NROW(s)){
      tm<- sce[o]
      d<- k[grep(pattern = tm,x = k$scenario),]
      d<- rbind(d, df)
      ref<- d$scenario[grep(pattern = "WB_NoCC_SSP", x = d$scenario)]
      ref<- unique(ref)
      d<- data.frame(d,"Cat"=ifelse(d$scenario==ref,"NoCC","CC"))
      d<-d[,-c(5:19)]
      
      
      d<- d %>% gather(year, val,5:35)
      d$year<- gsub(pattern = "X",replacement = "",x = d$year)
      d$year<- as.numeric(d$year)
      
      d<- d %>% group_by(impactparameter,commodity,region,
                         Cat,year) %>% summarise(mean=mean(val))%>% 
            as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
            mutate(perc_change=((CC-NoCC)/NoCC)*100)
      
      d$region<- revalue(d$region,c("SSA-Ivory Coast"="Ivory Coast",
                                    "SSA-Mali"="Mali"))
      
      
      ## logica de los valores
      nn<-  which(d$CC<0 & d$NoCC<0) # net trade negativo  importador neto
      pn<-  which(d$CC>0 & d$NoCC<0) # impacto positivo inicia como importador termina como exportador
      np<-  which(d$CC<0 & d$NoCC>0) # impacto negativo inicia como exportador termina como importador
      pp<-  which(d$CC>0 & d$NoCC>0) # net trade positivo  exportador neto
      
      
      #trends
      export<-   c(pp)
      import <-  c(nn)
      tran_XtoM<- c(np) # inicia exportador termina importador
      tran_MtoX<- c(pn) # inicia importador termina exportador
      
      
      
      # copia
      tanz<- d
      tanz$trend<- NA
      tanz$trend[import] <- "Negative"
      tanz$trend[export] <- "Positive"
      tanz$trend[tran_MtoX]<- "Transition from M to X"
      tanz$trend[tran_XtoM]<- "Transition from X to M"
      
      
      ssp<- substring(tm,8)
      tanz$ssp<- ssp
      rcp<- substring(tm,1,6)
      tanz$rcp<- rcp
      f[[o]]<- tanz
      
}


### apilar resultados  
bb<- do.call(rbind, f)
bb<- as.data.frame(bb)

write.csv(bb,paste("./tables/perc_changeByYear_", dataespecial,"_SSP3.csv"))


################### System production ############################
options(digits=3) 
options(scipen = 999)

xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP3", x = kk)]
# kk<- list(kk)
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$productiontype<- as.character(xx$productiontype)

# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

# phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)
### nocc

nocc<- phi$scenario[grep(pattern = "WB_NoCC_SSP", x = phi$scenario)]
nocc<- unique(nocc)

no<- phi %>% filter(scenario %in% nocc)
no <- no[!duplicated(no[c('impactparameter',"scenario", 
                          "commodity","productiontype","year" ,"Val")]),]
no$year<- as.numeric(no$year)
### calculate 2018 values 

# base<- base %>% spread(year,Val)
write.csv(no,paste("./tables/NOCC_SSP3.csv", sep = ""))


k<- list()
r<- list()
# i=1

for(i in seq_along(datasys)){
      
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==datasys[i]),]
      # filtro por numero de ref
      ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
      ref<- unique(ref)
      
      
      nocc<- k[[i]] %>% filter(scenario %in% ref)
      df <- nocc[!duplicated(nocc[c('impactparameter',"scenario","productiontype", 
                                    "commodity","year" ,"Val")]),]
      df$year<- as.numeric(df$year)
      ### calculate 2018 values 
      
      
      #filtro
      #Eliminando todos los NoCC
      k[[i]]<- k[[i]] %>% dplyr::filter(!scenario %in% ref)
      #apilando solo un NoCC
      k[[i]]<- rbind(k[[i]], df)
      df<- df %>% spread(year,Val)
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,productiontype,commodity,region)%>%
            spread ("year","Val") %>% as.data.frame()
      
      ##creando variable de scenario
      sce<- unique(k[[i]]$scenario)
      sce<- substring(sce, 9)
      s<- as.data.frame(table(sce))
      # u<- dim(s)[1]
      s<- s %>% filter(.,Freq==5 )
      
      # o=1
      f<-list()
      for(o in 1:NROW(s)){
            tm<- sce[o]
            d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
            d<- rbind(d, df)
            ref<- d$scenario[grep(pattern = "WB_NoCC_SSP", x = d$scenario)]
            ref<- unique(ref)
            d<- data.frame(d,"Cat"=ifelse(d$scenario==ref,"NoCC","CC"))
            d<-d[,-c(6:20)]
            
            d<- d %>% gather(year, val,6:36)
            d$year<- gsub(pattern = "X",replacement = "",x = d$year)
            d$year<- as.numeric(d$year)
            
            d<- d %>% group_by(impactparameter,commodity,region,productiontype,
                               Cat,year) %>% summarise(mean=mean(val))%>% 
                  as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
                  mutate(perc_change=((CC-NoCC)/NoCC)*100)
            
            d$region<- revalue(d$region,c("SSA-Ivory Coast"="Ivory Coast",
                                          "SSA-Mali"="Mali"))
            d$productiontype<- revalue(d$productiontype,c("air"="Irrigated",
                                                          "arf"="Rain-fed"))
            ssp<- substring(tm,8)
            d$ssp<- ssp
            rcp<- substring(tm,1,6)
            d$rcp<- rcp
            f[[o]]<- d
      }
      
      ### apilar resultados  
      bb<- do.call(rbind, f)
      bb<- as.data.frame(bb)
      
      write.csv(bb,paste("./tables/perc_changeByYear_", datasys[i],"_SSP3.csv"))
      
      
      
      print(i)
}  



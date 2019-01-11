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
suppressMessages(library(ff))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))


### directories
#Definir directorio de trabajo
# setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB")
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/RTB2")
yuca<- readRDS("./Y.rds")

### groups scenarios
g1<- c("_SSP1"); g2<- c("_SSP2"); g3<- c("_SSP3"); g4<- c("_SSP4");g5<- c("_SSP5")
ssps_eco<- list(g1,g2,g3,g4,g5)


### groups of variables 
# datos por sistema de produccion  riego y secano
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )
# datos categorias totales
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area", "QSXAgg -- Total Production")
# datos categorias agregados 
dataagg<- c("FoodAvailXAgg", "QDXAgg -- Total Demand")
# demandas
dataDemad<- c( "QFXAgg -- Household Demand",
               "QOTHRXAgg -- Other Demand",  
               "QBFXAgg -- Biofuel Feedstock Demand",
               "QLXAgg -- Livestock Feed Demand" )

################### TOPICS ################################################
ca<- c("RCP4.5_SSP2", "RCP8.5_SSP2", "RCP6.0_SSP2","RCP2.6_SSP2")

cores<- detectCores()
cl<- makeCluster(cores[1]-27)
registerDoParallel(cl)

############################################### datatotal ########################################
foreach(x=1:length(ssps_eco)) %dopar% {
      require(dplyr)
      require(tidyr)
      a<- yuca[grep(pattern=ssps_eco[[x]], x = yuca$scenario),]
      
      ### convirtiendo en archivo data
      a$impactparameter<- as.character(a$impactparameter)
      a$scenario<- as.character(a$scenario)
      a$commodity<- as.character(a$commodity)
      a$region<- as.character(a$region)
      a$productiontype<- as.character(a$productiontype)
      ################################### nocc #####################################
      nocc<- a$scenario[grep(pattern = "WB_NoCC_SSP", x = a$scenario)]
      nocc<- unique(nocc)
      
      no<- a %>% filter(scenario %in% nocc)
      no <- no[!duplicated(no[c('impactparameter',"scenario", 
                                "commodity","year" ,"Val")]),]
      no$year<- as.numeric(no$year)
      k<- list()
      r<- list()
      
      i=1
      for(i in seq_along(datatotal)){
            
            # selecciono el cultivo
            k[[i]]<- a[which(a$impactparameter==datatotal[i]),]
            # filtro por numero de ref
            ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
            ref<- unique(ref)
            
            
            nocc<- k[[i]] %>% filter(scenario %in% ref)
            df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                          "commodity","year" ,"Val")]),]
            df$year<- as.numeric(df$year)
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
                  d<-d[,-c(6:20)]
                  d<- d %>% gather(year, val,6:36)
                  d$year<- gsub(pattern = "X",replacement = "",x = d$year)
                  d$year<- as.numeric(d$year)
                  
                  d<- d %>% group_by(impactparameter,commodity,region,
                                     Cat,year) %>% summarise(mean=mean(val))%>% 
                        as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
                        mutate(perc_change=((CC-NoCC)/NoCC)*100)
                  
                  ssp<- substring(tm,8)
                  d$ssp<- ssp
                  climate<- substring(tm,1,6)
                  d$rcp<- climate
                  f[[o]]<- d
            }
            
            ### apilar resultados  
            bb<- do.call(rbind, f)
            bb<- as.data.frame(bb)
            
            write.csv(bb,paste("./tables/perc_changeByYear_", datatotal[i],"_",ssps_eco[[x]],".csv", sep = ""))
            
            
            
            print(i)
      }  
      rm(bb,d,ref,s,a)
      
}
stopCluster(cl) 

############################################### dataagg ########################################
cores<- detectCores()
cl<- makeCluster(cores[1]-27)
registerDoParallel(cl)
#x=1
foreach(x=1:length(ssps_eco)) %dopar% {
      require(dplyr)
      require(tidyr)
      a<- yuca[grep(pattern=ssps_eco[[x]], x = yuca$scenario),]
      
      ### convirtiendo en archivo data
      a$impactparameter<- as.character(a$impactparameter)
      a$scenario<- as.character(a$scenario)
      a$commodity<- as.character(a$commodity)
      a$region<- as.character(a$region)
      a$productiontype<- as.character(a$productiontype)
      ################################### nocc #####################################
      nocc<- a$scenario[grep(pattern = "WB_NoCC_SSP", x = a$scenario)]
      nocc<- unique(nocc)
      
      no<- a %>% filter(scenario %in% nocc)
      no <- no[!duplicated(no[c('impactparameter',"scenario", 
                                "commodity","year" ,"Val")]),]
      no$year<- as.numeric(no$year)
      k<- list()
      r<- list()
      
      #i=1
      for(i in seq_along(dataagg)){
            
            # selecciono el cultivo
            k[[i]]<- a[which(a$impactparameter==dataagg[i]),]
            # filtro por numero de ref
            ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
            ref<- unique(ref)
            
            
            nocc<- k[[i]] %>% filter(scenario %in% ref)
            df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                          "commodity","year" ,"Val")]),]
            df$year<- as.numeric(df$year)
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
            
            #o=1
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
                  
                  d<- d %>% group_by(impactparameter,commodity,region,
                                     Cat,year) %>% summarise(mean=mean(val))%>% 
                        as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
                        mutate(perc_change=((CC-NoCC)/NoCC)*100)
                  
                  ssp<- substring(tm,8)
                  d$ssp<- ssp
                  climate<- substring(tm,1,6)
                  d$rcp<- climate
                  f[[o]]<- d
            }
            
            ### apilar resultados  
            bb<- do.call(rbind, f)
            bb<- as.data.frame(bb)
            
            write.csv(bb,paste("./tables/perc_changeByYear_", dataagg[i],"_",ssps_eco[[x]],".csv", sep = ""))
            
            
            
            print(i)
      }  
      rm(bb,d,ref,s,a)
      
}
stopCluster(cl) 

############################################### datademand ########################################
cores<- detectCores()
cl<- makeCluster(cores[1]-27)
registerDoParallel(cl)
#x=1
foreach(x=1:length(ssps_eco)) %dopar% {
      require(dplyr)
      require(tidyr)
      a<- yuca[grep(pattern=ssps_eco[[x]], x = yuca$scenario),]
      
      ### convirtiendo en archivo data
      a$impactparameter<- as.character(a$impactparameter)
      a$scenario<- as.character(a$scenario)
      a$commodity<- as.character(a$commodity)
      a$region<- as.character(a$region)
      a$productiontype<- as.character(a$productiontype)
      ################################### nocc #####################################
      nocc<- a$scenario[grep(pattern = "WB_NoCC_SSP", x = a$scenario)]
      nocc<- unique(nocc)
      
      no<- a %>% filter(scenario %in% nocc)
      no <- no[!duplicated(no[c('impactparameter',"scenario", 
                                "commodity","year" ,"Val")]),]
      no$year<- as.numeric(no$year)
      k<- list()
      r<- list()
      
      #i=1
      for(i in seq_along(dataDemad)){
            
            # selecciono el cultivo
            k[[i]]<- a[which(a$impactparameter==dataDemad[i]),]
            # filtro por numero de ref
            ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
            ref<- unique(ref)
            
            
            nocc<- k[[i]] %>% filter(scenario %in% ref)
            df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                          "commodity","year" ,"Val")]),]
            df$year<- as.numeric(df$year)
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
            
            #o=1
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
                  
                  d<- d %>% group_by(impactparameter,commodity,region,
                                     Cat,year) %>% summarise(mean=mean(val))%>% 
                        as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)%>%
                        mutate(perc_change=((CC-NoCC)/NoCC)*100)
                  
                  ssp<- substring(tm,8)
                  d$ssp<- ssp
                  climate<- substring(tm,1,6)
                  d$rcp<- climate
                  f[[o]]<- d
            }
            
            ### apilar resultados  
            bb<- do.call(rbind, f)
            bb<- as.data.frame(bb)
            
            write.csv(bb,paste("./tables/perc_changeByYear_", dataDemad[i],"_",ssps_eco[[x]],".csv", sep = ""))
            
            
            
            print(i)
      }  
      rm(bb,d,ref,s,a)
      
}
stopCluster(cl) 

rm(k,no, nocc,r,cl,df,f)


############################################# procesamiento ######################################
############## load data
xx<- list.files( path = "./tables/",pattern ="csv", full.names = T)
kk<- unlist(xx)

k<- kk[grep(pattern="_SSP2", x = kk)]
ssp2<- lapply(k,read.csv) ### load data
k<- kk[grep(pattern="_SSP4", x = kk)]
ssp4<- lapply(k,read.csv) ### load data
k<- kk[grep(pattern="_SSP3", x = kk)]
ssp3<- lapply(k,read.csv) ### load data
k<- kk[grep(pattern="_SSP5", x = kk)]
ssp5<- lapply(k,read.csv) ### load data

### convirtiendo en archivo data
ssp2<- do.call(rbind, ssp2)
ssp2$ssp<- "SSP2"
ssp3<- do.call(rbind, ssp3)
ssp3$ssp<- "SSP3"
ssp4<- do.call(rbind, ssp4)
ssp4$ssp<- "SSP4"
ssp5<- do.call(rbind, ssp5)
ssp5$ssp<- "SSP5"
l.sce<- list(ssp2=ssp2,ssp3=ssp3,ssp4=ssp4,ssp5=ssp5)


############################################ datatotal ############################################
for(i in 1:length(datatotal)){
      
      xx<- list.files( path ="./tables/",pattern ="perc_changeByYear_", full.names = T)
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
      
      write.csv(xx,paste("./pic/",datatotal[i], "_tableRTB.csv", sep = ""))
      
}

############################################ dataagg ############################################
for(i in 1:length(dataagg)){
      
      xx<- list.files( path ="./tables/",pattern ="perc_changeByYear_", full.names = T)
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
      
            write.csv(xx,paste("./pic/",dataagg[i], "_tableRTB.csv", sep = ""))
      
}

############################################ datademand ############################################
for(i in 1:length(dataDemad)){
      
      xx<- list.files( path ="./tables/",pattern ="perc_changeByYear_", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern=dataDemad[i], x = kk)]
      
      
      pp<- lapply(kk,read.csv) ### load data
      
      ### convirtiendo en archivo data
      xx<- do.call(rbind, pp)
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      yr<- c('2030','2040', '2050')
      xx<- xx %>% select(impactparameter,commodity,region,year,perc_change,sce) %>% spread(sce,perc_change) %>%filter(year %in% yr)
      
      write.csv(xx,paste("./pic/",dataDemad[i], "_tableRTB.csv", sep = ""))
      
}




################################### values totales #########################################
cores<- detectCores()
cl<- makeCluster(cores[1]-27)
registerDoParallel(cl)
# x=1
foreach(x=1:length(ssps_eco)) %dopar% {
      require(dplyr)
      require(tidyr)
      a<- yuca[grep(pattern=ssps_eco[[x]], x = yuca$scenario),]
      
      ### convirtiendo en archivo data
      a$impactparameter<- as.character(a$impactparameter)
      a$scenario<- as.character(a$scenario)
      a$commodity<- as.character(a$commodity)
      a$region<- as.character(a$region)
      a$productiontype<- as.character(a$productiontype)
      ################################### nocc #####################################
      nocc<- a$scenario[grep(pattern = "WB_NoCC_SSP", x = a$scenario)]
      nocc<- unique(nocc)
      
      no<- a %>% filter(scenario %in% nocc)
      no <- no[!duplicated(no[c('impactparameter',"scenario", 
                                "commodity","year" ,"Val")]),]
      no$year<- as.numeric(no$year)
      k<- list()
      r<- list()
      
      # i=1
      for(i in seq_along(dataDemad)){
            
            # selecciono el cultivo
            k[[i]]<- a[which(a$impactparameter==dataDemad[i]),]
            # filtro por numero de ref
            ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
            ref<- unique(ref)
            
            
            nocc<- k[[i]] %>% filter(scenario %in% ref)
            df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                          "commodity","year" ,"Val")]),]
            df$year<- as.numeric(df$year)
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
                  d<-d[,-c(6:20)]
                  d<- d %>% gather(year, val,6:36)
                  d$year<- gsub(pattern = "X",replacement = "",x = d$year)
                  d$year<- as.numeric(d$year)
                  
                  d<- d %>% group_by(impactparameter,commodity,region,
                                     Cat,year) %>% summarise(mean=mean(val))%>% 
                        as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)
                  # %>%
                  #       mutate(perc_change=((CC-NoCC)/NoCC)*100)
                  
                  ssp<- substring(tm,8)
                  d$ssp<- ssp
                  climate<- substring(tm,1,6)
                  d$rcp<- climate
                  f[[o]]<- d
            }
            
            ### apilar resultados  
            bb<- do.call(rbind, f)
            bb<- as.data.frame(bb)
            
            write.csv(bb,paste("./tables/ValueByYear_", dataDemad[i],"_",ssps_eco[[x]],".csv", sep = ""))
            
            
            
            print(i)
      }  
      rm(bb,d,ref,s,a)
      
}
stopCluster(cl) 

rm(k,no, nocc,r,cl,df,f)

# i=1
for(i in 1:length(dataDemad)){
      
      xx<- list.files( path ="./tables/",pattern ="Value", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern=dataDemad[i], x = kk)]
      
      
      pp<- lapply(kk,read.csv) ### load data
      
      ### convirtiendo en archivo data
      xx<- do.call(rbind, pp)
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      # yr<- c('2030','2040', '2050')
      xx<- xx %>% select(impactparameter,commodity,region,year,CC, NoCC,sce)  
      # %>%filter(year %in% yr)
      # %>% spread(sce,perc_change)
      write.csv(xx,paste("./pic/",dataDemad[i], "_Value_tableRTB.csv", sep = ""))
      
}

################################ production #############################################
cores<- detectCores()
cl<- makeCluster(cores[1]-27)
registerDoParallel(cl)
# x=1
foreach(x=1:length(ssps_eco)) %dopar% {
      require(dplyr)
      require(tidyr)
      a<- yuca[grep(pattern=ssps_eco[[x]], x = yuca$scenario),]
      
      ### convirtiendo en archivo data
      a$impactparameter<- as.character(a$impactparameter)
      a$scenario<- as.character(a$scenario)
      a$commodity<- as.character(a$commodity)
      a$region<- as.character(a$region)
      a$productiontype<- as.character(a$productiontype)
      ################################### nocc #####################################
      nocc<- a$scenario[grep(pattern = "WB_NoCC_SSP", x = a$scenario)]
      nocc<- unique(nocc)
      
      no<- a %>% filter(scenario %in% nocc)
      no <- no[!duplicated(no[c('impactparameter',"scenario", 
                                "commodity","year" ,"Val")]),]
      no$year<- as.numeric(no$year)
      k<- list()
      r<- list()
      
      # i=1
      for(i in seq_along(datatotal)){
            
            # selecciono el cultivo
            k[[i]]<- a[which(a$impactparameter==datatotal[i]),]
            # filtro por numero de ref
            ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
            ref<- unique(ref)
            
            
            nocc<- k[[i]] %>% filter(scenario %in% ref)
            df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                          "commodity","year" ,"Val")]),]
            df$year<- as.numeric(df$year)
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
                  d<-d[,-c(6:20)]
                  d<- d %>% gather(year, val,6:36)
                  d$year<- gsub(pattern = "X",replacement = "",x = d$year)
                  d$year<- as.numeric(d$year)
                  
                  d<- d %>% group_by(impactparameter,commodity,region,
                                     Cat,year) %>% summarise(mean=mean(val))%>% 
                        as.data.frame() %>% spread(Cat,mean) %>% mutate(sce=tm)
                  # %>%
                  #       mutate(perc_change=((CC-NoCC)/NoCC)*100)
                  
                  ssp<- substring(tm,8)
                  d$ssp<- ssp
                  climate<- substring(tm,1,6)
                  d$rcp<- climate
                  f[[o]]<- d
            }
            
            ### apilar resultados  
            bb<- do.call(rbind, f)
            bb<- as.data.frame(bb)
            
            write.csv(bb,paste("./tables/ValueByYear_", datatotal[i],"_",ssps_eco[[x]],".csv", sep = ""))
            
            
            
            print(i)
      }  
      rm(bb,d,ref,s,a)
      
}
stopCluster(cl) 

rm(k,no, nocc,r,cl,df,f)

# i=1
for(i in 1:length(datatotal)){
      
      xx<- list.files( path ="./tables/",pattern ="Value", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern=datatotal[i], x = kk)]
      
      
      pp<- lapply(kk,read.csv) ### load data
      
      ### convirtiendo en archivo data
      xx<- do.call(rbind, pp)
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      # yr<- c('2030','2040', '2050')
      xx<- xx %>% select(impactparameter,commodity,region,year,CC, NoCC,sce)  
      # %>%filter(year %in% yr)
      # %>% spread(sce,perc_change)
      write.csv(xx,paste("./pic/",datatotal[i], "_Value_tableRTB.csv", sep = ""))
      
}
#### cluster analysis 
#### county Kenya.
g=gc;rm(list=ls())

###library--------
options(warn=-1); options(scipen = 999)
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(ncdf)){install.packages('ncdf'); library(ncdf)} else {library(ncdf)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(ff)){install.packages('ff'); library(ff)} else {library(ff)})
suppressMessages(if(!require(data.table)){install.packages('data.table'); library(data.table)} else {library(data.table)})
suppressMessages(if(!require(miscTools)){install.packages('miscTools'); library(miscTools)} else {library(miscTools)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})

### directory
setwd("//dapadfs/workspace_cluster_12/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/")
output<- c("//dapadfs/workspace_cluster_12/Kenya_KACCAL/OsiemoCarlosBrayan/")
### counties
countyList <- data.frame(Cluster=rep('Cluster 1', 31),county=c('Bomet', 'Kericho', 'Kakamega','Baringo',
                                                                'Baringo', 'Laikipia', 'Tharaka',
                                                                'Lamu','Marsabit', 'Isiolo', 'Wajir', 
                                                                'Garissa','Kwale', 'Makueni', 
                                                               'Taita_Taveta', 'Embu', 'Meru',
                                                                'Nyeri', 'Nyandarua', 'Nakuru',
                                                                'West_Pokot', 'Kajiado','keiyo-Marakwet',
                                                               'kisumu','Machakos','Uasin_Gishu','Busia',
                                                               'Homa_Bay', 'Mandera', 'Siaya', 'Tana_River'))


countyList$Cluster <- as.character(countyList$Cluster)
countyList$county <- as.character(countyList$county)


### shape files
shp <- readShapeSpatial(paste('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/', countyList$county[[i]], '.shp', sep=''),
                        proj4string=CRS("+proj=longlat +datum=WGS84"))

# modelos de clima
gcmsList<- data.frame(Cluster=rep('sce', 11), models=c("bcc_csm1_1","bcc_csm1_1_m",
                                                       "csiro_mk3_6_0","gfdl_cm3",
                                                       "gfdl_esm2g","gfdl_esm2m",
                                                       "ipsl_cm5a_mr", "miroc_esm",
                                                       "miroc_esm_chem", "miroc_miroc5",
                                                       "ncc_noresm1_m"))
gcmsList$Cluster <- as.character(gcmsList$Cluster)
gcmsList$models <- as.character(gcmsList$models)

# periodo 
timeList<- data.frame(Cluster=rep('periodo', 2), timep=c("2021_2045", "2041_2065"))
timeList$Cluster <- as.character(timeList$Cluster)
timeList$timep <- as.character(timeList$timep)

# rcps 
rcpsList<- data.frame(Cluster=rep('ssp', 4), rcp=c("rcp26", "rcp45","rcp60", "rcp85"))
rcpsList$Cluster <- as.character(rcpsList$Cluster)
rcpsList$rcp <- as.character(rcpsList$rcp)
yr<- c(paste("20", seq(from=21, to=45, by=1),sep = ""))
### load data for period 2021/2045
m=1 #models
r=1 #rcp
i=1 #counties



cfiles<- list()
total<- list() 
library(foreach)
library(doParallel)
cores<- detectCores()
cl<- makeCluster(cores[1]-1)
registerDoParallel(cl)

for(m in 1:length(gcmsList$models)){
#       for(r in 1:length(rcpsList$rcp)){
      
              for(i in 1:length(countyList$county)){
                    cores<- detectCores()
                    cl<- makeCluster(cores[1]-1)
                    registerDoParallel(cl)
                    
               cat(paste0(">>>> Procesed by models and counties ",gcmsList$models[m],"\n"))    
                  
            # cargando datos 
                 load(paste("//dapadfs/workspace_cluster_12/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/",
                            gcmsList$models[[m]],"/2021_2045/","rcp26","/",
                            countyList$county[[i]],"/prec/bc_qmap_prec_2021_2045.RData",sep = ""))
                 gcmFutBC<- as.data.frame(gcmFutBC)
                 vv<- foreach(a=1:nrow(gcmFutBC)) %dopar% {  # function(a){
                       require(dplyr)
                       require(tidyr)
                       require(lubridate) 
                       time.serie <- gcmFutBC[gcmFutBC$cellID[a], 1:(ncol(gcmFutBC)-3)] 
                       X <- time.serie; rm(time.serie) 
                       X <- X %>% gather(key = Date, value = Value, -(cellID:lat)) 
                       X$Year <- lubridate::year(as.Date(X$Date)) # to extract data by año 
                       X$Yday <- lubridate::yday(as.Date(X$Date)) 
                       X <- X %>% group_by(Year) %>% mutate(., TotalRain=sum(Value))
                       X$gcm<- paste(gcmsList$models[[m]])
                       X$county<- paste(countyList$county[[i]])
                       X$rcp<- "rcp26"
                       Xfinal<- X %>% select(.,cellID,lon, lat, Year,TotalRain,gcm,county,rcp)
                       Xfinal<- Xfinal[!duplicated(Xfinal),]
                       # cfiles[[a]]<- Xfinal
                 }
                     
                  stopCluster(cl)     
                       
                  cfiles[[i]]<- do.call(rbind, vv)
                       
                  cat(paste0(">>>> done !!!! ",countyList$county[i],"\n"))    
                  
                 
              }
                  
      tempfiles<- do.call(rbind,cfiles)
      cfiles[[m]]<- list(tempfiles)
      
#       
#       # convirtiendo matrix en data frame by pixel
#             gcmFutBC<- as.data.frame(gcmFutBC)
#             time.serie <- gcmFutBC[gcmFutBC$cellID[1], 1:(ncol(gcmFutBC)-3)] 
#             X <- time.serie; rm(time.serie) 
#             X <- X %>% gather(key = Date, value = Value, -(cellID:lat)) 
#             X$Year <- lubridate::year(as.Date(X$Date)) # to extract data by año 
#             X$Yday <- lubridate::yday(as.Date(X$Date)) 
#             X <- X %>% group_by(Year) %>% mutate(., TotalRain=sum(Value))
#             X$gcm<- paste(gcmsList$models[[m]])
#             X$county<- paste(countyList$county[[i]])
#             X$rcp<- paste(rcpsList$rcp[[r]])
#             Xfinal<- X %>% select(.,cellID,lon, lat, Year,TotalRain,gcm,county,rcp)
#             Xfinal<- Xfinal[!duplicated(Xfinal),]
#             
      cat(paste("Ready",gcmsList$models[[m]], " and ","rcp26"," and\n",
                countyList$county[[i]], "was done!!!\n"),sep = "")
      
                    
                    
#             }  
      } 




# 5. TOTRAIN: Total precipitation 
time.serie <- prec[which(prec$cellID == pixelList[i]), 1:(ncol(prec)-3)] 
X <- time.serie; rm(time.serie) 
X <- X %>% gather(key = Date, value = Value, -(cellID:lat)) 
X$Year <- lubridate::year(as.Date(X$Date)) 
X$Yday <- lubridate::yday(as.Date(X$Date)) 
         
totrain <- X %>% dplyr::group_by(Year) %>% dplyr::arrange(Date) %>% summarise(TOTRAIN = sum(Value)) 
totrain <- totrain %>% as.data.frame 
names(totrain)[2] <- "Value"; totrain$Variable <- "TOTRAIN" 
          
# 6. CDD: Drought spell: Maximum number of consecutive dry days (i.e. with precipitation < 1 mm day-1) 
dr_stress <- function(PREC, p_thresh = 1){ 
runs <- rle(PREC < p_thresh) 
cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE) 
return(cons_days) 
} 
dr_stressCMP <- cmpfun(dr_stress); rm(dr_stress) 
cdd <- X %>% dplyr::group_by(Year) %>% dplyr::arrange(Date) %>% summarise(CDD = dr_stressCMP(Value, p_thresh = 1)) 
cdd <- cdd %>% as.data.frame 
names(cdd)[2] <- "Value"; cdd$Variable <- "CDD" 


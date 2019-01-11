##### PAPER SAN 
##### Autores Guillermo & Carlos Edo

g=gc;rm(list = ls())

### decimals
options(warn = -1)
options(scipen = 999)

### directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/Paper SAN")
graph<- c("pic/")

### librerias
library(ggplot2); library(dplyr); library(tidyr)
suppressMessages(library(tseries)) ### codigo para eliminar mensajes
suppressMessages(library(RColorBrewer))
suppressMessages(library(gridExtra))
### archivo madre
cfiles<- read.csv("lamp_native_20180725-224247.csv")
xx<-cfiles # respaldo


xx$MODEL<- as.character(xx$MODEL)
xx$SCENARIO<- as.character(xx$SCENARIO)
xx$REGION<- as.character(xx$REGION)
xx$VARIABLE<- as.character(xx$VARIABLE)
xx$UNIT<- as.character(xx$UNIT)

### corregir y unificar nombres
var<- unique(xx$VARIABLE)

xx$REGION<- plyr::revalue(xx$REGION, c("Colombia"="COL",
                                       "Brazil"="BR",
                                       "Central America and Caribbean"="CAC",
                                       "cac"="CAC",
                                       "Argentina"="ARG",
                                       "clm"="COL",
                                       "Mexico"="MEX",
                                       "South America_Northern"="SUR_AN",
                                       "South America_Southern"="SUR_AS",
                                       "XLM"="RestLAC_ADAGE",
                                       "BRA"="BR",
                                       "mex"="MEX",
                                       "ola"="OtherLAC_PHOX",
                                       "ven"="VEN",
                                       "bra"="BR"))

reg<- unique(xx$REGION) 
length(reg)

lac<- c("ARG", "BR", "CAC", 
        "COL", "MEX", "SUR_AN", "SUR_AS", 
        "OtherLAC_PHOX","RestLAC_ADAGE", "VEN")
## scenarios 
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

#### lista de escenarios
listSce<- list(sce_cfe_tx,
               sce_cfe_notx,
               sce_nocfe_notx,
               sce_nocfe_tx) 

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

### eliminar paises innece
cc<- xx %>% dplyr::filter(REGION %in% lac)
unique(cc$REGION)

################################################################## SUBSETS ############################################
food<- cc[grep("Food Security", cc$VARIABLE),]

food<- list(food=food)

unique(food[[1]]$SCENARIO)
#### production, consumption, exports and imports--------------
i=1
lapply(1:length(tones), function(i){
      ## creating subset tipo variable
      cfiles<- tones[[i]]
      if(i==1){
            cfiles$parametro<- "Consumption"
      }else{}
      if(i==2){
            cfiles$parametro<- "Production"
      }else{}
      if(i==3){
            cfiles$parametro<- "Exports"
      }else{}
      if(i==4){
            cfiles$parametro<- "Imports"
      }else{}
      
      ### creating vectors
      variables<- unique(cfiles$UNIT)
      crops<- unique(cfiles$VARIABLE)
      sce<- unique(cfiles$SCENARIO)
      
      ### Creating subset filter  
      cfiles<- cfiles %>% dplyr:: filter(UNIT!="EJ/yr")%>% dplyr:: filter(VARIABLE!="Policy Cost|Consumption Loss") %>%
            dplyr::filter(UNIT!="unitless") %>% filter(VARIABLE!="unitless" )
      
      #### revalue variables
      colnames(cfiles)[4]<- "Crop"
      
      ### function manes
      source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/functionAjusteNames.R")
      cfiles<- ajusteNames(cfiles)
      
      ### Eliminate periods
      cfiles$X1990<- NULL; cfiles$X2004<- NULL
      cfiles<- cfiles %>% select(MODEL, SCENARIO,REGION, Crop, UNIT,X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050, parametro)
      
      
      ############################################# Trends Graph #####################################
      ### definiendo RCP
      cfiles$cat<- ifelse(grepl(pattern ="*_4p5_",x = cfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
      write.csv(cfiles,"./Results/ConsumoReview.csv")
      yy<- cfiles; yyy<- cfiles
      
      #       s=1
      for(s in 1:length(listSce)){
            xxx<- yy
            if(s==1){
                  xxx$sce<- "sce_cfe_tx"
            }else{}
            if(s==2){
                  xxx$sce<- "sce_cfe_notx"
            }else{}
            if(s==3){
                  xxx$sce<- "sce_nocfe_notx"
            }else{}
            if(s==4){
                  xxx$sce<- "sce_nocfe_tx"
            }else{}
            
            ### copia para el heatmap
            
            xxx<- xxx[c("MODEL","SCENARIO","REGION","Crop","UNIT","parametro","cat","sce" ,"X2020","X2025","X2030","X2035", "X2040","X2045","X2050")]    
            ytest45 <- xxx %>% filter(.,SCENARIO %in% listSce[[s]]) %>% gather(yr,val,9:ncol(xxx)) %>% 
                  filter(.,UNIT!="million US$2005/yr" ) %>% filter(., UNIT!="billion US$2005/yr")
            ytest45$yr<- sub(pattern = "X",replacement = "", x = ytest45$yr)
            ytest45$yr<- as.integer(ytest45$yr)
            ytest45$yr<- as.numeric(ytest45$yr) 
            
            ytest45$UNIT<- NULL
            datmin<- ytest45 %>% group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% summarize(datamin=min(val,na.rm=T))
            datmed<- ytest45 %>% group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% summarize(datmed=median(val,na.rm=T))
            datmax<- ytest45 %>% group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% summarize(datmax=max(val,na.rm=T))
            
            extremos<- merge(datmin,datmax)
            
            datost<- merge(extremos,datmed)
            datost<- datost[!is.infinite(datost$datamin),]
            
            write.csv(datost,paste("./Results/dataTotal_",unique(xxx$sce),"_",unique(ytest45$parametro),".csv", sep = ""))
            pots<- unique(xxx$REGION)
            py<- NULL
            
            #       p=1
            for(p in 1:length(pots)) {
                  
                  datax<- datost %>% filter(REGION==pots[p]) 
                  
                  tiff(filename= paste("pic/",unique(yy$parametro),"_",pots[p],"_",unique(xxx$sce),"_TrendModels.tiff",sep=""), 
                       width = 10, height = 7, units = 'in', res = 100)
                  
                  
                  py[[i]]<-print(ggplot(datax, aes(yr,datmed,group=Crop,color=Crop)) + 
                                       geom_line(linetype="dashed",size=1)+ facet_grid(cat~MODEL)+ 
                                       geom_ribbon(aes(ymin=datamin,ymax=datmax,
                                                       fill=Crop,colour=Crop,linetype=NA),
                                                   alpha=0.1) + 
                                       labs(title=paste("Parameter= ",unique(datax$parametro),"\nRegion= ", pots[p],
                                                        "\nScenario= ",unique(datax$sce),sep = ""))+
                                       labs(y="MT",x="Year")+
                                       theme(legend.position="bottom")
                                 
                  )
                  
                  
                  dev.off()
                  print(i)
            }
            
      }
      
      
      
      
      ############################################# Percentage differences HeatMAP #####################################
      #       s=1
      
      for(s in 1:length(listSce)){
            xxy<- yyy
            if(s==1){
                  xxy$sce<- "sce_cfe_tx"
            }else{}
            if(s==2){
                  xxy$sce<- "sce_cfe_notx"
            }else{}
            if(s==3){
                  xxy$sce<- "sce_nocfe_notx"
            }else{}
            if(s==4){
                  xxy$sce<- "sce_nocfe_tx"
            }else{}
            
            if(i>=3){
                  xfiles<- xxy
                  var_crops<- unique(xfiles$Crop)
                  gross<- var_crops[grepl("gross", var_crops)]
                  xfiles<- xfiles[c("MODEL","SCENARIO","REGION","Crop", "UNIT","parametro","cat","sce","X2010", "X2020","X2025","X2030","X2035","X2040",
                                    "X2045","X2050")]
                  xfiles<- xfiles %>% filter(., UNIT=="Mt/yr") %>% filter(., Crop %in% gross)
                  xfiles<- xfiles %>% rowwise(.)%>% mutate(change= ((X2050 - X2010)/X2010)*100) 
                  
            }else{
                  xfiles<- xxy
                  xfiles<- xfiles[c("MODEL","SCENARIO","REGION","Crop", "UNIT","parametro","cat","sce","X2010", "X2020","X2025","X2030","X2035","X2040",
                                    "X2045","X2050")]
                  xfiles<- xfiles %>% filter(., UNIT=="Mt/yr")
                  xfiles<- xfiles %>% rowwise(.)%>% mutate(change= ((X2050 - X2010)/X2010)*100) 
                  
            }
            
            #### Escenario
            f1<- xfiles %>% filter(SCENARIO %in% listSce[[s]]) %>% group_by(MODEL, REGION,Crop,cat,sce, parametro) %>%
                  summarize(mean=mean(change,na.rm=T)) %>% as.data.frame() 
            
            write.csv(f1,paste("./Results/PercentageDiff_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
            f1<-f1[!is.na(f1$mean),]
            
            # use the function to identify outliers
            temp <- FindOutliers(f1$mean)
            cfOut<- f1[temp,]
            cfilesNEt<- f1[-temp,]
            #Exports outliers 
            write.csv(cfOut,paste("./Results/Outliers_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
            
            
            hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
            png(filename= paste("./pic/",unique(f1$parametro),"_",unique(cfilesNEt$sce),"_PercentageDifferences_HeapMap.png",sep=""), 
                width = 9, height = 11, units = 'in', res = 300)
            
            labs2 = 'Percentage\nDifferences %'
            
            n<- ggplot(data =cfilesNEt, aes(REGION, Crop)) + 
                  geom_tile(aes(fill = mean), colour = "white")+  facet_grid(cat~MODEL, drop = T)+
                  labs(x=NULL, y=NULL, 
                       title= paste("Scenario= ",unique(cfilesNEt$sce),"\nParameter= ",unique(f1$parametro),
                                    "\nPercentage differences from 2010 to 2050",sep = "")) +
                  scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
                  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +  coord_equal(91/100)+ 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                  theme_grey() + labs(x = "",y = "")+
                  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
                  theme(axis.text.y = element_text(hjust = 1, size = 11))+
                  theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
                  theme(strip.text=element_text(size=8))+
                  theme(strip.text.y = element_text(angle = 0,size = 11)) 
            
            
            plot(n)
            dev.off()  
            
            
      }
      
      cat(paste("finish ", unique(cfiles$parametro), " \nit was done!!!",sep = "" ))
      
      
})

#### Yield--------------

## creating subset tipo variable-------------
cfiles<- yield
cfiles$parametro<- "Yield"


### creating vectors
variables<- unique(cfiles$UNIT)
crops<- unique(cfiles$VARIABLE)
sce<- unique(cfiles$SCENARIO)

### Creating subset filter  
# cfiles<- cfiles %>% dplyr:: filter(UNIT!="EJ/yr")%>% dplyr:: filter(VARIABLE!="Policy Cost|Consumption Loss") %>%
#       dplyr::filter(UNIT!="unitless") %>% filter(VARIABLE!="unitless" )

#### revalue variables
colnames(cfiles)[4]<- "Crop"

### function manes
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/functionAjusteNames.R")
cfiles<- ajusteNamesYield(cfiles)


### Eliminate periods
# cfiles$UNIT<- NULL
cfiles$X1990<- NULL
cfiles$X2004<- NULL

cfiles<- cfiles %>% dplyr::select(MODEL, SCENARIO,REGION, Crop, UNIT,X2010,X2015,X2020,X2025,X2030,X2035,X2040,X2045,X2050, parametro)

cfiles[,c(6:14)]<- (cfiles[,c(6:14)]-1)*100

cfiles<- cfiles[c("MODEL","SCENARIO","REGION", "Crop","X2020", "X2025","X2030","X2035",
                  "X2040","X2045","X2050","parametro")]
############################################# Trends Graph #####################################
### definiendo RCP
cfiles$cat<- ifelse(grepl(pattern ="*_4p5_",x = cfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
yy<- cfiles; yyy<- cfiles

# s=1
for(s in 1:length(listSce)){
      xxx<- yy
      if(s==1){
            xxx$sce<- "sce_cfe_tx"
      }else{}
      if(s==2){
            xxx$sce<- "sce_cfe_notx"
      }else{}
      if(s==3){
            xxx$sce<- "sce_nocfe_notx"
      }else{}
      if(s==4){
            xxx$sce<- "sce_nocfe_tx"
      }else{}
      
      ### copia para el heatmap
      
      xxx<- xxx[c("MODEL","SCENARIO","REGION","Crop","parametro","cat","sce" ,
                  "X2020","X2025","X2030","X2035", "X2040","X2045","X2050")]    
      ytest45 <- xxx %>% filter(.,SCENARIO %in% listSce[[s]]) %>% gather(yr,val,8:ncol(xxx)) 
      
      ytest45$yr<- sub(pattern = "X",replacement = "", x = ytest45$yr)
      ytest45$yr<- as.integer(ytest45$yr)
      ytest45$yr<- as.numeric(ytest45$yr) 
      
      datmin<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarise(datamin= min(val,na.rm=T))
      datmed<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarize(datmed= median(val,na.rm=T))
      datmax<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarize(datmax= max(val,na.rm=T))
      
      extremos<- merge(datmin,datmax)
      
      datost<- merge(extremos,datmed)
      datost<- datost[!is.infinite(datost$datamin),]
      
      write.csv(datost,paste("./Results/dataTotal_",unique(xxx$sce),"_Yield",".csv", sep = ""))
      pots<- unique(xxx$REGION)
      py<- NULL
      
      #       p=1
      for(p in 1:length(pots)) {
            
            datax<- datost %>% filter(REGION==pots[p]) 
            
            tiff(filename= paste("pic/",unique(yy$parametro),"_",pots[p],"_",unique(xxx$sce),"_TrendModels.tiff",sep=""), 
                 width = 10, height = 7, units = 'in', res = 100)
            
            
            py[[p]]<-print(ggplot(datax, aes(yr,datmed,group=Crop,color=Crop)) + 
                                 geom_line(linetype="dashed",size=1)+ facet_grid(cat~MODEL)+ 
                                 geom_ribbon(aes(ymin=datamin,ymax=datmax,
                                                 fill=Crop,colour=Crop,linetype=NA),
                                             alpha=0.1) + 
                                 labs(title=paste("Parameter= ",unique(datost$parametro),"\nRegion= ", pots[p],
                                                  "\nScenario= ",unique(xxx$sce),sep = ""))+
                                 labs(y="MT",x="Year")+
                                 theme(legend.position="bottom")
                           
                           
            )
            
            
            dev.off()
      }
      
}


############################################# Percentage differences HeatMAP #####################################

for(s in 1:length(listSce)){
      xxy<- yyy
      if(s==1){
            xxy$sce<- "sce_cfe_tx"
      }else{}
      if(s==2){
            xxy$sce<- "sce_cfe_notx"
      }else{}
      if(s==3){
            xxy$sce<- "sce_nocfe_notx"
      }else{}
      if(s==4){
            xxy$sce<- "sce_nocfe_tx"
      }else{}
      
      xfiles<- xxy
      xfiles<- xfiles[c("MODEL","SCENARIO","REGION","Crop", "parametro", "cat","sce","X2020", "X2050")]
      xfiles<- xfiles %>% rowwise(.)%>% mutate(difpp= (X2050 - X2020)) 
      write.csv(xfiles, "./Results/YieldResults.csv")
      xfiles50<- xfiles
      xfiles<- xfiles %>% dplyr::select(MODEL,SCENARIO,REGION,Crop, parametro, cat,sce, difpp)
      
      #### Escenario
      f1<- xfiles %>% filter(SCENARIO %in% listSce[[s]]) %>% group_by(MODEL, REGION,Crop,cat,sce, parametro) %>%
            dplyr::summarize(mean=mean(difpp,na.rm=T)) %>% as.data.frame() 
      
      write.csv(f1,paste("./Results/Diffpp_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
      
      #             # use the function to identify outliers
      #             temp <- FindOutliers(f1$mean)
      #             cfOut<- f1[temp,]
      #             cfilesNEt<- f1[-temp,]
      #             #Exports outliers 
      #             write.csv(cfOut,paste("./Results/Outliers_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
      #             
      #             
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename= paste("./pic/",unique(f1$parametro),"_",unique(f1$sce),"_Differences_HeapMap.png",sep=""), 
          width = 9, height = 11, units = 'in', res = 300)
      
      labs2 = 'Percentage Points (pp)'
      
      n<- ggplot(data =f1, aes(REGION, Crop)) + 
            geom_tile(aes(fill = mean), colour = "white")+  facet_grid(cat~MODEL, drop = T)+
            labs(x=NULL, y=NULL, 
                 title= paste("Scenario= ",unique(f1$sce),"\nParameter= ",unique(f1$parametro),
                              "\n differences pp from 2020 to 2050",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +  coord_equal(91/100)+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_grey() + labs(x = "",y = "")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) 
      
      
      plot(n)
      dev.off()  
      
      #### percentage 2050
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename= paste("./pic/",unique(f1$parametro),"_",unique(f1$sce),"_Differences_HeapMap.png",sep=""), 
          width = 9, height = 11, units = 'in', res = 300)
      
      labs2 = '%'
      
      n<- ggplot(data = xfiles50 %>% dplyr::select(MODEL,SCENARIO,REGION, Crop, parametro,cat,sce,X2050)%>%
                       filter(SCENARIO %in% listSce[[s]]), 
                 aes(REGION, Crop)) + 
            geom_tile(aes(fill = X2050), colour = "white")+  facet_grid(cat~MODEL, drop = T)+
            labs(x=NULL, y=NULL, 
                 title= paste("Scenario= ",unique(xfiles50$sce),"\nParameter= ",unique(xfiles50$parametro),
                              "\nPercentage change by 2050",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +  coord_equal(91/100)+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_grey() + labs(x = "",y = "")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) 
      
      
      plot(n)
      dev.off()  
      
      
}

#### Area--------------

## creating subset tipo variable-------------
cfiles<- land
cfiles$parametro<- "Land"


### creating vectors
variables<- unique(cfiles$UNIT)
crops<- unique(cfiles$VARIABLE)
sce<- unique(cfiles$SCENARIO)

### Creating subset filter  
# cfiles<- cfiles %>% dplyr:: filter(UNIT!="EJ/yr")%>% dplyr:: filter(VARIABLE!="Policy Cost|Consumption Loss") %>%
#       dplyr::filter(UNIT!="unitless") %>% filter(VARIABLE!="unitless" )

#### revalue variables
colnames(cfiles)[4]<- "Crop"

### function manes
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/functionAjusteNames.R")
cfiles<- ajustNamesLand(cfiles)


### Eliminate periods
# cfiles$UNIT<- NULL
cfiles$X1990<- NULL
cfiles$X2004<- NULL

cfiles<- cfiles %>% dplyr::select(MODEL, SCENARIO,REGION, Crop, UNIT,X2010,X2015,X2020,X2025,X2030,X2035,X2040,X2045,X2050, parametro)
cfiles<- cfiles[c("MODEL","SCENARIO","REGION", "Crop","X2020", "X2025","X2030","X2035",
                  "X2040","X2045","X2050","parametro")]
############################################# Trends Graph #####################################
### definiendo RCP
cfiles$cat<- ifelse(grepl(pattern ="*_4p5_",x = cfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
yy<- cfiles; yyy<- cfiles

# s=1
for(s in 1:length(listSce)){
      xxx<- yy
      if(s==1){
            xxx$sce<- "sce_cfe_tx"
      }else{}
      if(s==2){
            xxx$sce<- "sce_cfe_notx"
      }else{}
      if(s==3){
            xxx$sce<- "sce_nocfe_notx"
      }else{}
      if(s==4){
            xxx$sce<- "sce_nocfe_tx"
      }else{}
      
      ### copia para el heatmap
      
      xxx<- xxx[c("MODEL","SCENARIO","REGION","Crop","parametro","cat","sce" ,
                  "X2020","X2025","X2030","X2035", "X2040","X2045","X2050")]    
      ytest45 <- xxx %>% filter(.,SCENARIO %in% listSce[[s]]) %>% gather(yr,val,8:ncol(xxx)) 
      
      ytest45$yr<- sub(pattern = "X",replacement = "", x = ytest45$yr)
      ytest45$yr<- as.integer(ytest45$yr)
      ytest45$yr<- as.numeric(ytest45$yr) 
      
      datmin<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarise(datamin= min(val,na.rm=T))
      datmed<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarize(datmed= median(val,na.rm=T))
      datmax<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarize(datmax= max(val,na.rm=T))
      
      extremos<- merge(datmin,datmax)
      
      datost<- merge(extremos,datmed)
      datost<- datost[!is.infinite(datost$datamin),]
      
      write.csv(datost,paste("./Results/dataTotal_",unique(xxx$sce),"_Land",".csv", sep = ""))
      pots<- unique(xxx$REGION)
      py<- NULL
      
      #       p=1
      for(p in 1:length(pots)) {
            
            datax<- datost %>% filter(REGION==pots[p]) 
            
            tiff(filename= paste("pic/",unique(yy$parametro),"_",pots[p],"_",unique(xxx$sce),"_TrendModels.tiff",sep=""), 
                 width = 10, height = 7, units = 'in', res = 100)
            
            
            py[[p]]<-print(ggplot(datax, aes(yr,datmed,group=Crop,color=Crop)) + 
                                 geom_line(linetype="dashed",size=1)+ facet_grid(cat~MODEL)+ 
                                 geom_ribbon(aes(ymin=datamin,ymax=datmax,
                                                 fill=Crop,colour=Crop,linetype=NA),
                                             alpha=0.1) + 
                                 labs(title=paste("Parameter= ",unique(datost$parametro),"\nRegion= ", pots[p],
                                                  "\nScenario= ",unique(xxx$sce),sep = ""))+
                                 labs(y="MT",x="Year")+
                                 theme(legend.position="bottom")
                           
                           
            )
            
            
            dev.off()
      }
      
}


############################################# Percentage differences HeatMAP #####################################
# s=1
for(s in 1:length(listSce)){
      xxy<- yyy
      if(s==1){
            xxy$sce<- "sce_cfe_tx"
      }else{}
      if(s==2){
            xxy$sce<- "sce_cfe_notx"
      }else{}
      if(s==3){
            xxy$sce<- "sce_nocfe_notx"
      }else{}
      if(s==4){
            xxy$sce<- "sce_nocfe_tx"
      }else{}
      
      xfiles<- xxy
      #       xfiles<- xfiles[c("MODEL","SCENARIO","REGION","Crop", "parametro", "cat","sce","X2020", "X2050")]
      xfiles<- xfiles %>% rowwise(.)%>% mutate(change= ((X2050 - X2020)/X2020)*100) 
      write.csv(xfiles, "./Results/LandResults.csv")
      xfiles<- xfiles %>% dplyr::select(MODEL,SCENARIO,REGION,Crop, parametro, cat,sce, change)
      
      #### Escenario
      f1<- xfiles %>% filter(SCENARIO %in% listSce[[s]]) %>% group_by(MODEL, REGION,Crop,cat,sce, parametro) %>%
            dplyr::summarize(mean=mean(change,na.rm=T)) %>% as.data.frame() 
      
      write.csv(f1,paste("./Results/Perchange_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
      
      #eliminando NAs
      f1<- f1[!is.na(f1$mean),]
      
      # use the function to identify outliers
      temp <- FindOutliers(f1$mean)
      cfOut<- f1[temp,]
      cfilesNEt<- f1[-temp,]
      #Exports outliers 
      write.csv(cfOut,paste("./Results/Outliers_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
      
      
      hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
      png(filename= paste("./pic/",unique(cfilesNEt$parametro),"_",unique(cfilesNEt$sce),"_PercentaChange_HeapMap.png",sep=""), 
          width = 9, height = 11, units = 'in', res = 300)
      
      labs2 = 'Percentage Change (%)'
      
      n<- ggplot(data =cfilesNEt, aes(REGION, Crop)) + 
            geom_tile(aes(fill = mean), colour = "white")+  facet_grid(cat~MODEL, drop = T)+
            labs(x=NULL, y=NULL, 
                 title= paste("Scenario= ",unique(cfilesNEt$sce),"\nParameter= ",unique(cfilesNEt$parametro),
                              "\n Percentage change from 2020 to 2050",sep = "")) +
            scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +  coord_equal(91/100)+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_grey() + labs(x = "",y = "")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) 
      
      
      plot(n)
      dev.off()  
      
      
      
}



#### price--------------

## creating subset tipo variable-------------
cfiles<- price
cfiles$parametro<- "Price"


### creating vectors
variables<- unique(cfiles$UNIT)
crops<- unique(cfiles$VARIABLE)
sce<- unique(cfiles$SCENARIO)

### Creating subset filter  
cfiles<- cfiles %>% dplyr:: filter(UNIT=="US$2005/t")

#### revalue variables
colnames(cfiles)[4]<- "Crop"

### function manes
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/functionAjusteNames.R")
cfiles<- ajusteNamesPrice(cfiles)


### Eliminate periods
# cfiles$UNIT<- NULL
cfiles$X1990<- NULL
cfiles$X2004<- NULL

cfiles<- cfiles %>% dplyr::select(MODEL, SCENARIO,REGION, Crop, UNIT,X2010,X2015,X2020,X2025,X2030,X2035,X2040,X2045,X2050, parametro)
cfiles<- cfiles[c("MODEL","SCENARIO","REGION", "Crop","X2020", "X2025","X2030","X2035",
                  "X2040","X2045","X2050","parametro")]
############################################# Trends Graph #####################################
### definiendo RCP
cfiles$cat<- ifelse(grepl(pattern ="*_4p5_",x = cfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
yy<- cfiles; yyy<- cfiles

# s=1
for(s in 1:length(listSce)){
      xxx<- yy
      if(s==1){
            xxx$sce<- "sce_cfe_tx"
      }else{}
      if(s==2){
            xxx$sce<- "sce_cfe_notx"
      }else{}
      if(s==3){
            xxx$sce<- "sce_nocfe_notx"
      }else{}
      if(s==4){
            xxx$sce<- "sce_nocfe_tx"
      }else{}
      
      
      xxx<- xxx[c("MODEL","SCENARIO","REGION","Crop","parametro","cat","sce" ,
                  "X2020","X2025","X2030","X2035", "X2040","X2045","X2050")]    
      ytest45 <- xxx %>% filter(.,SCENARIO %in% listSce[[s]]) %>% gather(yr,val,8:ncol(xxx)) 
      
      ytest45$yr<- sub(pattern = "X",replacement = "", x = ytest45$yr)
      ytest45$yr<- as.integer(ytest45$yr)
      ytest45$yr<- as.numeric(ytest45$yr) 
      ytest45<- ytest45[!is.na(ytest45$val),]
      
      datmin<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarise(datamin= min(val,na.rm=T))
      datmed<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarize(datmed= median(val,na.rm=T))
      datmax<- ytest45 %>% dplyr::group_by(MODEL,REGION,Crop,yr,parametro,cat,sce) %>% dplyr::summarize(datmax= max(val,na.rm=T))
      
      extremos<- merge(datmin,datmax)
      
      datost<- merge(extremos,datmed)
      datost<- datost[!is.infinite(datost$datamin),]
      
      write.csv(datost,paste("./Results/dataTotal_",unique(xxx$sce),"_Prices",".csv", sep = ""))
      pots<- unique(xxx$REGION)
      py<- NULL
      
      #       p=3
      
      for(p in 1:length(pots)) {
            
            datax<- datost %>% filter(REGION==pots[p])
            
            
            if(nrow(datax)>0){
                  
                  
                  tiff(filename= paste("pic/",unique(datax$parametro),"_",pots[p],"_",unique(datax$sce),
                                       "_TrendModels.tiff",sep=""), 
                       width = 10, height = 7, units = 'in', res = 100)
                  
                  
                  py[[p]]<-print(ggplot(datax, aes(yr,datmed,group=Crop,color=Crop)) + 
                                       geom_line(linetype="dashed",size=1)+ facet_grid(cat~MODEL)+ 
                                       geom_ribbon(aes(ymin=datamin,ymax=datmax,
                                                       fill=Crop,colour=Crop,linetype=NA),
                                                   alpha=0.1) + 
                                       labs(title=paste("Parameter= ",unique(datax$parametro),"\nRegion= ", pots[p],
                                                        "\nScenario= ",unique(datax$sce),sep = ""))+
                                       labs(y="US$2005/t",x="Year")+
                                       theme(legend.position="bottom")
                  )
                  
                  dev.off()
                  
            }else{}
            
            
            
      }
      
}


############################################# Percentage differences HeatMAP #####################################
# s=1
for(s in 1:length(listSce)){
      xxy<- yyy
      if(s==1){
            xxy$sce<- "sce_cfe_tx"
      }else{}
      if(s==2){
            xxy$sce<- "sce_cfe_notx"
      }else{}
      if(s==3){
            xxy$sce<- "sce_nocfe_notx"
      }else{}
      if(s==4){
            xxy$sce<- "sce_nocfe_tx"
      }else{}
      
      xfiles<- xxy
      #       xfiles<- xfiles[c("MODEL","SCENARIO","REGION","Crop", "parametro", "cat","sce","X2020", "X2050")]
      xfiles<- xfiles %>% rowwise(.)%>% mutate(change= ((X2050 - X2020)/X2020)*100) 
      write.csv(xfiles, "./Results/PriceResults.csv")
      xfiles<- xfiles %>% dplyr::select(MODEL,SCENARIO,REGION,Crop, parametro, cat,sce, change)
      
      #### Escenario
      f1<- xfiles %>% filter(SCENARIO %in% listSce[[s]]) %>% group_by(MODEL, REGION,Crop,cat,sce, parametro) %>%
            dplyr::summarize(mean=mean(change,na.rm=T)) %>% as.data.frame() 
      
      write.csv(f1,paste("./Results/Perchange_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
      
      #eliminando NAs
      f1<- f1[!is.na(f1$mean),]
      
      # use the function to identify outliers
      temp <- FindOutliers(f1$mean)
      if(length(temp)>0){
            cfOut<- f1[temp,]
            cfilesNEt<- f1[-temp,]
            #Exports outliers 
            write.csv(cfOut,paste("./Results/Outliers_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
            
            hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
            png(filename= paste("./pic/",unique(cfilesNEt$parametro),"_",unique(cfilesNEt$sce),"_PercentaChange_HeapMap.png",sep=""), 
                width = 9, height = 11, units = 'in', res = 300)
            
            labs2 = 'Percentage Change (%)'
            
            n<- ggplot(data =cfilesNEt, aes(REGION, Crop)) + 
                  geom_tile(aes(fill = mean), colour = "white")+  facet_grid(cat~MODEL, drop = T)+
                  labs(x=NULL, y=NULL, 
                       title= paste("Scenario= ",unique(cfilesNEt$sce),"\nParameter= ",unique(cfilesNEt$parametro),
                                    "\n Percentage change from 2020 to 2050",sep = "")) +
                  scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
                  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +  coord_equal(91/100)+ 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                  theme_grey() + labs(x = "",y = "")+
                  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
                  theme(axis.text.y = element_text(hjust = 1, size = 11))+
                  theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
                  theme(strip.text=element_text(size=8))+
                  theme(strip.text.y = element_text(angle = 0,size = 11)) 
            
            
            plot(n)
            dev.off()  
            
            
            
            
      }else{ 
            hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
            png(filename= paste("./pic/",unique(f1$parametro),"_",unique(f1$sce),"_PercentaChange_HeapMap.png",sep=""), 
                width = 9, height = 11, units = 'in', res = 300)
            
            labs2 = 'Percentage Change (%)'
            
            n<- ggplot(data =f1, aes(REGION, Crop)) + 
                  geom_tile(aes(fill = mean), colour = "white")+  facet_grid(cat~MODEL, drop = T)+
                  labs(x=NULL, y=NULL, 
                       title= paste("Scenario= ",unique(f1$sce),"\nParameter= ",unique(f1$parametro),
                                    "\n Percentage change from 2020 to 2050",sep = "")) +
                  scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") + labs(fill=labs2)+ #  theme_grey() 
                  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +  coord_equal(91/100)+ 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                  theme_grey() + labs(x = "",y = "")+
                  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
                  theme(axis.text.y = element_text(hjust = 1, size = 11))+
                  theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
                  theme(strip.text=element_text(size=8))+
                  theme(strip.text.y = element_text(angle = 0,size = 11)) 
            
            
            plot(n)
            dev.off()  
            
      }
      
      
}

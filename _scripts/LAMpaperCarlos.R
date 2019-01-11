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
cfiles<- read.csv("lamp_native_20180926-212748.csv")
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
sce_core_nopol<- c("core_ref_noimpacts_nopol")
sce_core_pol<- c("core_4p5_noimpacts_ffict")

#### lista de escenarios
listSce<- list(sce_cfe_tx,
               sce_cfe_notx,
               sce_nocfe_notx,
               sce_nocfe_tx) 
listsce2<- list(sce_cfe_tx,
                sce_cfe_notx,
                sce_nocfe_notx,
                sce_nocfe_tx,
                sce_core_pol,
                sce_core_nopol) 

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

### eliminar paises fuera de la region
cc<- xx %>% dplyr::filter(REGION %in% lac)
# unique(cc$REGION)
cc$VARIABLE<- as.character(cc$VARIABLE)

############################################ Consumo Total ##############################
### filtro sobre la variable de consumo
cons<- cc[grep("*Consumption", cc$VARIABLE),]
cons$VARIABLE<- as.character(cons$VARIABLE)
out<- c("Policy Cost|Consumption Loss")     
consTotal<- cons %>% dplyr::filter(!VARIABLE %in% out ) ###  "Policy Cost|Consumption Loss"

### analisis sobre los cultivos agregados All
todo<- consTotal[grep("All", consTotal$VARIABLE),]
ppp<- unique(todo$VARIABLE)
### rename variables
todo$VARIABLE<- plyr::revalue(todo$VARIABLE,
                              c("Consumption|Agriculture|All Crops"="all",
                               "Consumption|Agriculture|Energy|All Crops"="eall",
                               "Consumption|Agriculture|Feed|All Crops"="feall",
                               "Consumption|Agriculture|Food|All Crops"="foall",  
                               "Consumption|Agriculture|Other|All Crops"="oall"))

### Eliminar periodos
todo$X1990<- NULL; todo$X2004<- NULL
todo<- todo %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                       X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
todo<- todo %>% gather(year,val,6:ncol(todo))
todo$year<- gsub(pattern = "X",replacement = "",x = todo$year,fixed = T)
todo$year<- as.numeric(todo$year)

### definiendo RCP
todo$rcp<- ifelse(grepl(pattern ="*_4p5_",x = todo$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
todo<- todo %>% spread(VARIABLE,val) %>% mutate(r_eall=(eall/all)*100,
                                                r_feall=(feall/all)*100,
                                                r_foall=(foall/all)*100,
                                                r_oall=(oall/all)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_eall,r_feall,r_foall,r_oall)



### crear una lista por escenario
sl<- split(todo,todo$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                           ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                  ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                         ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
      })

### data completa apilada
bbb<- do.call(rbind,aaa)

### rename columnas
colnames(bbb)<- c("MODEL","SCENARIO", "REGION",
                   "UNIT","year","rcp" ,"energy","feed","food","other","sce")
bbb<- bbb %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(bbb$MODEL)

labs2 = 'Types'
### RCP=4.5
xxx<- list()
# m=1
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_ConsumptionRCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                   filter(year!=2010) %>% filter(MODEL==model[[m]]), 
             aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste(model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
                  theme(axis.text.y = element_text(hjust = 1, size = 11))+
                  theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
                  theme(strip.text=element_text(size=8))+
                  theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
          
      plot(pic)
      dev.off()   
      
      cfiles<- bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]])
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) 
      xxx[[m]]<-cfiles 
      
            
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/total_RCP4.5_Consumption.csv")


### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste(model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]])
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) 
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/total_RCP8.5_Consumption.csv")

############################################ Sugar ###########################################
ll<- cons %>% filter(VARIABLE!=out)
azucar<- ll[grep("Sugar Crops", ll$VARIABLE),]
# unique(cons$VARIABLE)

### rename variables
azucar$VARIABLE<- plyr::revalue(azucar$VARIABLE,c("Consumption|Agriculture|Energy|Sugar Crops"="esugar",
                                                "Consumption|Agriculture|Food|Sugar Crops"="fosugar",  
                                                "Consumption|Agriculture|Other|Sugar Crops"="osugar",
                                                "Consumption|Agriculture|Sugar Crops"="sugarall",       
                                                "Consumption|Agriculture|Feed|Sugar Crops"="fesugar"))
                                                
### Eliminar periodos
azucar$X1990<- NULL; azucar$X2004<- NULL
azucar<- azucar %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                       X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
azucar<- azucar %>% gather(year,val,6:ncol(azucar))
azucar$year<- gsub(pattern = "X",replacement = "",x = azucar$year,fixed = T)
azucar$year<- as.numeric(azucar$year)

### definiendo RCP
azucar$rcp<- ifelse(grepl(pattern ="*_4p5_",x = azucar$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
azucar<- azucar %>% spread(VARIABLE,val) %>% mutate(r_esugar=(esugar/sugarall)*100,
                                                r_fesugar=(fesugar/sugarall)*100,
                                                r_fosugar=(fosugar/sugarall)*100,
                                                r_osugar=(osugar/sugarall)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_esugar,r_fesugar,r_fosugar,r_osugar)



### crear una lista por escenario
sl<- split(azucar,azucar$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","year","rcp" ,"energy","feed","food","other","sce")
www<- www %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)

labs2 = 'Types'
### RCP=4.5
m=1
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Azucar_Consumption_RCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Sugar, ",model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Sugar")
      
      xxx[[m]]<-cfiles 
      
      
}
zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/sugar_RCP4.5_Consumption.csv")

### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Sugar, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Sugar")
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/sugar_RCP8.5_Consumption.csv")

############################################ Maiz ###########################################
oo<- cons %>% filter(VARIABLE!=out)
maize<- oo[grep("Maize", oo$VARIABLE),]
# unique(oo$VARIABLE)

### rename variables
maize$VARIABLE<- plyr::revalue(maize$VARIABLE,c("Consumption|Agriculture|Feed|Maize"="femaize",
                                                "Consumption|Agriculture|Food|Maize"="fomaize",
                                                "Consumption|Agriculture|Maize"="maizeall",
                                                "Consumption|Agriculture|Other|Maize"="omaize", 
                                                "Consumption|Agriculture|Energy|Maize"="emaize"))

### Eliminar periodos
maize$X1990<- NULL; maize$X2004<- NULL
maize<- maize %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                           X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
maize<- maize %>% gather(year,val,6:ncol(maize))
maize$year<- gsub(pattern = "X",replacement = "",x = maize$year,fixed = T)
maize$year<- as.numeric(maize$year)

### definiendo RCP
maize$rcp<- ifelse(grepl(pattern ="*_4p5_",x = maize$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
maize<- maize %>% spread(VARIABLE,val) %>% mutate(r_emaize=(emaize/maizeall)*100,
                                                    r_femaize=(femaize/maizeall)*100,
                                                    r_fomaize=(fomaize/maizeall)*100,
                                                    r_omaize=(omaize/maizeall)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_emaize,r_femaize,r_fomaize,r_omaize)



### crear una lista por escenario
sl<- split(maize,maize$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","year","rcp" ,"energy","feed","food","other","sce")
www<- www %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)

labs2 = 'Types'
xxx<- list()
### RCP=4.5
# m=1
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Maize_Consumption_RCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Maize, ",model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()  
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Maize")
      xxx[[m]]<-cfiles 
      
}
zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/maize_RCP4.5_Consumption.csv")

xxx<- list()
### RCP=8.5
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"__Maize_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Maize, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Maize")
      
      xxx[[m]]<-cfiles 
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/maize_RCP8.5_Consumption.csv")

############################################ Rice ###########################################
oo<- cons %>% filter(VARIABLE!=out)
rice<- oo[grep("Rice", oo$VARIABLE),]
# unique(oo$VARIABLE)

### rename variables
rice$VARIABLE<- plyr::revalue(rice$VARIABLE,c("Consumption|Agriculture|Feed|Rice"="ferice",
                                                "Consumption|Agriculture|Food|Rice"="forice",
                                                "Consumption|Agriculture|Rice"="riceall",
                                                "Consumption|Agriculture|Other|Rice"="orice", 
                                                "Consumption|Agriculture|Energy|Rice"="erice"))

### Eliminar periodos
rice$X1990<- NULL; rice$X2004<- NULL
rice<- rice %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                         X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
rice<- rice %>% gather(year,val,6:ncol(rice))
rice$year<- gsub(pattern = "X",replacement = "",x = rice$year,fixed = T)
rice$year<- as.numeric(rice$year)

### definiendo RCP
rice$rcp<- ifelse(grepl(pattern ="*_4p5_",x = rice$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
rice<- rice %>% spread(VARIABLE,val) %>% mutate(r_erice=(erice/riceall)*100,
                                                  r_ferice=(ferice/riceall)*100,
                                                  r_forice=(forice/riceall)*100,
                                                  r_orice=(orice/riceall)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_erice,r_ferice,r_forice,r_orice)



### crear una lista por escenario
sl<- split(rice,rice$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","year","rcp" ,"energy","feed","food","other","sce")
www<- www %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)

labs2 = 'Types'
### RCP=4.5
# m=1
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Rice_Consumption_RCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Rice, ",model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()  
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Rice")
      xxx[[m]]<-cfiles 

}

zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/rice_RCP4.5_Consumption.csv")


### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"__Rice_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Rice, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Rice")
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/rice_RCP8.5_Consumption.csv")

############################################ Roots Tuber #########################################

oo<- cons %>% filter(VARIABLE!=out)
root<- oo[grep("Roots Tuber", oo$VARIABLE),]
# unique(oo$VARIABLE)

### rename variables
root$VARIABLE<- plyr::revalue(root$VARIABLE,c("Consumption|Agriculture|Feed|Roots Tuber"="feroot",
                                              "Consumption|Agriculture|Food|Roots Tuber"="foroot",
                                              "Consumption|Agriculture|Roots Tuber"="rootall",
                                              "Consumption|Agriculture|Other|Roots Tuber"="oroot", 
                                              "Consumption|Agriculture|Energy|Roots Tuber"="eroot"))

### Eliminar periodos
root$X1990<- NULL; root$X2004<- NULL
root<- root %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                       X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
root<- root %>% gather(year,val,6:ncol(root))
root$year<- gsub(pattern = "X",replacement = "",x = root$year,fixed = T)
root$year<- as.numeric(root$year)

### definiendo RCP
root$rcp<- ifelse(grepl(pattern ="*_4p5_",x = root$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
root<- root %>% spread(VARIABLE,val) %>% mutate(r_eroot=(eroot/rootall)*100,
                                                r_feroot=(feroot/rootall)*100,
                                                r_foroot=(foroot/rootall)*100,
                                                r_oroot=(oroot/rootall)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_eroot,r_feroot,r_foroot,r_oroot)



### crear una lista por escenario
sl<- split(root,root$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","year","rcp" ,"energy","feed","food","other","sce")
www<- www %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)

labs2 = 'Types'
### RCP=4.5
# m=1
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_R&T_Consumption_RCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("R&T, ",model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()  
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="R&T")
      xxx[[m]]<-cfiles 
      
}

zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/R&T_RCP4.5_Consumption.csv")


### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"__R&T_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("R&T, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="R&T")
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/R&T_RCP8.5_Consumption.csv")

############################################ Wheat #########################################

oo<- cons %>% filter(VARIABLE!=out)
wheat<- oo[grep("Wheat", oo$VARIABLE),]
# unique(oo$VARIABLE)

### rename variables
wheat$VARIABLE<- plyr::revalue(wheat$VARIABLE,c("Consumption|Agriculture|Feed|Wheat"="fewheat",
                                              "Consumption|Agriculture|Food|Wheat"="fowheat",
                                              "Consumption|Agriculture|Wheat"="wheatall",
                                              "Consumption|Agriculture|Other|Wheat"="owheat", 
                                              "Consumption|Agriculture|Energy|Wheat"="ewheat"))

### Eliminar periodos
wheat$X1990<- NULL; wheat$X2004<- NULL
wheat<- wheat %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                       X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
wheat<- wheat %>% gather(year,val,6:ncol(wheat))
wheat$year<- gsub(pattern = "X",replacement = "",x = wheat$year,fixed = T)
wheat$year<- as.numeric(wheat$year)

### definiendo RCP
wheat$rcp<- ifelse(grepl(pattern ="*_4p5_",x = wheat$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
wheat<- wheat %>% spread(VARIABLE,val) %>% mutate(r_ewheat=(ewheat/wheatall)*100,
                                                r_fewheat=(fewheat/wheatall)*100,
                                                r_fowheat=(fowheat/wheatall)*100,
                                                r_owheat=(owheat/wheatall)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_ewheat,r_fewheat,r_fowheat,r_owheat)



### crear una lista por escenario
sl<- split(wheat,wheat$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","year","rcp" ,"energy","feed","food","other","sce")
www<- www %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)

labs2 = 'Types'
### RCP=4.5
# m=1
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Wheat_Consumption_RCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Wheat, ",model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()  
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Wheat")
      xxx[[m]]<-cfiles 
      
}

zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/Wheat_RCP4.5_Consumption.csv")


### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"__Wheat_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Wheat, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Wheat")
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/Wheat_RCP8.5_Consumption.csv")

############################################ Fiber Crops #########################################

oo<- cons %>% filter(VARIABLE!=out)
fiber<- oo[grep("Fiber Crops", oo$VARIABLE),]
# unique(ener$VARIABLE)

### rename variables
fiber$VARIABLE<- plyr::revalue(fiber$VARIABLE,c("Consumption|Agriculture|Feed|Fiber Crops" ="fefiber",
                                                "Consumption|Agriculture|Food|Fiber Crops"="fofiber",
                                                "Consumption|Agriculture|Fiber Crops"="fiberall",
                                                "Consumption|Agriculture|Other|Fiber Crops" ="ofiber", 
                                                "Consumption|Agriculture|Energy|Fiber Crops"="efiber"))

### Eliminar periodos
fiber$X1990<- NULL; fiber$X2004<- NULL
fiber<- fiber %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                         X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
fiber<- fiber %>% gather(year,val,6:ncol(fiber))
fiber$year<- gsub(pattern = "X",replacement = "",x = fiber$year,fixed = T)
fiber$year<- as.numeric(fiber$year)

### definiendo RCP
fiber$rcp<- ifelse(grepl(pattern ="*_4p5_",x = fiber$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
fiber<- fiber %>% spread(VARIABLE,val) %>% mutate(r_efiber=(efiber/fiberall)*100,
                                                  r_fefiber=(fefiber/fiberall)*100,
                                                  r_fofiber=(fofiber/fiberall)*100,
                                                  r_ofiber=(ofiber/fiberall)*100)%>% 
      select(MODEL,SCENARIO,REGION,UNIT,year,rcp,r_efiber,r_fefiber,r_fofiber,r_ofiber)



### crear una lista por escenario
sl<- split(fiber,fiber$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","year","rcp" ,"energy","feed","food","other","sce")
www<- www %>% gather(cat,val,7:10)


base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)

labs2 = 'Types'
### RCP=4.5
# m=1
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Fiber Crops_Consumption_RCP4.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+ #stat = "identity",
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Fiber Crops, ",model[m],", RCP4.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11)) +
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()  
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Fiber Crops")
      xxx[[m]]<-cfiles 
      
}

zzz<- do.call(rbind,xxx)

write.csv(zzz,"./pic/Fiber Crops_RCP4.5_Consumption.csv")


### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Fiber Crops_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Fiber Crops, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Fiber Crops")
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/Fiber Crops_RCP8.5_Consumption.csv")

############################################### Price ###########################################
### filtro sobre la variable de consumo
price<- cc[grep("*Price", cc$VARIABLE),]
price$VARIABLE<- as.character(price$VARIABLE)


###### categorias precios
vv<- c("Price|Agriculture|All Crops")
price<- price %>% filter(!VARIABLE %in% vv)


### rename variables
price$VARIABLE<- as.character(price$VARIABLE)
price$VARIABLE<- plyr::revalue(price$VARIABLE,c("Price|Agriculture|Maize"="p_agri_maize",
                                          "Price|Agriculture|Oil Crops"="p_agri_oil",   
                                          "Price|Agriculture|Other Crops"="p_agri_othercrops",
                                          "Price|Agriculture|Other Grain"="p_agri_othergrain",
                                          "Price|Agriculture|Sugar Crops"="p_agri_sugar", 
                                          "Price|Agriculture|Wheat"="p_agri_wheat",
                                          "Price|Carbon"="p_carbon",
                                          "Price|Primary Energy|Coal"="p_pri_e_coal",     
                                          "Price|Primary Energy|Gas"="p_pri_e_gas",
                                          "Price|Primary Energy|Oil"="p_pri_oil",
                                          "Price|Agriculture|Energy Crops"="p_agri_energycrops",
                                          "Price|Agriculture|Fiber Crops"="p_agri_fibercrops",
                                          "Price|Agriculture|Rice"="p_agri_rice",
                                          "Price|Agriculture|Roots Tuber"="p_agri_root", 
                                          "Price|Primary Energy|Biomass"="p_pri_e_biomass"))

### Eliminar periodos
price$X1990<- NULL; price$X2004<- NULL
price<- price %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                         X2020,X2025,X2030,X2035,X2040,X2045,X2050)
price<- price %>% gather(year,val,6:ncol(price))
price$year<- gsub(pattern = "X",replacement = "",x = price$year,fixed = T)
price$year<- as.numeric(price$year)

xfiles<- price

### definiendo RCP
xfiles$rcp<- ifelse(grepl(pattern ="*_4p5_",x = xfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")


### calculo de las tasas de participacion de cada categoria sobre el total
xfiles<- xfiles %>%  group_by(MODEL,SCENARIO,REGION,UNIT,rcp, VARIABLE)%>% 
      spread(year,val) 
jfiles<-xfiles

sl<- split(jfiles,xfiles$SCENARIO)
### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
jjjfiles<- do.call(rbind,aaa)

jjjfiles<- jjjfiles %>% gather(year,val,7:13) %>% group_by(MODEL,VARIABLE,sce,rcp,year,REGION, UNIT)%>% 
      summarise(mean=mean(val))

write.csv(qfiles,paste("./pic/",par[p],"_RCP4.5_price.csv"))



# %>% mutate(base=2020/2020,
#                                   i_25=2025/2020,
#                                   i_30=2030/2020,
#                                   i_35=2035/2020,
#                                   i_40=2040/2020,
#                                   i_45=2045/2020,
#                                   i_50=2050/2020) %>% 
xfiles$base<-xfiles$`2020`/xfiles$`2020`
xfiles$i_25<-xfiles$`2025`/xfiles$`2020`
xfiles$i_30<-xfiles$`2030`/xfiles$`2020`
xfiles$i_35<-xfiles$`2035`/xfiles$`2020`
xfiles$i_40<-xfiles$`2040`/xfiles$`2020`
xfiles$i_45<-xfiles$`2045`/xfiles$`2020`
xfiles$i_50<-xfiles$`2050`/xfiles$`2020` 
      
xfiles<- xfiles %>% select(MODEL,SCENARIO,REGION,UNIT,VARIABLE,rcp,base,i_25,i_30,i_35,i_40,i_45,i_50)


### crear una lista por escenario
sl<- split(xfiles,xfiles$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","VARIABLE","rcp",2020,2025,2030,2035,2040,2045,2050,"sce")

www<- www %>% gather(year,val,7:13)
www$year<- as.numeric(www$year)

base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)
par<- unique(www$VARIABLE)
labs2 = 'Types'
### RCP=4.5
p=1 ## parametro
xxx<- list()
for(p in 1:length(par)){
            
      qfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5")%>% 
            filter(VARIABLE==par[p])
      qfiles<- qfiles %>%group_by(MODEL,VARIABLE,sce,year,REGION, UNIT)%>% 
            summarise(mean=mean(val))
      write.csv(qfiles,paste("./pic/",par[p],"_RCP4.5_price.csv"))
      
}
      


### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Fiber Crops_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Fiber Crops, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Fiber Crops")
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/Fiber Crops_RCP8.5_Consumption.csv")

############################################### Carbon Concentration ######################
allpar<- unique(cc$VARIABLE)

co<- c("Concentration|CO2")

co2<- cc %>% filter( VARIABLE %in% co) 
co2$VARIABLE<- plyr::revalue(co2$VARIABLE,c("Concentration|CO2"="ConcentrationCO2ppm"))


### Eliminar periodos
co2<- co2 %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
co2<- co2 %>% gather(year,val,6:ncol(co2))
co2$year<- gsub(pattern = "X",replacement = "",x = co2$year,fixed = T)
co2$year<- as.numeric(co2$year)


### definiendo RCP
co2$rcp<- ifelse(grepl(pattern ="*_4p5_",x = co2$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### calculo de las tasas de participacion de cada categoria sobre el total
co2<- co2 %>% spread(year,val) 
co2$Change<- NA
co2$Change<- ((co2$`2050`- co2$`2010`)/co2$`2010`)*100
co2<- co2 %>% select(MODEL,SCENARIO,REGION,UNIT,rcp,VARIABLE,Change)


### crear una lista por escenario
sl<- split(co2,co2$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})

### data completa apilada
bbb<- do.call(rbind,aaa)
base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(bbb$MODEL)
# pol<- c("sce_cfe_notx","sce_cfe_tx")
labs2 = 'Types'
### RCP=4.5
qfiles<- bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5")%>%
      group_by(MODEL,VARIABLE,sce,UNIT)%>%
      summarise(mean=mean(Change)) 
            
         
write.csv(qfiles,paste("./pic/_CarbonConcentrationRCP4.5V2_price.csv"))

########################################## Emissions CO #########################################
co<- c( "Emissions|CO2")

co2<- cc %>% filter( VARIABLE %in% co) 
co2$VARIABLE<- plyr::revalue(co2$VARIABLE,c("Emissions|CO2"="Emissions CO2"))


### Eliminar periodos
co2<- co2 %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
co2<- co2 %>% gather(year,val,6:ncol(co2))
co2$year<- gsub(pattern = "X",replacement = "",x = co2$year,fixed = T)
co2$year<- as.numeric(co2$year)


### definiendo RCP
# co2$rcp<- ifelse(grepl(pattern ="*_4p5_",x = co2$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
co2$rcp<- ifelse(grepl(pattern ="*_8p5_",x = co2$SCENARIO,ignore.case = T),"RCP8.5", "RCP4.5")

### calculo de las tasas de participacion de cada categoria sobre el total
co2<- co2 %>% spread(year,val) 
co2$Change<- NA
co2$Change<- ((co2$`2050`- co2$`2010`)/co2$`2010`)*100
co2<- co2 %>% select(MODEL,SCENARIO,REGION,UNIT,rcp,VARIABLE,Change)


### crear una lista por escenario
sl<- split(co2,co2$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})

### data completa apilada
bbb<- do.call(rbind,aaa)
base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(bbb$MODEL)
# pol<- c("sce_cfe_notx","sce_cfe_tx")
labs2 = 'Types'
### RCP=4.5
qfiles<- bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5")%>%
      group_by(MODEL,VARIABLE,sce,UNIT)%>%
      summarise(mean=mean(Change)) 


write.csv(qfiles,paste("./pic/EmissionsRCP4.5V2.csv"))
write.csv(qfiles,paste("./pic/EmissionsRCP8.5V2.csv"))
############################################### Food Security Carlangas Version ######################
allpar<- unique(cc$VARIABLE)
### price
price<- cc[grep("Price", cc$VARIABLE),]
listprice<- unique(price$VARIABLE)
how<- c( "Price|Agriculture|Maize",
         "Price|Agriculture|Oil Crops",
         "Price|Agriculture|Other Crops",
         "Price|Agriculture|Other Grain",
         "Price|Agriculture|Sugar Crops",
         "Price|Agriculture|Wheat",  
         "Price|Agriculture|Energy Crops",
         "Price|Agriculture|Fiber Crops",
         "Price|Agriculture|Rice",
         "Price|Agriculture|Roots Tuber")
indp<- price %>% filter(VARIABLE %in% how) %>% mutate(cat="Prices")

### consumo
tragar<- cc[grep("Consumption", cc$VARIABLE),]
listra<- unique(tragar$VARIABLE)
 
canti<- c( "Consumption|Agriculture|Food|Maize" ,
          "Consumption|Agriculture|Food|Oil Crops",
          "Consumption|Agriculture|Food|Other Crops",
          "Consumption|Agriculture|Food|Other Grain",
          "Consumption|Agriculture|Food|Sugar Crops",
          "Consumption|Agriculture|Food|Wheat",
          "Consumption|Agriculture|Food|Energy Crops",
          "Consumption|Agriculture|Food|Fiber Crops",
          "Consumption|Agriculture|Food|Rice",
          "Consumption|Agriculture|Food|Roots Tuber")
indc<- tragar %>% filter(VARIABLE %in% canti) 

### definiendo RCP
indc$rcp<- ifelse(grepl(pattern ="*_4p5_",x = indc$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")
indp$rcp<- ifelse(grepl(pattern ="*_4p5_",x = indp$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")



### cambiar nombres
indc$VARIABLE<- plyr::revalue(indc$VARIABLE,c("Consumption|Agriculture|Food|Maize"="Maize" ,
                                              "Consumption|Agriculture|Food|Oil Crops"="Oil Crops",
                                              "Consumption|Agriculture|Food|Other Crops"="Other Crops",
                                              "Consumption|Agriculture|Food|Other Grain"="Other Grain",
                                              "Consumption|Agriculture|Food|Sugar Crops"="Sugar Crops",
                                              "Consumption|Agriculture|Food|Wheat"="Wheat",
                                              "Consumption|Agriculture|Food|Energy Crops"="Energy Crops",
                                              "Consumption|Agriculture|Food|Fiber Crops"="Fiber Crops",
                                              "Consumption|Agriculture|Food|Rice"="Rice",
                                              "Consumption|Agriculture|Food|Roots Tuber"= "Roots Tuber"))

indp$VARIABLE<- plyr::revalue(indp$VARIABLE,c("Price|Agriculture|Maize"="Maize",
                                              "Price|Agriculture|Oil Crops"="Oil Crops",
                                              "Price|Agriculture|Other Crops"="Other Crops",
                                              "Price|Agriculture|Other Grain"="Other Grain",
                                              "Price|Agriculture|Sugar Crops"="Sugar Crops",
                                              "Price|Agriculture|Wheat"="Wheat",  
                                              "Price|Agriculture|Energy Crops"="Energy Crops",
                                              "Price|Agriculture|Fiber Crops"="Fiber Crops",
                                              "Price|Agriculture|Rice"="Rice",
                                              "Price|Agriculture|Roots Tuber"="Roots Tuber"))


### Eliminar periodos
indp<- indp %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,rcp,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
indc<- indc %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,rcp,X2020,X2025,X2030,X2035,X2040,X2045,X2050)

indp<- indp %>% gather(year,val,7:ncol(indp))
indc<- indc %>% gather(year,val,7:ncol(indc))

indp$year<- gsub(pattern = "X",replacement = "",x = indp$year,fixed = T)
indp$year<- as.numeric(indp$year)
indc$year<- gsub(pattern = "X",replacement = "",x = indc$year,fixed = T)
indc$year<- as.numeric(indc$year)
indc<- indc %>% mutate(cat="Consumption")
indp<- indp %>% mutate(cat="Price")

indc<- indc %>% spread(VARIABLE,val)
indp<- indp %>% spread(VARIABLE,val)


crops<- c("Energy Crops","Fiber Crops" , "Maize",   "Oil Crops","Other Crops" ,
          "Other Grain",  "Rice" , "Roots Tuber","Sugar Crops",  "Wheat"  )

i=9
#### with sugar
agro<- list()
valpro<- list()
for(i in 1:length(crops)){
      con<- indc %>% select(MODEL, SCENARIO,REGION,rcp, year,crops[[i]])
      colnames(con)[ncol(con)]<- "Consumo"
      pri<- indp %>% select(MODEL, SCENARIO,REGION,rcp,year,crops[[i]]) 
      colnames(pri)[ncol(pri)]<- "Price"
      test<- plyr::join(con,pri, by=c("MODEL", "SCENARIO", "REGION","rcp","year"),type="full")
      test <- test %>% mutate(expend=Consumo*Price) %>% 
            select(MODEL, SCENARIO,REGION,rcp,year,expend) %>% mutate(cultivo=crops[[i]])
      
      ### crear una lista por escenario
      sl<- split(test,test$SCENARIO)
      
      ### asignacion de categoria escenario 
      aaa<- lapply(1:length(sl),function(s){
            ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                                ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                       ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                              ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
            return(ee)      
      })
      
      ### data completa apilada
      bbb<- do.call(rbind,aaa)
      base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
      model<- unique(bbb$MODEL)
      # pol<- c("sce_cfe_notx","sce_cfe_tx")
      ### RCP=4.5
      qfiles<- bbb %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5")%>%
            group_by(MODEL,sce,REGION,rcp,year,cultivo)%>%
            summarise(promedio=mean(expend)) 
      
      valpro[[i]]<- qfiles
      
      #### diferencias porcentuales para 2050
      qfiles1<- qfiles %>% spread(year,promedio) %>% select( MODEL , sce,REGION,rcp ,cultivo,`2050`)%>%
            spread(sce,`2050`) %>% mutate(diffp=((sce_cfe_tx-sce_cfe_notx)/sce_cfe_notx)*100) %>%
            select(MODEL ,REGION,rcp ,cultivo,diffp)      
      
      agro[[i]]<- qfiles1
      
            
}

lll<- do.call(rbind,agro)
write.csv(lll,"./pic/gasto_CHANGE_RCP4.5_.csv")
uuu<- do.call(rbind,valpro)
write.csv(uuu,"./pic/gasto_RCP4.5_.csv")

crops2<- c("Energy Crops","Fiber Crops" , "Maize",   "Oil Crops","Other Crops" ,
          "Other Grain",  "Rice" , "Roots Tuber", "Wheat"  )
uuu$promedio[is.na(uuu$promedio)]<-0
conazucar <- uuu %>% group_by(MODEL,sce,REGION,rcp,year) %>%
      summarise(total=sum(promedio,rm.na=TRUE)) %>% as.data.frame()
colnames(conazucar)[6]<-"withsugar" 

sinazucar <- uuu %>% filter(cultivo %in% crops2) %>% group_by(MODEL,sce,REGION,rcp,year) %>%
      summarise(total=sum(promedio,rm.na=TRUE)) %>% as.data.frame()
colnames(sinazucar)[6]<-"withoutsugar" 



testsugar<- plyr::join(conazucar,sinazucar, 
                       by=c("MODEL", "sce", "REGION","rcp","year"),type="full")

testsugar$diff<- ((testsugar$withsugar-testsugar$withoutsugar)/testsugar$withoutsugar)*100
write.csv(testsugar,"./pic/testsugar_RCP4.5_.csv")








"GDP|PPP"
"Food Security|Dietary Adequacy|Share US 2010 cal/cap/day"
"Food Security|Food Expenditure Share GDP"
"Food Security|Food Expenditure Share Income"
"Food Security|Food Import Share Merchandise Export"
"Food Security|Food Production Value|Agricultural Sector"
"Food Security|Food Production Value|Food Processing Sector"
"Food Security|Grain Import Ratio"
"Food Security|Protein Animal"
"Food Security|Protein Total"
"Food Security|Share Staple Calories"

############################################### Graficas PAPER Comparar ###################
### a) agricutural commodity price (y) carbon price (x)
unique(cc$VARIABLE)
a1<- c("Price|Carbon","Price|Agriculture|All Crops")
abs<- cc %>% dplyr::filter(., VARIABLE %in% a1)

abs$VARIABLE<- plyr::revalue(abs$VARIABLE,c( "Price|Agriculture|All Crops"="AgriPrice",
                                    "Price|Carbon"="Carbon Price"  ))

### Eliminar periodos
abs$X1990<- NULL; abs$X2004<- NULL
abs<- abs %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,
                       X2010,X2020,X2025,X2030,X2035,X2040,X2045,X2050)
abs<- abs %>% gather(year,val,6:ncol(abs))
abs$year<- gsub(pattern = "X",replacement = "",x = abs$year,fixed = T)
abs$year<- as.numeric(abs$year)

### definiendo RCP
abs$rcp<- ifelse(grepl(pattern ="*_4p5_",x = abs$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")

### crear una lista por escenario
sl<- split(abs,abs$SCENARIO)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})

##### data completa apilada
bbb<- do.call(rbind,aaa)
bbb$UNIT<- NULL
base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
bbb <- bbb %>% filter(., !SCENARIO %in% base ) %>% filter(., year!=2010)%>% 
      filter(.,MODEL!="Phoenix")%>%
      spread(year,val)


bbb$base<-bbb$`2020`/bbb$`2020`
bbb$i_25<-bbb$`2025`/bbb$`2020`
bbb$i_30<-bbb$`2030`/bbb$`2020`
bbb$i_35<-bbb$`2035`/bbb$`2020`
bbb$i_40<-bbb$`2040`/bbb$`2020`
bbb$i_45<-bbb$`2045`/bbb$`2020`
bbb$i_50<-bbb$`2050`/bbb$`2020` 

bbb<- bbb %>% select(MODEL,SCENARIO,REGION,VARIABLE,rcp,sce,base,i_25,i_30,i_35,
                     i_40,i_45,i_50) %>% gather(index,val,7:ncol(.)) %>% 
      group_by(MODEL,REGION,VARIABLE,rcp,sce,index) %>%
      summarise(ave=mean(val,rm.na=T)) %>% na.omit()%>% 
      group_by(MODEL,REGION,rcp,sce,index) %>%
      spread(VARIABLE,ave)

write.csv(bbb,"./pic/priceCarbon_Agricultural.csv")

###solo los precios de agricultura
nnn<- bbb
nnn<- nnn %>% group_by(MODEL, rcp, sce, index) %>%
      summarise(meanCar=mean(`Carbon Price`), meanAgr=mean(AgriPrice))
write.csv(nnn,"./pic/avepriceCarbon_Agricultural.csv")


############ reference
##### data completa apilada
### crear una lista por escenario
sl<- split(abs,abs$SCENARIO)

### asignacion de categoria escenario 

aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listsce2[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listsce2[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listsce2[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listsce2[[4]],"sce_nocfe_tx",
                                                               ifelse(SCENARIO %in% listsce2[[5]],"sce_core_pol",
                                                                        ifelse(SCENARIO %in% listsce2[[6]],"sce_core_nopol","")))))))
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
      summarise(ave=mean(`2050`)) %>% spread(sce, ave) %>% 
      select(MODEL,VARIABLE, REGION,rcp, sce_cfe_tx,sce_core_pol) %>% filter(.,rcp=="RCP4.5")

bbb$index<-  bbb$sce_cfe_tx/bbb$sce_core_pol

bbb<- bbb %>% select(MODEL,VARIABLE,REGION,index,rcp) %>%  spread(VARIABLE,index) %>% filter(MODEL!="Phoenix")




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
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
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



############################################### Price carbon  ###########################################
### filtro sobre la variable de consumo
price<- cc[grep("*Price", cc$VARIABLE),]
price$VARIABLE<- as.character(price$VARIABLE)


###### categorias precios
vv<- c("Price|Carbon")
price<- price %>% filter(VARIABLE %in% vv)


### rename variables
price$VARIABLE<- as.character(price$VARIABLE)
price$VARIABLE<- plyr::revalue(price$VARIABLE,c("Price|Carbon"="Price Carbon"))

### Eliminar periodos
price$X1990<- NULL; price$X2004<- NULL
price<- price %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE, X2010,
                         X2020,X2025,X2030,X2035,X2040,X2045,X2050)
price<- price %>% gather(year,val,6:ncol(price))
price$year<- gsub(pattern = "X",replacement = "",x = price$year,fixed = T)
price$year<- as.numeric(price$year)

xfiles<- price

### definiendo RCP
xfiles$rcp<- ifelse(grepl(pattern ="*_4p5_",x = xfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")


### calculo de las tasas de participacion de cada categoria sobre el total
xfiles<- xfiles %>%  group_by(MODEL,SCENARIO,REGION,UNIT,rcp, VARIABLE)%>% 
      spread(year,val) 
jfiles<-xfiles

sl<- split(jfiles,xfiles$SCENARIO)
### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
jjjfiles<- do.call(rbind,aaa)

jjjfiles<- jjjfiles %>% gather(year,val,7:14) %>% group_by(MODEL,VARIABLE,sce,rcp,year,REGION, UNIT)%>% 
      summarise(mean=mean(val)) %>% filter(.,rcp!="RCP8.5")

write.csv(jjjfiles,paste("./pic/CarbonPrice_RCP4.5_price.csv"))


############################################### Price agricultural commodities ###########################################
### filtro sobre la variable de consumo
price<- cc[grep("*Price", cc$VARIABLE),]
price$VARIABLE<- as.character(price$VARIABLE)


###### categorias precios
vv<- c("Price|Agriculture|All Crops")
price<- price %>% filter(VARIABLE %in% vv)


### rename variables
price$VARIABLE<- as.character(price$VARIABLE)
price$VARIABLE<- plyr::revalue(price$VARIABLE,c("Price|Agriculture|All Crops"="Price All Crops"))

### Eliminar periodos
price$X1990<- NULL; price$X2004<- NULL
price<- price %>% select(MODEL, SCENARIO,REGION, UNIT,VARIABLE,X2010,
                         X2020,X2025,X2030,X2035,X2040,X2045,X2050)
price<- price %>% gather(year,val,6:ncol(price))
price$year<- gsub(pattern = "X",replacement = "",x = price$year,fixed = T)
price$year<- as.numeric(price$year)

xfiles<- price

### definiendo RCP
xfiles$rcp<- ifelse(grepl(pattern ="*_4p5_",x = xfiles$SCENARIO,ignore.case = T),"RCP4.5", "RCP8.5")


### calculo de las tasas de participacion de cada categoria sobre el total
xfiles<- xfiles %>%  group_by(MODEL,SCENARIO,REGION,UNIT,rcp, VARIABLE)%>% 
      spread(year,val) 
jfiles<-xfiles

sl<- split(jfiles,xfiles$SCENARIO)
### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
jjjfiles<- do.call(rbind,aaa)

jjjfiles<- jjjfiles %>% gather(year,val,7:14) %>% group_by(MODEL,VARIABLE,sce,rcp,year,REGION, UNIT)%>% 
      summarise(mean=mean(val))

write.csv(jjjfiles,paste("./pic/AllcropsAllFood_price.csv"))


########################################## relation price carbon price food ###########################

pfood<- read.csv("./pic/AllcropsAllFood_price.csv")
pcarbon<- read.csv("./pic/CarbonPrice_RCP4.5_price.csv")
p.all$MODEL<- as.character(p.all$MODEL)
p.all$sce<- as.character(p.all$sce)
p.all$rcp<- as.character(p.all$rcp)
p.all$REGION<- as.character(p.all$REGION)
# p.all$UNIT<- as.character(p.all$UNIT)

p.all<- rbind(pfood,pcarbon)
p.all$X<- NULL
p.all$UNIT<- NULL
write.csv(p.all,"./pic/RCP4.5Food&carbon.csv")


p.all<- p.all %>% spread(VARIABLE,mean)
p.all$rcp<- as.character(p.all$rcp)
p.all$MODEL<- as.character(p.all$MODEL)

ggplot(p.all %>% filter(.,sce=="sce_cfe_tx")%>% filter(.,rcp=="RCP4.5")%>%
             filter(.,MODEL!="Phoenix"),
       aes(`Price Carbon`,`Price All Crops`))+
      geom_point(aes(colour=factor(MODEL)))




# %>% mutate(base=2020/2020,
#                                   i_25=2025/2020,
#                                   i_30=2030/2020,
#                                   i_35=2035/2020,
#                                   i_40=2040/2020,
#                                   i_45=2045/2020,
#                                   i_50=2050/2020) %>% 
xfiles$base<-xfiles$`2020`/xfiles$`2020`
xfiles$i_25<-xfiles$`2025`/xfiles$`2020`
xfiles$i_30<-xfiles$`2030`/xfiles$`2020`
xfiles$i_35<-xfiles$`2035`/xfiles$`2020`
xfiles$i_40<-xfiles$`2040`/xfiles$`2020`
xfiles$i_45<-xfiles$`2045`/xfiles$`2020`
xfiles$i_50<-xfiles$`2050`/xfiles$`2020` 

xfiles<- xfiles %>% select(MODEL,SCENARIO,REGION,UNIT,VARIABLE,rcp,base,i_25,i_30,i_35,i_40,i_45,i_50)


### crear una lista por escenario
sl<- split(xfiles,xfiles$SCENARIO)


### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(SCENARIO %in% listSce[[1]],"sce_cfe_tx",
                                          ifelse(SCENARIO %in% listSce[[2]],"sce_cfe_notx",
                                                 ifelse(SCENARIO %in% listSce[[3]],"sce_nocfe_notx",
                                                        ifelse(SCENARIO %in% listSce[[4]],"sce_nocfe_tx","")))))
      return(ee)      
})
### data completa apilada
www<- do.call(rbind,aaa)


### rename columnas
colnames(www)<- c("MODEL","SCENARIO", "REGION",
                  "UNIT","VARIABLE","rcp",2020,2025,2030,2035,2040,2045,2050,"sce")

www<- www %>% gather(year,val,7:13)
www$year<- as.numeric(www$year)

base<- c("core_4p5_noimpacts_ffict","core_ref_noimpacts_nopol")
model<- unique(www$MODEL)
par<- unique(www$VARIABLE)
labs2 = 'Types'
### RCP=4.5
p=1 ## parametro
xxx<- list()
for(p in 1:length(par)){
      
      qfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP4.5")%>% 
            filter(VARIABLE==par[p])
      qfiles<- qfiles %>%group_by(MODEL,VARIABLE,sce,year,REGION, UNIT)%>% 
            summarise(mean=mean(val))
      write.csv(qfiles,paste("./pic/",par[p],"_RCP4.5_price.csv"))
      
}



### RCP=8.5
xxx<- list()
for(m in 1:length(model)){
      
      png(filename= paste("./pic/",model[[m]],"_Fiber Crops_ConsumptionRCP8.5.png",sep=""), 
          width = 14, height = 12, units = 'in', res = 300)
      
      pic<- ggplot(www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
                         filter(year!=2010) %>% filter(MODEL==model[[m]]), 
                   aes(x = year, y = val, fill = cat)) + 
            geom_bar(position = "fill", 
                     stat = "summary", fun.y = "mean") + facet_grid(sce~REGION)+
            facet_grid(sce~REGION)+
            labs(x="Year",y="Proportion of total\nconsumption(%)",
                 title= paste("Fiber Crops, ",model[m],", RCP8.5" ,sep = ""))+
            labs(fill=labs2)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
            theme(axis.text.y = element_text(hjust = 1, size = 11))+
            theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
            theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 11))+
            theme(legend.position="bottom")
      
      plot(pic)
      dev.off()   
      
      cfiles<- www %>% dplyr::filter(!SCENARIO %in% base) %>% filter(rcp=="RCP8.5") %>%
            filter(year!=2010) %>% filter(MODEL==model[[m]]) 
      cfiles<- cfiles %>% group_by(sce,MODEL,REGION,UNIT,year,rcp,cat) %>% 
            summarize(mean=mean(val,na.rm=T)) %>% mutate(Crop="Fiber Crops")
      
      xxx[[m]]<-cfiles 
      
      
}

zzz<- do.call(rbind,xxx)
write.csv(zzz,"./pic/Fiber Crops_RCP8.5_Consumption.csv")


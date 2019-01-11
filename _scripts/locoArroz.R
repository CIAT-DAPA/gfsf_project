#### arroz loco

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
suppressMessages(library(cumplyr))
suppressMessages(library(scales))

### directories
#Definir directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation")
gen<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB/rice/")

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

# datos de precios
precios<- c("PCXAgg -- Consumer Prices","PPXAgg -- Producer Prices")
# datos categorias animales
dataanimal<- c("AnmlNumXAgg -- Animal Numbers" , "AnmlYldXAgg -- Animal Yield")
# data tratamiento especial
dataespecial<- c("QNXAgg -- Net Trade")

### SSPs
g1<- c("_SSP1"); g2<- c("_SSP2"); g3<- c("_SSP3"); g4<- c("_SSP4");g5<- c("_SSP5")
rcp<- list(g1,g2,g3,g4,g5)

options(digits=3) 
options(scipen = 999)

#load data
rice<- read.csv("./ArrozLoco.csv")
rice$impactparameter<- as.character(rice$impactparameter)
rice$scenario<- as.character(rice$scenario)
rice$commodity<- as.character(rice$commodity)
rice$region<- as.character(rice$region)
rice$productiontype<- as.character(rice$productiontype)

par<- unique(rice$impactparameter)
spots<- c("SSA-Mali",  "EAP-Cambodia", "WLD", "EAP","EUR","FSU","LAC","MEN","NAM","SAS","SSA")
r<- rice %>% filter(region %in% spots) %>% filter(year=="2050")
tet<- r[grep("WB_NoCC_", x = r$scenario),]
tet<- unique(tet$scenario)


#1
r2.6ssp5<- r[grep("RCP2.6_SSP5", x = r$scenario),]
r2.6ssp5<- unique(r2.6ssp5$scenario)
#2
r8.5ssp4<- r[grep("RCP8.5_SSP4", x = r$scenario),]
r8.5ssp4<- unique(r8.5ssp4$scenario)
#3
r4.5ssp5<- r[grep("RCP4.5_SSP5", x = r$scenario),]
r4.5ssp5<- unique(r4.5ssp5$scenario)
#4
r4.5ssp4<- r[grep("RCP4.5_SSP4", x = r$scenario),]
r4.5ssp4<- unique(r4.5ssp4$scenario)
#5
r4.5ssp2<- r[grep("RCP4.5_SSP2", x = r$scenario),]
r4.5ssp2<- unique(r4.5ssp2$scenario)
#6
r4.5ssp3<- r[grep("RCP4.5_SSP3", x = r$scenario),]
r4.5ssp3<- unique(r4.5ssp3$scenario)
#7
r4.5ssp1<- r[grep("RCP4.5_SSP1", x = r$scenario),]
r4.5ssp1<- unique(r4.5ssp1$scenario)
#8
r8.5ssp5<- r[grep("RCP8.5_SSP5", x = r$scenario),]
r8.5ssp5<- unique(r8.5ssp5$scenario)
#9
r8.5ssp3<- r[grep("RCP8.5_SSP3", x = r$scenario),]
r8.5ssp3<- unique(r8.5ssp3$scenario)
#10
r2.6ssp2<- r[grep("RCP2.6_SSP2", x = r$scenario),]
r2.6ssp2<- unique(r2.6ssp2$scenario)
#11
r6.0ssp2<- r[grep("RCP6.0_SSP2", x = r$scenario),]
r6.0ssp2<- unique(r6.0ssp2$scenario)
#12
r8.5ssp2<- r[grep("RCP8.5_SSP2", x = r$scenario),]
r8.5ssp2<- unique(r8.5ssp2$scenario)
#13
r2.6ssp5<- r[grep("RCP2.6_SSP5", x = r$scenario),]
r2.6ssp5<- unique(r2.6ssp5$scenario)
#14
r4.5ssp1<- r[grep("RCP4.5_SSP1", x = r$scenario),]
r4.5ssp1<- unique(r4.5ssp1$scenario)
#15
r2.6ssp4<- r[grep("RCP2.6_SSP4", x = r$scenario),]
r2.6ssp4<- unique(r2.6ssp4$scenario)



listSce<- list(r2.6ssp5,r8.5ssp4,r4.5ssp5,
               r4.5ssp4,r4.5ssp2,r4.5ssp3,
               r4.5ssp1,r8.5ssp5,r8.5ssp3,
               r2.6ssp2,r6.0ssp2,r8.5ssp2,
               r2.6ssp5,r4.5ssp1,r2.6ssp4)
scelist<- c("r2.6ssp2" , "r2.6ssp5" ,"r4.5ssp1" ,"r4.5ssp2", "r4.5ssp3", "r4.5ssp4",
            "r4.5ssp5", "r6.0ssp2","r8.5ssp2", "r8.5ssp3" ,"r8.5ssp5")

### crear una lista por escenario
sl<- split(r,r$scenario)

### asignacion de categoria escenario 
aaa<- lapply(1:length(sl),function(s){
      ee<- sl[[s]] %>% mutate(sce= ifelse(scenario %in% listSce[[1]],"r2.6_SSP5",
                                          ifelse(scenario %in% listSce[[2]],"r4.5_SSP5",
                                                 ifelse(scenario %in% listSce[[3]],"r4.5_SSP5",
                                                        ifelse(scenario %in% listSce[[4]],"r4.5_SSP4",
                                                               ifelse(scenario %in% listSce[[5]],"r4.5_SSP2",
                                                                      ifelse(scenario %in% listSce[[6]],"r4.5_SSP3",
                                                                             ifelse(scenario %in% listSce[[7]],"r4.5_SSP1",
                                                                                    ifelse(scenario %in% listSce[[8]],"r8.5_SSP5",
                                                                                           ifelse(scenario %in% listSce[[9]],"r8.5_SSP3",
                                                                                                  ifelse(scenario %in% listSce[[10]],"r2.6_SSP2",
                                                                                                         ifelse(scenario %in% listSce[[11]],"r6.0_SSP2",
                                                                                                                ifelse(scenario %in% listSce[[12]],"r8.5_SSP2",
                                                                                                                       ifelse(scenario %in% listSce[[13]],"r2.6_SSP5",
                                                                                                                              ifelse(scenario %in% listSce[[14]],"r4.5_SSP1",
                                                                                                                                     ifelse(scenario %in% listSce[[15]],"r2.6ssp4", "NoCC"))))))))))))))))
      return(ee)      
})

### data completa apilada
bbb<- do.call(rbind,aaa)
bbb<- data.frame(bbb,"Cat"=ifelse(bbb$scenario %in% tet,"NoCC","CC"))

############################### Datatotal ###########################################
# i=2
yy<- list()
for(i in 1:length(rcp)){
            
      x<- bbb[grep(pattern = rcp[[i]], x = bbb$sce),]
      tm<- unique(x$sce)
      base<- paste("WB_NoCC", rcp[[i]], sep = "")

      # t=1
      rr<- lapply(1:length(tm), function(t){
            # xx<- x[grep(pattern = tm[[t]], x = x$sce),]
            xx<- x %>% filter(sce==tm[[t]])
            b<- bbb %>% filter(scenario %in% base)
      
            rice<- rbind(xx,b)
            rice<- rice %>% filter(impactparameter %in% datatotal) %>% 
                  group_by(impactparameter,commodity,productiontype,Cat,region,year) %>%
                  summarise(mean=mean(Val)) %>% 
                  spread(Cat,mean) %>% 
                  mutate(change=((CC-NoCC)/NoCC)*100) %>%
                  mutate(gcm=tm[[t]])
         
      })
      g<- do.call(rbind,rr)
      yy[[i]]<- g
}

qqq<- do.call(rbind,yy) %>% as.data.frame() %>% select(impactparameter,commodity,
                                                       productiontype,region,year, 
                                                       gcm,change)
T.data<- qqq %>% write.csv(.,paste(gen, "datatotal.csv", sep = ""))

rm(yy,rr)
############################### dataagg ###########################################
yy<- list()
for(i in 1:length(rcp)){
      
      x<- bbb[grep(pattern = rcp[[i]], x = bbb$sce),]
      tm<- unique(x$sce)
      base<- paste("WB_NoCC", rcp[[i]], sep = "")
      
      rr<- lapply(1:length(tm), function(t){
            xx<- x[grep(pattern = tm[[t]], x = x$sce),]
            b<- bbb %>% filter(scenario %in% base)
            
            rice<- rbind(xx,b)
            rice<- rice %>% filter(impactparameter %in% dataagg) %>% 
                  group_by(impactparameter,commodity,productiontype,Cat,region,year) %>%
                  summarise(mean=mean(Val)) %>% 
                  spread(Cat,mean) %>% 
                  mutate(change=((CC-NoCC)/NoCC)*100) %>%
                  mutate(gcm=tm[[t]])
            
      })
      g<- do.call(rbind,rr)
      yy[[i]]<- g
}

qqq<- do.call(rbind,yy) %>% as.data.frame() %>% select(impactparameter,commodity,
                                                       productiontype,region,year, 
                                                       gcm,change)
T.datag<- qqq %>% write.csv(.,paste(gen, "dataag.csv", sep = ""))
rm(yy,rr)
############################### prices ###########################################
yy<- list()
for(i in 1:length(rcp)){
      
      x<- bbb[grep(pattern = rcp[[i]], x = bbb$sce),]
      tm<- unique(x$sce)
      base<- paste("WB_NoCC", rcp[[i]], sep = "")
      
      rr<- lapply(1:length(tm), function(t){
            xx<- x[grep(pattern = tm[[t]], x = x$sce),]
            b<- bbb %>% filter(scenario %in% base)
            
            rice<- rbind(xx,b)
            rice<- rice %>% filter(impactparameter %in% precios) %>% 
                  group_by(impactparameter,commodity,productiontype,Cat,region,year) %>%
                  summarise(mean=mean(Val)) %>% 
                  spread(Cat,mean) %>% 
                  mutate(change=((CC-NoCC)/NoCC)*100) %>%
                  mutate(gcm=tm[[t]])
            
      })
      g<- do.call(rbind,rr)
      yy[[i]]<- g
}

qqq<- do.call(rbind,yy) %>% as.data.frame() %>% select(impactparameter,commodity,
                                                       productiontype,region,year, 
                                                       gcm,change)
T.prices<- qqq %>% write.csv(.,paste(gen, "dataprices.csv", sep = ""))
rm(yy,rr)
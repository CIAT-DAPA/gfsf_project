#### second roand 

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




############## load data
xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP4", x = kk)]
# kk<- list(kk)
ssp4<- lapply(kk,read.csv) ### load data

xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP3", x = kk)]
# kk<- list(kk)
ssp3<- lapply(kk,read.csv) ### load data


xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP5", x = kk)]
# kk<- list(kk)
ssp5<- lapply(kk,read.csv) ### load data


### convirtiendo en archivo data
ssp3<- do.call(rbind, ssp3)
ssp3$ssp<- "SSP3"
ssp4<- do.call(rbind, ssp4)
ssp4$ssp<- "SSP4"
ssp5<- do.call(rbind, ssp5)
ssp5$ssp<- "SSP5"
l.sce<- list(ssp3=ssp3,ssp4=ssp4,ssp5=ssp5)

########################################### Production ###################################

totals<- do.call(rbind,l.sce)
totals$impactparameter<- as.character(totals$impactparameter)
totals$scenario<- as.character(totals$scenario)
totals$commodity<- as.character(totals$commodity)
totals$region<- as.character(totals$region)
totals$productiontype<- as.character(totals$productiontype)
totals$ssp<- as.character(totals$ssp)
nocc<- totals[grep("WB_NoCC", x = totals$scenario),]
nocc<- unique(nocc$scenario)
totals<- totals %>% filter(., !scenario %in% nocc  )

sce<- unique(totals$scenario)
# sce<- substring(sce, 9)

c45<- sce[grep("RCP4.5", sce)]
c85<- sce[grep("RCP8.5", sce)]
c26<- sce[grep("RCP2.6", sce)]



totals<- totals %>% mutate(.,rcp=ifelse(scenario %in% c45,"RCP4.5",
                                        ifelse(scenario %in% c85,"RCP8.5",
                                               ifelse(scenario %in% c26, "RCP2.6","")))) %>%
      filter( impactparameter=="QSXAgg -- Total Production") %>% group_by(impactparameter,region,year,rcp,ssp) %>%
      summarise(all=sum(Val)) #%>% spread(ssp, all)  


totals$region<- revalue(totals$region,c("SSA-Ivory Coast"="Ivory Coast",
                              "SSA-Mali"="Mali"))


write.csv(totals,"./tables/ProductionData.csv")

# ### graghs normal 
# options(scipen=999)
# mali<- ggplot(data=totals %>% filter(.,region=="Mali") %>% filter(., year>=2020), 
#               aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(30000, 120000,by = 10000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Production\n(000 mt)",x="Year", title="Mali, Total Production") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Mali_Production.png",sep=""),mali,width = 10,height = 7)
# dev.off()
# 
# ivory<- ggplot(data=totals %>% filter(.,region=="Ivory Coast") %>% filter(., year>=2020), 
#                aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(100000, 220000,by = 20000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Production\n(000 mt)",x="Year", title="Ivory Coast, Total Producion") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Ivory Coast_Production.png",sep=""),ivory,width = 10,height = 7)
# dev.off()    
# 

########################################### Demand ###################################
totals<- do.call(rbind,l.sce)
totals$impactparameter<- as.character(totals$impactparameter)
totals$scenario<- as.character(totals$scenario)
totals$commodity<- as.character(totals$commodity)
totals$region<- as.character(totals$region)
totals$productiontype<- as.character(totals$productiontype)
totals$ssp<- as.character(totals$ssp)
nocc<- totals[grep("WB_NoCC", x = totals$scenario),]
nocc<- unique(nocc$scenario)
totals<- totals %>% filter(., !scenario %in% nocc  )

sce<- unique(totals$scenario)
# sce<- substring(sce, 9)

c45<- sce[grep("RCP4.5", sce)]
c85<- sce[grep("RCP8.5", sce)]
c26<- sce[grep("RCP2.6", sce)]



totals<- totals %>% mutate(.,rcp=ifelse(scenario %in% c45,"RCP4.5",
                                        ifelse(scenario %in% c85,"RCP8.5",
                                               ifelse(scenario %in% c26, "RCP2.6","")))) %>%
      filter( impactparameter=="QDXAgg -- Total Demand") %>% group_by(impactparameter,region,year,rcp,ssp) %>%
      summarise(all=sum(Val)) #%>% spread(ssp, all)  

totals$region<- revalue(totals$region,c("SSA-Ivory Coast"="Ivory Coast",
                                        "SSA-Mali"="Mali"))
write.csv(totals,"./tables/DemandData.csv")

# ### graghs normal 
# options(scipen=999)
# mali<- ggplot(data=totals %>% filter(.,region=="Mali") %>% filter(., year>=2020), 
#               aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(30000, 120000,by = 10000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Demand\n(000 mt)",x="Year", title="Mali, Total Demand") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Mali_Demand.png",sep=""),mali,width = 10,height = 7)
# dev.off()
# 
# ivory<- ggplot(data=totals %>% filter(.,region=="Ivory Coast") %>% filter(., year>=2020), 
#                aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(100000, 250000,by = 20000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Demand\n(000 mt)",x="Year", title="Ivory Coast, Total Demand") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Ivory Coast_Demand.png",sep=""),ivory,width = 10,height = 7)
# dev.off()   

########################################### Exports ###################################
totals<- do.call(rbind,l.sce)
totals$impactparameter<- as.character(totals$impactparameter)
totals$scenario<- as.character(totals$scenario)
totals$commodity<- as.character(totals$commodity)
totals$region<- as.character(totals$region)
totals$productiontype<- as.character(totals$productiontype)
totals$ssp<- as.character(totals$ssp)
nocc<- totals[grep("WB_NoCC", x = totals$scenario),]
nocc<- unique(nocc$scenario)
totals<- totals %>% filter(., !scenario %in% nocc  )

sce<- unique(totals$scenario)
# sce<- substring(sce, 9)

c45<- sce[grep("RCP4.5", sce)]
c85<- sce[grep("RCP8.5", sce)]
c26<- sce[grep("RCP2.6", sce)]



totals<- totals %>% mutate(.,rcp=ifelse(scenario %in% c45,"RCP4.5",
                                        ifelse(scenario %in% c85,"RCP8.5",
                                               ifelse(scenario %in% c26, "RCP2.6","")))) %>%
      filter( impactparameter=="QEXAgg -- Export") %>% group_by(impactparameter,region,year,rcp,ssp) %>%
      summarise(all=sum(Val)) #%>% spread(ssp, all)  

totals$region<- revalue(totals$region,c("SSA-Ivory Coast"="Ivory Coast",
                                        "SSA-Mali"="Mali"))
write.csv(totals,"./tables/ExportsData.csv")

### graghs normal 
# options(scipen=999)
# mali<- ggplot(data=totals %>% filter(.,region=="Mali") %>% filter(., year>=2020), 
#               aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(1500, 25000,by = 5000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Exports\n(000 mt)",x="Year", title="Mali, Total Exports") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Mali_Exports.png",sep=""),mali,width = 10,height = 7)
# dev.off()
# 
# ivory<- ggplot(data=totals %>% filter(.,region=="Ivory Coast") %>% filter(., year>=2020), 
#                aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(15000, 70000,by = 5000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Exports\n(000 mt)",x="Year", title="Ivory Coast, Total Exports") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Ivory Coast_Exports.png",sep=""),ivory,width = 10,height = 7)
# dev.off()   

########################################### Imports ###################################
totals<- do.call(rbind,l.sce)
totals$impactparameter<- as.character(totals$impactparameter)
totals$scenario<- as.character(totals$scenario)
totals$commodity<- as.character(totals$commodity)
totals$region<- as.character(totals$region)
totals$productiontype<- as.character(totals$productiontype)
totals$ssp<- as.character(totals$ssp)
nocc<- totals[grep("WB_NoCC", x = totals$scenario),]
nocc<- unique(nocc$scenario)
totals<- totals %>% filter(., !scenario %in% nocc  )

sce<- unique(totals$scenario)
# sce<- substring(sce, 9)

c45<- sce[grep("RCP4.5", sce)]
c85<- sce[grep("RCP8.5", sce)]
c26<- sce[grep("RCP2.6", sce)]



totals<- totals %>% mutate(.,rcp=ifelse(scenario %in% c45,"RCP4.5",
                                        ifelse(scenario %in% c85,"RCP8.5",
                                               ifelse(scenario %in% c26, "RCP2.6","")))) %>%
      filter( impactparameter=="QMXAgg -- Import") %>% group_by(impactparameter,region,year,rcp,ssp) %>%
      summarise(all=sum(Val)) #%>% spread(ssp, all)  

totals$region<- revalue(totals$region,c("SSA-Ivory Coast"="Ivory Coast",
                                        "SSA-Mali"="Mali"))
write.csv(totals,"./tables/ImportsData.csv")

# ### graghs normal 
# options(scipen=999)
# mali<- ggplot(data=totals %>% filter(.,region=="Mali") %>% filter(., year>=2020), 
#               aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(2000, 30000,by = 5000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Imports\n(000 mt)",x="Year", title="Mali, Total Imports") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Mali_Imports.png",sep=""),mali,width = 10,height = 7)
# dev.off()
# 
# ivory<- ggplot(data=totals %>% filter(.,region=="Ivory Coast") %>% filter(., year>=2020), 
#                aes(x=year, y=all,  colour=ssp)) + #group=ssp, ,
#       geom_line(aes(linetype=ssp),size = 2)+ facet_grid(~rcp)+
#       # geom_point(size=3)+
#       scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )  +
#       scale_y_continuous(breaks =c(seq(10000, 50000,by = 5000)) )+
#       theme(legend.position="top") +
#       labs(y="Total Imports\n(000 mt)",x="Year", title="Ivory Coast, Total Imports") +
#       theme(legend.text=element_text(size=12),
#             axis.text= element_text(size = 11),
#             axis.title=element_text(size = 12),
#             legend.title=element_blank())+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))
# ggsave(file= paste("./pic/Ivory Coast_Imports.png",sep=""),ivory,width = 10,height = 7)
# dev.off()   
# 

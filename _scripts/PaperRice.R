# Actualizado Programa para formatear los resultados del modelo IMPACT y realizar los analisis graficos-----
#Por: ajustado por Carlos Edo
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))


#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")

#Dirreción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/"

# Regions
r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA",  "WLD")


#Cargar marco de datos principal
# md<-read.csv("Resultados_Ciat_StCty_31_08_16_new.csv",header=T)
md<-read.csv("Phase2/V2_allRegions.csv",header=T)
cfiles<- md
cfiles$commodity<- revalue(cfiles$commodity, c("crice"="Rice",
                                               "jrice"="Rice"))
cfiles$region<- as.character(cfiles$region)
cfiles$impactparameter<- as.character(cfiles$impactparameter)
variables<- c("TYldXAgg -- Total Yield", "QESHXAgg -- Export Share of Production")
cfiles<- filter(cfiles, impactparameter %in% variables) %>% filter(., commodity=="Rice") %>% filter(., region %in% r) 
cfiles$impactparameter<-  revalue(cfiles$impactparameter,c("TYldXAgg -- Total Yield"="Yield","QESHXAgg -- Export Share of Production"="ProExports"))

# tfiles<-  cfiles %>% split(cfiles$region)


f<- cfiles
temp<-  f 
temp1<- filter(temp, impactparameter=="Yield") %>% select(-c(productiontype))
temp2<- filter(temp, impactparameter=="ProExports" )%>% select(-c(productiontype))

ff<- rbind(temp1, temp2)
ff<- ff %>% filter(., year>=2020 )%>% spread(impactparameter, Val)

ff$ProExports2<- (ff$ProExports)*100

ggplot(ff, aes(Yield, ProExports2, fill=region, color=region))+ geom_point(shape=15, size=2)+ geom_jitter()+ facet_wrap(~scenario, nrow(3))+
      theme_minimal()
      


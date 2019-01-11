######## RTB crops los resultados del modelo IMPACT
######## Procesamiento 
######## Por: Ajustado por Carlos Edo
g=gc;rm(list = ls())

### directorios
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(sf))
suppressMessages(library(rgdal))
suppressMessages(library(raster))
options(digits=2)


#### Definir directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation")

#Dirreción graficos
grd<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/IMPACT/")


#### Shapefile
alc <- raster::shapefile(paste(grd,"Shapefiles/April_2013_IMPACT_Regions_v03.shp",sep = ""))

dataShp<- alc@data
fpuAlc<-unique(dataShp$Impact_Reg)

# #### Cargar marco de datos principal
# md<- read.csv("./RTBcropsAna.csv",header=T)

### exportar a RDS
# saveRDS(md,paste(grd,"md.rds",sep = ""))
readRDS(paste(grd,"md.rds",sep = ""))

phi<- md
phi$impactparameter<- as.character(phi$impactparameter)
phi$scenario<- as.character(phi$scenario)
phi$commodity<- as.character(phi$commodity)
phi$region<- as.character(phi$region)
phi$productiontype<- as.character(phi$productiontype)



### solo cultivos RTB
phi<- phi[grep("R&T", x= phi$commodity),]
phi$scenario<- revalue(phi$scenario, c("NoCC_SSP2"="NoCC_SSP2_4.5" ))

rm(md)
### areaF resultado
Map_LatinAmerica<- fortify(alc)
if (!require(gpclib)) install.packages("gpclib", type="source")
maptools::gpclibPermit()




# 1. datos categorias totales
tab<- table(phi$impactparameter, phi$productiontype=="total")
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area","QSXAgg -- Total Production")

#### labels regions
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/labelsRTBRegions.R")

#1. Datos totales--------------------------------

k<- list()
y<- list()
# i=1
for(i in seq_along(datatotal)){
      
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      
      #creo variable para  con CC o NoCC
      nocc<-c("NoCC_SSP2_4.5","NoCC_SSP2_8.5") 
      k[[i]]<- k[[i]] %>% mutate(., cat=ifelse(scenario %in% nocc, "NOCC", "CC"))
      k[[i]]<- k[[i]] %>% mutate(., rcp=ifelse(grepl("_4.5",scenario), "RCP4.5", "RCP8.5"))
      
      k[[i]]<- k[[i]] %>% mutate(., ssp=ifelse(grepl("SSP2", scenario), "SSP2", "SSP3")) %>% 
            dplyr::select(-c(6:20)) %>% dplyr::select(impactparameter,scenario,commodity, region, productiontype,
                                               cat,ssp,rcp,everything()) %>% unite(cat_ssp_rcp,cat,ssp,rcp)%>%
            gather(yr,val,7:ncol(.)) %>% 
            group_by(commodity,region, productiontype,impactparameter,cat_ssp_rcp,yr) %>% summarize(ave=median(val)) %>%
            purrr::modify_at(c("scenario","val"),~NULL) 
      
      k[[i]]<- as.data.frame(k[[i]])
      s<- k[[i]]
      s$sce<- s$cat_ssp_rcp
      
      ###creando una funcion 
      spf<- function(df){df %>% spread(sce,ave)}
      ### reshape anidando los dataframes   
      s<- s %>%  dplyr::group_by(cat_ssp_rcp) %>%
            nest() %>% dplyr::select(., data) %>% mutate(re=purrr::map(data,spf)) 
      # s<- lapply(1:2, function(x){s$data[[x]] %>% spread(cat,ave)})
      
      ### subset
      tt<- do.call(cbind,s$re)
      tt<- tt[,c(1:6,12,18,24)] %>% dplyr::filter(., yr=="2050") 
      tt$r45<- ((tt$CC_SSP2_RCP4.5-tt$NOCC_SSP2_RCP4.5)/tt$NOCC_SSP2_RCP4.5)*100
      tt$r85<- ((tt$CC_SSP2_RCP8.5-tt$NOCC_SSP2_RCP8.5)/tt$NOCC_SSP2_RCP8.5)*100
      tt<- tt %>% dplyr::select(commodity, region, impactparameter, r45,r85) %>% gather(sce,diff_per,4:5 )
      

      tt$zone<- NA
      tt$zone[c(grepl("SSA",tt$region))]<- "SSA"
      tt$zone[c(grepl("MEN",tt$region))]<- "MEN"
      tt$zone[c(grepl("FSU",tt$region))]<- "FSU"
      tt$zone[c(grepl("SAS",tt$region))]<- "SAS"
      tt$zone[c(grepl("EUR",tt$region))]<- "EUR"
      tt$zone[c(grepl("EAP",tt$region))]<- "EAP"
      tt$zone[c(grepl("EAS",tt$region))]<- "EAS"
      tt$zone[c(grepl("LAC",tt$region))]<- "LAC"
      tt$zone[c(grepl("NAM",tt$region))]<- "NAM"


      tt$region<- gsub("SSA-", replacement = "", x = tt$region)
      tt$region<- gsub("MEN-", replacement = "", x = tt$region)
      tt$region<- gsub("FSU-", replacement = "", x = tt$region)
      tt$region<- gsub("EUR-", replacement = "", x = tt$region)
      tt$region<- gsub("EAP-", replacement = "", x = tt$region)
      tt$region<- gsub("EAS-", replacement = "", x = tt$region)
      tt$region<- gsub("SAS-", replacement = "", x = tt$region)
      tt$region<- gsub("LAC-", replacement = "", x = tt$region)
      tt$region<- gsub("NAM-", replacement = "", x = tt$region)
      
      
      
      tt<- tt[!is.na(tt$zone),]
      #creo una variable= cambio porcentual 2020-2050
      y[[i]]<- tt
      return 
}

z<- do.call(rbind, y)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(grd, "RTB.xlsx", sep = ""),
            sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)



######################################################  Coropletras ######################################################## 
t=1 ###  crops
s=1 ### impactparameter
crops<- unique(z$commodity)
crops<- c("R&T-Cassava","R&T-Potato" , "R&T-Sweet Potato" ,"R&T-Yams" ) 
abc<- c("SSA", "MEN", "FSU", "SAS", "EUR", "EAP", "EAS")
datatotal

pic<- list()
for(t in 1:length(crops)){
      proof<- z %>% filter(., commodity==crops[t]) %>% filter(., sce=="r45") %>% 
            filter(., !region %in% abc) %>%
            filter(., impactparameter==datatotal[s])
            

      
      colnames(proof)[2]<-"Impact_Reg"
      require(sf)
      require(maptools)
      require(rgdal)
      alc.sf <- st_as_sf(alc)
      alc.sf <- left_join(alc.sf, proof, by = c('Impact_Reg'))
      alc.sh <- as(alc.sf, 'Spatial')
      shp<- alc.sh
#       writeOGR(alc.sh, dsn = './oldpic/shapes', layer = paste("Area_Rice_", sistemas[t],sep = ""), driver = 'ESRI Shapefile')
      ftfy <- fortify(shp, region = 'Impact_Reg')
      tst <- left_join(ftfy, shp@data, by = c('id' = 'Impact_Reg'))
      
      #Coropleta 
      png(filename = paste(grd,crops[t],"_coropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      labs2 = 'Percentage Change by 2050'
      pic[[t]]<- ggplot() +
            geom_polygon(data = tst, aes(x = long, y = lat, group = group, fill = diff_per)) +
            geom_path(data = tst, aes(x = long, y = lat, group = group), colour="black", size=0.5) +
            coord_fixed()+ theme()+ coord_equal() + labs(fill=labs2)+
            scale_fill_gradient2(low="#ca0020", mid="white", high="#5e3c99", midpoint = 0, na.value="grey",breaks=seq(-50,50,10)) +
#             labs(x=NULL, y=NULL, title= paste("Area Harvest of Rice ", sistemas[t], sep = ""))+
            theme(strip.text.x = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 16, face = "bold.italic"))+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"))
      
      
      
      plot(pic[[t]])
      dev.off()
      
}

library(ggpubr)
#Coropleta 
png(filename = paste("./","coropletaAllScenarios.png", sep=""), width = 9, height = 6, units = 'in', res = 400)

gg <- ggarrange( pic[[1]], pic[[2]], labels = c("A", "B"), ncol = 2, nrow = 2,common.legend = T)

plot(gg)
dev.off()

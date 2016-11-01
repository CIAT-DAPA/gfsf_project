### preparacion de los datos para presentacion de yuca.
## Autor: CArlos Eduardo Gonzalez
##  prep

#librerias------
library(XML)
library(treemap)
library(migest)
library(circlize)
library(rworldmap)
library(maptree) #maps of 
library(ggplot2) #plot
library(dplyr)  # reshape
library(raster) # procesar Raster
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)
library(rgdal)
library(sp)
library(maptools)
library(maps)
library(tiff)
library(rasterVis)
library(dismo)
library(ncdf4)

#definir directorios de trabajo-----
#directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/cassava") # print the current working directory - cwd 
#directorio de graficos
pic <-("C:/Users/CEGONZALEZ/Documents/cassava/graph/")

#Limitar numero de decimales y remove scientific notation-----
options(digits=3) 
options(scipen=999)

#Importar datos-----
#datos produccion, area y rendimientos
fao_oferta<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAO_Prod_Yield_Area.csv", header = TRUE)
fao_land_area<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOLandArea.csv", header = TRUE)
  

#datos de precios
fao_price<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAO_PriceProducer.csv", header = TRUE)
fao_pricePro_index<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOProducerPriceIndicesAnnual.csv", header = TRUE)
fao_pricePro_monthly<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOPriceProducerMonthly.csv", header = TRUE)

#datos de comercio exportaciones e importaciones
fao_trade<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOTrade.csv", header = TRUE)

#matrices de relaciones comerciales 
fao_matrix_trade<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOMatrixTrade.csv", header = TRUE)

#datos sobre valor agregado 
fao_valueAgg<-read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOValueAgProd.csv", header = TRUE)

#datos  sobre Harmonized Commodity Description and Coding System "HS" 
comtrade_1<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/TradeCassava2015-2011.csv", header = TRUE)
comtrade_2<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/TradeCassava2010-2006.csv", header = TRUE)
comtrade_3<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/TradeCassava2005-2001.csv", header = TRUE)
comtrade_4<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/TradeCassava2000-1996.csv", header = TRUE)
comtrade_5<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/TradeCassava1995-1991.csv", header = TRUE)

#Importar Spatial Data------
#SPAM Spatial Allocation Model
spam_prod<-raster("C:/Users/CEGONZALEZ/Documents/cassava/SPATIAL/spam2005v2r0_production_cassava_total.tiff")
spam_yield<-raster("C:/Users/CEGONZALEZ/Documents/cassava/SPATIAL/spam2005v2r0_yield_cassava_rainfed.tiff")
levelplot(spam_prod) # visualizar datos spatial.
levelplot(spam_yield)

#Tuber Roots and Bananas CGIAR
rtb_prod_yield_area<-("C:/Users/CEGONZALEZ/Documents/cassava/SPATIAL/cassava_HarvAreaYield_NetCDF/cassava_AreaYieldProduction")
rtb_pya<- paste(rtb_prod_yield_area,".nc", sep = "")

map_rtb<- nc_open(rtb_pya)
print(map_rtb)

map_rtb <- brick(rtb_pya, lvar=4) # dimensiones son 4 creando mapas por layer
yield <- map_rtb[[2]]  
yield_q <- map_rtb[[4]]  
harvest<- map_rtb[[1]]
harvest_h <- map_rtb[[5]]  
production <- map_rtb[[6]]  


# brecha de rendimientos 
rtb_yieldgap<-("C:/Users/CEGONZALEZ/Documents/cassava/SPATIAL/cassava_yieldgap_netcdf/YieldGap_cassava_2000_BaseGDD_8_MaxYieldPct_95_ContourFilteredClimateSpace_10x10_prec_YieldGap")
rtb_gap<- paste(rtb_yieldgap,".nc", sep = "")

map_gap <- raster::stack(rtb_gap) # los :: es para indica que la opcion corresponde a ese paquete
map_gap <- raster::brick(rtb_gap)
map_gap <- raster::raster(rtb_gap)



# shape file World

World_shp <- "C:/Users/CEGONZALEZ/Documents/cassava/SPATIAL/gisdata/"
World_1 <- shapefile(paste0(World_shp,"G2014_2013_0.shp"))
World_2 <- shapefile(paste0(World_shp,"G2014_2013_1.shp"))
World_3 <- shapefile(paste0(World_shp, "G2014_2013_2.shp"))
plot(World_1)

#procesamiento de datos-----

fao_oferta # data produccion, area y rendimiento
names(fao_oferta)
#cambiar nombres de variables y eliminar variables innecesarias
fao_oferta$Domain.Code<- NULL
fao_oferta$Flag<- NULL
fao_oferta$FlagD<- NULL
fao_oferta$ItemCode<- NULL
fao_oferta$ElementCode<- NULL
fao_oferta<- fao_oferta[-(17031:17032),]
names(fao_oferta)[names(fao_oferta) == 'ElementName'] <- 'variable'
names(fao_oferta)[names(fao_oferta) == 'AreaName'] <- 'region'

fao_land_area # data uso de tierra
fao_land_area$Domain.Code<- NULL
fao_land_area$Flag<- NULL
fao_land_area$FlagD<- NULL
fao_land_area$ItemCode<- NULL
fao_land_area$ElementCode<- NULL
fao_land_area$ElementName<-NULL
fao_land_area<- fao_land_area[-(51923:51924),]
names(fao_land_area)[names(fao_land_area) == 'ItemName'] <- 'variable'
names(fao_land_area)[names(fao_land_area) == 'AreaName'] <- 'region'


fao_price # data sobre precios
fao_price$Domain.Code<- NULL
fao_price$Flag<- NULL
fao_price$FlagD<- NULL
fao_price$ItemCode<- NULL
fao_price$ElementCode<- NULL
fao_price$ItemName<-NULL
fao_price$Domain<-NULL
fao_price<- fao_price[-(3366:3367),]
names(fao_price)[names(fao_price) == 'ElementName'] <- 'variable'
names(fao_price)[names(fao_price) == 'AreaName'] <- 'region'


fao_pricePro_index # data sobre indice de precios
fao_pricePro_index$Domain.Code<- NULL
fao_pricePro_index$Flag<- NULL
fao_pricePro_index$FlagD<- NULL
fao_pricePro_index$ItemCode<- NULL
fao_pricePro_index$ElementCode<- NULL
fao_pricePro_index$Domain<-NULL
fao_pricePro_index<- fao_oferta[-(1317:1318),]
names(fao_pricePro_index)[names(fao_pricePro_index) == 'ElementName'] <- 'variable'
names(fao_pricePro_index)[names(fao_pricePro_index) == 'AreaName'] <- 'region'

fao_pricePro_monthly # data indice de precios  mensuales 
fao_pricePro_monthly$Domain.Code<- NULL
fao_pricePro_monthly$Flag<- NULL
fao_pricePro_monthly$FlagD<- NULL
fao_pricePro_monthly$ItemCode<- NULL
fao_pricePro_monthly$ElementCode<- NULL
fao_pricePro_monthly$Domain<-NULL
fao_pricePro_monthly<- fao_pricePro_monthly[-(867:868),]
names(fao_pricePro_monthly)[names(fao_pricePro_monthly) == 'ElementName'] <- 'Moth'
names(fao_pricePro_monthly)[names(fao_pricePro_monthly) == 'AreaName'] <- 'region'


fao_trade # data de comercio
fao_trade$Domain.Code<- NULL
fao_trade$Flag<- NULL
fao_trade$FlagD<- NULL
fao_trade$ItemCode<- NULL
fao_trade$ElementCode<- NULL
fao_trade$Domain<-NULL
fao_trade<- fao_trade[-(37749:377502),]
names(fao_trade)[names(fao_trade) == 'ElementName'] <- 'variable'
names(fao_trade)[names(fao_trade) == 'AreaName'] <- 'region'


fao_matrix_trade # data comercio como matrix 
fao_matrix_trade$Domain.Code<- NULL
fao_matrix_trade$Flag<- NULL
fao_matrix_trade$FlagD<- NULL
fao_matrix_trade$ItemCode<- NULL
fao_matrix_trade$Element.Code<- NULL
fao_matrix_trade$Domain<-NULL
fao_matrix_trade$Item.Code<-NULL
fao_matrix_trade$Year.Code<-NULL
fao_matrix_trade$NoRecords<-NULL
fao_matrix_trade$Flag.Description<-NULL
fao_matrix_trade<- fao_matrix_trade[-(28389),]
names(fao_matrix_trade)[names(fao_matrix_trade) == 'Element'] <- 'variable'
names(fao_matrix_trade)[names(fao_matrix_trade) == 'AreaName'] <- 'region'

#data comercio 
# listados
ListComtrade <- list(comtrade_1, comtrade_2, comtrade_3, comtrade_4, comtrade_5 )
lapply(ListComtrade, dim)

for(i in 1:length(ListComtrade)) {
  cat('Delete columns in file:',i,'\n')
 ListComtrade[[i]]<- ListComtrade[[i]][ , -which(names(ListComtrade[[i]]) %in% c("Aggregate.Level",
                                                      "Is.Leaf.Code", 
                                                      "X2nd.Partner.Code",
                                                      "X2nd.Partner", 
                                                      "X2nd.Partner.ISO",
                                                      "Customs.Proc..Code", 
                                                      "Mode.of.Transport.Code",
                                                      "Mode.of.Transport",
                                                      "Customs", 
                                                      "Flag", 
                                                      "FOB.Trade.Value..US..",
                                                      "CIF.Trade.Value..US..", 
                                                      "Period", "Period.Desc.", 
                                                      "Partner.ISO", 
                                                      "Alt.Qty.Unit.Code",
                                                      "Qty", 
                                                      "Alt.Qty", 
                                                      "Gross.weight..kg.",
                                                      "Partner.Code" ))]
  

 }

# comprobar que se eliminaron columnas
lapply(ListComtrade, dim)

c_1 <- ListComtrade[[1]]
c_2 <- ListComtrade[[2]]
c_3 <- ListComtrade[[3]]
c_4 <- ListComtrade[[4]]
c_5 <- ListComtrade[[5]]

# unir los datos en un append
TradeCom <- rbind(c_1, c_2, c_3, c_4, c_5) # para poner bases de datos uno sobre el otros


 

# convertir de Hg a Kg relacion es 1 hg a 0.1 kkfao_oferta-----------
r<- which(fao_oferta$variable=="Yield")

for(i in 1:nrow(fao_oferta)){
  if(i %in% r){
    fao_oferta$Value[i]<- fao_oferta$Value[i] * 0.1
  } else {  }
}

#Generar graficos-----
#parametros para realizar graficas 

# test a 95% datos
testcarlos<- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), median(Value, na.rm=TRUE)))
testcarlos_median <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), median(Value, na.rm=TRUE)))
testcarlos_mean <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), mean(Value, na.rm = TRUE)))
colnames(testcarlos_mean)[4] <- 'Value'
colnames(testcarlos_median)[4] <- 'Value'
colnames(testcarlos)[4] <- 'Value'

quantileFun <- function(x){z <- stats::quantile(x, probs=0.05, na.rm=TRUE); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), quantileFun(Value)))
testcarlos$p0_05 <- as.numeric(aux[,ncol(aux)])
testcarlos_mean$p0_05<- as.numeric(aux[,ncol(aux)])
testcarlos_median$p0_05<- as.numeric(aux[,ncol(aux)])

quantileFun <- function(x){z <- stats::quantile(x, probs=0.95, na.rm=TRUE); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), quantileFun(Value)))
testcarlos$p0_95 <- as.numeric(aux[,ncol(aux)])
testcarlos_mean$p0_95 <- as.numeric(aux[,ncol(aux)])
testcarlos_median$p0_95 <- as.numeric(aux[,ncol(aux)])
# testcarlos$gap<- por diseñar

# test 90% datos
testcarlos <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), median(Value, na.rm=TRUE)))
colnames(testcarlos)[4] <- 'Value'
quantileFun <- function(x){z <- stats::quantile(x, probs=0.1, na.rm=TRUE); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), quantileFun(Value)))
testcarlos$pmin_10 <- as.numeric(aux[,ncol(aux)])
quantileFun <- function(x){z <- stats::quantile(x, probs=0.8, na.rm=TRUE); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), quantileFun(Value)))
testcarlos$pmax_80 <- as.numeric(aux[,ncol(aux)])

testcarlos$gab <- testcarlos$p0_95-testcarlos$p0_05  


# filtros 
yield_cassava <- which(fao_oferta$variable=="Yield" & fao_oferta$ItemName=="Cassava")
produ_cassava<- which(fao_oferta$variable=="Production" & fao_oferta$ItemName=="Cassava")
area_cassava<- which(fao_oferta$variable=="Area harvested" & fao_oferta$ItemName=="Cassava")
fao_oferta$region <- as.character(fao_oferta$region)

grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
thailandia = c("Thailand")
thailandia <- as.numeric(unlist(grep2(pattern=thailandia, x=as.character(fao_oferta$region))))
thailandia <- base::intersect(yield_cassava, thailandia)

grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
Nigeria = c("Nigeria")
Nigeria <- as.numeric(unlist(grep2(pattern=Nigeria, x=as.character(fao_oferta$region))))
Nigeria <- base::intersect(yield_cassava, Nigeria)

grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
brazil = c("Brazil")
brazil <- as.numeric(unlist(grep2(pattern=brazil, x=as.character(fao_oferta$region))))
brazil <- base::intersect(yield_cassava, brazil)

# # Grafico Base-------
gg<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value, color=region), alpha=0.2)
gg<- gg + geom_line(aes(group = region, alpha=0.1)) 
gg<- gg + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg<- gg + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg<- gg + ggtitle("World Cassava Yield \n (1960-2014)")
gg<- gg + theme(legend.position="none")
gg<- gg + ylab('Yield Kg/Ha') + xlab('Years') + theme()
gg<- gg + theme(axis.text.x=element_text(size=14, angle=45))
gg<- gg + theme(axis.text.y=element_text(size=14, angle=360))
gg<- gg + theme(plot.title=element_text(size=24, face = 'bold')) 
gg # locura

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldAllCountries.png", gg, width=15, height=10.5, units='in') #saves g



gg1<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value))
gg1<- gg1 + geom_line(aes(group = region), alpha=0.1) 
gg1<- gg1 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='red', alpha=0.4)
gg1<- gg1 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg1<- gg1 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg1<- gg1 + ggtitle("World Cassava Yield \n (1960-2014)")
gg1<- gg1 + theme(legend.position="none")
gg1<- gg1 + ylab('Yield Kg/Ha') + xlab('Years') + theme()
gg1<- gg1 + theme(axis.text.x=element_text(size=14, angle=45))
gg1<- gg1 + theme(axis.text.y=element_text(size=14, angle=360))
gg1<- gg1 + theme(plot.title=element_text(size=24, face = 'bold')) 
gg1<- gg1 + theme()
gg1

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldAllCountries1.png", gg1, width=15, height=10.5, units='in') #saves g


gg2<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value))
gg2<- gg2 + geom_line(aes(group = region), alpha=0) 
gg2<- gg2 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='red', alpha=0.4)
gg2<- gg2 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg2<- gg2 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg2<- gg2 + ggtitle("World Cassava Yield \n (1960-2014)")
gg2<- gg2 + theme(legend.position="none")
gg2<- gg2 + ylab('Yield Kg/Ha') + xlab('Years') + theme()
gg2<- gg2 + theme(axis.text.x=element_text(size=14, angle=45))
gg2<- gg2 + theme(axis.text.y=element_text(size=14, angle=360))
gg2<- gg2 + theme(plot.title=element_text(size=24, face = 'bold')) 
gg2<- gg2 + theme()
gg2

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldAllCountries2.png", gg2, width=15, height=10.5, units='in') #saves g


gg3<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value))
gg3<- gg3 + geom_line(aes(group = region), alpha=0) 
gg3<- gg3 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='red', alpha=0.4)
gg3<- gg3 + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='black', alpha=1)
gg3<- gg3 + geom_line(data=testcarlos_mean[which(testcarlos_mean$variable=='Yield' & testcarlos_mean$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='blue', alpha=1)
gg3<- gg3 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg3<- gg3 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg3<- gg3 + ggtitle("World Cassava Yield \n (1960-2014)")
gg3<- gg3 + theme(legend.position="none")
gg3<- gg3 + ylab('Yield Kg/Ha') + xlab('Years') + theme()
gg3<- gg3 + theme(axis.text.x=element_text(size=14, angle=45))
gg3<- gg3 + theme(axis.text.y=element_text(size=14, angle=360))
gg3<- gg3 + theme(plot.title=element_text(size=24, face = 'bold')) 
gg3<- gg3 + theme()
gg3<- gg3 + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
gg3<- gg3 + annotate(geom="text", x=2009, y=13000, label= "Mean", size=15, color="blue",  fontface="italic")
gg3


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldAllCountries3.png", gg3, width=15, height=10.5, units='in') #saves g

# gg4<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value))
# gg4<- gg4 + geom_line(aes(group = region), alpha=0) 
# gg4<- gg4 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='red', alpha=0.4)
# gg4<- gg4 + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='black', alpha=0.5)
# gg4<- gg4 + geom_line(data=testcarlos_mean[which(testcarlos_mean$variable=='Yield' & testcarlos_mean$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='blue', alpha=0.5)
# gg4<- gg4 + geom_bardata=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=gab),size=1.5, colour='red', alpha=1)
#   
#   geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=gab),size=1.5, colour='red', alpha=1)
# gg4<- gg4 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
# gg4<- gg4 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
# gg4<- gg4 + ggtitle("World Cassava Yield \n (1960-2014)")
# gg4<- gg4 + theme(legend.position="none")
# gg4<- gg4 + ylab('Yield Kg/Ha') + xlab('Years') + theme()
# gg4<- gg4 + theme(axis.text.x=element_text(size=14, angle=45))
# gg4<- gg4 + theme(axis.text.y=element_text(size=14, angle=360))
# gg4<- gg4 + theme(plot.title=element_text(size=24, face = 'bold')) 
# gg4<- gg4 + theme()
# gg4<- gg4 + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
# gg4<- gg4 + annotate(geom="text", x=2009, y=13000, label= "Mean", size=15, color="blue",  fontface="italic")
# gg4<- gg4 + annotate(geom="text", x=2008, y=22000, label= "Gap", size=15, color="red",  fontface="italic")
# gg4


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldAllCountries4.png", gg4, width=15, height=10.5, units='in') #saves g



gg<- gg + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='orange', alpha=1)
gg<- gg + geom_line(data=fao_oferta[Nigeria,], aes(x=Year, y=Value, group = region), size=1.2, colour='red') 
gg<- gg + geom_line(data=fao_oferta[thailandia,], aes(x=Year, y=Value, group = region), size=1.2, colour='green') 
gg<- gg + geom_line(data=fao_oferta[brazil,], aes(x=Year, y=Value, group = region), size=1.2, colour='pink') 
#gg<- gg + theme_bw(20) + theme(legend.position="none")
gg<- gg + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg<- gg + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg<- gg + ggtitle("Cassava Yield of World \n & Confidence Intervals (1960-2014)")
gg<- gg + ylab('Yield Kg/Ha') + xlab('Years')
gg<- gg + theme(axis.text.x=element_text(size=14, angle=45))
gg<- gg + theme(axis.text.y=element_text(size=14, angle=360))
gg<- gg + theme(plot.title=element_text(size=24, face = 'bold'))
gg<- gg + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
gg<- gg + annotate(geom="text", x=2008, y=12000, label= "Nigeria", size=15, color="black",  fontface="italic")
gg<- gg + annotate(geom="text", x=2008, y=20000, label= "Thailandia", size=15, color="black",  fontface="italic")
gg<- gg + annotate(geom="text", x=2008, y=15000, label= "Brazil", size=15, color="black",  fontface="italic")
gg


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldCassava.png", gg, width=15, height=10.5, units='in') #saves g

# solo el intervalo con todos los paises 
gg1 <- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value)) # con datos originales
gg1 <- gg1 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='blue', alpha=0.7) # con newdata
#gg1 <- gg1 + geom_line(aes(group = region),alpha=0.2)
gg1 <- gg1 + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='orange', alpha=1)
gg1<- gg1 + geom_line(data=fao_oferta[Nigeria,], aes(x=Year, y=Value, group = region), size=1.2, colour='red') 
gg1<- gg1 + geom_line(data=fao_oferta[thailandia,], aes(x=Year, y=Value, group = region), size=1.2, colour='green') 
gg1<- gg1 + geom_line(data=fao_oferta[brazil,], aes(x=Year, y=Value, group = region), size=1.2, colour='pink') 
gg1<- gg1 + theme_bw(20) + theme(legend.position="none")
gg1<- gg1 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg1<- gg1 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg1<- gg1 + ggtitle("Cassava Yield of World \n & Confidence Intervals (1960-2014)")
gg1<- gg1 + ylab('Yield Kg/Ha') + xlab('Years')
gg1<- gg1 + theme(axis.text.x=element_text(size=14, angle=45))
gg1<- gg1 + theme(axis.text.y=element_text(size=14, angle=360))
gg1<- gg1 + theme(plot.title=element_text(size=24, face = 'bold'))
gg1<- gg1 + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
gg1<- gg1 + annotate(geom="text", x=2008, y=12000, label= "Nigeria", size=15, color="black",  fontface="italic")
gg1<- gg1 + annotate(geom="text", x=2008, y=20000, label= "Thailandia", size=15, color="black",  fontface="italic")
gg1<- gg1 + annotate(geom="text", x=2008, y=15000, label= "Brazil", size=15, color="black",  fontface="italic")
gg1


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/CIYields.png", gg1, width=15, height=10.5, units='in') #saves g



#solo el intervalo
gg2 <- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value)) # con datos originales
gg2 <- gg2 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='blue', alpha=0.7) # con newdata
#gg1 <- gg1 + geom_line(aes(group = region),alpha=0.2)
gg2 <- gg2 + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='orange', alpha=1)
#gg1<- gg1 + geom_line(data=fao_oferta[Nigeria,], aes(x=Year, y=Value, group = region), size=1.2, colour='red') 
#gg1<- gg1 + geom_line(data=fao_oferta[thailandia,], aes(x=Year, y=Value, group = region), size=1.2, colour='green') 
#gg1<- gg1 + geom_line(data=fao_oferta[brazil,], aes(x=Year, y=Value, group = region), size=1.2, colour='blue') 
gg2<- gg2 + theme_bw(20) + theme(legend.position="none")
gg2<- gg2 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg2<- gg2 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg2<- gg2 + ggtitle("Cassava Yield of World \n & Confidence Intervals (1960-2014)")
gg2<- gg2 + ylab('Yield Kg/Ha') + xlab('Years')
gg2<- gg2 + theme(axis.text.x=element_text(size=14, angle=45))
gg2<- gg2 + theme(axis.text.y=element_text(size=14, angle=360))
gg2<- gg2 + theme(plot.title=element_text(size=24, face = 'bold'))
gg2<- gg2 + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
#gg1<- gg1 + annotate(geom="text", x=2008, y=12000, label= "Nigeria", size=15, color="black",  fontface="italic")
#gg1<- gg1 + annotate(geom="text", x=2008, y=20000, label= "Thailandia", size=15, color="black",  fontface="italic")
#gg1<- gg1 + annotate(geom="text", x=2008, y=15000, label= "Brazil", size=15, color="black",  fontface="italic")
gg2


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/onlyCIYields.png", gg2, width=15, height=10.5, units='in') #saves g

# graph Yield-----

# graphs by countries-----
yth<- ggplot(data = fao_oferta[yield_cassava,], 
             aes(x=Year, y=Value))
yth<- yth + geom_ribbon(aes(ymin=2500, ymax=20000), fill="lightblue", alpha=0.7)
yth<- yth + geom_line(aes(y= 20000), colour = 'black', size = 0.5, linetype="dashed") + geom_line(aes(y= 2500), colour = 'black', size = 0.5, linetype="dashed")
yth<- yth + geom_line(aes(group = region), alpha=0.2) 
yth<- yth + geom_line(data=fao_oferta[thailandia,], aes(x=Year, y=Value, group = region), size=2, colour='red') 
yth<- yth + theme_gray(20)+ theme(legend.position="none")
#yth<- yth + ggtitle("Thailand, Cassava Yield")
yth<- yth + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 4),1)) 
yth<- yth + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
yth<- yth + ylab('Yield Kg') + xlab('Years')
yth<- yth + theme(axis.text.x=element_text(size=14, angle=360))
yth<- yth + theme(axis.text.y=element_text(size=14, angle=360))
yth<- yth + theme(plot.title=element_text(size=24, face = 'bold'))
yth<- yth + annotate(geom="text", x=1990, y=10000, label= "Thailand", size=30, color="black",  fontface="italic")
yth

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/Thailand.png", yth, width=10, height=10.5, units='in') #saves g



# Brazil
yield_cassava <- which(fao_oferta$variable=="Yield" & fao_oferta$ItemName=="Cassava")
fao_oferta$region <- as.character(fao_oferta$region)
grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
brazil = c("Brazil")
brazil <- as.numeric(unlist(grep2(pattern=brazil, x=as.character(fao_oferta$region))))
brazil <- base::intersect(yield_cassava, brazil)


yb<- ggplot(data = fao_oferta[yield_cassava,], 
             aes(x=Year, y=Value))
yb<- yb + geom_ribbon(aes(ymin=2500, ymax=20000), fill="lightblue", alpha=0.7)
yb<- yb + geom_line(aes(y= 20000), colour = 'black', size = 0.5, linetype="dashed") + geom_line(aes(y= 2500), colour = 'black', size = 0.5, linetype="dashed")
yb<- yb + geom_line(aes(group = region), alpha=0.2) 
yb<- yb + geom_line(data=fao_oferta[brazil,], aes(x=Year, y=Value, group = region), size=2, colour='red') 
yb<- yb + theme_gray(20)+ theme(legend.position="none")
yb<- yb + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 4),1)) 
yb<- yb + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
yb<- yb + ylab('Yield Kg') + xlab('Years')
yb<- yb + theme(axis.text.x=element_text(size=14, angle=360))
yb<- yb + theme(axis.text.y=element_text(size=14, angle=360))
yb<- yb + theme(plot.title=element_text(size=24, face = 'bold'))
yb<- yb + annotate(geom="text", x=1990, y=10000, label= "Brazil", size=30, color="black",  fontface="italic")
yb

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/Brazil.png", yb, width=10, height=10.5, units='in') #saves g

# Nigeria
yield_cassava <- which(fao_oferta$variable=="Yield" & fao_oferta$ItemName=="Cassava")
fao_oferta$region <- as.character(fao_oferta$region)
grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
Nigeria = c("Nigeria")
Nigeria <- as.numeric(unlist(grep2(pattern=Nigeria, x=as.character(fao_oferta$region))))
Nigeria <- base::intersect(yield_cassava, Nigeria)


yn<- ggplot(data = fao_oferta[yield_cassava,], 
             aes(x=Year, y=Value))
yn<- yn + geom_ribbon(aes(ymin=2500, ymax=20000), fill="lightblue", alpha=0.7)
yn<- yn + geom_line(aes(y= 20000), colour = 'black', size = 0.5, linetype="dashed") + geom_line(aes(y= 2500), colour = 'black', size = 0.5, linetype="dashed")
yn<- yn + geom_line(aes(group = region), alpha=0.2) 
yn<- yn + geom_line(data=fao_oferta[Nigeria,], aes(x=Year, y=Value, group = region), size=2, colour='red') 
yn<- yn + theme_gray(20)+ theme(legend.position="none")
yn<- yn + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 4),1)) 
yn<- yn + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
yn<- yn + ylab('Yield Kg') + xlab('Years')
yn<- yn + theme(axis.text.x=element_text(size=14, angle=360))
yn<- yn + theme(axis.text.y=element_text(size=14, angle=360))
yn<- yn + theme(plot.title=element_text(size=24, face = 'bold'))
yn<- yn + annotate(geom="text", x=1990, y=10000, label= "Nigeria", size=30, color="black",  fontface="italic")
yn

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/Nigeria.png", yn , width=10, height=10.5, units='in') #saves g

# Thailand Nigeria and Brazil
yield_cassava <- which(fao_oferta$variable=="Yield" & fao_oferta$ItemName=="Cassava")
fao_oferta$region <- as.character(fao_oferta$region)
grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')


yall<- ggplot(data = fao_oferta[yield_cassava,], 
            aes(x=Year, y=Value))
yall<- yall + geom_ribbon(aes(ymin=2500, ymax=20000), fill="lightblue", alpha=0.7)
yall<- yall + geom_line(aes(y= 20000), colour = 'black', size = 0.5, linetype="dashed") + geom_line(aes(y= 2500), colour = 'black', size = 0.5, linetype="dashed")
yall<- yall + geom_line(aes(group = region), alpha=0.2) 
yall<- yall + geom_line(data=fao_oferta[Nigeria,], aes(x=Year, y=Value, group = region), size=1.5, colour='blue') 
yall<- yall + geom_line(data=fao_oferta[thailandia,], aes(x=Year, y=Value, group = region), size=1.5, colour='green') 
yall<- yall + geom_line(data=fao_oferta[brazil,], aes(x=Year, y=Value, group = region), size=1.5, colour='purple') 
yall<- yall + theme_gray(20)+ theme(legend.position="none")
yall<- yall + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 4),1)) 
yall<- yall + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
yall<- yall + ylab('Yield Kg') + xlab('Years')
yall<- yall + theme(axis.text.x=element_text(size=14, angle=360))
yall<- yall + theme(axis.text.y=element_text(size=14, angle=360))
yall<- yall + theme(plot.title=element_text(size=24, face = 'bold'))
#yall<- yall + geom_dl(aes(label = Nigeria), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
#yall<- yall + annotate(geom="text", x=1990, y=10000, label= "Nigeria", size=30, color="black",  fontface="italic")
yall


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldCassava.png", yf2, width=10, height=10.5, units='in') #saves g

# graph production-------

ggq<- ggplot(data = fao_oferta[produ_cassava,], aes(x=Year, y=Value)) # con datos originales
ggq<- ggq + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Production'  & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=pmin_10, ymax=pmax_80, linetype=NA), fill='red', alpha=0.6) # con newdata
#ggq<- ggq + geom_line(data=testcarlos[which(testcarlos$variable=='Production' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='orange', alpha=1)
ggq<- ggq + theme_bw(20) + theme(legend.position="none")
ggq<- ggq + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
ggq<- ggq + scale_y_continuous(breaks = round(seq(min(100000), max(14000000), by = 500000),1)) 
ggq<- ggq + ggtitle("Cassava Production of World, \n & Intervals 90% (1960-2014)")
ggq<- ggq + ylab('Tonnes') + xlab('Years')
ggq<- ggq + theme(axis.text.x=element_text(size=14, angle=45))
ggq<- ggq + theme(axis.text.y=element_text(size=14, angle=360))
ggq<- ggq + theme(plot.title=element_text(size=24, face = 'bold'))
ggq

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/production90.png", ggq, width=15, height=10.5, units='in') #saves g

# graph Area------

gga<- ggplot(data = fao_oferta[area_cassava,], aes(x=Year, y=Value)) # con datos originales
gga<- gga + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Area harvested'  & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=pmin_10, ymax=pmax_80, linetype=NA), fill='green', alpha=0.6) # con newdata
#gga<- gga + geom_line(data=testcarlos[which(testcarlos$variable=='Production' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='orange', alpha=1)
gga<- gga + theme_bw(20) + theme(legend.position="none")
gga<- gga + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gga<- gga + scale_y_continuous(breaks = round(seq(min(20000), max(300000), by = 20000),1)) 
gga<- gga + ggtitle("Cassava Area of World, \n & Intervals 90% (1960-2014)")
gga<- gga + ylab('Ha') + xlab('Years')
gga<- gga + theme(axis.text.x=element_text(size=14, angle=45))
gga<- gga + theme(axis.text.y=element_text(size=14, angle=360))
gga<- gga + theme(plot.title=element_text(size=24, face = 'bold'))
gga

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/Area90.png", gga, width=15, height=10.5, units='in') #saves g

# graph Spatial information -----
#SPAM
spam_yield  # raster 
plot(World_1) # shapefile

class(World_1)
getinfo.shape(World_1) 
head(World_1@data) # con el arroba puedo conocer los datos dentro de shape file
View(World_1)

# para eliminar a la Antarctiva, greeland
World_1<- World_1[World_1@data$ADM0_NAME!="Antarctica",]
World_1<- World_1[World_1@data$ADM0_NAME!="Greenland",]

#graph spam
plot(spam_yield)
plot(World_1, bg="transparent", add=TRUE)



#visualizaciones tipo google earth
mymap <- gmap("Colombia", type = "satellite")
mapta <- gmap("thailand", type = "satellite")
mapssa <- gmap("SSA Africa", type = "satellite")
plot(mymap)
plot(mapssa)



#visualizacion de raster
levelplot(spam_yield)

p <- levelplot(spam_yield, layers=1,par.settings = RdBuTheme, margin = FALSE, main = "Cassava yield in tons/ha")
p<- p  + layer(sp.lines(World_1, lwd=0.7, col="darkgray")) 


tiff(filename=paste(pic,"spamyield.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p

dev.off()


# solo donde hay rendimientos

spam_yield[spam_yield[]==0]<- NA # smap_yield[] responde a todos los valores de raster
p1 <- levelplot(spam_yield, layers=1,par.settings = RdBuTheme, margin = FALSE, main = "Cassava Yield in kg/ha\nSPAM")
p1<- p1  + layer(sp.lines(World_1, lwd=0.7, col="darkgray")) 


tiff(filename=paste(pic,"spamyield1.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
p1

dev.off()


#   Rendimiento, produccion, area y brechas de rendimiento de RTB
yield <- map_rtb[[2]]  
hist(yield)
levelplot(yield)

# rendimientos
yield[yield[]==0]<- NA # smap_yield[] responde a todos los valores de raster
prtb <- levelplot(yield, layers=1,par.settings = RdBuTheme, margin = FALSE, main = "Cassava Yield in kg/ha\nRTB")
prtb<- prtb  + layer(sp.lines(World_1, lwd=0.7, col="darkgray")) 


tiff(filename=paste(pic,"RTByield.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
prtb

dev.off()

# brecha de rendimientos 
map_gap
sum(map_gap)
histogram(map_gap)
map_gap[map_gap[]==0]<- NA 

gap_yield <- levelplot(map_gap, layers=1,par.settings = RdBuTheme, margin = FALSE, main = "Cassava Gap Yield in kg/ha\nRTB")
gap_yield<- gap_yield  + layer(sp.lines(World_1, lwd=0.7, col="darkgray")) 


tiff(filename=paste(pic,"RTBGapYield.tif",sep=""), 
     width = 12, height = 8, units = 'in', res = 300)
gap_yield

dev.off()

#global trends----
# grafico heatmap
names(TradeCom)
View(fao_oferta)

#Trade network----

fao_matrix_trade
names(fao_matrix_trade)

#Cassava dried 1986 -----
flow_x_casDried_1986<- fao_matrix_trade[which(fao_matrix_trade$variable=="Export Quantity" & 
                              fao_matrix_trade$Item=="Cassava dried" & fao_matrix_trade$Year==1986),]
flow_x_casDried_1986$RecordOrder<-NULL
flow_x_casDried_1986$Reporter.Country.Code<-NULL
flow_x_casDried_1986$Partner.Country.Code<-NULL
flow_x_casDried_1986$Item<-NULL
flow_x_casDried_1986$Unit<-NULL
flow_x_casDried_1986$Year<- NULL
flow_x_casDried_1986$variable<- NULL
rownames(flow_x_casDried_1986) <- 1:nrow(flow_x_casDried_1986)


# length(unique(flow_x_casDried_1986$Reporter.Countries))
# length(unique(flow_x_casDried_1986$Partner.Countries))
# 
# dim(expand.grid(unique(df0$orig_reg), unique(df0$dest_reg)))
# dim(df0)
# dim(expand.grid(unique(flow_x_casDried_1986$Reporter.Countries), unique(flow_x_casDried_1986$Partner.Countries)))
# dim(flow_x_casDried_1986)

aux <- expand.grid(unique(flow_x_casDried_1986$Reporter.Countries), unique(flow_x_casDried_1986$Partner.Countries))
colnames(aux) <- colnames(flow_x_casDried_1986[,1:2])
aux2 <- flow_x_casDried_1986[,1:2]

require(sqldf)
aux2 <- sqldf("select * from aux except select * from aux2")
dim(aux2)
aux2$Value <- 0
flow_x_casDried_1986 <- rbind(flow_x_casDried_1986, aux2)
dim(flow_x_casDried_1986)

#flow_x_casDried_1986 <- as.matrix(flow_x_casDried_1986)
#flow_x_casDried_1986 <- as.data.frame(flow_x_casDried_1986)
flow_x_casDried_1986$Reporter.Countries <- as.character(flow_x_casDried_1986$Reporter.Countries)
flow_x_casDried_1986$Partner.Countries <- as.character(flow_x_casDried_1986$Partner.Countries)
flow_x_casDried_1986$Value <- as.numeric(flow_x_casDried_1986$Value)
flow_x_casDried_1986$Value[which(flow_x_casDried_1986$Value==0)] <- 1
flow_x_casDried_1986$Value <- log(flow_x_casDried_1986$Value, base=exp(1))
flow_x_casDried_1986$Value[which(flow_x_casDried_1986$Value==0)] <- 0.5

png('circos_test.png', width=8, height=8, units='in', res=300)
chordDiagram(x = flow_x_casDried_1986[flow_x_casDried_1986$Value>.6,])
dev.off()

png('circos_testyyy.png', width=8, height=8, units='in', res=300)
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))
chordDiagram(x = flow_x_casDried_1986[flow_x_casDried_1986$Value>9,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
#             annotationTrack = "name", annotationTrackHeight = c(0.05, 0.05),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
dev.off()

##plot parameters

circos.clear()

circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


#Exportaciones 1986. Cassava Dried

png('X_cassavaDried1986.png', width=8, height=8, units='in', res=300)
chordDiagram(x = flow_x_casDried_1986[flow_x_casDried_1986$Value>0.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()

#Casssava dried 2000----

flow_x_casDried_2000<- fao_matrix_trade[which(fao_matrix_trade$variable=="Export Quantity" & 
                                                fao_matrix_trade$Item=="Cassava dried" & fao_matrix_trade$Year==2000),]
flow_x_casDried_2000$RecordOrder<-NULL
flow_x_casDried_2000$Reporter.Country.Code<-NULL
flow_x_casDried_2000$Partner.Country.Code<-NULL
flow_x_casDried_2000$Item<-NULL
flow_x_casDried_2000$Unit<-NULL
flow_x_casDried_2000$Year<- NULL
flow_x_casDried_2000$variable<- NULL
rownames(flow_x_casDried_2000) <- 1:nrow(flow_x_casDried_2000)



# length(unique(flow_x_casDried_2000$Reporter.Countries))
# length(unique(flow_x_casDried_2000$Partner.Countries))
# dim(expand.grid(unique(flow_x_casDried_2000$Reporter.Countries), unique(flow_x_casDried_2000$Partner.Countries)))
# dim(flow_x_casDried_2000)

auxCarlos <- expand.grid(unique(flow_x_casDried_2000$Reporter.Countries), unique(flow_x_casDried_2000$Partner.Countries))
colnames(auxCarlos) <- colnames(flow_x_casDried_2000[,1:2])
auxCarlos2 <- flow_x_casDried_2000[,1:2]

require(sqldf)
auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
dim(auxCarlos2)
auxCarlos2$Value <- 0
flow_x_casDried_2000 <- rbind(flow_x_casDried_2000, auxCarlos2) # apilar 
dim(flow_x_casDried_2000)


flow_x_casDried_2000$Reporter.Countries <- as.character(flow_x_casDried_2000$Reporter.Countries)
flow_x_casDried_2000$Partner.Countries <- as.character(flow_x_casDried_2000$Partner.Countries)
flow_x_casDried_2000$Value <- as.numeric(flow_x_casDried_2000$Value)
flow_x_casDried_2000$Value[which(flow_x_casDried_2000$Value==0)] <- 1
flow_x_casDried_2000$Value <- log(flow_x_casDried_2000$Value, base=exp(1))
flow_x_casDried_2000$Value[which(flow_x_casDried_2000$Value==0)] <- 0.5


##plot parameters

circos.clear()

circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

#Exportaciones 2000. Cassava Dried

png('X_cassavaDried2000.png', width=8, height=8, units='in', res=300)
chordDiagram(x = flow_x_casDried_2000[flow_x_casDried_2000$Value>1.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()

#Casssava dried 2013----

flow_x_casDried_2013<- fao_matrix_trade[which(fao_matrix_trade$variable=="Export Quantity" & 
                                                fao_matrix_trade$Item=="Cassava dried" & fao_matrix_trade$Year=="2013"),]
flow_x_casDried_2013$RecordOrder<-NULL
flow_x_casDried_2013$Reporter.Country.Code<-NULL
flow_x_casDried_2013$Partner.Country.Code<-NULL
flow_x_casDried_2013$Item<-NULL
flow_x_casDried_2013$Unit<-NULL
flow_x_casDried_2013$Year<- NULL
flow_x_casDried_2013$variable<- NULL
rownames(flow_x_casDried_2013) <- 1:nrow(flow_x_casDried_2013)



# length(unique(flow_x_casDried_2013$Reporter.Countries))
# length(unique(flow_x_casDried_2013$Partner.Countries))
# dim(expand.grid(unique(flow_x_casDried_2013$Reporter.Countries), unique(flow_x_casDried_2013$Partner.Countries)))
# dim(flow_x_casDried_2013)

ac <- expand.grid(unique(flow_x_casDried_2013$Reporter.Countries), unique(flow_x_casDried_2013$Partner.Countries))
colnames(ac) <- colnames(flow_x_casDried_2013[,1:2])
ac2 <- flow_x_casDried_2013[,1:2]

require(sqldf)
ac2 <- sqldf("select * from ac except select * from ac2")
dim(ac2)
ac2$Value <- 0
flow_x_casDried_2013 <- rbind(flow_x_casDried_2013, ac2) # apilar 
dim(flow_x_casDried_2013)


flow_x_casDried_2013$Reporter.Countries <- as.character(flow_x_casDried_2013$Reporter.Countries)
flow_x_casDried_2013$Partner.Countries <- as.character(flow_x_casDried_2013$Partner.Countries)
flow_x_casDried_2013$Value <- as.numeric(flow_x_casDried_2013$Value)
flow_x_casDried_2013$Value[which(flow_x_casDried_2013$Value==0)] <- 1
flow_x_casDried_2013$Value <- log(flow_x_casDried_2013$Value, base=exp(1))
flow_x_casDried_2013$Value[which(flow_x_casDried_2013$Value==0)] <- 0.5


circos.clear()

circos.par(start.degree = 90, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

png('X_cassavaDried2013.png', width=8, height=8, units='in', res=300)
chordDiagram(x = flow_x_casDried_2013[flow_x_casDried_2013$Value>0.5,], transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)


dev.off()




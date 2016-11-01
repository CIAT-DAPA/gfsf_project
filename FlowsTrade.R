### trade yuca

### preparacion de los datos para presentacion de yuca.

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

#directorio
setwd("C:/Users/CEGONZALEZ/Documents/cassava") # print the current working directory - cwd 

# directorio de archivos para graficar
pic <-("C:/Users/CEGONZALEZ/Documents/cassava/graph/")


# Cargarlos los datos
bra<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/Comtrade_Brazil_starch_ootputR.csv", header = TRUE)
nig<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/Comtrade_Nigeria_starch_outputR.csv", header = TRUE)
tha<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/COMTRADE/Comtrade_Thailand-starch_outputR.csv", header = TRUE)



# organizar tipos de datos

#Limitar numero de decimales y remove scientific notation-----
options(digits=3) 
options(scipen=999)

# apilar los datos
# unir los datos en un append
yuca <- rbind(nig,bra,tha) # para poner bases de datos uno sobre el otros

# exportaciones periodo 1------------
colnames(yuca)[colnames(yuca)=="Netweight.kg"] <- "Value"
yucax<- subset.data.frame(x = yuca,yuca$TradeFlow=="Export")   # x + exportaciones 
yucax1<- subset.data.frame(x = yucax,yucax$Period==1 )  
yucax1$Period<-NULL
yucax1$TradeFlow<-NULL

# evaluacion de dimensiones 
dim(expand.grid(unique(yucax1$Reporter), unique(yucax1$Partner)))
dim(yucax1)


# crear objetos para emparejar las dimensiones 
aux_c <- expand.grid(unique(yucax1$Reporter), unique(yucax1$Partner)) # pone los datos unicos de los paises 
colnames(aux_c) <- colnames(yucax1[,1:2])  # cohincidir nombres
aux2_c <- yucax1[,1:2]

require(sqldf)
aux2_c <- sqldf("select * from aux_c except select * from aux2_c")
dim(aux2_c)
dim(yucax1)
aux2_c$Value <- 0
yucax1 <- rbind(yucax1, aux2_c)
dim(yucax1)

yucax1$Reporter<- as.character(yucax1$Reporter)
yucax1$Partner <- as.character(yucax1$Partner)
yucax1$Value <- as.numeric(yucax1$Value)
yucax1$Value[which(yucax1$Value==0)] <- 1
yucax1$Value <- log(yucax1$Value, base=exp(1))
yucax1$Value[which(yucax1$Value==0)] <- 0.5
rownames(yucax1) <- 1:nrow(yucax1)


circos.clear()

circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('yucaPatoPrueba1.png', width=9, height=9, units='in', res=300)
chordDiagram(x = yucax1[yucax1$Value>12,], transparency = 0.25,
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

# exportaciones periodo 2------------
colnames(yuca)[colnames(yuca)=="Netweight.kg"] <- "Value"
yucax<- subset.data.frame(x = yuca,yuca$TradeFlow=="Export" )  
yucax2<- subset.data.frame(x = yucax,yucax$Period==2 )  
yucax2$Period<-NULL
yucax2$TradeFlow<-NULL

# evaluacion de dimensiones 
dim(expand.grid(unique(yucax2$Reporter), unique(yucax2$Partner)))
dim(yucax2)


# crear objetos para emparejar las dimensiones 
aux_c <- expand.grid(unique(yucax2$Reporter), unique(yucax2$Partner)) # pone los datos unicos de los paises 
colnames(aux_c) <- colnames(yucax2[,1:2])  # cohincidir nombres
aux2_c <- yucax2[,1:2]

require(sqldf)
aux2_c <- sqldf("select * from aux_c except select * from aux2_c")
dim(aux2_c)
dim(yucax2)
aux2_c$Value <- 0
yucax2 <- rbind(yucax2, aux2_c)
dim(yucax2)

yucax2$Reporter<- as.character(yucax2$Reporter)
yucax2$Partner <- as.character(yucax2$Partner)
yucax2$Value <- as.numeric(yucax2$Value)
yucax2$Value[which(yucax2$Value==0)] <- 1
yucax2$Value <- log(yucax2$Value, base=exp(1))
yucax2$Value[which(yucax2$Value==0)] <- 0.5
rownames(yucax2) <- 1:nrow(yucax2)


circos.clear()

circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('yucaPato2.png', width=9, height=9, units='in', res=300)
chordDiagram(x = yucax2[yucax2$Value>13,], transparency = 0.25,
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

# exportaciones periodo 3------------
colnames(yuca)[colnames(yuca)=="Netweight.kg"] <- "Value"
yucax<- subset.data.frame(x = yuca,yuca$TradeFlow=="Export" )  
yucax3<- subset.data.frame(x = yucax,yucax$Period==2 )  
yucax3$Period<-NULL
yucax3$TradeFlow<-NULL

# evaluacion de dimensiones 
dim(expand.grid(unique(yucax3$Reporter), unique(yucax3$Partner)))
dim(yucax3)


# crear objetos para emparejar las dimensiones 
aux_c <- expand.grid(unique(yucax3$Reporter), unique(yucax3$Partner)) # pone los datos unicos de los paises 
colnames(aux_c) <- colnames(yucax3[,1:2])  # cohincidir nombres
aux2_c <- yucax3[,1:2]

require(sqldf)
aux2_c <- sqldf("select * from aux_c except select * from aux2_c")
dim(aux2_c)
dim(yucax3)
aux2_c$Value <- 0
yucax3 <- rbind(yucax3, aux2_c)
dim(yucax3)

yucax3$Reporter<- as.character(yucax3$Reporter)
yucax3$Partner <- as.character(yucax3$Partner)
yucax3$Value <- as.numeric(yucax3$Value)
yucax3$Value[which(yucax3$Value==0)] <- 1
yucax3$Value <- log(yucax3$Value, base=exp(1))
yucax3$Value[which(yucax3$Value==0)] <- 0.5
rownames(yucax3) <- 1:nrow(yucax3)


circos.clear()

circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('yucaPato33.png', width=9, height=9, units='in', res=300)
chordDiagram(x = yucax3[yucax3$Value>12.5,], transparency = 0.25,
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

# importaciones periodo 1--------

# importaciones periodo 2--------

# importaciones periodo 3--------

# graficas rendimiento FAOSTAT--------
#importar datos
fao_oferta<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAO_Prod_Yield_Area.csv", header = TRUE)
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


#matrices de relaciones comerciales 
fao_matrix_trade<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAOMatrixTrade.csv", header = TRUE)
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


#convertir de Hg a Kg relacion es 1 hg a 0.1 kkfao_oferta
r<- which(fao_oferta$variable=="Yield")

for(i in 1:nrow(fao_oferta)){
  if(i %in% r){
    fao_oferta$Value[i]<- fao_oferta$Value[i] * 0.1
  } else {  }
}

# parametros para realizar graficas ---------

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

#gap
testcarlos$gap <- testcarlos$p0_95-testcarlos$p0_05  


# filtros -------
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


# Area
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


# Rango
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

#mean and median
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


# graph brecha
gg4<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value))
gg4<- gg4 + geom_line(aes(group = region), alpha=0)
gg4<- gg4 + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=p0_05, ymax=p0_95, linetype=NA), fill='red', alpha=0.3)
gg4<- gg4 + geom_bar(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=gap), stat="identity",  fill="steelblue", alpha=0.7)  
gg4<- gg4 + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='black', alpha=0.8)
gg4<- gg4 + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg4<- gg4 + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg4<- gg4 + ggtitle("World Cassava Yield \n (1960-2014)")
gg4<- gg4 + geom_line(data=testcarlos_mean[which(testcarlos_mean$variable=='Yield' & testcarlos_mean$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='blue', alpha=0.8)
gg4<- gg4 + theme(axis.text.x=element_text(size=14, angle=45))
gg4<- gg4 + theme(axis.text.y=element_text(size=14, angle=360))
gg4<- gg4 + theme(plot.title=element_text(size=24, face = 'bold')) 
gg4<- gg4 + theme()
gg4<- gg4 + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
gg4<- gg4 + annotate(geom="text", x=2009, y=13000, label= "Mean", size=15, color="blue",  fontface="italic")
gg4<- gg4 + annotate(geom="text", x=2008, y=22000, label= "Gap", size=15, color="steelblue",  fontface="italic")
gg4<- gg4 + ylab('Yield Kg/Ha') + xlab('Years') 
gg4


ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/YieldAllCountries4.png", gg4, width=15, height=10.5, units='in') #saves g




#line  countries highlights  brazil, nigeria, 
gg<- ggplot(data = fao_oferta[yield_cassava,], aes(x=Year, y=Value))
gg<- gg + geom_line(aes(group = region), colour='purple', alpha=0.1)
#gg<- gg + geom_line(data=testcarlos[which(testcarlos$variable=='Yield' & testcarlos$ItemName=='Cassava'),], aes(x=Year, y=Value),size=1.5, colour='orange', alpha=1)
gg<- gg + geom_line(data=fao_oferta[Nigeria,], aes(x=Year, y=Value, group = region), size=1.3, colour='green') 
gg<- gg + geom_line(data=fao_oferta[thailandia,], aes(x=Year, y=Value, group = region), size=1.3, colour='orange') 
gg<- gg + geom_line(data=fao_oferta[brazil,], aes(x=Year, y=Value, group = region), size=1.3, colour='blue') 
gg<- gg + theme(legend.position="none") + theme_classic()
gg<- gg + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gg<- gg + scale_y_continuous(breaks = round(seq(min(0), max(40000), by = 5000),1)) 
gg<- gg + ggtitle("World Cassava Yield \n (1960-2014)")
gg<- gg + ylab('Yield Kg/Ha') + xlab('Years')
gg<- gg + theme(axis.text.x=element_text(size=14, angle=45))
gg<- gg + theme(axis.text.y=element_text(size=14, angle=360))
gg<- gg + theme(plot.title=element_text(size=24, face = 'bold'))
#gg<- gg + annotate(geom="text", x=2008, y=9000, label= "Median", size=15, color="black",  fontface="italic")
gg<- gg + annotate(geom="text", x=2008, y=9000, label= "Nigeria", size=15, color="green",  fontface="italic")
gg<- gg + annotate(geom="text", x=2008, y=26000, label= "Thailand", size=15, color="orange",  fontface="italic")
gg<- gg + annotate(geom="text", x=2008, y=18000, label= "Brazil", size=15, color="blue",  fontface="italic")
gg



tiff(filename = paste("NigerBrazilThai.tiff"),width = 15, height = 10.5, units = 'in', res = 300)

gg

dev.off()

ggsave(file="C:/Users/CEGONZALEZ/Documents/cassava/graph/NigerBrazilThai.png", gg, width=15, height=10.5, units='in') 


# Production------
# production total
fao_oferta<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAO_Prod_Yield_Area.csv", header = TRUE)
#cambiar nombres de variables y eliminar variables innecesarias
fao_oferta$Domain.Code<- NULL
fao_oferta$Flag<- NULL
fao_oferta$FlagD<- NULL
fao_oferta$ItemCode<- NULL
fao_oferta$ElementCode<- NULL
fao_oferta<- fao_oferta[-(17031:17032),]
names(fao_oferta)[names(fao_oferta) == 'ElementName'] <- 'variable'
names(fao_oferta)[names(fao_oferta) == 'AreaName'] <- 'region'
produ_cassava<- which(fao_oferta$variable=="Production" & fao_oferta$ItemName=="Cassava")

for(j in 1:nrow(fao_oferta)){
  
  if(j %in% produ_cassava){
    fao_oferta$Value[j] <- fao_oferta$Value[j] /1000000
    #  testcarlos$pm[j] <- testcarlos$pm[j] /1000000
    # testcarlos$ph[j] <- testcarlos$ph[j] /1000000
    
  } else {  }
  
}

testcarlos<- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), sum(Value, na.rm=TRUE)))
colnames(testcarlos)[4] <- 'Value'


quantileFun <- function(x){z <- stats::quantile(x, probs=0.05, na.rm=TRUE); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), quantileFun(Value)))
testcarlos$pm <- as.numeric(aux[,ncol(aux)])

quantileFun <- function(x){z <- stats::quantile(x, probs=1, na.rm=TRUE); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), quantileFun(Value)))
testcarlos$ph <- as.numeric(aux[,ncol(aux)])


# graph
ggq<- ggplot(data = fao_oferta[produ_cassava,], aes(x=Year, y=Value)) # con datos originales
ggq<- ggq + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Production'  & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=0, ymax=Value, linetype=NA), fill="blue", alpha=0.7) 
ggq<- ggq + theme() + theme(legend.position="none")
ggq<- ggq + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
ggq<- ggq + scale_y_continuous(breaks = round(seq(min(0), max(290), by =40),1)) 
ggq<- ggq + ggtitle("World Cassava Production \n  (1960-2014)")
ggq<- ggq + ylab('Million of Tons') + xlab('Years')
ggq<- ggq + theme(axis.text.x=element_text(size=14, angle=45))
ggq<- ggq + theme(axis.text.y=element_text(size=14, angle=360))
ggq<- ggq + theme(plot.title=element_text(size=24, face = 'bold'))
ggq



tiff(filename = paste("ProductioTotal.tiff"),width = 15, height = 10.5, units = 'in', res = 300)

ggq

dev.off()


# Area-----------------
fao_oferta<- read.csv("C:/Users/CEGONZALEZ/Documents/cassava/FAOSTAT/FAO_Prod_Yield_Area.csv", header = TRUE)
#cambiar nombres de variables y eliminar variables innecesarias
fao_oferta$Domain.Code<- NULL
fao_oferta$Flag<- NULL
fao_oferta$FlagD<- NULL
fao_oferta$ItemCode<- NULL
fao_oferta$ElementCode<- NULL
fao_oferta<- fao_oferta[-(17031:17032),]
names(fao_oferta)[names(fao_oferta) == 'ElementName'] <- 'variable'
names(fao_oferta)[names(fao_oferta) == 'AreaName'] <- 'region'
area_cassava<- which(fao_oferta$variable=="Area harvested" & fao_oferta$ItemName=="Cassava")

for(j in 1:nrow(fao_oferta)){
  
  if(j %in% area_cassava){
    fao_oferta$Value[j] <- fao_oferta$Value[j] /1000000

  } else {  }
  
}

testcarlos<- as.data.frame(dplyr::summarise(group_by(fao_oferta,variable,ItemName,Year), sum(Value, na.rm=TRUE)))
colnames(testcarlos)[4] <- 'Value'



# graph
gga<- ggplot(data = fao_oferta[area_cassava,], aes(x=Year, y=Value)) # con datos originales
gga<- gga + geom_ribbon(data=testcarlos[which(testcarlos$variable=='Area harvested'  & testcarlos$ItemName=='Cassava'),], aes(x=Year, ymin=0, ymax=Value, linetype=NA), fill="orange", alpha=0.7) 
gga<- gga + theme() + theme(legend.position="none")
gga<- gga + scale_x_continuous(breaks = round(seq(min(1960), max(2014), by = 2),1)) 
gga<- gga + scale_y_continuous(breaks = round(seq(min(0), max(25), by =5),1)) 
gga<- gga + ggtitle("World Cassava Area harvested \n  (1960-2014)")
gga<- gga + ylab('Millions of area harvested') + xlab('Years')
gga<- gga + theme(axis.text.x=element_text(size=14, angle=45))
gga<- gga + theme(axis.text.y=element_text(size=14, angle=360))
gga<- gga + theme(plot.title=element_text(size=24, face = 'bold'))
gga



tiff(filename = paste("AreaTotalCosechada.tiff"),width = 15, height = 10.5, units = 'in', res = 300)

gga

dev.off()

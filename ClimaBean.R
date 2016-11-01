# Analisis de datos  de climata de Ricky Roberson 

# librerias-----------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)


# Directorio de trabajo----
setwd("C:/Users/CEGONZALEZ/Documents/GFSF/")
#Dirrectorio de graficos
graphB<-"C:/Users/CEGONZALEZ/Documents/GFSF/"

# Importa datos ----
clima<-read.csv("C:/Users/CEGONZALEZ/Documents/GFSF/DataClimateBeans.csv")

# eliminar y depurar datos
clima<-clima[!(clima$region=="AA World"),]
clima<-clima[!(clima$region=="AC Major Beans"),]
clima<-clima[!(clima$region=="AC Major Rice"),]
clima<-clima[!(clima$region=="Latin America and Caribbean"),]


# importante para renombrar las columnas
rownames(clima) <- 1:nrow(clima)

# there are manu RCPs
#rcp2p6_2041_2070 rcp4p5_2041_2070 rcp6p0_2041_2070 rcp8p5_2041_2070
clima_rcp8p5 <- clima[grep(pattern = "rcp8p5_2041_2070",x =clima$scenarios, fixed = FALSE ),]

# rename variables
clima_rcp8p5$scenarios<-revalue(clima_rcp8p5$scenarios,c("noresm1_m__future_rcp8p5_2041_2070"="noresm1_m"))
clima_rcp8p5$scenarios<-revalue(clima_rcp8p5$scenarios,c("hadgem2_es__future_rcp8p5_2041_2070"="hadgem2_es"))
clima_rcp8p5$scenarios<-revalue(clima_rcp8p5$scenarios,c("miroc_esm_chem__future_rcp8p5_2041_2070"="miroc_esm_chem"))
clima_rcp8p5$scenarios<-revalue(clima_rcp8p5$scenarios,c("ipsl_cm5a_lr__future_rcp8p5_2041_2070"="ipsl_cm5a_lr"))
clima_rcp8p5$scenarios<-revalue(clima_rcp8p5$scenarios,c("gfdl_esm2m__future_rcp8p5_2041_2070"="gfdl_esm2m"))
clima_rcp8p5$scenarios <- as.character(clima_rcp8p5$scenarios)
clima_rcp8p5$region<- as.character(clima_rcp8p5$region)
rownames(clima_rcp8p5) <- 1:nrow(clima_rcp8p5)

#levels(clima_rcp8p5$scenarios)

# graphs-----
# type tile rainfall annual and areas rainfed

gg1 <- ggplot(data=clima_rcp8p5[clima_rcp8p5$climate=="rainfall, annual total (mm)" & clima_rcp8p5$scenarios!="base_2000" & clima_rcp8p5$systems=="over rainfed bean areas",] , aes(y=scenarios, x=region, fill=average)) + geom_tile(color="white", size=0.1)
gg1 <- gg1 + scale_fill_gradient2(name='Average Rain (mm)', low="red", high="blue", guide="colorbar") 
gg1 <- gg1 + coord_equal()
gg1 <- gg1 + ggtitle('Rainfall, annual total (mm)\n average (2041-2070)') + ylab('Scenarios') + xlab('Countries')
gg1 <- gg1 + theme_bw()
gg1 <- gg1 + theme(axis.text.x=element_text(size=10, angle=90))
gg1 <- gg1 + theme(axis.text.y=element_text(size=12))
gg1 <- gg1 + theme(axis.title.x=element_text(size=12, face='bold'))
gg1 <- gg1 + theme(plot.title=element_text(size=15, face = 'bold'))
gg1 <- gg1 + theme(legend.text = element_text(size=10))
gg1 <- gg1 + theme(legend.title = element_text(size=10, face = 'bold'))
#gg1


gg2 <- ggplot(data=clima_rcp8p5[clima_rcp8p5$climate=="high temperature, annual average (C)" & clima_rcp8p5$scenarios!="base_2000" & clima_rcp8p5$systems=="over rainfed bean areas",] , aes(y=scenarios, x=region, fill=average)) + geom_tile(color="white", size=0.1)
gg2 <- gg2 + scale_fill_gradient2(name='Average temperature (mm)', low="blue", high="Red", guide="colorbar") 
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + ggtitle('High temperature, annual average (C)\n average (2041-2070)') + ylab('Scenarios') + xlab('Countries')
gg2 <- gg2 + theme_bw()
gg2 <- gg2 + theme(axis.text.x=element_text(size=10, angle=90))
gg2 <- gg2 + theme(axis.text.y=element_text(size=12))
gg2 <- gg2 + theme(axis.title.x=element_text(size=12, face='bold'))
gg2 <- gg2 + theme(plot.title=element_text(size=15, face = 'bold'))
gg2 <- gg2 + theme(legend.text = element_text(size=10))
gg2 <- gg2 + theme(legend.title = element_text(size=10, face = 'bold'))
#gg2


grid.arrange(gg1, gg2)

g <- arrangeGrob(gg1, gg2, nrow=2) #generates g
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/ClimateBean.png", g, width=10, height=10.5, units='in') #saves g


# tabla de medias versus base
names(clima_rcp8p5)
climaRain<- clima_rcp8p5[ which(clima_rcp8p5$climate=="rainfall, annual total (mm)"),]    
# importante para renombrar las columnas
rownames(climaRain) <- 1:nrow(climaRain)

climaRain$total.positive.area<- NULL
climaRain$total<- NULL
climaRain$total.area<- NULL


#reshape
climatest <- climaRain %>%
  spread("scenarios","average")

climatest$rain_mean <- rowMeans(x=climatest[,4:ncol(climatest)], na.rm=TRUE)

rainfed_climate<- climatest[ which(climatest$systems=="over rainfed bean areas"),]    

rainfed_climate$region <- factor(rainfed_climate$region, levels=rainfed_climate$region[order(rainfed_climate$rain_mean)])  # ordenar las regiones por valor de los impactos Value


# graph rain GCM average
gg <- ggplot(rainfed_climate, aes(x=rainfed_climate$region, y=rainfed_climate$rain_mean, fill=rainfed_climate$rain_mean)) + geom_bar(stat="identity") + coord_flip()
gg <- gg + ggtitle("Average Rain \n (2041-2070)") + xlab('Countries') + ylab('mm')
gg <- gg + theme_bw()
gg <- gg + scale_fill_gradient2(name='Average rain', low="red", high="blue", guide="colorbar") 
gg <- gg + theme(axis.text.y=element_text(size=12))
gg <- gg + theme(axis.text.y=element_text(size=12))
gg <- gg + theme(axis.title.x=element_text(size=12, face='bold'))
gg <- gg + theme(plot.title=element_text(size=15, face = 'bold'))
gg
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/AverageRain.png", gg, width=10, height=10.5, units='in') #saves g


# diferences mean base and  climate
clima
levels(clima$scenarios)
clima_rcp8p5 <- clima[grep(pattern = "rcp8p5_2041_2070",x =clima$scenarios, fixed = FALSE ),]
clima_base <- clima[grep(pattern = "base_2000" ,x =clima$scenarios, fixed = FALSE ),]

data <- clima_rcp8p5
data<- data[which(data$climate=="rainfall, annual total (mm)"),]    
data$total.positive.area<- NULL
data$total<- NULL
data$total.area<- NULL

# rename variables
data$scenarios<-revalue(data$scenarios,c("noresm1_m__future_rcp8p5_2041_2070"="noresm1_m"))
data$scenarios<-revalue(data$scenarios,c("hadgem2_es__future_rcp8p5_2041_2070"="hadgem2_es"))
data$scenarios<-revalue(data$scenarios,c("miroc_esm_chem__future_rcp8p5_2041_2070"="miroc_esm_chem"))
data$scenarios<-revalue(data$scenarios,c("ipsl_cm5a_lr__future_rcp8p5_2041_2070"="ipsl_cm5a_lr"))
data$scenarios<-revalue(data$scenarios,c("gfdl_esm2m__future_rcp8p5_2041_2070"="gfdl_esm2m"))
data$scenarios <- as.character(data$scenarios)
data$region<- as.character(data$region)
rownames(data) <- 1:nrow(data)
datamean <- data %>%
  spread("scenarios","average")

datamean$rain_mean <- rowMeans(x=datamean[,4:ncol(datamean)], na.rm=TRUE)
datamean$gfdl_esm2m<-NULL
datamean$hadgem2_es<-NULL
datamean$ipsl_cm5a_lr<-NULL
datamean$miroc_esm_chem<-NULL
datamean$noresm1_m<-NULL


clima_base
clima_base$total.positive.area<- NULL
clima_base$total<- NULL
clima_base$total.area<- NULL
rownames(clima_base) <- 1:nrow(clima_base)

database <- clima_base %>%
  spread("scenarios","average")
database<- database[which(database$climate=="rainfall, annual total (mm)"),]    
rownames(database) <- 1:nrow(database)


# join -------

ca <- merge(datamean,database, by=c("region", "climate", "systems"))
ca$Change<- (ca$rain_mean-ca$base_2000)/ca$base_2000*100
names(ca)
ca<- ca [c("region", "climate", "systems","base_2000", "rain_mean", "Change")]

# graph rain GCM average
x <- ggplot(data=ca[which(ca$climate=="rainfall, annual total (mm)" & ca$systems=="over rainfed bean areas"),], aes(x=region, y=Change, fill=Change)) + geom_bar(stat="identity") + coord_flip()
x <- x + ggtitle("Rainfall change, Base Vs GCMs \n (2041-2070)") + xlab('Countries') + ylab('percentage')
#x <- x + theme_bw()
x <- x + scale_fill_gradient2(name='Average Change', low="red", high="blue", guide="colorbar") 
x <- x + theme(axis.text.y=element_text(size=14))
x <- x + theme(axis.text.x=element_text(size=14))
x <- x + theme(axis.title.x=element_text(size=14, face='bold'))
x <- x + theme(plot.title=element_text(size=15, face = 'bold'))
x
ggsave(file="C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/datos procesados/ChangeRain.png", x, width=10, height=10.5, units='in') #saves g


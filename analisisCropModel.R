# analisis de datos  crops model. 


#Cargar librerias-----
library(reshape)
library(nplot2)
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)

# Limitar numero de decimales-----
options(digits=3) 

# Definir directorios de trabajo y de graficos------
#Directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados")
#Dirrectorio de graficos
pictures<-"C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/"

c <- read.csv("C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/AnalysisBeanModelCrop.csv",header=T)
names(c)
str(c$gcm)
levels(c$gcm)  # niveles GCMS
levels(c$pais)

# crear variable Zonas------
#Africa 
#c_africa<-subset(c, c$pais=="AGO"|c$pais=="BDI"|c$pais== "BEN"|c$pais=="BFA"|c$pais== "BWA"|c$pais== "CAF"|c$pais=="CIV"|
#                   c$pais== "CMR"|c$pais== "COD"|c$pais=="COG"|c$pais== "DJI"|c$pais== "DZA"|c$pais=="EGY"|c$pais== "ERI"|
#                  c$pais== "ETH"|c$pais=="GAB"|c$pais== "GHA"|c$pais== "GIN"|c$pais=="GMB"|c$pais=="GNB" |c$pais=="GNQ"|
#                 c$pais=="KEN"|c$pais== "LBR"|c$pais== "LBY"|c$pais== "LSO"|c$pais== "MDG"|c$pais== "MLI"|c$pais=="MOR"|
#                 c$pais== "MOZ"|c$pais== "MRT"|c$pais=="MWI"|c$pais== "NAM"|c$pais== "NER"|c$pais=="NGA") 

#ALC     
#c_alc<-subset(c, c$pais=="ARG"|c$pais== "BLZ"|c$pais== "BOL" |c$pais=="BRA" |c$pais=="CAN" |c$pais=="CHL"|c$pais=="COL" |
#               c$pais=="CRI"|c$pais== "ECU"|c$pais== "GRL" |c$pais=="GSA" |c$pais=="GTM" |c$pais=="HND" |c$pais=="MEX" |
#                c$pais=="NIC")

grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
afList2<- c("AGO", "BDI","BEN","BFA","BWA","CAF","CIV","CMR","COD","COG","DJI","DZA","EGY","ERI","ETH","GAB","GHA","GIN",
            "GMB","GNB","GNQ","KEN","LBR","LBY","LSO","MDG","MLI","MOR","MOZ","MRT","MWI","NAM","NER","NGA")
c_africa2 <- c[unlist(grep2(pattern=afList2, x=c$pais)),]

afList3<- c("ARG","BLZ","BOL","BRA","CAN","CHL","COL","CRI","ECU","GRL","GSA","GTM","HND", "MEX","NIC", "CRB","CUB","DOM","HTI","JAM")
c_alc2<-c[unlist(grep2(pattern = afList3, x =c$pais)),]

# Grafico que integra todos los escenarios Bean Normal y Bean techn----
# Africa graficos

eg <- ggplot(c_africa2, aes(gcm,yield)) 
eg <- eg + geom_boxplot(outlier.colour ="red" ) 
eg <- eg + ggtitle(label = "Rendimientos para Africa")
eg <- eg + theme(axis.text.x=element_text(size=10, angle=45))

tiff(filename=paste(pictures,"BoxTYieldAfrica.tif",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)
eg

dev.off()



# LAC graficos

l <- ggplot(c_alc2, aes(gcm,yield)) 
l <- l + geom_boxplot(outlier.colour ="red" ) 
l <- l + ggtitle(label = "Rendimientos para ALC")
l <- l + theme(axis.text.x=element_text(size=10, angle=45))

tiff(filename=paste(pictures,"BoxTYieldLAC.tif",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)
l

dev.off()

plot(l)

# subset normal y tech----
#africa
# primero cambiar  forma o reshape
africa<-c_africa2 %>%
  spread("gcm","yield")

alc<-c_alc2 %>%
  spread("gcm","yield")

#extraer datos de frijol normal y de frijol techn
#Base
names(africa)
baseafrica<- c("yrs", "pais", "BeanTechn", "BeanNormal") 
africaBase<- africa[baseafrica]
africaBase<- africaBase[-c(31:60),]

names(alc)
basealc<- c("yrs", "pais", "BeanTechn", "BeanNormal") 
alcBase<- alc[basealc]
alcBase<- alcBase[-c(15:28),]
alcBase

#clima
climaAfrica<- c("yrs", "pais","BeanNormalgfdl_esm2m", "BeanNormalhadgem2_es", "BeanNormalipsl_cm5a_lr",
                "BeanNormalmiroc_esm_chem", "BeanNormalnoresm1_m","BeanTechngfdl_esm2m","BeanTechnhadgem2_es",
                "BeanTechnipsl_cm5a_lr", "BeanTechnmiroc_esm_chem","BeanTechnnoresm1_m__" )
africaclima<- africa[climaAfrica]
africaclima<- africaclima[-c(1:29),]

climaalc<- c("yrs", "pais","BeanNormalgfdl_esm2m", "BeanNormalhadgem2_es", "BeanNormalipsl_cm5a_lr",
                "BeanNormalmiroc_esm_chem", "BeanNormalnoresm1_m","BeanTechngfdl_esm2m","BeanTechnhadgem2_es",
                "BeanTechnipsl_cm5a_lr", "BeanTechnmiroc_esm_chem","BeanTechnnoresm1_m__" )
alcclima<- alc[climaalc]
alcclima<- alcclima[-c(1:14),]

# calcular las medias del escenario con cambio climatico-----
#africa
africaclima$meanCCNormal <- rowMeans(x=africaclima[,3:7], na.rm=TRUE)
africaclima$meanCCTech <- rowMeans(x=africaclima[,8:12], na.rm=TRUE)
#alc
alcclima$meanCCNormal<- rowMeans(x=alcclima[,3:7], na.rm = TRUE)
alcclima$meanCCTech<- rowMeans(x=alcclima[,8:12], na.rm = TRUE)

# unir datos frijol normal y frijol con tecnologia-----
africabase2005 <- africaBase 
africabase2005$yrs <- NULL
africagcm <- africaclima
africagcm$yrs <- NULL

alcBase2005 <- alcBase
alcBase2005$yrs <- NULL
alcgcm <- alcclima
alcgcm$yrs <- NULL

# join -------

m_africa <- merge(africabase2005,africagcm, by.x = "pais") 
m_alc <- merge(alcBase2005,alcgcm, by.x = "pais") 
m_zonas <- rbind(m_alc, m_africa) # para poner bases de datos uno sobre el otros

# crear una variable nueva que integre las regiones
m <-m_zonas
m$zonas <- NA
m$zonas[1:14] <- "ALC"
m$zonas[15:nrow(m)] <- "Africa"

# cambio de forma de base de datos
btotal <- m # copia de los datos con todos los escenarios
bsum  <- m  # copia de los datos solo con las medias
bsum <- bsum[,-(4:13)] # eliminando variables de gcm no necesarias ya tengo la media

# calcular impacto
names(bsum)
im<- bsum
im$impacto <- (im$meanCCTech - im$meanCCNormal)/im$meanCCNormal * 100
im$impacto <- as.numeric(im$impacto)
im <- im [c("pais", "zonas" , "BeanTechn","BeanNormal", "meanCCNormal", "meanCCTech", "impacto"  )] # reorganizando las variables 
names(im)
im <- im %>% gather(Escenario, Value, 3:7)



#reshape long
dplyr::glimpse(btotal) # obtengo informacion basica de un base datos densa
btotal <- btotal %>% gather(Escenarios, Value, 2:15)
bsum <- bsum %>% gather(Escenarios, Value, 2:5)

# graficos -------

labels <- as.data.frame(bsum %>% group_by(zonas, Escenarios) %>% summarise(Mean=mean(Value)))
labels$Mean <- round(labels$Mean, 1)

t <- ggplot(bsum, aes(x=Escenarios, y=Value, fill=zonas))
t <- t + geom_bar(stat="summary", fun.y = "mean", position="dodge", size=.3, show.legend=TRUE)
t <- t + xlab("Scenarios") + ylab("Yields")
t <- t + geom_text(data=labels, aes(x=Escenarios, y=Mean, label=as.character(Mean)), size=5.5, position=position_dodge(width=0.9), vjust=-0.25) # , hjust=0.5
t <- t + theme(axis.text.x=element_text(size=12))
t <- t + theme(axis.text.y=element_text(size=12))
t <- t + theme(axis.title.x=element_text(size=14, face='bold'))
t <- t + theme(axis.title.y=element_text(size=14, face='bold'))
t <- t + theme(plot.title=element_text(size=16, face='bold'))
t <- t + theme(legend.text = element_text(size=12))
t <- t + theme(legend.title = element_text(size=12, face = 'bold'))
t <- t + ggtitle(label = "Rendimientos promedios con tecnología y sin tecnología por Regiones")
t <- t + theme(plot.title = element_text(lineheight=.8, face="bold")) 
t <- t + scale_fill_manual(values=c("forestgreen", "blue")) 
t <- t + scale_x_discrete(breaks = c("BeanNormal", "BeanTechn", "meanCCNormal", "meanCCTech"), 
                              labels=c("NoCCNoTech", "NoCCTech", "CCNoTech", "CCTech"))


tiff(filename=paste(pictures,"grafico3.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 120)

t

dev.off()


# grafico ganancias de la tecnologia modelo de cultivos
names(im)
# Yield plot
im$Escenario<- as.character(im$Escenario)

#AFrica
n <- ggplot(im[im$Escenario!="impacto" & im$zonas=="Africa",], aes(x=Escenario, y=pais , fill=Value)) + geom_tile(color="white", size=0.1)
n <- n + scale_fill_gradient2(name='Promedio', low="darkblue", high="blue", guide="colorbar") 
n <- n + coord_equal()
n <- n + gtitle('Análisis de Rendimientos') + ylab('Paises') + xlab('Escenarios')
n <- n + theme_bw()
n <- n + theme(axis.text.x=element_text(size=12, angle=90))
n <- n + theme(axis.text.y=element_text(size=12))
n <- n + theme(axis.title.x=element_text(size=12, face='bold'))
n <- n + theme(plot.title=element_text(size=15, face = 'bold'))
n <- n + theme(legend.text = element_text(size=14))
n <- n + theme(legend.title = element_text(size=14, face = 'bold'))
n <- n + scale_x_discrete(breaks = c("BeanNormal", "BeanTechn", "meanCCNormal", "meanCCTech"), 
                                   labels=c("NoCCNoTech", "NoCCTech", "CCNoTech", "CCTech"))



tiff(filename=paste(pictures,"grafico3Baf.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 120)

n

dev.off()

# ALC
al <- ggplot(im[im$Escenario!="impacto" & im$zonas=="ALC",], aes(x=Escenario, y=pais , fill=Value)) + geom_tile(color="white", size=0.1)
al <- al + scale_fill_gradient2(name='Promedio', low="darkblue", high="blue", guide="colorbar") 
al <- al + coord_equal()
al <- al + gtitle('Análisis de Rendimientos') + ylab('Paises') + xlab('Escenarios')
al <- al + theme_bw()
al <- al + theme(axis.text.x=element_text(size=12, angle=90))
al <- al + theme(axis.text.y=element_text(size=12))
al <- al + theme(axis.title.x=element_text(size=12, face='bold'))
al <- al + theme(plot.title=element_text(size=15, face = 'bold'))
al <- al + theme(legend.text = element_text(size=14))
al <- al + theme(legend.title = element_text(size=14, face = 'bold'))
al <- al + scale_x_discrete(breaks = c("BeanNormal", "BeanTechn", "meanCCNormal", "meanCCTech"), 
                          labels=c("NoCCNoTech", "NoCCTech", "CCNoTech", "CCTech"))



tiff(filename=paste(pictures,"grafico3Bal.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 120)

al

dev.off()

# graph ganancias de la tecnologia 


f <- ggplot(im[im$Escenario=="impacto",], aes(x=zonas, y=Value))
f <- f + geom_bar(stat="summary", fun.y = "mean", position="dodge", size=.3, show.legend=TRUE)




tiff(filename=paste(pictures,"grafico3Btotal.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 120)

f

dev.off()



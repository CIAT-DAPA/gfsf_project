# Comprobación de datos
# este script se ha desarrollado para analizar los resultados del informe de frijol

#Cargar librerias-----
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
library(foreign)
library(rgdal)
library(psych)
library(cluster)
library(fpc)
library(mclust)
library(d3heatmap)
library(RColorBrewer)

#Limitar numero de decimales-----
options(digits=3) 

#Definir directorios de trabajo ------
#Directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/")
#Dirrectorio de graficos
grd<-"C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/graph/"

#Cargar lista de nombres de los archivos----
nombres <- list.files(pattern ="*.csv" )
#Crear muna lista con los nombre de los marco de datos
nombres_md_list<-strsplit(nombres,".csv")
#nombres_md<-c(do.call("cbind",nombres_md_list))  
categoria<-gsub(pattern="Bean_",replacement="",x=nombres_md_list)

#Crear una lista vacia
pl<-NULL

#Cargar los datos dentro de la lista
for (i in 1: length(nombres_md_list)) {
  pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-read.csv(file=\'',nombres[i],'\',header=T)',sep='')))
  pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-data.frame(',nombres_md_list[i],',','"categoria"','=',
                                 "categoria[i]",')',sep='')))
  print(i)
}


#Juntar los elemntos de la lista en un marco de datos
md<-do.call(rbind, pl)

#Hacer un subconjunto que sólo contenga las variables de mi interés-----
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                md$impactparameter=="TAreaXAgg -- Total Area" |
                md$impactparameter== "TYldXAgg -- Total Yield" | 
                md$impactparameter== "QDXAgg -- Total Demand" |
                md$impactparameter== "QEXAgg -- Export" | 
                md$impactparameter== "QMXAgg -- Import" |
                md$impactparameter== "PWXAgg -- World Prices")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QSXAgg -- Total Production"="Production",  
                                                        "TAreaXAgg -- Total Area"="Area",
                                                        "TYldXAgg -- Total Yield"="Yield",
                                                        "QDXAgg -- Total Demand"="Demand",
                                                        "QEXAgg -- Export"="Export",
                                                        "QMXAgg -- Import"="Import",
                                                        "PWXAgg -- World Prices"="W Prices"))


#Hacer un subconjunto que sólo contenga frijol
mdsubcrop<-droplevels(subset(mdsub,mdsub$commodity=="PUL-Beans" | mdsub$commodity=="-"))
mdsubcrop$commodity<-revalue(mdsubcrop$commodity,c("PUL-Beans"="Beans"))



#Hacer un subconjunto de md que sólo contenga los paises de LAC y SSA (Easter, Middle, Western, Southern)
lac_africa_reg<-levels(mdsubcrop$region)[c(68:96,121,127:161)]

mdsubcrop_lac_afri<-mdsubcrop[which(mdsubcrop$region %in% lac_africa_reg),]

mdsubcrop_lac_afri<-droplevels(mdsubcrop_lac_afri)

mdwide <- reshape(mdsubcrop_lac_afri, v.names = "Val", idvar = 
                    c("scenario","commodity","region","productiontype",
                      "impactparameter","categoria"),timevar = "year", direction = "wide")

#subset solo rendimientos------
yieldNotech<- subset(mdwide, mdwide$categoria=="CC_NoTec" & mdwide$impactparameter=="Yield")
yieldtech<- subset(mdwide, mdwide$categoria=="CC_Tec" & mdwide$impactparameter=="Yield")
yieldNotech$cambio<- (yieldNotech$Val.2050-yieldNotech$Val.2020)/yieldNotech$Val.2020 *100
yieldtech$cambio<- (yieldtech$Val.2050-yieldtech$Val.2020)/yieldtech$Val.2020 *100

#exportar a stata y excel-----
#write.dta(yieldtech, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/yield.dta")
write.table(yieldtech, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/yieldtech.txt", sep="\t")
write.table(yieldNotech, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/yieldNotech.txt", sep="\t")

#graficas-------
#Con tecnologia
yt<-ggplot(yieldtech, aes(x=yieldtech$cambio, colour=yieldtech$scenario)) + geom_density(alpha=.3) 
  
yt +  xlab("Cambios") + 
  scale_colour_discrete(name="Escenarios")+
  ggtitle("Cambios porcentuales de rendimientos, \n entre 2020 y 2050 con Tecnologia") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/CambioYtec.png")                                                                                      

#Sin Tecnologia
ynt<-ggplot(yieldNotech, aes(x=yieldNotech$cambio, colour=yieldNotech$scenario)) + geom_density(alpha=.3) 

ynt +  xlab("Cambios") + 
  scale_colour_discrete(name="Escenarios")+
  ggtitle("Cambios porcentuales de rendimientos, \n entre 2020 y 2050 sin Tecnologia") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/CambioYNotec.png")                                                                                      


#Reshape de tech y no tech-----
#Tech
dplyr::glimpse(yieldtech) # obtengo informacion basica de un base datos densa
ytc<- yieldtech %>% 
  gather(yrs, ytech ,7:13)
ytc$commodity<-NULL # elimintar variables innecesarias
ytc$productiontype<-NULL # elimintar variables innecesarias
ytc$categoria<-NULL
#Notech
dplyr::glimpse(yieldNotech) # obtengo informacion basica de un base datos densa
yntc<- yieldNotech %>% 
  gather(yrs, yieldNotech ,7:13)
yntc$commodity<-NULL # elimintar variables innecesarias
yntc$productiontype<-NULL # elimintar variables innecesarias
yntc$categoria<-NULL

#eliminar palabra val. base de datos
ytc$yrs<- gsub(pattern='Val.', replacement='', x=ytc$yrs)
yntc$yrs<- gsub(pattern='Val.', replacement='', x=yntc$yrs)

# merge long y graficos----
ytc$scenario<- gsub(pattern='SSP2-', replacement='', x=ytc$scenario)
ytc$scenario<-gsub(pattern = "-8P5-379-TECH", replacement="", x=ytc$scenario)
yntc$scenario<- gsub(pattern='SSP2-', replacement='', x=yntc$scenario)
yntc$scenario<-gsub(pattern = "-8P5-379-NOTECH", replacement="", x=yntc$scenario)

mergeData<-merge(yntc, ytc, by=c("region", "scenario", "yrs"))
mergeData$impactparameter.x<-NULL
mergeData$impactparameter.y<-NULL
mergeData<-mergeData[!(mergeData$yrs=="cambio"),]
write.table(mergeData, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/mergedata.txt", sep="\t")

#graficas
rendimientos<-ggplot(mergeData,aes(x=mergeData$yieldNotech, y=mergeData$ytech, colour=mergeData$region)) + geom_point() +
  theme(axis.text.x = element_text(face="bold", color=, 
                                  size=9, angle=0),
       axis.text.y = element_text(face="bold", color=, 
                                  size=9, angle=0))+
        scale_y_continuous(name="Rendimientos TECH")+
        scale_x_continuous(name="Rendimientos NoTECH")
        ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/pointsYield.png")   

# merge wide y graficos----

#spread datos
#Melt datos
Database<- mergeData
Database<-Database %>%
  spread("yrs", "ytech", "yieldnotech")

wide <- reshape(Database, v.names = c("yieldNotech","ytech") , idvar = c("region", "scenario"),
                timevar = "yrs", direction = "wide")

wide <- wide[,c("region",	"scenario","yieldNotech.2005",	"yieldNotech.2010",	"yieldNotech.2020",
                "yieldNotech.2030",	"yieldNotech.2040",	"yieldNotech.2050",	"ytech.2005",	
                "ytech.2010",	"ytech.2020",	"ytech.2030",	"ytech.2040",	"ytech.2050")]

write.table(wide, "C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/wide2.txt", sep="\t")

# cambios importacion csv-----
## Read CSV file (header assumed), then put that into "csv.data" data object (any name is ok).
c <- read.csv("C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/WideYieldAnalisis.csv",header=T)

cambios<-ggplot(c,aes(x=c$CambioTech, y=c$CambioNotech, colour=c$region)) + geom_point() +
  theme(axis.text.x = element_text(face="bold", color=, 
                                   size=9, angle=0),
        axis.text.y = element_text(face="bold", color=, 
                                   size=9, angle=0))+
  scale_y_continuous(name="Cambios de Rendimientos TECH")+
  scale_x_continuous(name="Cambios de Rendimientos NoTECH")
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/cambiosYield.png")   



#Graficar el perfil de impacto por paises----
w<- c %>% 
  gather(cambio, value ,16:17)
w$yieldNotech.2005<-NULL
w$yieldNotech.2010<-NULL
w$yieldNotech.2020<-NULL
w$yieldNotech.2030<-NULL
w$yieldNotech.2040<-NULL
w$yieldNotech.2050<-NULL
w$ytech.2005<-NULL
w$ytech.2010<-NULL
w$ytech.2020<-NULL
w$ytech.2030<-NULL
w$ytech.2040<-NULL
w$ytech.2050<-NULL
w$alivio<-NULL

#grafico en prueba
gg<-ggplot(w, aes(x=w$cambio, y=w$region, fill=w$value)) 
gg<-gg +  geom_tile(color="white", size=0.45) 
gg<-gg+labs(x=NULL, y=NULL, title=NULL)
gg<-gg+theme(axis.ticks=element_blank()) 
gg<-gg+theme(axis.text=element_text(size=10))
gg<-gg+theme(legend.title=element_blank())
gg<-gg+scale_fill_continuous()


?geom_tile



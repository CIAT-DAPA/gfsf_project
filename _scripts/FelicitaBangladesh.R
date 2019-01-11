# Codigos para el procesamiento y organizacion de los graficos para el CSA. Climate Smart Acgriculture

#librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(dplyr)
library(xlsx)
library(foreign)

#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/")
#Direción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/CSA/"
#Direción copias seguridad
copy<-"C:/Users/CEGONZALEZ/Documents/CSA/copy/"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

################################################ PARTE A. ####################################################################


#Cargar marco de datos principal-------------
md<-read.csv(paste("./Felicitas&Ban.csv", sep = ""),header=T)
write.dta(md,"./BangladeshFelicitas3.3.dta")

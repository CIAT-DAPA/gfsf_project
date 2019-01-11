## data major grains in LAC

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

#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Desktop/IMPACT3-Model-ver3.3/OutputFiles/Aggregation")
#Direción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/CSA/"
#Direción copias seguridad
copy<-"C:/Users/CEGONZALEZ/Documents/CSA/copy/"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

################################################ PARTE A. ####################################################################


#Cargar marco de datos principal-------------
md<-read.csv("MajorGrains.csv",header=T)
phi<- md

#Conocer los cultivos y agregaciones 
cult<- unique(phi$commodity)

# cambiar los nombres de los cultivos
phi$commodity<-revalue(phi$commodity, c( "cmaiz"="Maize",
                                         "jmaiz"="Maize",
                                         "crice"="Rice",
                                         "jrice"="Rice",
                                         "cmg"="Major grains",
                                         "jmg"="Major grains",
                                         "jwhea"="Wheat",
                                         "cwhea"= "Wheat"))


# Dataframe para hacer analisis por systmas de produccion
phi_sys<- phi 


#Filtros por tipos de impactparameters------------------
# datos por sistema de produccion  riego y secano
tab<- table(phi$impactparameter, phi$productiontype!="total" & phi$productiontype!="-") #filtro
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )

# 1. datos categorias totales
tab<- table(phi$impactparameter, phi$productiontype=="total")
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area")

# 2. datos categorias agregados 
tab<- table(phi$impactparameter, phi$productiontype=="-")
dataagg<- c("FoodAvailXAgg", "PerCapKCalCXAgg -- PcKcal by Commodity","QDXAgg -- Total Demand",
            "QEXAgg -- Export","QINTXAgg -- Intermediate Demand","QMXAgg -- Import")

rm(tab)


# 4. data tratamiento especial
dataespecial<- c(  "QMSHXAgg -- Import Share of Demand",  "QNXAgg -- Net Trade")

# transformar  factor to character
phi$impactparameter<- as.character(phi$impactparameter)
phi$scenario<- as.character(phi$scenario)
phi$commodity<- as.character(phi$commodity)
phi$region<- as.character(phi$region)
phi$productiontype<- as.character(phi$productiontype)

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL
crops<- unique(phi$commodity)
cultivations<-  c( "Maize","Rice","Wheat", "Major grains")
cultivationsTrade<- c( "Maize","Rice","Wheat", "Major grains")
other<- c("-")
row.names(phi)<- 1: nrow(phi)

#filter regiones
patrones<- c("MEN-", "SAS-","EAP-", "LAC-","MEN-")
p=1
for(p in 1:length(patrones)){
      gdata<- phi[grep(pattern =patrones[p],x = phi$region, ignore.case = T),]
      gdata$zone<- paste(patrones[p])
      # selecciono el cultivo
      gdata<- gdata[which(gdata$impactparameter==datatotal[1]),]
      #reordeno los datos
      rownames(gdata)<- 1: nrow(gdata)
      #reshape a lo ancho  
      gdata<- gdata %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      # k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
      #elimino fila innecesarias
      gdata<-gdata[,-c(6:16)]
      #creo una variable= cambio porcentual 2020-2050
      gdata$Percentage_Change<- ((gdata$`2050`-gdata$`2020`)/gdata$`2020`)*100
      #Elimino columnas inncesarias y de paso los organizo  
      gdata<- gdata[, c("impactparameter","scenario","zone","commodity", "region", "Percentage_Change")]
      
      gdata$ssp <- ifelse(grepl("_ssp2",gdata$scenario),'SSP2','SSP3')
      write.csv(x = gdata, file = paste(copy,datatotal[1],"_",patrones[p],".csv",sep = "" ))

     
}

rm(gdata)

gdata<- list.files(copy, pattern = "TYldXAgg -- Total Yield",full.names = T)
gdata<- lapply(gdata, read.csv, header=T)
gdata<- do.call(rbind,gdata)
gdata$X<- NULL

write.csv(x = gdata, file = paste(copy,"Total_Yield_Completed.csv",sep = "" ))


k<- list()

for(p in 1:length(patrones)){
      
      if(p==1){
            gdata<- phi[grep(pattern =patrones[p],x = phi$region, ignore.case = T),]
            gdata$zone<- paste(patrones[p])
            # selecciono el cultivo
            gdata<- gdata[which(gdata$impactparameter==datatotal[1]),]
            
            rownames(gdata)<- 1: nrow(gdata)
            #reshape a lo ancho  
            gdata<- gdata %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
            #elimino fila innecesarias
            k[[i]]<-k[[i]][,-c(5:19)]
            #creo una variable= cambio porcentual 2020-2050
            k[[i]]$p.p<- (k[[i]]$X2050-k[[i]]$X2020)
            #Elimino columnas inncesarias y de paso los organizo  
            k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","p.p")]
            #Reshape para tener cambio porcentual por GCM
            k[[i]]<- k[[i]] %>%
                  spread(scenario, p.p)
            #Calculo de la media por los gcms
            k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
            #Selecciono las variable necesarias 
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
            k[[i]]$NoCC<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      } else {
            # selecciono el cultivo
            k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
            #elimino fila innecesarias
            k[[i]]<-k[[i]][,-c(5:19)]
            #creo una variable= cambio porcentual 2020-2050
            k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
            #Elimino columnas inncesarias y de paso los organizo  
            k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
            #Reshape para tener cambio porcentual por GCM
            k[[i]]<- k[[i]] %>%
                  spread(scenario, Percentage_Change)
            #Calculo de la media por los gcms
            k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
            #Selecciono las variable necesarias 
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
            k[[i]]$NoCC<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
            return 
            
      }
      
}
z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "ZIM.xlsx", sep = ""),
            sheetName = "DatosEspeciales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataespecial)){
      
      png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataespecial[i],"ZIM.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(z[which(z$impactparameter==dataespecial[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataespecial[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Impacts') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
      )
      dev.off()
      print(i)
}  

rm(z)

#5. Datos por sistemas de produccion--------

#sys<- c("air","arf")

k<- list()

for(i in seq_along(datasys)){
      
      # selecciono el cultivo
      k[[i]]<- phi_sys[which(phi_sys$impactparameter==datasys[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
      k[[i]]<-k[[i]][,-c(6:19)]
      k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
      #Elimino columnas inncesarias y de paso los organizo  
      k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","productiontype" ,"Cat","Percentage_Change")]
      #Reshape para tener cambio porcentual por GCM
      k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
      #Calculo de la media por los gcms
      k[[i]]$mean <- rowMeans(x=k[[i]][,6:ncol(k[[i]])], na.rm=TRUE)
      #Selecciono las variable necesarias 
      k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "productiontype", "Cat", "mean", "NoCC")]
      k[[i]]$NoCC<- NULL
      #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
      k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      return 
}


z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "ZIM.xlsx", sep = ""),
            sheetName = "SystemsProduction", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 5:ncol(z))

z$productiontype<- as.character(z$productiontype)
z$impactparameter<- as.character(z$impactparameter)
z$region<- as.character(z$region)
z$scenarios<- as.character(z$scenarios)
z$commodity<- as.character(z$commodity)

n<- list()
for (i in seq_along(datasys)){
      
      png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datasys[i],"ZIM.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(z[which(z$impactparameter==datasys[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datasys[i])+
                           facet_grid(productiontype~.)+ theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
      )
      dev.off()
      print(i)
}  



# limpiar todo---------
#g=gc; rm(list = ls())


# Codigos para el procesamiento y organizacion de los graficos para el CSA. Climate Smart Acgriculture
# Harold and Carlos 

#librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(lattice)
library(latticeExtra)
library(colorspace)
library(Rcpp)


#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/")
#Direción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/"
#Direción copias seguridad
copy<-"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/TNC project/TNC/"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

#Cargar marco de datos principal-------------
md<-read.csv("MaizeCol.csv",header=T)
nepal<- md
nepal$region<- as.character(nepal$region)
nepal$impactparameter<- as.character(nepal$impactparameter)
nepal$commodity<- as.character(nepal$commodity)
nepal$productiontype<- as.character(nepal$productiontype)
nepal$scenario<- as.character(nepal$scenario)

nepal<- nepal[which(nepal$region=="LAC-Colombia"),]
alc<- nepal[grepl("LAC-",x = nepal$region),]
row.names(alc)<- 1:nrow(alc)
# cropsAgg<- c("jpoultry","cpoultry", "clivestock","jlivestock",
#              "cleche", "jleche", "csoybean", "jsoybean", "cpalma",
#              "jpalma", "jpalm kernel", "cpalm kernel")
# crops<- unique(alcCrops$commodity)

cropsAgg<- c("cmaiz","jmaiz")
alcCrops<- nepal %>% dplyr::filter(., commodity %in% cropsAgg)

write.csv(alcCrops,paste(grd,"MaizeColtotal.csv", sep = ""))

alcCrops$commodity<- revalue(alcCrops$commodity, c("jpoultry"="Poultry",
                                        "cpoultry"="Poultry",
                                        "clivestock"="Livestock",
                                        "jlivestock"="Livestock",
                                        "cleche"="Milk",
                                        "jleche"="Milk",
                                        "csoybean"="Soybean",
                                        "jsoybean"="Soybean",
                                        "cpalma"="Palma",
                                        "jpalma"="Palma",
                                        "jpalm Kernel"="Palm Kernel",
                                        "cpalm Kernel"="Palm Kernel"))
rm(nepal)
rm(alc)
##### processing 
phi<- alcCrops 


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

# 3. datos categorias animales
dataanimal<- c("AnmlNumXAgg -- Animal Numbers" , "AnmlYldXAgg -- Animal Yield")

# 4. data tratamiento especial
dataespecial<- c(  "QMSHXAgg -- Import Share of Demand",  "QNXAgg -- Net Trade")

# transformar  factor to character
phi$impactparameter<- as.character(phi$impactparameter)
phi$scenario<- as.character(phi$scenario)
phi$commodity<- as.character(phi$commodity)
phi$region<- as.character(phi$region)
phi$productiontype<- as.character(phi$productiontype)


####################################################################
# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL
crops<- unique(phi$commodity)


cultivations<-  crops
      
cultivationsTrade<- crops

animals<- c( "Livestock", "Poultry"  , "Milk"  )
other<- c("-")
row.names(phi)<- 1: nrow(phi)

####################################################################################
sce<- unique(phi$scenario)
buss<- phi[grepl(pattern = "SSP2_4.5|NoCC_SSP2", phi$scenario),]
pess<- phi[grepl(pattern = "SSP3_8.5|NoCC_SSP3", phi$scenario),]


sceL<- list(B=buss,P=pess)




#1. Datos totales--------------------------------

k<- list()

# i=2
for(i in seq_along(datatotal)){
      
      # selecciono el cultivo
      k[[i]]<- sceL$P[which(sceL$P$impactparameter==datatotal[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<- data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC_SSP3","NoCC","CC"))
      #elimino fila innecesarias
#       k[[i]]<-k[[i]][,-c(5:19)]
      #creo una variable= cambio porcentual 2020-2050
      k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2005)/k[[i]]$X2005)*100
      #Elimino columnas inncesarias y de paso los organizo  
      k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
      #Reshape para tener cambio porcentual por GCM
      k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
      #Calculo de la media por los gcms
      k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
      #Selecciono las variable necesarias 
      k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC_SSP3")]
      k[[i]]$NoCC_SSP3<- NULL
      #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
      k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "Pess.xlsx", sep = ""),
            sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
# i=2
for (i in seq_along(datatotal)){
      
      png(filename=paste(copy,datatotal[i],"PESS.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      xx<- z %>% dplyr::filter(., impactparameter==datatotal[i])
      n[[i]]<- print(ggplot(xx, aes(x=region, y=Val, fill=scenarios)) + 
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datatotal[i])+
                           facet_grid(.~commodity)+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity")+ scale_y_continuous(limits = c(-100, 100))
      )
      dev.off()
      print(i)
}  

rm(z)



############## data agg

k<- list()
# i=2
for(i in seq_along(dataagg)){
      
      if (i== 2){
            
            # selecciono el cultivo
#             k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
            k[[i]]<- sceL$P[which(sceL$P$impactparameter==dataagg[i]),]
            # k[[i]]<- xx
            
            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC_SSP3","NoCC","CC"))
            #elimino fila innecesarias
#             k[[i]]<-k[[i]][,-c(5:7)]
            #creo una variable= cambio porcentual 2020-2050
            k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2005)/k[[i]]$X2005)*100
            #Elimino columnas inncesarias y de paso los organizo  
            k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
            #Reshape para tener cambio porcentual por GCM
            k[[i]]<- k[[i]] %>%
                  spread(scenario, Percentage_Change)
            #Calculo de la media por los gcms
            k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
            #Selecciono las variable necesarias 
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC_SSP3")]
            k[[i]]$NoCC_SSP3<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      }  else {
            
            # selecciono el cultivo
#             k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
            k[[i]]<- sceL$P[which(sceL$P$impactparameter==dataagg[i]),]
#             k[[i]]<- XX

            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC_SSP3","NoCC","CC"))
            #elimino fila innecesarias
#             k[[i]]<-k[[i]][,-c(5:19)]
            #creo una variable= cambio porcentual 2020-2050
            k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2005)/k[[i]]$X2005)*100
            #Elimino columnas inncesarias y de paso los organizo  
            k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
            #Reshape para tener cambio porcentual por GCM
            k[[i]]<- k[[i]] %>%
                  spread(scenario, Percentage_Change)
            #Calculo de la media por los gcms
            k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
            #Selecciono las variable necesarias 
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC_SSP3")]
            k[[i]]$NoCC_SSP3<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
            
      }
      return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "Pess.xlsx", sep = ""),
            sheetName = "DataAgregados", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
# i=1
for (i in seq_along(dataagg)){
      
      xx<- z %>% dplyr::filter(., impactparameter==dataagg[i])
      
      png(filename=paste(copy,dataagg[i],"PESS.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(xx, aes(x=region, y=Val, fill=scenarios)) +   facet_grid(.~commodity)+
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataagg[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity") #+ scale_y_continuous(limits = c(-20, 50))
      )
      dev.off()
      print(i)
}  

rm(z)

#3. Datos animales-----------------


k<- list()
# i=1
for(i in seq_along(dataanimal)){
      
      # selecciono el cultivo
      k[[i]]<- sceL$P[which(sceL$P$impactparameter==dataanimal[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC_SSP3","NoCC","CC"))
      #elimino fila innecesarias
#       k[[i]]<-k[[i]][,-c(5:19)]
      #creo una variable= cambio porcentual 2020-2050
      k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2005)/k[[i]]$X2005)*100
      #Elimino columnas inncesarias y de paso los organizo  
      k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
      #Reshape para tener cambio porcentual por GCM
      k[[i]]<- k[[i]] %>%
            spread(scenario, Percentage_Change)
      #Calculo de la media por los gcms
      k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
      #Selecciono las variable necesarias 
      k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC_SSP3")]
      k[[i]]$NoCC_SSP3<- NULL
      #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
      k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(copy, "Pess.xlsx", sep = ""),
            sheetName = "DataAnimal", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
# i=1
for (i in seq_along(dataanimal)){
      
      xx<- z %>% dplyr::filter(., impactparameter==dataanimal[i])
      
      png(filename=paste(copy,dataanimal[i],"PESS.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(xx, aes(x=region, y=Val, fill=scenarios)) +   facet_grid(.~commodity)+
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataanimal[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
      )
      dev.off()
      print(i)
}  

rm(z)


# 
# 
# var<- c("TAreaXAgg -- Total Area","TYldXAgg -- Total Yield" , "YldXAgg -- Yield"  ,"AreaXAgg -- Area" , 
#         "QSXAgg -- Total Production" ,"QSupXAgg -- Commodity Supply" ) 



nepal<- nepal %>% filter(., impactparameter %in% var)


write.csv(nepal,paste(copy,"ColombiaMaizeBogota.csv", sep = ""), row.names = FALSE)

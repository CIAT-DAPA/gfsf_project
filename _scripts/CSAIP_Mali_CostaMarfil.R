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
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/")
#Direción graficos
grd<-"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/CSAIP/"
#Direción copias seguridad
copy<-"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/CSAIP/"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

################################################ PARTE A. ####################################################################


#Cargar marco de datos principal-------------
md<-read.csv("CSAIPV2.csv",header=T)
phi<- md

#Conocer los cultivos y agregaciones 
cult<- unique(phi$commodity)

# cambiar los nombres de los cultivos
# phi$commodity<-revalue(phi$commodity, c("CER-Rice"="Rice"))

# Dataframe para hacer analisis por systmas de produccion
phi_sys<- phi 


#Filtros por tipos de impactparameters------------------
# datos por sistema de produccion  riego y secano
tab<- table(phi$impactparameter, phi$productiontype!="total" & phi$productiontype!="-") #filtro
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )

# 1. datos categorias totales
tab<- table(phi$impactparameter, phi$productiontype=="total")
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area",  "QSXAgg -- Total Production" )

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

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL
cropsout<- c("AMT", "AOT", "CER", "PUL", "SGR", "SGC", "MLS", "COT", "OLS", "OIL", "R&T", "F&V", "All", "AllC")
# phi<- phi %>% filter(., commodity== "Rice")
crops<- unique(phi$commodity)

carne<- unique(phi$commodity[grepl(pattern="AMT|AOT", phi$commodity)])
jusCrop<- phi %>% filter(., !commodity %in% carne) %>%select(commodity) %>% unique(.)
jCrop<-  as.vector(jusCrop['commodity'])
jCrop<-  jusCrop[,'commodity']

cultivations<-  c("CER-Barley", "CER-Maize", "CER-Millet" , "CER-Rice" , "CER-Sorghum" ,"CER-Wheat" ,
                    "CER-Other Cereals" , "R&T-Cassava" , "R&T-Potato" ,"R&T-Other Roots" ,"PUL-Beans" ,
                    "PUL-Chickpeas","PUL-Cowpeas" , "PUL-Lentils", "PUL-Pigeonpeas" ,"PUL-Other Pulses",
                    "F&V-Banana","F&V-Plantain","F&V-Tropical Fruit" , "F&V-Temperate Fruit","F&V-Vegetables",
                    "SGC-Sugarcane" ,"SGR-Sugar","OLS-Groundnut", "OIL-Groundnut Oil" , "MLS-Groundnut meal" ,
                    "OLS-Rapeseed", "OIL-Rapeseed Oil" ,"OLS-Soybean", "OIL-Soybean Oil"  ,"MLS-Soybean Meal" ,
                    "OLS-Sunflower" ,"OIL-Sunflower Oil" ,  "MLS-Sunflower Meal" , "OIL-Palm Fruit Oil" ,
                    "OIL-Palm Kernel Oil", "OLS-Other Oilseeds" , "OIL-Other Oils", "MLS-Other meals", "COT-Cacao",
                    "COT-Coffee", "COT-Cotton", "COT-Tea" , "COT-Other")
cultivationsTrade<- c("CER-Barley", "CER-Maize", "CER-Millet" , "CER-Rice" , "CER-Sorghum" ,"CER-Wheat" ,
                      "CER-Other Cereals" , "R&T-Cassava" , "R&T-Potato" ,"R&T-Other Roots" ,"PUL-Beans" ,
                      "PUL-Chickpeas","PUL-Cowpeas" , "PUL-Lentils", "PUL-Pigeonpeas" ,"PUL-Other Pulses",
                      "F&V-Banana","F&V-Plantain","F&V-Tropical Fruit" , "F&V-Temperate Fruit","F&V-Vegetables",
                      "SGC-Sugarcane" ,"SGR-Sugar","OLS-Groundnut", "OIL-Groundnut Oil" , "MLS-Groundnut meal" ,
                      "OLS-Rapeseed", "OIL-Rapeseed Oil" ,"OLS-Soybean", "OIL-Soybean Oil"  ,"MLS-Soybean Meal" ,
                      "OLS-Sunflower" ,"OIL-Sunflower Oil" ,  "MLS-Sunflower Meal" , "OIL-Palm Fruit Oil" ,
                      "OIL-Palm Kernel Oil", "OLS-Other Oilseeds" , "OIL-Other Oils", "MLS-Other meals", "COT-Cacao",
                      "COT-Coffee", "COT-Cotton", "COT-Tea" , "COT-Other", "AMT-Beef", "AMT-Pork", "AMT-Lamb", "AMT-Poultry",
                      "AOT-Milk", "AOT-Dairy" )

animals<- c( "AMT-Beef", "AMT-Pork", "AMT-Lamb", "AMT-Poultry", "AOT-Milk", "AOT-Dairy" )
other<- c("-")
row.names(phi)<- 1: nrow(phi)

pots<- unique(phi$region)
p=2  # 1= costa  2= Mali
# i=1
#1. Datos totales--------------------------------

k<- list()

for(i in seq_along(datatotal)){
    
    # selecciono el cultivo
    k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
    k[[i]]<- k[[i]] %>% filter(., region==pots[p])
    #reordeno los datos
    rownames(k[[i]])<- 1: nrow(k[[i]])
    
    test<- data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC")) 
    test<- test %>% group_by(impactparameter, commodity,region,year, Cat)%>% summarise(mean=mean(Val))%>%
          spread(Cat,mean)
    write.csv(test,paste(copy,datatotal[i],"_", pots[p], ".csv",sep = "" ))
    
    #reshape a lo ancho  
    k[[i]]<- k[[i]] %>%
        spread ("year","Val")
    #creo variable para  con CC o NoCC
    k[[i]]<- data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
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
    k[[i]] <- k[[i]] %>% select(impactparameter,commodity,region,Cat,mean)
#     k[[i]]$NoCC<- NULL
    #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
    k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    return 
}

# zC<- do.call(rbind, k) # costa de marfil
jM<- do.call(rbind, k) # mali

# tt<- rbind(zC,jM)

# exportar datos a excel
require(xlsx)
write.xlsx( x = jM,file= paste(copy, "CSAIP2.xlsx", sep = ""),
            sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)


# 
# #reshape
# z<- z %>%
#     gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# i=1
# for (i in seq_along(datatotal)){
#     
#     png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datatotal[i],"MAL.png",sep=""), 
#          width = 10, height = 10, units = 'in', res = 300)
#     
#     n[[i]]<- print(ggplot(z[which(z$impactparameter==datatotal[i]),], aes(x=region, y=Val, fill=scenarios)) + facet_grid(.~commodity)+
#                        theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datatotal[i])+
#                        theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                        ylab('Percentage Change') +  xlab("Commodity")+ scale_y_continuous(limits = c(-20, 50))
#     )
#     dev.off()
#     print(i)
# }  
# 
# rm(z)

#2. Datos agregados--------------------------------

# p=2 # pais


k<- list()

for(i in seq_along(dataagg)){
    
    if (i== 2){
        
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
        k[[i]]<- k[[i]] %>% filter(., region==pots[p])
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        
        test<- data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC")) 
        test<- test %>% group_by(impactparameter, commodity,region,year, Cat)%>% summarise(mean=mean(Val))%>%
              spread(Cat,mean)
        write.csv(test,paste(copy,dataagg[i],"_", pots[p], ".csv",sep = "" ))
        
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
        #elimino fila innecesarias
        k[[i]]<-k[[i]][,-c(5:7)]
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
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean")]
#         k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    }  else {
        
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
        k[[i]]<- k[[i]] %>% filter(., region==pots[p])
               #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        test<- data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC")) 
        test<- test %>% group_by(impactparameter, commodity,region,year, Cat)%>% summarise(mean=mean(Val))%>%
              spread(Cat,mean)
        write.csv(test,paste(copy,dataagg[i],"_", pots[p], ".csv",sep = "" ))
        
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
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
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean")]
#         k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
        
    }
    return 
}


# zC<- do.call(rbind, k) # costa de marfil
jM<- do.call(rbind, k) # mali

# tt<- rbind(zC,jM)

# exportar datos a excel
require(xlsx)
write.xlsx( x = jM,file= paste(copy, "CSAIP2.xlsx", sep = ""),
            sheetName = "DataAgregados", col.names = TRUE, append = TRUE, showNA = FALSE)


# 
# #reshape
# z<- z %>%
#     gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(dataagg)){
#     
#     png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataagg[i],"MAL.png",sep=""), 
#          width = 10, height = 10, units = 'in', res = 300)
#     
#     n[[i]]<- print(ggplot(z[which(z$impactparameter==dataagg[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                        theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataagg[i])+
#                        theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                        ylab('Percentage Change') +  xlab("Commodity") #+ scale_y_continuous(limits = c(-20, 50))
#     )
#     dev.off()
#     print(i)
# }  
# 
# rm(z)

#3. Datos animales-----------------

p=2
k<- list()

for(i in seq_along(dataanimal)){
    
    # selecciono el cultivo
    k[[i]]<- phi[which(phi$impactparameter==dataanimal[i]),]
    k[[i]]<- k[[i]] %>% filter(., region==pots[p])
    
    #reordeno los datos
    rownames(k[[i]])<- 1: nrow(k[[i]])
    #reshape a lo ancho  
    k[[i]]<- k[[i]] %>%
        spread ("year","Val")
    #creo variable para  con CC o NoCC
    k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
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
    k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean")]
#     k[[i]]$NoCC<- NULL
    #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
    k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    return 
}


# zC<- do.call(rbind, k) # costa de marfil
jM<- do.call(rbind, k) # mali

# tt<- rbind(zC,jM)

# exportar datos a excel
require(xlsx)
write.xlsx( x = jM,file= paste(copy, "CSAIP2.xlsx", sep = ""),
            sheetName = "DataAnimal", col.names = TRUE, append = TRUE, showNA = FALSE)


# 
# #reshape
# z<- z %>%
#     gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(dataanimal)){
#     
#     png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataanimal[i],"MAL.png",sep=""), 
#          width = 10, height = 10, units = 'in', res = 300)
#     
#     n[[i]]<- print(ggplot(z[which(z$impactparameter==dataanimal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                        theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataanimal[i])+
#                        theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                        ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
#     )
#     dev.off()
#     print(i)
# }  
# 
# rm(z)

#4. Datos especiales---------------
p=2
k<- list()

for(i in seq_along(dataespecial)){
    
    if(i==1){
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
        k[[i]]<- k[[i]] %>% filter(., region==pots[p])
        
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
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
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean")]
#         k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    } else {
        # selecciono el cultivo
        k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
        k[[i]]<- k[[i]] %>% filter(., region==pots[p])
        
        #reordeno los datos
        rownames(k[[i]])<- 1: nrow(k[[i]])
        #reshape a lo ancho  
        k[[i]]<- k[[i]] %>%
            spread ("year","Val")
        #creo variable para  con CC o NoCC
        k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
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
        k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean")]
#         k[[i]]$NoCC<- NULL
        #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
        k[[i]] <- k[[i]]%>% spread(Cat, mean) 
        return 
        
    }
    
}
# zC<- do.call(rbind, k) # costa de marfil
jM<- do.call(rbind, k) # mali

# tt<- rbind(zC,jM)

# exportar datos a excel
require(xlsx)
write.xlsx( x = jM,file= paste(copy, "CSAIP2.xlsx", sep = ""),
            sheetName = "DatosEspeciales", col.names = TRUE, append = TRUE, showNA = FALSE)


# 
# #reshape
# z<- z %>%
#     gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(dataespecial)){
#     
#     png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataespecial[i],"MAL.png",sep=""), 
#          width = 10, height = 10, units = 'in', res = 300)
#     
#     n[[i]]<- print(ggplot(z[which(z$impactparameter==dataespecial[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                        theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataespecial[i])+
#                        theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                        ylab('Impacts') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
#     )
#     dev.off()
#     print(i)
# }  
# 
# rm(z)

#5. Datos por sistemas de produccion--------

#sys<- c("air","arf")
p=2
k<- list()

for(i in seq_along(datasys)){
    
    # selecciono el cultivo
    k[[i]]<- phi_sys[which(phi_sys$impactparameter==datasys[i]),]
    k[[i]]<- k[[i]] %>% filter(., region==pots[p])
    #reordeno los datos
    rownames(k[[i]])<- 1: nrow(k[[i]])
    #reshape a lo ancho  
    k[[i]]<- k[[i]] %>%
        spread ("year","Val")
    #creo variable para  con CC o NoCC
    k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCCSSP3_8.5","NoCC","CC"))
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
    k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "productiontype", "Cat", "mean")]
#     k[[i]]$NoCC<- NULL
    #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
    k[[i]] <- k[[i]]%>% spread(Cat, mean) 
    return 
}


# zC<- do.call(rbind, k) # costa de marfil
jM<- do.call(rbind, k) # mali

# tt<- rbind(zC,jM)

# exportar datos a excel
require(xlsx)
write.xlsx( x = jM,file= paste(copy, "CSAIP2.xlsx", sep = ""),
            sheetName = "SystemsProduction", col.names = TRUE, append = TRUE, showNA = FALSE)


# 
# #reshape
# z<- z %>%
#     gather("scenarios", "Val" , 5:ncol(z))
# 
# z$productiontype<- as.character(z$productiontype)
# z$impactparameter<- as.character(z$impactparameter)
# z$region<- as.character(z$region)
# z$scenarios<- as.character(z$scenarios)
# z$commodity<- as.character(z$commodity)
# 
# n<- list()
# for (i in seq_along(datasys)){
#     
#     png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datasys[i],"MAL.png",sep=""), 
#          width = 10, height = 10, units = 'in', res = 300)
#     
#     n[[i]]<- print(ggplot(z[which(z$impactparameter==datasys[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                        theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datasys[i])+
#                        facet_grid(productiontype~.)+ theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                        ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
#     )
#     dev.off()
#     print(i)
# }  
# 




##############################################################################################################################
################################################ PARTE B Generador de tablas. #################################################
p=1 # esta parte se corre por pais
phii<- phi %>% dplyr::filter(., region==pots[p])
#Hacer un subconjunto que sólo contenga las variables de mi interés----------------
mdsub<-subset(phii,phii$impactparameter=="TAreaXAgg -- Total Area" |
                  phii$impactparameter== "QNXAgg -- Net Trade" | 
                  phii$impactparameter== "TYldXAgg -- Total Yield"|
                  phii$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
                  phii$impactparameter=="AnmlYldXAgg -- Animal Yield")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"= "Yield",
                                                        "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
                                                        "AnmlYldXAgg -- Animal Yield"="Animal Yield"))


mdsub<-data.frame(mdsub,"Cat"=ifelse(mdsub$scenario=="NoCC_SSP2","NoCC","CC"))

row.names(mdsub)<- 1: nrow(mdsub)

mdwide_tan<- mdsub %>%
    spread ("year", "Val")

# elimino los periodos antes del 2020
mdwide_tan<- mdwide_tan[,-c(6:20)]
row.names(mdwide_tan)<- 1: nrow(mdwide_tan)


#Copia de seguridad pais filtrado------
write.csv(mdwide_tan,paste(copy,pots[p],"_mdwideTotal.csv", sep = ""), row.names = FALSE)

#Net Trade y  filtros logicos-----------------
tznet<- mdwide_tan[which(mdwide_tan$impactparameter=="Net Trade"),]
row.names(tznet)<- 1:nrow(tznet)

## logica de los valores 
nn<-  which(tznet$`2050`<0 & tznet$`2020`<0) # net trade negativo  importador neto
pn<-  which(tznet$`2050`>0 & tznet$`2020`<0) # impacto positivo inicia como importador termina como exportador
np<-  which(tznet$`2050`<0 & tznet$`2020`>0) # impacto negativo inicia como exportador termina como importador
pp<-  which(tznet$`2050`>0 & tznet$`2020`>0) # net trade positivo  exportador neto


# desempeño 
export<-   c(pp)
import <-  c(nn)
tran_XtoM<- c(np) # inicia exportador termina importador
tran_MtoX<- c(pn) # inicia importador termina exportador

hard<- c(nn,pp,pn,np)

tznet$impacto<- NA # para poner el impacto  cambio relativo

#loops 
for(j in 1:nrow(tznet)){
    
    if(j %in% hard){
        tznet$impacto[j] <- ((tznet$`2050`[j] - tznet$`2020`[j])/tznet$`2020`[j]) * 100
    } else {  }
    
}

# ##Copia de seguridad cambios relativos y vectores de desempeño
# write.csv(tznet,paste(copy,pots[p],"_NetTrade.csv", sep = ""), row.names = FALSE)

# copia
tanz<- tznet
tanz$trend<- NA
tanz$trend[import] <- "Negative"
tanz$trend[export] <- "Positive"
tanz$trend[tran_MtoX]<- "Transition from M to X"
tanz$trend[tran_XtoM]<- "Transition from X to M"
                 
uu <- list()

for (i in 1:length(cultivationsTrade)){
    
    if(length(which(tanz$commodity==cultivationsTrade[[i]]))>0){
        z <- tanz[which(tanz$commodity==cultivationsTrade[[i]]),]
        x <- sort(table(z$trend[z$Cat=='CC']),decreasing = T)
        z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
        if(x>= 1){
            z$trend <- names(x)[1]
        }
        uu[[i]] <- z
    } else{
        cat(paste('Commodity:', cultivationsTrade[i], 'does not have data\n', sep=''))
        
    }
    
}



##Unir los cultivos
c <- do.call(rbind,uu)  

##Calcular la media de los impactos
c$CC_mean <- rowMeans(x=c[,5:8], na.rm=TRUE)
c$NoCC_mean <- c[,9]
c<- c[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
write.csv(c, paste(copy, pots[p], "_NettradeTOTAL.csv", sep = ""),row.names = FALSE)

#Area  y rendimientos---------------------------------------
tznet2<- mdwide_tan
tznet2<-  tznet2[which(tznet2$impactparameter!="Net Trade"),]
tznet2$Percentage_Change<- ((tznet2$`2050`-tznet2$`2020`)/tznet2$`2020`)*100
tznet2<- tznet2[,-c(6:36)]

##Reshape
tznet2_t<-tznet2 %>%
    spread("scenario","Percentage_Change")

##Calculo de la media
tznet2_t$mean <- rowMeans(x=tznet2_t[,5:ncol(tznet2_t)], na.rm=TRUE)
tznet2_t <- tznet2_t[,c("impactparameter", "commodity", "region", "Cat", "mean")]
# tznet2_t$NoCC<- NULL
#Reshape
tznet2_t <- tznet2_t %>% spread(Cat, mean)

write.csv(x =tznet2_t, file =paste(copy,pots[p],"areayield.csv",sep = ""))

############### Net trade ##########################
md<- phii

#Hacer un subconjunto que sólo contenga las variables de mi interés
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                    md$impactparameter=="TAreaXAgg -- Total Area" |
                    md$impactparameter== "QNXAgg -- Net Trade" | 
                    md$impactparameter== "QDXAgg -- Total Demand" |
                    md$impactparameter=="TYldXAgg -- Total Yield")

mdsub$impactparameter<- as.character(mdsub$impactparameter)
mdsub$scenario<- as.character(mdsub$scenario)
mdsub$commodity<- as.character(mdsub$commodity)
mdsub$region<- as.character(mdsub$region)
# mdsub$productiontype<- as.character(mdsub$productiontype)

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand",
                                                        "QNXAgg -- Net Trade"="Net Trade",
                                                        "QSXAgg -- Total Production"="Total Production",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"="Total Yield"))

# #Hacer un subconjunto que sólo contenga los cinco cultivos analizados
# mdsubcrop<-subset(mdsub,mdsub$commodity=="jbean"| mdsub$commodity=="jmaiz" |
#                         mdsub$commodity=="jrice" | mdsub$commodity=="cs" |
#                         mdsub$commodity=="jwhea" | mdsub$commodity=="cbean" |
#                         mdsub$commodity=="cmaiz" | mdsub$commodity=="crice" |
#                         mdsub$commodity=="js" | mdsub$commodity=="cwhea")

alc<- mdsub

#reshape
mdwide<- alc %>% spread(year, Val)
# mdwide$commodity<- revalue(mdwide$commodity, c("cbean"= "Fríjol",
#                                                "cmaiz"="Maíz",
#                                                "crice"="Arroz",
#                                                "cs"="Soya",
#                                                "cwhea"="Trigo",
#                                                "jbean"="Fríjol",
#                                                "jmaiz"="Maíz",
#                                                "jrice"="Arroz",
#                                                "js"="Soya",
#                                                "jwhea"="Trigo"))

mdwide<-data.frame(mdwide,"Cat"=ifelse(mdwide$scenario=="NoCCSSP3_8.5","NoCC","CC"))

rend_all<- mdwide[,-c(5:19)]
rend_all$Percentage_Change<-((rend_all$X2050-rend_all$X2020)/rend_all$X2020)*100
write.csv(rend_all,paste(grd, "Data.csv", sep = ""))

#Mediana de los cambios porcentuales por categorias.

anal_datag<- aggregate(rend_all[,"Percentage_Change"],
                       by=list(rend_all$region,rend_all$impactparameter,
                               rend_all$commodity,rend_all$Cat),
                       FUN=median)
colnames(anal_datag)<- c("Region", "Parameter", "Crop", "Sce", "Val")
anal_datag$Sce<- as.character(anal_datag$Sce)
write.csv(anal_datag,paste(grd, "DataMedian.csv", sep = ""))
croput<- c("AllA")
anal_datag<- anal_datag %>% dplyr::filter(.,!Crop %in% croput)
#Realizar proceso para graficar todas las variables por regiones.
tiff(filename=paste(grd,"CSAIP_",unique(anal_datag$Region),"_country_report.tiff",sep=""), 
     width = 12, height = 7, units = 'in', res = 100)


      
      ggplot(data=(subset(anal_datag,anal_datag$Region==pots[i]
                                          & anal_datag$Parameter!="Net Trade" 
                                          & anal_datag$Sce != "NoCC")),aes(x=Crop,y=Val
                                                                           ,fill=Parameter))  +
                            facet_wrap(~Parameter,ncol=4)+
                            geom_bar(stat="identity")+
                            geom_point(aes(shape=Sce),data=(subset(anal_datag,anal_datag$Region== pots[i]
                                                                   & anal_datag$Parameter !="Net Trade" 
                                                                   & anal_datag$Sce == "NoCC")),
                                       alpha = 0.4,size=4) +
                            guides(fill=FALSE)+
                            theme(#axis.text.x = element_text(angle = 0, hjust = 1),
                                  legend.title=element_blank(),
                                  axis.text= element_text(face = "bold.italic", size = 20),
                                  axis.title=element_text(face = "bold.italic", size = 20))+
                            theme(legend.position="bottom",legend.text=element_text(size=20),
                                  strip.text.x = element_text(size = 16, angle = 0))+
                            labs(y="Percentage Change",x="")
 
      dev.off()
              


#Evolution of Net Trade by crop and by region

datos<- subset(rend_all,rend_all$impactparameter=="Net Trade")
datos<- datos %>% dplyr::filter(.,!commodity %in% croput )
# datos<- datos %>% dplyr::filter(.,!commodity %in% croput )
datmin<- aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=min)
datmin<- datmin %>% gather("time","datmin", 3:ncol(datmin))
names(datmin)<-c("Region","Crop","time","datmin")
datmin$time<-  gsub("X", "",datmin$time)
datmin$time<- as.numeric(datmin$time)


datmed<- datos %>% dplyr::filter(.,scenario=="NoCCSSP3_8.5")
datmed$scenario<- NULL
datmed$impactparameter<- NULL
datmed$Percentage_Change<- NULL
datmed$Cat<- NULL
datmed<- datmed %>% gather("time","datmed", 3:ncol(datmed))
names(datmed)<-c("Crop","Region","time","datmed")
datmed$time<-  gsub("X", "",datmed$time)
datmed$time<- as.numeric(datmed$time)
datmed<- datmed[c("Region","Crop","time","datmed")]

datmax<-aggregate(datos[,paste("X20",20:50,sep="")],by=list(datos$region,datos$commodity),FUN=max)
datmax<- datmax %>% gather("time","datmax", 3:ncol(datmax))
names(datmax)<-c("Region","Crop","time","datmax")
datmax$time<-  gsub("X", "",datmax$time)
datmax$time<- as.numeric(datmax$time)

extremos<-merge(datmin,datmax)

datost<-merge(extremos,datmed)

py<- NULL

# croput<- c("Soybean", "Wheat")

tiff(filename=paste(grd,"CSAIP_",pots[i],"_net_trade.tiff",sep=""), 
     width = 10, height = 7, units = 'in', res = 100)
      
    ggplot(data=subset(datost,datost$Region==pots[i]),
                            aes(time,datmed,group=Crop,color=Crop)) + 
                           geom_line(linetype="dashed",size=1)+
                           geom_ribbon(aes(ymin=datmin,ymax=datmax,
                                           fill=Crop,colour=Crop,
                                           linetype=NA),
                                       alpha=0.1) +
                           labs(y="Net Trade (000 tm)",x="Year")+ 
                           theme(legend.position="bottom", 
                                 legend.text=element_text(size=20),
                                 axis.text= element_text(face = "bold.italic", size = 15),
                                 axis.title=element_text(face = "bold.italic", size = 15),
                                 legend.title=element_blank())
      
      
      
      dev.off()





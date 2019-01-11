# Author: Carlos Edo Gonzalez
# Import and proccesing data from GDX
# Download and active package appropiate for access and processing gdx files 
# https://support.gams.com/gdxrrw:interfacing_gams_and_r 

g=gc;rm(list = ls())

#library---------------
library(plyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(dplyr)
library(gdxrrw)
library(tidyr)

# Initial options---------
abbreviate("percentage") 
options(warn = -1); options(scipen = 999) 
options(digits=3) 

# Directories-----------
# These links should be uptaded 
ggir<- "C:/Users/CEGONZALEZ/Desktop/IMPACT3-Model-ver3.3/OutputFiles/Scenarios/"
gres<- c("C:/Users/CEGONZALEZ/Desktop/exp/")

# To read GDX files we need the gdxrrw package----
igdx(gamsSysDir ="C:/GAMS/win64/24.4")
#read the data created in GAMS


# Creating vectors---- 
## regions
r2<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA", "Africa","Americas","DVD", "DVG","WLD")
r3<- c("Africa","Americas", "Asia","Europe", "Oceania")
r4<- c("Australia and New Zealand","Caribbean","Central America", "Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia",
       "Middle Africa","Northern Africa","Northern America","Northern Europe","South America","South-Eastern Asia","Southern Africa","Southern Asia",
       "Southern Europe","Western Africa","Western Asia", "Western Europe", "Western and Central Asia")
r5<- c("MENg","EAPg")
rall<- c(r2,r3,r4, r5)

## fill using all list of parameters for analysis
colnames<- list("QSX0","ANMLNUMCTYX0","AREACTYX0","FoodAvailability","GDPHX0", "GDPX0","pcGDPX0","PCX0",
                "PerCapKCal","PerCapKCal_Com","PEX0","PMX0","PNETX0","POPHX0","PopulationAtRisk","POPX0","PPX0","PWX0","QBFX0",
                "QDX0","QEX0","QFX0","QHDX0","QINTX0","QLX0","QMX0","QNX0","QOTHRX0","QSUPX0","QSX0","ShareAtRisk","TotalMalnourished","YldCliShkCtyX0","YLDCTYX0","YldInt2CtyX0")

## this should be updated  
scenario<- "IPSL4.5"

# load data from GDX, example---------------
Test <- lapply(1:length(colnames), function(x){
     rgdx.param(gdxName =paste(ggir,"TeCNoCC.gdx", sep = ""),symName = colnames[[x]],ts = T,compress = F) 

})


# creating groups using attr/ncols--------
Exp <- Test #creating copy
LL3<- lapply(Exp, function(x){
      if(ncol(x)==3){
            x$parameter<- colnames(x)[ncol(x)]
            colnames(x)[(ncol(x))-1]<-"Val"
            return(x)
      }else{}
})

LL4<- lapply(Exp, function(x){
      if(ncol(x)==4){
            x$parameter<- colnames(x)[ncol(x)]
            colnames(x)[(ncol(x))-1]<-"Val"
            return(x)
            
      }else{}

})

LL5<- lapply(Exp, function(x){
      if(ncol(x)==5){
            x$parameter<- colnames(x)[ncol(x)]
            colnames(x)[(ncol(x))-1]<-"Val"
            return(x)
      }else{}
      
})



# Eliminate  NULLs into list 
LL4<- LL4[lapply(LL4,length)!=0] 
LL5<- LL5[lapply(LL5,length)!=0] 
LL3<- LL3[lapply(LL3,length)!=0] 

# labels appropiate  for each parameter
parameter_names<- read.csv("C:/Users/CEGONZALEZ/Desktop/exp/labels.csv")
colnames(parameter_names)[1]<- "parameter"
crops_names<- read.csv("C:/Users/CEGONZALEZ/Desktop/exp/LabelsCrops.csv")

### Explore data
LL3<- lapply(LL3, function(x){
      
      if(colnames(x)[1]=="C"){
            colnames(x)[1]<- "Crops"
#             x<- left_join((x),crops_names, by=("Crops"))   
#             x$Crops<- NULL
#             colnames(x)[ncol(x)]<- "Crops"
#             x<- x[c("Crops","YRS","Val","parameter")]
            x$Crops<- as.character(x$Crops)
            x$YRS<- as.character(x$YRS)
            x$YRS<- as.numeric(x$YRS)
            x<- left_join(x,parameter_names, by=("parameter"))
            x$parameter<- NULL
            colnames(x)[ncol(x)]<- "Parameter"
            x$Parameter<- as.character(x$Parameter)
      }else{
            colnames(x)[1]<- "Regions"
            x$YRS<- as.character(x$YRS)
            x$YRS<- as.numeric(x$YRS)
            x<- left_join(x,parameter_names, by=("parameter"))
            x$parameter<- NULL
            colnames(x)[ncol(x)]<- "Parameter"
            x$Regions<- as.character(x$Regions)
            x$Parameter<- as.character(x$Parameter)
      }
      
  
      return(x)
})



LL4<- lapply(LL4, function(x){
      
      if(colnames(x)[1]!="H"){
            
            colnames(x)[1]<- "Crops"
#             x$Crops<- as.character(x$Crops)
#             x<- left_join(x,crops_names, by=("Crops"))   
#             x$Crops<- NULL
#             colnames(x)[ncol(x)]<- "Crops"
#             x<- x[c("Crops", "CTY","YRS","Val","parameter")]
            x$Crops<- as.character(x$Crops)
            x$CTY<- as.character(x$CTY)
            x$YRS<- as.character(x$YRS)
            x$YRS<- as.numeric(x$YRS)
            x<- left_join(x,parameter_names, by=("parameter"))
            x$parameter<- NULL
            colnames(x)[ncol(x)]<- "Parameter"
            colnames(x)[colnames(x)=="CTY"]<- "Regions"
            x$Regions<- as.character(x$Regions)
            x$Parameter<- as.character(x$Parameter)
            
      }else{
            colnames(x)[1]<- "GeoDistri"
            x$CTY<- as.character(x$CTY)
            x$YRS<- as.character(x$YRS)
            x$YRS<- as.numeric(x$YRS)
            x<- left_join(x,parameter_names, by=("parameter"))
            x$parameter<- NULL
            colnames(x)[ncol(x)]<- "Parameter"
            colnames(x)[colnames(x)=="CTY"]<- "Regions"
            x$Regions<- as.character(x$Regions)
            x$Parameter<- as.character(x$Parameter)
      }
      
      
      return(x)
})



LL5<- lapply(LL5, function(x){
      
      colnames(x)[1]<- "Crops"
      x$Crops<- as.character(x$Crops)
#       x<- left_join(x,crops_names, by=("Crops"))   
#       x$Crops<- NULL
#       colnames(x)[ncol(x)]<- "Crops"
      x$Crops<- as.character(x$Crops)
      x$YRS<- as.character(x$YRS)
      x$YRS<- as.numeric(x$YRS)
      x<- left_join(x,parameter_names, by=("parameter"))
      x$parameter<- NULL
      colnames(x)[ncol(x)]<- "Parameter"
      colnames(x)[colnames(x)=="CTY"]<- "Regions"
      colnames(x)[colnames(x)=="FCTR"]<- "ProductionSystem"
      colnames(x)[colnames(x)=="H"]<- "GeoDistri"
      colnames(x)[colnames(x)=="LND"]<- "ProductionSystem"
      x$Regions<- as.character(x$Regions)
      x$Parameter<- as.character(x$Parameter)
      
      return(x) 
})



############################################################################################
################################ Distribution and analysis----- ############################

TestCarlos<- LL4
TestCarlos<- lapply(TestCarlos, function(x){
}) 

# condition logical just dataframes with crops 
if(names(TestCarlos[[1]])[1]%in% "Crops" == TRUE){
      
      TestCarlos[[1]]$sce<- scenario
      TestCarlos[[1]]<- left_join(TestCarlos[[1]],crops_names, by=("Crops"))
      TestCarlos[[1]]<- TestCarlos[[1]][c("Parameter","sce","Crops","New.Crops","Regions","YRS","Val")]
      
      # reshape from long to wide  
      TestCarlos[[1]]<- TestCarlos[[1]]%>% spread("YRS","Val")
      #creo una variable= cambio porcentual 2010-2050
      TestCarlos[[1]]$Percentage_Change<- (( TestCarlos[[1]]$`2050`- TestCarlos[[1]]$`2010`)/ TestCarlos[[1]]$`2010`)*100
      #Elimino columnas inncesarias y de paso los organizo  
      TestCarlos[[1]]<-  TestCarlos[[1]][, c("Parameter","sce", "Crops","New.Crops","Regions", "Percentage_Change")]
      
      ## graphs by groups  
      hh<- TestCarlos[[1]] %>% split(TestCarlos[[1]]$New.Crops)
      hh<- lapply()
      png(filename=paste(gres,unique(TestCarlos[[1]]$Parameter),".png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(z[which(z$impactparameter==datatotal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datatotal[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity")+ scale_y_continuous(limits = c(-20, 50))
      )
      }else{
            cast(paste("This file doesn't have crops\n processing", sep = ""))}



# if(grepl("^Crops$", names(TestCarlos[[3]])[1])==TRUE){




# 
# 
# #1. Datos totales--------------------------------
# 
# k<- list()
# 
# for(i in seq_along(datatotal)){
#       
#       # selecciono el cultivo
#       k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
#       #reordeno los datos
#       rownames(k[[i]])<- 1: nrow(k[[i]])
#       #reshape a lo ancho  
#       k[[i]]<- k[[i]] %>%
#             spread ("year","Val")
#       #creo variable para  con CC o NoCC
#       k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#       #elimino fila innecesarias
#       k[[i]]<-k[[i]][,-c(5:19)]
#       #creo una variable= cambio porcentual 2020-2050
#       k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
#       #Elimino columnas inncesarias y de paso los organizo  
#       k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
#       #Reshape para tener cambio porcentual por GCM
#       k[[i]]<- k[[i]] %>%
#             spread(scenario, Percentage_Change)
#       #Calculo de la media por los gcms
#       k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
#       #Selecciono las variable necesarias 
#       k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
#       k[[i]]$NoCC<- NULL
#       #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#       k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#       return 
# }
# 
# z<- do.call(rbind, k)
# 
# # exportar datos a excel
# require(xlsx)
# write.xlsx( x = z,file= paste(copy, "PK.xlsx", sep = ""),
#             sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)
# 
# 
# 
# #reshape
# z<- z %>%
#       gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(datatotal)){
#       
#       png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datatotal[i],"PK.png",sep=""), 
#           width = 10, height = 10, units = 'in', res = 300)
#       
#       n[[i]]<- print(ggplot(z[which(z$impactparameter==datatotal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                            theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datatotal[i])+
#                            theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                            ylab('Percentage Change') +  xlab("Commodity")+ scale_y_continuous(limits = c(-20, 50))
#       )
#       dev.off()
#       print(i)
# }  
# 
# rm(z)
# 
# #2. Datos agregados--------------------------------
# 
# k<- list()
# 
# for(i in seq_along(dataagg)){
#       
#       if (i== 2){
#             
#             # selecciono el cultivo
#             k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
#             #reordeno los datos
#             rownames(k[[i]])<- 1: nrow(k[[i]])
#             #reshape a lo ancho  
#             k[[i]]<- k[[i]] %>%
#                   spread ("year","Val")
#             #creo variable para  con CC o NoCC
#             k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#             #elimino fila innecesarias
#             k[[i]]<-k[[i]][,-c(5:7)]
#             #creo una variable= cambio porcentual 2020-2050
#             k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
#             #Elimino columnas inncesarias y de paso los organizo  
#             k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
#             #Reshape para tener cambio porcentual por GCM
#             k[[i]]<- k[[i]] %>%
#                   spread(scenario, Percentage_Change)
#             #Calculo de la media por los gcms
#             k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
#             #Selecciono las variable necesarias 
#             k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
#             k[[i]]$NoCC<- NULL
#             #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#             k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#       }  else {
#             
#             # selecciono el cultivo
#             k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
#             #reordeno los datos
#             rownames(k[[i]])<- 1: nrow(k[[i]])
#             #reshape a lo ancho  
#             k[[i]]<- k[[i]] %>%
#                   spread ("year","Val")
#             #creo variable para  con CC o NoCC
#             k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#             #elimino fila innecesarias
#             k[[i]]<-k[[i]][,-c(5:19)]
#             #creo una variable= cambio porcentual 2020-2050
#             k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
#             #Elimino columnas inncesarias y de paso los organizo  
#             k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
#             #Reshape para tener cambio porcentual por GCM
#             k[[i]]<- k[[i]] %>%
#                   spread(scenario, Percentage_Change)
#             #Calculo de la media por los gcms
#             k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
#             #Selecciono las variable necesarias 
#             k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
#             k[[i]]$NoCC<- NULL
#             #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#             k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#             
#       }
#       return 
# }
# 
# z<- do.call(rbind, k)
# 
# # exportar datos a excel
# require(xlsx)
# write.xlsx( x = z,file= paste(copy, "PK.xlsx", sep = ""),
#             sheetName = "DataAgregados", col.names = TRUE, append = TRUE, showNA = FALSE)
# 
# 
# 
# #reshape
# z<- z %>%
#       gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(dataagg)){
#       
#       png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataagg[i],"PK.png",sep=""), 
#           width = 10, height = 10, units = 'in', res = 300)
#       
#       n[[i]]<- print(ggplot(z[which(z$impactparameter==dataagg[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                            theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataagg[i])+
#                            theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                            ylab('Percentage Change') +  xlab("Commodity") #+ scale_y_continuous(limits = c(-20, 50))
#       )
#       dev.off()
#       print(i)
# }  
# 
# rm(z)
# 
# #3. Datos animales-----------------
# 
# 
# k<- list()
# 
# for(i in seq_along(dataanimal)){
#       
#       # selecciono el cultivo
#       k[[i]]<- phi[which(phi$impactparameter==dataanimal[i]),]
#       #reordeno los datos
#       rownames(k[[i]])<- 1: nrow(k[[i]])
#       #reshape a lo ancho  
#       k[[i]]<- k[[i]] %>%
#             spread ("year","Val")
#       #creo variable para  con CC o NoCC
#       k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#       #elimino fila innecesarias
#       k[[i]]<-k[[i]][,-c(5:19)]
#       #creo una variable= cambio porcentual 2020-2050
#       k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
#       #Elimino columnas inncesarias y de paso los organizo  
#       k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
#       #Reshape para tener cambio porcentual por GCM
#       k[[i]]<- k[[i]] %>%
#             spread(scenario, Percentage_Change)
#       #Calculo de la media por los gcms
#       k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
#       #Selecciono las variable necesarias 
#       k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
#       k[[i]]$NoCC<- NULL
#       #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#       k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#       return 
# }
# 
# z<- do.call(rbind, k)
# 
# # exportar datos a excel
# require(xlsx)
# write.xlsx( x = z,file= paste(copy, "PK.xlsx", sep = ""),
#             sheetName = "DataAnimal", col.names = TRUE, append = TRUE, showNA = FALSE)
# 
# 
# 
# #reshape
# z<- z %>%
#       gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(dataanimal)){
#       
#       png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataanimal[i],"PK.png",sep=""), 
#           width = 10, height = 10, units = 'in', res = 300)
#       
#       n[[i]]<- print(ggplot(z[which(z$impactparameter==dataanimal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                            theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataanimal[i])+
#                            theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                            ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
#       )
#       dev.off()
#       print(i)
# }  
# 
# rm(z)
# 
# #4. Datos especiales---------------
# 
# k<- list()
# 
# for(i in seq_along(dataespecial)){
#       
#       if(i==1){
#             # selecciono el cultivo
#             k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
#             #reordeno los datos
#             rownames(k[[i]])<- 1: nrow(k[[i]])
#             #reshape a lo ancho  
#             k[[i]]<- k[[i]] %>%
#                   spread ("year","Val")
#             #creo variable para  con CC o NoCC
#             k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#             #elimino fila innecesarias
#             k[[i]]<-k[[i]][,-c(5:19)]
#             #creo una variable= cambio porcentual 2020-2050
#             k[[i]]$p.p<- (k[[i]]$X2050-k[[i]]$X2020)
#             #Elimino columnas inncesarias y de paso los organizo  
#             k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","p.p")]
#             #Reshape para tener cambio porcentual por GCM
#             k[[i]]<- k[[i]] %>%
#                   spread(scenario, p.p)
#             #Calculo de la media por los gcms
#             k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
#             #Selecciono las variable necesarias 
#             k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
#             k[[i]]$NoCC<- NULL
#             #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#             k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#       } else {
#             # selecciono el cultivo
#             k[[i]]<- phi[which(phi$impactparameter==dataespecial[i]),]
#             #reordeno los datos
#             rownames(k[[i]])<- 1: nrow(k[[i]])
#             #reshape a lo ancho  
#             k[[i]]<- k[[i]] %>%
#                   spread ("year","Val")
#             #creo variable para  con CC o NoCC
#             k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#             #elimino fila innecesarias
#             k[[i]]<-k[[i]][,-c(5:19)]
#             #creo una variable= cambio porcentual 2020-2050
#             k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
#             #Elimino columnas inncesarias y de paso los organizo  
#             k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region", "Cat","Percentage_Change")]
#             #Reshape para tener cambio porcentual por GCM
#             k[[i]]<- k[[i]] %>%
#                   spread(scenario, Percentage_Change)
#             #Calculo de la media por los gcms
#             k[[i]]$mean <- rowMeans(x=k[[i]][,5:ncol(k[[i]])], na.rm=TRUE)
#             #Selecciono las variable necesarias 
#             k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
#             k[[i]]$NoCC<- NULL
#             #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#             k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#             return 
#             
#       }
#       
# }
# z<- do.call(rbind, k)
# 
# # exportar datos a excel
# require(xlsx)
# write.xlsx( x = z,file= paste(copy, "PK.xlsx", sep = ""),
#             sheetName = "DatosEspeciales", col.names = TRUE, append = TRUE, showNA = FALSE)
# 
# 
# 
# #reshape
# z<- z %>%
#       gather("scenarios", "Val" , 4:ncol(z))
# 
# 
# n<- list()
# for (i in seq_along(dataespecial)){
#       
#       png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",dataespecial[i],"PK.png",sep=""), 
#           width = 10, height = 10, units = 'in', res = 300)
#       
#       n[[i]]<- print(ggplot(z[which(z$impactparameter==dataespecial[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                            theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataespecial[i])+
#                            theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                            ylab('Impacts') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
#       )
#       dev.off()
#       print(i)
# }  
# 
# rm(z)
# 
# #5. Datos por sistemas de produccion--------
# 
# #sys<- c("air","arf")
# 
# k<- list()
# 
# for(i in seq_along(datasys)){
#       
#       # selecciono el cultivo
#       k[[i]]<- phi_sys[which(phi_sys$impactparameter==datasys[i]),]
#       #reordeno los datos
#       rownames(k[[i]])<- 1: nrow(k[[i]])
#       #reshape a lo ancho  
#       k[[i]]<- k[[i]] %>%
#             spread ("year","Val")
#       #creo variable para  con CC o NoCC
#       k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
#       k[[i]]<-k[[i]][,-c(6:19)]
#       k[[i]]$Percentage_Change<- ((k[[i]]$X2050-k[[i]]$X2020)/k[[i]]$X2020)*100
#       #Elimino columnas inncesarias y de paso los organizo  
#       k[[i]]<- k[[i]][, c("impactparameter","scenario", "commodity", "region","productiontype" ,"Cat","Percentage_Change")]
#       #Reshape para tener cambio porcentual por GCM
#       k[[i]]<- k[[i]] %>%
#             spread(scenario, Percentage_Change)
#       #Calculo de la media por los gcms
#       k[[i]]$mean <- rowMeans(x=k[[i]][,6:ncol(k[[i]])], na.rm=TRUE)
#       #Selecciono las variable necesarias 
#       k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "productiontype", "Cat", "mean", "NoCC")]
#       k[[i]]$NoCC<- NULL
#       #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
#       k[[i]] <- k[[i]]%>% spread(Cat, mean) 
#       return 
# }
# 
# 
# z<- do.call(rbind, k)
# 
# # exportar datos a excel
# require(xlsx)
# write.xlsx( x = z,file= paste(copy, "PK.xlsx", sep = ""),
#             sheetName = "SystemsProduction", col.names = TRUE, append = TRUE, showNA = FALSE)
# 
# 
# 
# #reshape
# z<- z %>%
#       gather("scenarios", "Val" , 5:ncol(z))
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
#       png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datasys[i],"PK.png",sep=""), 
#           width = 10, height = 10, units = 'in', res = 300)
#       
#       n[[i]]<- print(ggplot(z[which(z$impactparameter==datasys[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
#                            theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(datasys[i])+
#                            facet_grid(productiontype~.)+ theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
#                            ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
#       )
#       dev.off()
#       print(i)
# }  
# 
# 
# #Exportar datos para consultor
# a<-phi_sys[which(phi_sys$impactparameter=="YldXAgg -- Yield"),]
# b<-phi_sys[which(phi_sys$impactparameter=="AreaXAgg -- Area"),]
# 
# y<- rbind(a,b)
# y<- y %>% spread(year,Val)
# write.csv(y, paste(copy, "SystemProductionMOZ.csv", sep = ""))
# 
# rm(z)
# 
# # limpiar todo---------
# #g=gc; rm(list = ls())
# 
# 
# 
# ################################################ PARTE B Generador de tablas. #################################################
# 
# #Hacer un subconjunto que sólo contenga las variables de mi interés----------------
# mdsub<-subset(phi,phi$impactparameter=="TAreaXAgg -- Total Area" |
#                     phi$impactparameter== "QNXAgg -- Net Trade" | 
#                     phi$impactparameter== "TYldXAgg -- Total Yield"|
#                     phi$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
#                     phi$impactparameter=="AnmlYldXAgg -- Animal Yield")
# 
# mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
#                                                         "TAreaXAgg -- Total Area"="Total Area",
#                                                         "TYldXAgg -- Total Yield"= "Yield",
#                                                         "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
#                                                         "AnmlYldXAgg -- Animal Yield"="Animal Yield"))
# 
# 
# mdsub<-data.frame(mdsub,"Cat"=ifelse(mdsub$scenario=="NoCC","NoCC","CC"))
# 
# row.names(mdsub)<- 1: nrow(mdsub)
# 
# mdwide_tan<- mdsub %>%
#       spread ("year", "Val")
# 
# # elimino los periodos antes del 2020
# mdwide_tan<- mdwide_tan[,-c(6:20)]
# row.names(mdwide_tan)<- 1: nrow(mdwide_tan)
# 
# 
# #Copia de seguridad pais filtrado------
# write.csv(mdwide_tan,paste(copy,"mdwidePAKTotal.csv", sep = ""), row.names = FALSE)
# 
# #Net Trade y  filtros logicos-----------------
# tznet<- mdwide_tan[which(mdwide_tan$impactparameter=="Net Trade"),]
# row.names(tznet)<- 1:nrow(tznet)
# 
# ## logica de los valores 
# nn<-  which(tznet$`2050`<0 & tznet$`2020`<0) # net trade negativo  importador neto
# pn<-  which(tznet$`2050`>0 & tznet$`2020`<0) # impacto positivo inicia como importador termina como exportador
# np<-  which(tznet$`2050`<0 & tznet$`2020`>0) # impacto negativo inicia como exportador termina como importador
# pp<-  which(tznet$`2050`>0 & tznet$`2020`>0) # net trade positivo  exportador neto
# 
# 
# # desempeño 
# export<-   c(pp)
# import <-  c(nn)
# tran_XtoM<- c(np) # inicia exportador termina importador
# tran_MtoX<- c(pn) # inicia importador termina exportador
# 
# hard<- c(nn,pp,pn,np)
# 
# tznet$impacto<- NA # para poner el impacto  cambio relativo
# 
# #loops 
# for(j in 1:nrow(tznet)){
#       
#       if(j %in% hard){
#             tznet$impacto[j] <- abs(tznet$`2050`[j] - tznet$`2020`[j])/max(abs(tznet$`2050`[j]), abs(tznet$`2020`[j]), na.rm=TRUE) * 100
#       } else {  }
#       
# }
# 
# ##Copia de seguridad cambios relativos y vectores de desempeño
# write.csv(tznet,paste(copy,"CambiosRelativosPAKTOTAL.csv", sep = ""), row.names = FALSE)
# 
# # copia
# tanz<- tznet
# tanz$trend<- NA
# tanz$trend[import] <- "Negative"
# tanz$trend[export] <- "Positive"
# tanz$trend[tran_MtoX]<- "Transition from M to X"
# tanz$trend[tran_XtoM]<- "Transition from X to M"
# 
# c <- list()
# 
# for (i in 1:length(cultivationsTrade)){
#       
#       if(length(which(tanz$commodity==cultivationsTrade[[i]]))>0){
#             z <- tanz[which(tanz$commodity==cultivationsTrade[[i]]),]
#             x <- sort(table(z$trend[z$Cat=='CC']),decreasing = T)
#             z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
#             if(x>= 3){
#                   z$trend <- names(x)
#             }
#             c[[i]] <- z
#       } else{
#             cat(paste('Commodity:', cultivationsTrade[i], 'does not have data\n', sep=''))
#             
#       }
#       
# }
# 
# 
# ##Unir los cultivos
# c <- do.call(rbind, c)  
# 
# ##Calcular la media de los impactos
# c$CC_mean <- rowMeans(x=c[,5:8], na.rm=TRUE)
# c$NoCC_mean <- c[,9]
# c<- c[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
# write.csv(c, paste(copy, "NettradePAKTOTAL.csv", sep = ""),row.names = FALSE)
# 
# #### asunto maíz
# 
# maizedf<- tanz[which(tanz$commodity==maize),]
# chicha<-maizedf[-1,]
# chicha <- chicha[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto", "trend")] %>% spread("scenario","impacto")
# chicha$CC_mean <- rowMeans(x=chicha[,6:8], na.rm=TRUE)
# chicha$NoCC_mean <- chicha[,9]
# chicha<- chicha[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
# write.csv(chicha, paste(copy, "NettradeMaizPAKTOTAL.csv", sep = ""),row.names = FALSE)
# 
# 
# #Area  y rendimientos---------------------------------------
# tznet2<- mdwide_tan
# tznet2<-  tznet2[which(tznet2$impactparameter!="Net Trade"),]
# tznet2$Percentage_Change<- ((tznet2$`2050`-tznet2$`2020`)/tznet2$`2020`)*100
# tznet2<- tznet2[,-c(6:36)]
# 
# ##Reshape
# tznet2_t<-tznet2 %>%
#       spread("scenario","Percentage_Change")
# 
# ##Calculo de la media
# tznet2_t$mean <- rowMeans(x=tznet2_t[,5:ncol(tznet2_t)], na.rm=TRUE)
# tznet2_t <- tznet2_t[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
# tznet2_t$NoCC<- NULL
# #Reshape
# tznet2_t <- tznet2_t %>% spread(Cat, mean)
# 
# write.csv(x =tznet2_t, file =paste(copy,"areayieldPK.csv",sep = ""))
# 
# 
# ## Asunto maiz
# 
# 
# #Solicitud datos CSA-------------
# mdwide_tzaCSA<- mdwide_tan
# mdwide_tzaCSA<- mdwide_tzaCSA[,c("impactparameter", "scenario","commodity","region",2020,2025,2030,2035,2040,2045,2050, "Cat" )]
# mdwide_tzaCSA<- mdwide_tzaCSA[-which(mdwide_tzaCSA=="Net Trade"),]
# rownames(mdwide_tzaCSA)<- 1:nrow(mdwide_tzaCSA)
# 
# 
# #Listas para desarrollar el proceso
# cultivations
# 
# #periodos
# k<- list()
# testyear<- list()
# y<- c(2020,2025,2030,2035,2040,2045,2050)
# v<- c("Total Area", "Yield" )
# 
# library(dplyr)
# library(tidyr)
# 
# csa <- mdwide_tzaCSA[,c("impactparameter", "commodity", paste(seq(from=2020, to=2050, by=5)), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
# csa <- csa %>% 
#       gather(Year, Value, 4:10)
# csa <- csa %>% 
#       spread(Cat, Value)
# csa$Year <- as.numeric(x = csa$Year)
# csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100
# 
# write.csv(csa, paste(copy, "PAKCSAAreaYield.csv", sep = ""), row.names = FALSE)
# 
# 
# #grafico Area de cultivo
# png(filename=paste(grd,"AreaCommoditiesPAK.png", sep=""), width = 10, height = 10, units = 'in', res =800)
# 
# n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change ))
# n<- n + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
# n<- n + geom_area(position = "stack", alpha = 0.4)
# n<- n + ggtitle("Area") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
# n<- n + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
# n<- n + coord_cartesian(ylim = c(-10, 10)) +   scale_y_continuous( breaks=seq(-10, 10, 2))
# n<- n + coord_cartesian(xlim = c(2020, 2050)) +  scale_x_continuous( breaks=seq(2020,2050,5))
# 
# #ggsave(file=paste(grd,"AreaCommodities.png", sep=""), n, width=10, height=10.5, units='in') #saves g
# 
# n
# dev.off()
# 
# 
# 
# #grafico numero de Aminales 
# png(filename=paste(grd,"Numer Animales PK.png", sep=""), width = 10, height = 10, units = 'in', res =800)
# 
# n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
# n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
# n1<- n1 + geom_area(position = "stack", alpha = 0.4)
# n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
# n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
# n1<- n1 + coord_cartesian(ylim = c(-0.5, 0.5)) + scale_y_continuous( breaks=seq(-0.5,0.5,0.2))
# n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))
# 
# n1
# dev.off()
# 
# 
# 
# 
# 

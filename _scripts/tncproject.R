### TNC project
### carlos Eduardo Gonzalez 

g=gc;rm(list = ls())
## Directories
outpu<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/TNC project/")
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")

## decimales
options(warn = -1)
options(scipen = 999)


## Libraries
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))
suppressMessages(library(curl))
suppressMessages(library(shiny))
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(parallelsugar))
suppressMessages(library(doParallel))
suppressMessages(library(R.utils))
suppressMessages(library(RColorBrewer))
suppressMessages(library(forecast))
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(reshape))
suppressMessages(library(TTR))
suppressMessages(library(stats))



### load dataBase
f<- read.csv(file = "./FoodBalanceSheets_E_All_Data_(Normalized)/FoodBalanceSheets_E_All_Data_(Normalized).csv", header = T)
q<- read.csv(file = "./Production_Crops_E_All_Data_(Normalized)/Production_Crops_E_All_Data_(Normalized).csv", header = T)
t<- read.csv(file="./Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv", header = T)
l<- read.csv(file="./Environment_LandUse_E_All_Data_(Normalized)/Environment_LandUse_E_All_Data_(Normalized).csv", header = T)
vv<- read.csv(file = "./Value_of_Production_E_All_Data_(Normalized)/Value_of_Production_E_All_Data_(Normalized).csv", header = T)

### vectores paises 
ca<- c( "Mexico", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago","Barbados","Panama", 
         "El Salvador", "Suriname", "Honduras","Martinique", "Nicaragua","Saint Vincent and the Grenadines" ,
         "Saint Lucia","Haiti","Costa Rica", "Aruba", "Belize","Dominican Republic", "Bahamas",
         "Central America","Caribbean","Puerto Rico")

crops.ca<- unique(q$Item)

## ranking and importace for many crops on Centro America and caribbean 
### production
str(q)
q$Item<- as.character(q$Item); q$Area<- as.character(q$Area)
q$Element<- as.character(q$Element)
q$Unit<- as.character(q$Unit)
q$Year<- as.character(q$Year); q$Year<- as.numeric(q$Year)
q$Area.Code<- NULL; q$Item.Code<- NULL; q$Element.Code<- NULL; q$Year.Code<- NULL; q$Flag<- NULL
q$Unit<- NULL
#### elementos
ele.q<- c("Area harvested","Yield","Production")

### filter elementos y paises
qy<- q %>% filter( Element %in% ele.q)
qy<- qy %>% filter(Area %in% ca)
qy$Value<- qy$Value*0.1

### growht yield
q.yield<- qy %>% filter( Element=="Yield") %>%
      filter(., Year=='1970'|Year=='1980' |Year=='1990' | Year=='2000'|Year=='2016')%>%
      spread(., Year, Value) %>%
      select(- Element)
# %>%
#       gather(Year, Val, 3:ncol(.))

q.yield$"70_80"<- (q.yield[,4]-q.yield[,3])/q.yield[,3]*100
q.yield$"80_90"<- (q.yield[,5]-q.yield[,4])/q.yield[,4]*100
q.yield$"90_20"<- (q.yield[,6]-q.yield[,5])/q.yield[,5]*100
q.yield$"20_16"<- (q.yield[,7]-q.yield[,6])/q.yield[,6]*100
write.csv(q.yield, file = paste(outpu, "dataYields.csv",sep=""))
q.yieldg<- q.yield %>% select("Area","Item","70_80","80_90","90_20","20_16")
q.yieldg<- q.yieldg %>% gather(period, val, 3:ncol(q.yieldg))
qyieldt<- q.yieldg %>% filter(Area=="Central America"|Area=="Caribbean")


### Shocks yield and area
pots<- c("Central America","Caribbean") 
# p=1
for(p in 1:length(pots)){
      cfiles<- filter(q, Area==pots[[p]]) %>% filter(., Element=="Yield")
      cropslist<- split(cfiles, cfiles$Item)
      
      lapply(cropslist, function(c){
            c$Value<- (c$Value)*0.1
            test_i<- ts(c[5], start =c(1961))
            plot(test_i)
            
            
            #smooth using Holt Winter
            hw<-HoltWinters(x = test_i,gamma = F) 

            
            HWplot<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
                  
                  hw_object<- HoltWinters(ts_objet,gamma = F)
                  forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
                  for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
                  fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
                  actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
                  
                  save(for_values, fitted_values, actual_values,file = paste(outpu,"temp/",pots[[p]],".RData", sep = ""))
                  
                  
                  graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
                  graphset<- merge(graphset, for_values, all=TRUE, by='time')
                  graphset[is.na(graphset$dev), ]$dev<-0
                  graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),
                                      fitted_values$value_fitted, for_values$value_forecast)
                  
                  graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
                  p<- ggplot(graphset.met, aes(x=time, y=value))+
                        geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
                        geom_line(aes(colour=variable), size=line.size)+ labs(title= paste(pots[[p]], ", Time series",sep=""))+
                        geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
                  
                  return(p)
                  
            }
            
            HWplot(test_i) # usando la funcion 

            # cargar datos 
            load(paste(outpu,"/temp/",pots[[p]],".RData", sep = ""))
            
            names(actual_values)[2]<-"val"
            names(fitted_values)[2]<-"val"
            names(for_values)[2]<-"val"
            
            actual_values$des<- "actual"
            fitted_values$des<- "fitted"
            for_values$des<- "forecast"
            for_values$dev<- NULL
            v<- data.frame(for_values)
            j<- data.frame(actual_values)
            h<- data.frame(fitted_values)
            
            
            v$time<- as.character(v$time)
            j$time<- as.character(j$time)
            h$time<- as.character(h$time)
            bdList<- list(v,h,j)
            db<- do.call(rbind, bdList)
            db$crop<-unique(c$Item)
            write.csv(db,file = paste(outpu,"/temp/", pots[[p]], "OutPutHWFAO.csv", sep = ""))
            db<- filter(db,des!="forecast") %>%  filter(.,time<=2016) %>% spread(des, val)
            db$fitted[is.na(db$fitted)]<- db$actual
            
            localShock<- db$actual-db$fitted
            
            
#             #smooth other techniques
#             test_a<- ts(c[5], start =c(1961,1))
#             plot.ts(test_a)
            
            ## test seasonal
            fit<- tbats(test_i)
            seasonal<- !is.null(fit$seasonal.periods)
            cat(paste("The country ", pots[[p]], " has/hasnot seasonality ==", seasonal, ", done!!\n", sep = "")) 
            
            # decomposing non-seasonal data
            test_a_sma<- SMA(test_i,n = 3) # smooth using simple movil average

            
            ## graph smooth using exponential movil average      
            test_a_ema<- EMA(test_i, n = 3) # smooth using exponential movil average

            # dat frame     
            data_sma<- data.frame(time=time(test_a_sma), val=as.matrix(test_a_sma))
            data_sma$des<- "SMA"
            data_ema<- data.frame(time=time(test_a_ema), val=as.matrix(test_a_ema))
            data_ema$des<- "EMA"
            data_sma$time<- as.character(data_sma$time)
            data_ema$time<- as.character(data_ema$time)
            bdList_smooth<- list(data_sma,data_ema,j)
            db_smooth<- do.call(rbind, bdList_smooth)
            db_smooth$crop<- unique(c$Item)
            write.csv(db_smooth,file = paste(outpu,"/temp/", pots[[p]],"OutPutSmoothFAO.csv", sep = ""))
            db_smooth<- db_smooth %>% spread(des, val)
            db_smooth$EMA[is.na(db_smooth$EMA)]<- db_smooth$actual
            db_smooth$SMA[is.na(db_smooth$SMA)]<- db_smooth$actual
            
            #ema
            localShock_smooth_ema<- db_smooth %>% group_by(time) %>% summarise(localShock_smooth_ema=actual-EMA) %>% 
                  select(localShock_smooth_ema)
            
            #sma
            localShock_smooth_sma<- db_smooth %>% group_by(time) %>% summarise(localShock_smooth_sma=actual-SMA) %>% 
                  select(localShock_smooth_sma)
            
            #deviations
            brief<- data.frame(time= db_smooth$time,country= pots[[p]], localHV= localShock, 
                               localsma=localShock_smooth_sma, 
                               localema=localShock_smooth_ema,
                               crop=unique(c$Item))
            
           
            write.csv(brief,file = paste(outpu,"/temp/", pots[[p]],unique(c$Item), "briefDesy.csv", sep = ""))
            
            
            return(c)   
            
            
      })
      
      

}


###### load data 
shockW<- list.files(path=paste(outpu,"/temp/",sep = "") , pattern = "briefDesy", all.files = T,full.names = T)
shockW<- lapply(shockW, read.csv ,header=T)
shockW<- do.call(rbind, shockW)
shockW$X<- NULL 
yr<-unique(shockW$time)
write.csv(shockW,file = paste(outpu,"/temp/","shockNumbers.csv", sep = ""))



### economic Val
str(vv)
vv$Item<- as.character(vv$Item); vv$Area<- as.character(vv$Area)
vv$Element<- as.character(vv$Element)
vv$Unit<- as.character(vv$Unit)
vv$Year<- as.character(vv$Year); vv$Year<- as.numeric(vv$Year)
vv$Area.Code<- NULL; vv$Item.Code<- NULL; vv$Element.Code<- NULL; vv$Year.Code<- NULL; vv$Flag<- NULL
vv$Unit<- NULL

vw<- vv %>% filter(Element=="Gross Production Value (constant 2004-2006 million US$)") %>%
      filter(Area %in% ca)

vw<- vw %>% spread(., Year, Value)
vw$"70_80"<- ((vw$`1980`- vw$`1970`)/vw$`1970`)*100
vw$"80_90"<- ((vw$`1990`- vw$`1980`)/vw$`1980`)*100
vw$"90_20"<- ((vw$`2000`- vw$`1990`)/vw$`1990`)*100
vw$"20_14"<- ((vw$`2014`- vw$`2000`)/vw$`2000`)*100
vw<- vw %>% select(Area,Item,Element,"70_80","80_90","90_20","20_14")

write.csv(vw, file = paste(outpu, "dataGrossproduction.csv",sep=""))


## exploration results of IMPACT model 
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

# manejo de digitos
options(digits=3) 
options(scipen = 999)

################################################ PARTE A. ####################################################################


#Cargar marco de datos principal-------------
md<-read.csv(paste("C:/Users/CEGONZALEZ/Desktop/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/TNC.csv", sep =""),header=T)
phi<- md

#Conocer los cultivos y agregaciones 
cult<- unique(phi$commodity)

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
            "QEXAgg -- Export","QMXAgg -- Import") #"QINTXAgg -- Intermediate Demand"

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
crops<- unique(phi$commodity)
# cultivations<-  c( "Maize","Sorghum","Wheat","Potato", "Beans","Vegetable as group")
# cultivationsTrade<- c( "Maize","Sorghum","Wheat","Potato", "Beans","Vegetable as group","Pork",
#                        "Sheeps and goats","Poultry","Dairy Production")
# 
# animals<- c("Pork","Sheeps and goats","Poultry","Dairy Production" )
# other<- c("-")
# row.names(phi)<- 1: nrow(phi)

#1. Datos totales--------------------------------

phi<- phi[grep("LAC-",x = phi$region),]
phi$scenario<- revalue(phi$scenario, c("HGEM8.5_CC_ssp2"="HGEM8.5",
                                       "MIRO8.5_CC_ssp2"="MIRO8.5",
                                       "NoCC_CC_ssp2"="NoCC"))
regin<- c("LAC-Mexico","LAC-Nicaragua","LAC-Other Caribbean","LAC-Panama",
          "LAC-Haiti","LAC-Honduras", "LAC-Jamaica",
          "LAC-Dominican Republic","LAC-El Salvador", "LAC-Guatemala",
          "LAC-Costa Rica","LAC-Cuba", "LAC-Belize" ,"LAC")
cropsin<- c("PUL","CER","F&V","OLS" ,"FOR", "SGC","R&T" )

phi<- phi %>% filter(commodity %in% cropsin) %>% filter(.,region %in% regin)
rownames(phi)<- 1:nrow(phi)
k<- list()
# i=1
for(i in seq_along(datatotal)){
      rrr<- phi
      rrr$region<- sub(pattern = "LAC-",replacement = "",x = rrr$region)
      
      # selecciono el cultivo
      k[[i]]<- rrr[which(rrr$impactparameter==datatotal[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
      #elimino fila innecesarias
      k[[i]]<-k[[i]][,-c(5:15)]
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

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(outpu, "TNC.xlsx", sep = ""),
            sheetName = "DatosTotales", col.names = TRUE, append = TRUE, showNA = FALSE)


#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))
rownames(z)<- 1:nrow(z)


n<- list()
# i=1
for (i in 1:length(datatotal)){
      
      png(filename=paste(outpu,datatotal[i],"TNC.png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      
      ggplot(z[which(z$impactparameter==datatotal[i]),], 
                      aes(x=commodity, y=Val, fill=scenarios)) +
                           theme_bw() + 
                           geom_bar(stat = "identity", position=position_dodge())+
                           facet_grid(.~region)+  coord_equal()+
                           coord_flip() + ggtitle(datatotal[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ 
                           theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity")+ 
                           scale_y_continuous(limits = c(-50, 50))
      
      dev.off()
      
}  

rm(z)

#2. Datos agregados--------------------------------

k<- list()
#i=5
for(i in seq_along(dataagg)){
      
      if (i== 2){
            
            # selecciono el cultivo
            k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
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
            k[[i]] <- k[[i]][,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
            k[[i]]$NoCC<- NULL
            #Reshape final para obtener el cambio porcentual promedio por CC o NoCC
            k[[i]] <- k[[i]]%>% spread(Cat, mean) 
      }  else {
            
            # selecciono el cultivo
            k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
            #reordeno los datos
            rownames(k[[i]])<- 1: nrow(k[[i]])
            #reshape a lo ancho  
            k[[i]]<- k[[i]] %>%
                  spread ("year","Val")
            #creo variable para  con CC o NoCC
            k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
            #elimino fila innecesarias
            k[[i]]<-k[[i]][,-c(5:15)]
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
            
      }
      return 
}

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(outpu, "TNC.xlsx", sep = ""),
            sheetName = "DataAgregados", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataagg)){
      
      png(filename=paste(outpu,dataagg[i],"TNC.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(z[which(z$impactparameter==dataagg[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                           facet_grid(.~region)+
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

for(i in seq_along(dataanimal)){
      
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==dataanimal[i]),]
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>%
            spread ("year","Val")
      #creo variable para  con CC o NoCC
      k[[i]]<-data.frame(k[[i]],"Cat"=ifelse(k[[i]]$scenario=="NoCC","NoCC","CC"))
      #elimino fila innecesarias
      k[[i]]<-k[[i]][,-c(5:15)]
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

z<- do.call(rbind, k)

# exportar datos a excel
require(xlsx)
write.xlsx( x = z,file= paste(outpu, "TNC.xlsx", sep = ""),
            sheetName = "DataAnimal", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataanimal)){
      
      png(filename=paste(outpu,dataanimal[i],"TNC.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(z[which(z$impactparameter==dataanimal[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataanimal[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Percentage Change') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
      )
      dev.off()
      print(i)
}  

rm(z)

#4. Datos especiales---------------

k<- list()

for(i in seq_along(dataespecial)){
      
      if(i==1){
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
            k[[i]]<-k[[i]][,-c(5:15)]
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
            k[[i]]<-k[[i]][,-c(5:15)]
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
write.xlsx( x = z,file= paste(outpu, "TNC.xlsx", sep = ""),
            sheetName = "DatosEspeciales", col.names = TRUE, append = TRUE, showNA = FALSE)



#reshape
z<- z %>%
      gather("scenarios", "Val" , 4:ncol(z))


n<- list()
for (i in seq_along(dataespecial)){
      
      png(filename=paste(outpu,dataespecial[i],"TNC.png",sep=""), 
          width = 10, height = 10, units = 'in', res = 300)
      
      n[[i]]<- print(ggplot(z[which(z$impactparameter==dataespecial[i]),], aes(x=commodity, y=Val, fill=scenarios)) + 
                           facet_grid(.~region)+
                           theme_bw() + geom_bar(stat = "identity", position=position_dodge())+ coord_flip() + ggtitle(dataespecial[i])+
                           theme(axis.title.x=element_text(size=12, face='bold'))+ theme(plot.title=element_text(size=15, face = 'bold'))+
                           ylab('Impacts') +  xlab("Commodity")#+ scale_y_continuous(limits = c(-20, 50))
      )
      dev.off()
      print(i)
}  

rm(z)

# #5. Datos por sistemas de produccion--------
# 
# #sys<- c("air","arf")
# 
# k<- list()
# i=1
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
# write.xlsx( x = z,file= paste(copy, "LES.xlsx", sep = ""),
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
#       png(filename=paste("C:/Users/CEGONZALEZ/Documents/CSA/",datasys[i],"LES.png",sep=""), 
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


# limpiar todo---------
#g=gc; rm(list = ls())



################################################ PARTE B Generador de tablas. #################################################

#Hacer un subconjunto que sólo contenga las variables de mi interés----------------
mdsub<-subset(phi,phi$impactparameter=="TAreaXAgg -- Total Area" |
                    phi$impactparameter== "QNXAgg -- Net Trade" | 
                    phi$impactparameter== "TYldXAgg -- Total Yield"|
                    phi$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
                    phi$impactparameter=="AnmlYldXAgg -- Animal Yield")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
                                                        "TAreaXAgg -- Total Area"="Total Area",
                                                        "TYldXAgg -- Total Yield"= "Yield",
                                                        "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
                                                        "AnmlYldXAgg -- Animal Yield"="Animal Yield"))


mdsub<-data.frame(mdsub,"Cat"=ifelse(mdsub$scenario=="NoCC","NoCC","CC"))

row.names(mdsub)<- 1: nrow(mdsub)

mdwide_tan<- mdsub %>%
      spread ("year", "Val")

# elimino los periodos antes del 2020
mdwide_tan<- mdwide_tan[,-c(6:16)]
row.names(mdwide_tan)<- 1: nrow(mdwide_tan)


# #Copia de seguridad pais filtrado------
# write.csv(mdwide_tan,paste(copy,"mdwideLESTotal.csv", sep = ""), row.names = FALSE)
# 
# #Net Trade y  filtros logicos-----------------
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
            tznet$impacto[j] <- abs(tznet$`2050`[j] - tznet$`2020`[j])/max(abs(tznet$`2050`[j]), abs(tznet$`2020`[j]), na.rm=TRUE) * 100
      } else {  }
      
}

##Copia de seguridad cambios relativos y vectores de desempeño
write.csv(tznet,paste(outpu,"CambiosRelativosLESTOTAL.csv", sep = ""), row.names = FALSE)

# copia
tanz<- tznet
tanz$trend<- NA
tanz$trend[import] <- "Negative"
tanz$trend[export] <- "Positive"
tanz$trend[tran_MtoX]<- "Transition from M to X"
tanz$trend[tran_XtoM]<- "Transition from X to M"

cultivationsTrade<- unique(tanz$commodity)
c <- list()
for (i in 1:length(cultivationsTrade)){
      
      if(length(which(tanz$commodity==cultivationsTrade[[i]]))>0){
            z <- tanz[which(tanz$commodity==cultivationsTrade[[i]]),]
            x <- sort(table(z$trend[z$Cat=='CC']),decreasing = T)
            z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
            if(x>= 3){
                  z$trend <- names(x)[1]
            }
            c[[i]] <- z
      } else{
            cat(paste('Commodity:', cultivationsTrade[i], 'does not have data\n', sep=''))
            
      }
      
}



##Unir los cultivos
c <- do.call(rbind, c)  

##Calcular la media de los impactos
c$CC_mean <- rowMeans(x=c[,5:7], na.rm=TRUE)
c$NoCC_mean <- c[,8]
c<- c[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
write.csv(c, paste(outpu, "NettradeLESMTOTAL.csv", sep = ""),row.names = FALSE)


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
tznet2_t <- tznet2_t[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
tznet2_t$NoCC<- NULL
#Reshape
tznet2_t <- tznet2_t %>% spread(Cat, mean)

write.csv(x =tznet2_t, file =paste(outpu,"areayieldBEL.csv",sep = ""))


## Asunto maiz


#Solicitud datos CSA-------------
mdwide_tzaCSA<- mdwide_tan
mdwide_tzaCSA<- mdwide_tzaCSA[,c("impactparameter", "scenario","commodity","region",2020,2025,2030,2035,2040,2045,2050, "Cat" )]
mdwide_tzaCSA<- mdwide_tzaCSA[-which(mdwide_tzaCSA=="Net Trade"),]
rownames(mdwide_tzaCSA)<- 1:nrow(mdwide_tzaCSA)


#Listas para desarrollar el proceso
cultivations

#periodos
k<- list()
testyear<- list()
y<- c(2020,2025,2030,2035,2040,2045,2050)
v<- c("Total Area", "Yield" )

library(dplyr)
library(tidyr)

csa <- mdwide_tzaCSA[,c("impactparameter", "commodity", paste(seq(from=2020, to=2050, by=5)), "Cat")] %>%
      group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
csa <- csa %>% 
      gather(Year, Value, 4:10)
csa <- csa %>% 
      spread(Cat, Value)
csa$Year <- as.numeric(x = csa$Year)
csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100

write.csv(csa, paste(copy, "LESCSAAreaYield.csv", sep = ""), row.names = FALSE)


#grafico Area de cultivo
png(filename=paste(grd,"AreaCommoditieLES.png", sep=""), width = 10, height = 10, units = 'in', res =800)

n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change ))
n<- n + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n<- n + geom_area(position = "stack", alpha = 0.4)
n<- n + ggtitle("Area") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n<- n + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n<- n + coord_cartesian(ylim = c(-10, 10)) +   scale_y_continuous( breaks=seq(-10, 10, 2))
n<- n + coord_cartesian(xlim = c(2020, 2050)) +  scale_x_continuous( breaks=seq(2020,2050,5))

#ggsave(file=paste(grd,"AreaCommodities.png", sep=""), n, width=10, height=10.5, units='in') #saves g

n
dev.off()



#grafico numero de Aminales 
png(filename=paste(grd,"Numer Animales LES.png", sep=""), width = 10, height = 10, units = 'in', res =800)

n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n1<- n1 + geom_area(position = "stack", alpha = 0.4)
n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n1<- n1 + coord_cartesian(ylim = c(-0.5, 0.5)) + scale_y_continuous( breaks=seq(-0.5,0.5,0.2))
n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))

n1
dev.off()




############################################# END CODE ########################################

# 
# 
# hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
# png(filename=paste(outpu,"REgionyields.png",sep=""), 
#     width = 12, height = 12, units = 'in', res = 300)
# 
# 
# n<- ggplot(data = q.yieldt[which(q.yieldt$Area=="Central America"),], aes(Item, Year,fill = Val)) + 
#       +  facet_grid(.~Item, drop = T)+
#       labs(x=NULL, y=NULL, title=paste("Rendimientos",sep = "")) +
#       scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") +  #  theme_grey() 
#       scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme_grey() + labs(x = "",y = "")+
#       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
#       theme(axis.text.y = element_text(hjust = 1, size = 11))+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#       theme(strip.text=element_text(size=8))+
#       theme(strip.text.y = element_text(angle = 0,size = 11)) 
# ggplot(df, aes(x=region, y=Value, fill=trend)) + theme_bw() + geom_bar(stat="identity") + coord_flip()
# 
# plot(n)
# dev.off()  

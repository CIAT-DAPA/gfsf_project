# Rendimientos y serie de tiempo, arroz. Analisis destinado paper de arroz y otros usos. 
# carlos Eduardo


#libraries
# R options
options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))
suppressMessages(library(curl))
suppressMessages(library(shiny))
suppressMessages(library(TTR))
suppressMessages(library(stats))

suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(networkD3))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(reshape))
suppressMessages(library(forecast))



############ Yields 

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")
faoStat<- read.csv(file = "./Production_Crops_E_All_Data_(Normalized)/Production_Crops_E_All_Data_(Normalized).csv", header = T)
str(faoStat)
faoStat$Item<- as.character(faoStat$Item); faoStat$Area<- as.character(faoStat$Area)
faoStat$Element<- as.character(faoStat$Element)
faoStat$Unit<- as.character(faoStat$Unit)


faoStatTrade<- read.csv(file = "./Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv", header = T)
faoStat$Element<- plyr::revalue(faoStat$Element, c("Area harvested"="AreaH"))

var<- unique(faoStat$Element)
crops<- unique(faoStat$Item)
faoStat$Area.Code<- NULL; faoStat$Item.Code<- NULL; faoStat$Element.Code<- NULL; faoStat$Year.Code<- NULL; faoStat$Flag<- NULL
faoStat$Unit<- NULL


# calculando rendimientos y filtrando
rice<-  filter(faoStat,Item=="Rice, paddy") %>% filter(., Element!="Seed") %>% filter(., Value!=0)
# rice <- rice %>% spread(Element,Value) %>% mutate(.,Ys= (.$Production/.$AreaH)*1000)
# rice<- rice[c(1,3,7)]

rice$Area<- plyr::revalue(rice$Area, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                       "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                       "Côte d'Ivoire"="Ivory Coast",
                                       "China, Hong Kong SAR"='Hong Kong',
                                       "China, Taiwan Province of"='Taiwan'))


lac<- c( "Argentina", "Mexico", "Venezuela (Bolivarian Republic of)","Paraguay", "Peru", "Brazil","Chile", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago",
         "Barbados","Panama", "El Salvador", "Suriname", "Uruguay", "Honduras","Martinique", "Nicaragua", "Haiti","Colombia", "Costa Rica", 
         "Aruba", "Belize","Ecuador", "Guatemala","Chile", "Bolivia (Plurinational State of)", "Guyana", "Dominican Republic", "Bahamas")

potsOut<- c("Least Developed Countries" ,"Land Locked Developing Countries","Small Island Developing States",
            "Low Income Food Deficit Countries","Net Food Importing Developing Countries","European Union",
            "World","Africa","Eastern Africa","Middle Africa" ,"Northern Africa","Southern Africa","Western Africa",
            "Central America", "Caribbean","South America" ,"Asia", "Central Asia", "Eastern Asia" ,"Southern Asia",
            "Europe","Eastern Europe","Southern Europe" ,"Western Europe" , "Oceania","Australia & New Zealand","Melanesia",
            "Micronesia", "Americas","Northern America","South-Eastern Asia","Western Asia","China, Macao SAR","China, mainland")

###################
##    LAC      ####
###################
lacRice<- rice %>% filter( Area %in% lac)
pots<-unique(lacRice$Area)
# c=1

# ####LAC----------------
# lapply(1:length(pots), function(c){
#       
#       #################################################Country###############################################################
#       workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/"
#       cfiles<- filter(lacRice, Area==pots[[c]]) %>% filter(., Element=="Yield")
#       cfiles$Value<- (cfiles$Value)*0.1
#       test_i<- ts(cfiles[5], start =c(1961,1),frequency = 1 )
# 
#       #smooth using Holt Winter
#       hw<-HoltWinters(x = test_i,beta = T, gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
# 
# #       # Grafico 
# #       png(filename = paste(workdi,pots[[c]], "_graph1HW.png", sep = ""))
# #       plot(hw,main= paste(pots[[c]],",fitted using Holt Winters method."))
# #       dev.off()
# 
# 
#       HWplot<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
# 
#             hw_object<- HoltWinters(ts_objet,beta = T, gamma = F)
#             forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
#             for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
#             fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
#             actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
#             
#             save(for_values, fitted_values, actual_values,file = paste(workdi,pots[[c]], ".RData", sep = ""))
#             graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
#             graphset<- merge(graphset, for_values, all=TRUE, by='time')
#             graphset[is.na(graphset$dev), ]$dev<-0
#             graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
#             
#             graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
#             p<- ggplot(graphset.met, aes(x=time, y=value))+
#                   geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
#                   geom_line(aes(colour=variable), size=line.size)+ labs(title= paste(pots[[c]], ", Time series",sep=""))+
#                   geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
#             
#             return(p)
#             
#       }
#       
#       
#       # Grafico 
#       tiff(filename= paste(workdi,pots[[c]],"_graph2HW.tiff",sep=""), 
#            width = 10, height = 7, units = 'in', res = 100)
#       HWplot(test_i) # usando la funcion 
#       dev.off()
#       
#       
#       # cargar datos 
#       load(paste("./RicePaper/", pots[[c]], ".RData", sep = ""))
#       
#       names(actual_values)[2]<-"val"
#       names(fitted_values)[2]<-"val"
#       names(for_values)[2]<-"val"
# 
#       actual_values$des<- "actual"
#       fitted_values$des<- "fitted"
#       for_values$des<- "forecast"
#       for_values$dev<- NULL
#       v<- data.frame(for_values)
#       j<- data.frame(actual_values)
#       h<- data.frame(fitted_values)
# 
#       
#       v$time<- as.character(v$time)
#       j$time<- as.character(j$time)
#       h$time<- as.character(h$time)
#       bdList<- list(v,h,j)
#       db<- do.call(rbind, bdList)
#       write.csv(db,file = paste("./RicePaper/", pots[[c]], "OutPutHW.csv", sep = ""))
#       db<- filter(db,des!="forecast") %>%  filter(.,time<=2016) %>% spread(des, val)
#       db$fitted[is.na(db$fitted)]<- db$actual
#       
#       localShock<- sum((db$fitted-(db$actual))^2)/(length(db$fitted)-1)
#       localShock<- (localShock)^0.5
#       
#       #smooth other techniques
#       test_a<- ts(cfiles[5], start =c(1961,1),frequency = 1)
#       plot.ts(test_a)
#       
#       ## test seasonal
#       fit<- tbats(test_a)
#       seasonal<- !is.null(fit$seasonal.periods)
#       cat(paste("The country ", pots[[c]], " has/hasnot seasonality ==", seasonal, ", done!!\n", sep = "")) 
#       
#       # decomposing non-seasonal data
#       ## order 3
#       test_a_sma<- SMA(test_a,n = 3) # smooth using simple movil average
#       png(filename= paste(workdi,pots[[c]],"_graphSMA.png",sep=""), 
#            width = 10, height = 7, units = 'in', res = 100)
#       plot.ts(test_a_sma)
#       dev.off()
#       
#     
#       ## graph smooth using exponential movil average      
#       png(filename= paste(workdi,pots[[c]],"_graphEMA.png",sep=""), 
#           width = 10, height = 7, units = 'in', res = 100)
#       test_a_ema<- EMA(test_a, n = 3) # smooth using exponential movil average
#       plot.ts(test_a_ema)
#       dev.off()
# 
#       # dat frame     
#       data_sma<- data.frame(time=time(test_a_sma), val=as.matrix(test_a_sma))
#       data_sma$des<- "SMA"
#       data_ema<- data.frame(time=time(test_a_ema), val=as.matrix(test_a_ema))
#       data_ema$des<- "EMA"
#       data_sma$time<- as.character(data_sma$time)
#       data_ema$time<- as.character(data_ema$time)
#       bdList_smooth<- list(data_sma,data_ema,j)
#       db_smooth<- do.call(rbind, bdList_smooth)
#       write.csv(db_smooth,file = paste("./RicePaper/", pots[[c]], "OutPutSmooth.csv", sep = ""))
#       db_smooth<- db_smooth %>% spread(des, val)
#       db_smooth$EMA[is.na(db_smooth$EMA)]<- db_smooth$actual
#       db_smooth$SMA[is.na(db_smooth$SMA)]<- db_smooth$actual
#       
#       #ema
#       localShock_smooth_ema<- sum((db_smooth$EMA -(db_smooth$actual))^2)/(length(db_smooth$EMA)-1)
#       localShock_smooth_ema<- (localShock_smooth_ema)^0.5
#       
#       #sma
#       localShock_smooth_sma<- sum((db_smooth$SMA -(db_smooth$actual))^2)/(length(db_smooth$SMA)-1)
#       localShock_smooth_sma<- (localShock_smooth_sma)^0.5
#       
#       #deviations
#       brief<- data.frame(country= pots[[c]], localHV= localShock, localsma=localShock_smooth_sma, localema=localShock_smooth_ema)
#       write.csv(brief,file = paste("./RicePaper/", pots[[c]], "briefDesy.csv", sep = ""))
#       
# 
#      
#       
#       
#       cat(paste("country: ", pots[[c]],  " it has been done!!!/n", sep = ""))
#       return(c)
#       
#       
# })

# #################################################WORLD###############################################################
# workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/"
# cfilesWorld<- filter(rice, Area!=pots[[c]]) %>% filter(.,!Area %in% potsOut) %>% spread(Element, Value)%>%
#       group_by(Year) %>% summarize(Value=sum(Production,na.rm=T)/sum(AreaH,na.rm=T))
# 
# 
# 
#       cfilesWorld$Value<- (cfilesWorld$Value)*1000
#       test_WW<- ts(cfilesWorld[2], start =c(1961,1),frequency = 1 )
#       
#       #smooth using Holt Winter
#       hw_W<-HoltWinters(x = test_WW,beta = T, gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
#       
#       
#       
#       HWplotWorld<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
#             
#             hw_object<- HoltWinters(ts_objet,beta = T, gamma = F)
#             forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
#             for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
#             fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
#             actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
#             
#             save(for_values, fitted_values, actual_values,file = paste(workdi,"World_",pots[[c]], ".RData", sep = ""))
#             
#             
#             graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
#             graphset<- merge(graphset, for_values, all=TRUE, by='time')
#             graphset[is.na(graphset$dev), ]$dev<-0
#             graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
#             
#             graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
#             p<- ggplot(graphset.met, aes(x=time, y=value))+
#                   geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
#                   geom_line(aes(colour=variable), size=line.size)+ labs(title= paste("World and ",pots[[c]], ", Time series",sep=""))+
#                   geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
#             
#             return(p)
#             
#       }
#       
#       # Grafico 
#       tiff(filename= paste(workdi,pots[[c]],"World_graphHW.tiff",sep=""), 
#            width = 10, height = 7, units = 'in', res = 100)
#       HWplotWorld(test_WW) # usando la funcion 
#       dev.off()
#       
#       # cargar datos 
#       load(paste("./RicePaper/", "World_",pots[[c]], ".RData", sep = ""))
#       
#       names(actual_values)[2]<-"val"
#       names(fitted_values)[2]<-"val"
#       names(for_values)[2]<-"val"
#       
#       actual_values$des<- "actual"
#       fitted_values$des<- "fitted"
#       for_values$des<- "forecast"
#       for_values$dev<- NULL
#       vw<- data.frame(for_values)
#       jw<- data.frame(actual_values)
#       hw<- data.frame(fitted_values)
#       
#       
#       vw$time<- as.character(vw$time)
#       jw$time<- as.character(jw$time)
#       hw$time<- as.character(hw$time)
#       bdListw<- list(vw,hw,jw)
#       dbw<- do.call(rbind, bdListw)
#       write.csv(dbw,file = paste("./RicePaper/", "World_",pots[[c]], "OutPutHW.csv", sep = ""))
#       dbw<- filter(dbw,des!="forecast") %>%  filter(.,time<=2016) %>% spread(des, val)
#       dbw$fitted[is.na(dbw$fitted)]<- dbw$actual
#       
#       foreingShock<- sum((dbw$fitted-(dbw$actual))^2)/(length(dbw$fitted)-1)
#       foreingShock<- (foreingShock)^0.5
#       
#       #smooth other techniques
#       test_aw<- ts(cfilesWorld[2], start =c(1961,1),frequency = 1)
#       plot.ts(test_aw)
#       
#       ## test seasonal
#       fit<- tbats(test_aw)
#       seasonal<- !is.null(fit$seasonal.periods)
# 
#       # smooth using simple movil average
#       ## order 3
#       test_w_sma<- SMA(test_aw,n = 3) # smooth using simple movil average
# 
#       ## graph smooth using exponential movil average      
#       test_w_ema<- EMA(test_aw, n = 3) # smooth using exponential movil average
#       plot.ts(test_w_ema)
#       dev.off()
#       
#       # dat frame     
#       dataW_sma<- data.frame(time=time(test_w_sma), val=as.matrix(test_w_sma))
#       dataW_sma$des<- "SMA"
#       dataW_ema<- data.frame(time=time(test_w_ema), val=as.matrix(test_w_ema))
#       dataW_ema$des<- "EMA"
#       dataW_sma$time<- as.character(dataW_sma$time)
#       dataW_ema$time<- as.character(dataW_ema$time)
#       bdListW_smooth<- list(dataW_sma,dataW_ema,j)
#       dbW_smooth<- do.call(rbind, bdListW_smooth)
#       write.csv(dbW_smooth,file = paste("./RicePaper/", "World_",pots[[c]], "_OutPutSmooth.csv", sep = ""))
#       dbW_smooth<- dbW_smooth %>% spread(des, val)
#       dbW_smooth$EMA[is.na(dbW_smooth$EMA)]<- dbW_smooth$actual
#       dbW_smooth$SMA[is.na(dbW_smooth$SMA)]<- dbW_smooth$actual
#       
#       #ema
#       foreignShock_smooth_ema<- sum((dbW_smooth$EMA -(dbW_smooth$actual))^2)/(length(dbW_smooth$EMA)-1)
#       foreignShock_smooth_ema<- (foreignShock_smooth_ema)^0.5
#       
#       #sma
#       foreignShock_smooth_sma<- sum((dbW_smooth$SMA -(dbW_smooth$actual))^2)/(length(dbW_smooth$SMA)-1)
#       foreignShock_smooth_sma<- (foreignShock_smooth_sma)^0.5
#       
#       #deviations
#       briefW<- data.frame(country= pots[[c]], foreignHV= foreingShock, foreignsma=foreignShock_smooth_sma, foreignema=foreignShock_smooth_ema)
#       write.csv(briefW,file = paste("./RicePaper/", pots[[c]], "World_briefDesy.csv", sep = ""))
#       
#       
#       p<- merge(brief,briefW,all=T)
#       p$zone<- "ALC"
#       write.csv(p,file = paste("./RicePaper/", pots[[c]], "World_briefDesy.csv", sep = ""))
#       
#   
# 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

###################
##    World    ####
###################
WorldRice<- rice %>% filter(!Area %in% potsOut)  #%>% filter(!Area %in% lac)
pots<-unique(WorldRice$Area)
# c=1

####LAC----------------
lapply(1:length(pots), function(c){
      
      #################################################Country###############################################################
      workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/"
      cfiles<- filter(WorldRice, Area==pots[[c]]) %>% filter(., Element=="Yield")
      
      cfiles$Value<- (cfiles$Value)*0.1
      test_i<- ts(cfiles[5], start =c(1961,1),frequency = 1 )
      
      #smooth using Holt Winter
      hw<-HoltWinters(x = test_i,beta = T, gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
      
#       # Grafico 
#       png(filename = paste(workdi,pots[[c]], "_graph1HW.png", sep = ""))
#       plot(hw,main= paste(pots[[c]],",fitted using Holt Winters method."))
#       dev.off()
      
      
      HWplot<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
            
            hw_object<- HoltWinters(ts_objet,beta = T, gamma = F)
            forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
            for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
            fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
            actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
            
            save(for_values, fitted_values, actual_values,file = paste(workdi,pots[[c]], ".RData", sep = ""))
            
            
            graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
            graphset<- merge(graphset, for_values, all=TRUE, by='time')
            graphset[is.na(graphset$dev), ]$dev<-0
            graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
            
            graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
            p<- ggplot(graphset.met, aes(x=time, y=value))+
                  geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
                  geom_line(aes(colour=variable), size=line.size)+ labs(title= paste(pots[[c]], ", Time series",sep=""))+
                  geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
            
            return(p)
            
      }
      
      
      # Grafico 
      tiff(filename= paste(workdi,pots[[c]],"_graph2HW.tiff",sep=""), 
           width = 10, height = 7, units = 'in', res = 100)
      HWplot(test_i) # usando la funcion 
      dev.off()
      
      
      # cargar datos 
      load(paste("./RicePaper/", pots[[c]], ".RData", sep = ""))
      
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
      write.csv(db,file = paste("./RicePaper/", pots[[c]], "OutPutHW.csv", sep = ""))
      db<- filter(db,des!="forecast") %>%  filter(.,time<=2016) %>% spread(des, val)
      db$fitted[is.na(db$fitted)]<- db$actual
      
      localShock<- sum((db$fitted-(db$actual))^2)/(length(db$fitted)-1)
      localShock<- (localShock)^0.5
      
      #smooth other techniques
      test_a<- ts(cfiles[5], start =c(1961,1),frequency = 1)
      plot.ts(test_a)
      
      ## test seasonal
      fit<- tbats(test_a)
      seasonal<- !is.null(fit$seasonal.periods)
      cat(paste("The country ", pots[[c]], " has/hasnot seasonality ==", seasonal, ", done!!\n", sep = "")) 
      
      # decomposing non-seasonal data
      ## order 3
      test_a_sma<- SMA(test_a,n = 3) # smooth using simple movil average
#       png(filename= paste(workdi,pots[[c]],"_graphSMA.png",sep=""), 
#           width = 10, height = 7, units = 'in', res = 100)
#       plot.ts(test_a_sma)
#       dev.off()
      
      
      ## graph smooth using exponential movil average      
      test_a_ema<- EMA(test_a, n = 3) # smooth using exponential movil average
#       png(filename= paste(workdi,pots[[c]],"_graphEMA.png",sep=""), 
#           width = 10, height = 7, units = 'in', res = 100)
#       plot.ts(test_a_ema)
#       dev.off()
      
      # dat frame     
      data_sma<- data.frame(time=time(test_a_sma), val=as.matrix(test_a_sma))
      data_sma$des<- "SMA"
      data_ema<- data.frame(time=time(test_a_ema), val=as.matrix(test_a_ema))
      data_ema$des<- "EMA"
      data_sma$time<- as.character(data_sma$time)
      data_ema$time<- as.character(data_ema$time)
      bdList_smooth<- list(data_sma,data_ema,j)
      db_smooth<- do.call(rbind, bdList_smooth)
      write.csv(db_smooth,file = paste("./RicePaper/", pots[[c]], "OutPutSmooth.csv", sep = ""))
      db_smooth<- db_smooth %>% spread(des, val)
      db_smooth$EMA[is.na(db_smooth$EMA)]<- db_smooth$actual
      db_smooth$SMA[is.na(db_smooth$SMA)]<- db_smooth$actual
      
      #ema
      localShock_smooth_ema<- sum((db_smooth$EMA -(db_smooth$actual))^2)/(length(db_smooth$EMA)-1)
      localShock_smooth_ema<- (localShock_smooth_ema)^0.5
      
      #sma
      localShock_smooth_sma<- sum((db_smooth$SMA -(db_smooth$actual))^2)/(length(db_smooth$SMA)-1)
      localShock_smooth_sma<- (localShock_smooth_sma)^0.5
      
      #deviations
      brief<- data.frame(country= pots[[c]], localHV= localShock, localsma=localShock_smooth_sma, localema=localShock_smooth_ema)
      
      #average production
      cfilesPro1<- filter(WorldRice, Element=="Production")%>% spread(Year, Value) %>% mutate(total=rowSums(.[4:ncol(.)],na.rm=T ))
      totalQ<- sum(cfilesPro1$total)

      cfilesPro2<- filter(WorldRice, Area==pots[[c]]) %>% filter(., Element=="Production")
      
      temQ<- cfilesPro2 %>% spread(Year, Value) %>% mutate(total=rowSums(.[4:ncol(.)],na.rm=T ))%>% select(Area, Element, total)
      brief$Qave<- temQ[,3] 

      brief$localHV_W<- (brief$Qave/totalQ)* brief$localHV
      brief$localsma_W<- (brief$Qave/totalQ)* brief$localsma
      brief$localema_W<- (brief$Qave/totalQ)* brief$localema  
      
      
      write.csv(brief,file = paste("./RicePaper/", pots[[c]], "briefDesy.csv", sep = ""))
      

      return(c)
      
      
}) 
      
### load data 
shockW<- list.files(path="./RicePaper/", pattern = "briefDesy", all.files = T,full.names = T)
shockW<- lapply(shockW, read.csv ,header=T)
shockW<- do.call(rbind, shockW)
shockW$X<- NULL 

# c=1
lapply(1:length(pots), function(c){
      
      workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/"
      foreign<- filter(shockW, country!=pots[[c]]) %>% mutate(sumlocalHV_W=sum(localHV_W),
                                                              sumlocalsma_W=sum(localsma_W),
                                                              sumlocalema_W=sum(localema_W)) 
      
      local<- filter(shockW, country==pots[[c]])
      local$f_hvW<- unique(foreign$sumlocalHV_W)
      local$f_smaW<- unique(foreign$sumlocalsma_W) 
      local$f_emaW<- unique(foreign$sumlocalema_W)
      write.csv(local,file = paste("./RicePaper/", pots[[c]], "_shocksLocal&Foreigh.csv", sep = ""))
      

      return(c)

})

### load data 
shockComplete<- list.files(path="./RicePaper/", pattern = "shocksLocal&Foreigh", all.files = T,full.names = T)
shockComplete<- lapply(shockComplete, read.csv ,header=T)
shockComplete<- do.call(rbind, shockComplete)
shockComplete$X<- NULL 



#### getting zscore 
## local 

l_hw_sd<- sd(shockComplete$localHV)*sqrt((length(shockComplete$localHV)-1)/(length(shockComplete$localHV))) 
l_hw_mean<- mean(shockComplete$localHV)
z_l_hw<-(shockComplete$localHV - l_hw_mean)/ l_hw_sd

## dataframe local and foreign
z_l_hw<- data.frame(country=unique(shockComplete$country),zscore_l_hw=z_l_hw)

var<- c("localHV","localsma","localema","f_hvW", "f_smaW","f_emaW")

# 
# test<- grepl(pattern = "^f_",x = names(shockComplete))
# p<- shockComplete
# p<-p[test]

# v=1
z_dataL<- list()
for(v in 1:length(var)){
      sd_var<- sd(shockComplete[,var[v]])
      length_var<- length(shockComplete[,var[v]])
      
#       a<- (sd_var)*sqrt(length_var-1)/length_var
#       b<- mean(shockComplete[,var[v]])
#       z<- (shockComplete[,var[v]]-b)/a
      a<- sd_var
      b<- mean(shockComplete[,var[v]])
      z<- (shockComplete[,var[v]]-b)/a
      
#       l_hw_sd<- sd(shockComplete$var[v])*sqrt((length(shockComplete$localHV)-1)/(length(shockComplete$localHV))) 
#       l_hw_mean<- mean(shockComplete$localHV)
#       z_l_hw<-(shockComplete$localHV - l_hw_mean)/ l_hw_sd
#       
      ## dataframe local and foreign
      # z_l_hw<- data.frame(country=unique(shockComplete$country),zscore=z)
      z_dataL[[v]]<- data.frame(country=unique(shockComplete$country),zscore=z, Z_shock=var[v] )
      
      cat(paste("Shock", var[v], "it was done!!!\n"))
}

## reuniendo zscore de los shocks
gdata<- do.call(rbind, z_dataL)
gdata<- gdata %>% spread(Z_shock, zscore)
temp_data<- list()
# v=1
for ( v in 1:length(var)){
      
      ## logica de los valores 
      hn<-  which(gdata[,var[v]]<=-1)
      nn<-  which(gdata[,var[v]]<0 & gdata[,var[v]]>-1)
      hp<-  which(gdata[,var[v]]>1)    
      mr<-  which(gdata[,var[v]]>0 & gdata[,var[v]]<1)
      
      # desempeño 
      highNegative<-  c(hn)
      negativeNegative<-  c(nn)
      highPositive<- c(hp) 
      middleRange<- c(mr) 
      
      
      # copia
      tanz<- gdata
      tanz$trend<- NA
      tanz$trend[highNegative] <- "High_Negative"
      tanz$trend[negativeNegative] <- "Negative_Negative"
      tanz$trend[highPositive]<- "High_Positive"
      tanz$trend[middleRange]<- "Middle_Range"
      tanz<-tanz[,c("country","trend")]
      # tanz$shock<- var[v]
      colnames(tanz)[2]<-paste("cat_",var[v],sep = "")
      
      temp_data[[v]]<- tanz
      
      
      cat(paste("Shock", var[v], "it was done!!!\n"))
      
}


### processing 
sdata<- do.call(cbind, temp_data)
sdata<- sdata[,-c(3,5,7,9,11)]
cols<- c("cat_localHV","cat_localsma","cat_localema","cat_f_hvW","cat_f_smaW","cat_f_emaW" )
sdata<- sdata %>% mutate_each(funs(factor(.)), cols)


### holt and winter
t1<-table(sdata$cat_localHV,sdata$cat_f_hvW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

hv<- prop.table(t1)
write.csv(hv,file = paste("./RicePaper/","hv_shocksLocal&Foreigh.csv", sep = ""))

#### sma 
t1<-table(sdata$cat_localsma,sdata$cat_f_smaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

sma_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/","sma_shocksLocal&Foreigh.csv", sep = ""))

### ema
t1<-table(sdata$cat_localema,sdata$cat_f_emaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

ema_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/","ema_shocksLocal&Foreigh.csv", sep = ""))



############ analysis just for LAC countries ######################
glac<- filter(sdata, country %in% lac)

### holt and winter
t1<-table(glac$cat_localHV,glac$cat_f_hvW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

hv<- prop.table(t1)
write.csv(hv,file = paste("./RicePaper/","lac_hv_shocksLocal&Foreigh.csv", sep = ""))

#### sma 
t1<-table(glac$cat_localsma,glac$cat_f_smaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

sma_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/","lac_sma_shocksLocal&Foreigh.csv", sep = ""))

### ema
t1<-table(glac$cat_localema,glac$cat_f_emaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

ema_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/","lac_ema_shocksLocal&Foreigh.csv", sep = ""))


###################################### 





# 
# 
# 
#       #################################################WORLD###############################################################
#       workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/"
#       cfilesWorld<- filter(rice, Area!=pots[[c]]) %>% filter(.,!Area %in% potsOut) %>% spread(Element, Value)%>%
#             group_by(Year) %>% summarize(Value=sum(Production,na.rm=T)/sum(AreaH,na.rm=T))
#       
#       
#       cfilesWorld$Value<- (cfilesWorld$Value)*1000
#       test_WW<- ts(cfilesWorld[2], start =c(1961,1),frequency = 1 )
#       
#       #smooth using Holt Winter
#       hw_W<-HoltWinters(x = test_WW,beta = T, gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
#       
#       
#       
#       HWplotWorld<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
#             
#             hw_object<- HoltWinters(ts_objet,beta = T, gamma = F)
#             forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
#             for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
#             fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
#             actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
#             
#             save(for_values, fitted_values, actual_values,file = paste(workdi,"World_",pots[[c]], ".RData", sep = ""))
#             
#             
#             graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
#             graphset<- merge(graphset, for_values, all=TRUE, by='time')
#             graphset[is.na(graphset$dev), ]$dev<-0
#             graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
#             
#             graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
#             p<- ggplot(graphset.met, aes(x=time, y=value))+
#                   geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
#                   geom_line(aes(colour=variable), size=line.size)+ labs(title= paste("World and ",pots[[c]], ", Time series",sep=""))+
#                   geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
#             
#             return(p)
#             
#       }
#       
#       # Grafico 
#       tiff(filename= paste(workdi,pots[[c]],"World_graphHW.tiff",sep=""), 
#            width = 10, height = 7, units = 'in', res = 100)
#       HWplotWorld(test_WW) # usando la funcion 
#       dev.off()
#       
#       # cargar datos 
#       load(paste("./RicePaper/", "World_",pots[[c]], ".RData", sep = ""))
#       
#       names(actual_values)[2]<-"val"
#       names(fitted_values)[2]<-"val"
#       names(for_values)[2]<-"val"
#       
#       actual_values$des<- "actual"
#       fitted_values$des<- "fitted"
#       for_values$des<- "forecast"
#       for_values$dev<- NULL
#       vw<- data.frame(for_values)
#       jw<- data.frame(actual_values)
#       hw<- data.frame(fitted_values)
#       
#       
#       vw$time<- as.character(vw$time)
#       jw$time<- as.character(jw$time)
#       hw$time<- as.character(hw$time)
#       bdListw<- list(vw,hw,jw)
#       dbw<- do.call(rbind, bdListw)
#       write.csv(dbw,file = paste("./RicePaper/", "World_",pots[[c]], "OutPutHW.csv", sep = ""))
#       dbw<- filter(dbw,des!="forecast") %>%  filter(.,time<=2016) %>% spread(des, val)
#       dbw$fitted[is.na(dbw$fitted)]<- dbw$actual
#       
#       foreingShock<- sum((dbw$fitted-(dbw$actual))^2)/(length(dbw$fitted)-1)
#       foreingShock<- (foreingShock)^0.5
#       
#       #smooth other techniques
#       test_aw<- ts(cfilesWorld[2], start =c(1961,1),frequency = 1)
#       plot.ts(test_aw)
#       
#       ## test seasonal
#       fit<- tbats(test_aw)
#       seasonal<- !is.null(fit$seasonal.periods)
#       
#       # smooth using simple movil average
#       ## order 3
#       test_w_sma<- SMA(test_aw,n = 3) # smooth using simple movil average
#       
#       ## graph smooth using exponential movil average      
#       test_w_ema<- EMA(test_aw, n = 3) # smooth using exponential movil average
#       plot.ts(test_w_ema)
#       dev.off()
#       
#       # dat frame     
#       dataW_sma<- data.frame(time=time(test_w_sma), val=as.matrix(test_w_sma))
#       dataW_sma$des<- "SMA"
#       dataW_ema<- data.frame(time=time(test_w_ema), val=as.matrix(test_w_ema))
#       dataW_ema$des<- "EMA"
#       dataW_sma$time<- as.character(dataW_sma$time)
#       dataW_ema$time<- as.character(dataW_ema$time)
#       bdListW_smooth<- list(dataW_sma,dataW_ema,j)
#       dbW_smooth<- do.call(rbind, bdListW_smooth)
#       write.csv(dbW_smooth,file = paste("./RicePaper/", "World_",pots[[c]], "_OutPutSmooth.csv", sep = ""))
#       dbW_smooth<- dbW_smooth %>% spread(des, val)
#       dbW_smooth$EMA[is.na(dbW_smooth$EMA)]<- dbW_smooth$actual
#       dbW_smooth$SMA[is.na(dbW_smooth$SMA)]<- dbW_smooth$actual
#       
#       #ema
#       foreignShock_smooth_ema<- sum((dbW_smooth$EMA -(dbW_smooth$actual))^2)/(length(dbW_smooth$EMA)-1)
#       foreignShock_smooth_ema<- (foreignShock_smooth_ema)^0.5
#       
#       #sma
#       foreignShock_smooth_sma<- sum((dbW_smooth$SMA -(dbW_smooth$actual))^2)/(length(dbW_smooth$SMA)-1)
#       foreignShock_smooth_sma<- (foreignShock_smooth_sma)^0.5
#       
#       #deviations
#       briefW<- data.frame(country= pots[[c]], foreignHV= foreingShock, foreignsma=foreignShock_smooth_sma, foreignema=foreignShock_smooth_ema)
#       write.csv(briefW,file = paste("./RicePaper/", pots[[c]], "World_briefDesy.csv", sep = ""))
#       
#       
#       p<- merge(brief,briefW,all=T)
#       p$zone<- "World"
#       write.csv(p,file = paste("./RicePaper/", pots[[c]], "World_briefDesy.csv", sep = ""))
#       
#       return(c)
#       
#       
# })
# 
# ### load data 
# shockW<- list.files(path="./RicePaper/", pattern = "World_briefDesy", all.files = T,full.names = T)
# shockW<- lapply(shockW, read.csv ,header=T)
# shockW<- do.call(rbind, shockW)
# shockW$X<- NULL
# 
# 
# ### cross tabulation 
# z<- sd(shockW$localHV)* sqrt(length(shockW$localHV)-1)/length(shockW$localHV)
# 
# 
# # 
# # setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID")
# # 
# # #Dirreción graficos
# # grd<-"C:/Users/CEGONZALEZ/Documents/BIDCarlos/"
# # 
# # # Regions
# # r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA",  "WLD")
# # 
# # 
# # #Cargar marco de datos principal
# # # md<-read.csv("Resultados_Ciat_StCty_31_08_16_new.csv",header=T)
# # md<-read.csv("Phase2/V2_allRegions.csv",header=T)
# # cfiles<- md
# # cfiles$commodity<- revalue(cfiles$commodity, c("crice"="Rice",
# #                                                "jrice"="Rice"))
# # cfiles$region<- as.character(cfiles$region)
# # cfiles$impactparameter<- as.character(cfiles$impactparameter)
# # variables<- c("TYldXAgg -- Total Yield", "QESHXAgg -- Export Share of Production")
# # cfiles<- filter(cfiles, impactparameter %in% variables) %>% filter(., commodity=="Rice") %>% filter(., region %in% r) 
# # cfiles$impactparameter<-  revalue(cfiles$impactparameter,c("TYldXAgg -- Total Yield"="Yield","QESHXAgg -- Export Share of Production"="ProExports"))
# # 
# # # tfiles<-  cfiles %>% split(cfiles$region)
# # 
# # # 
# # # 
# # # library(forecast)
# # # countries<- unique(rice$Area)
# # # c=1
# # # lapply(1:length(countries), function(c){
# # #       cfiles<- filter(rice, Area==countries[[c]])
# # #       cfiles$Value<- (cfiles$Value)*0.1
# # #       plot(cfiles$Value,type = "l", xlab=cfiles$Area, ylab="Rendimiento")
# # #       mean(cfiles$Value)
# # #       sd(cfiles$Value)
# # #       
# # #       # hallar la curtosis
# # #       library(moments)
# # #       kurtosis(cfiles$Value) # >0 leptocurtiva, <0 platicurtica 
# # #       # prueba de normalidad
# # #       shapiro.test(cfiles$Value)$p.value # recordando que la Ho: normalidad
# # #       
# # #       # prueba dickey-fuller (raiz unitaria, es o no estacionaria)
# # #       library(tseries)
# # #       adf.test(cfiles$Value) # ho: stacionaria 
# # #       acf(cfiles$Value) # correlograma, correlacion simple
# # #       pacf(cfiles$Value) # correlacion parcial 
# # #       
# # #       # creando diferencias, serie diferenciada
# # #       dcfiles<- diff(cfiles$Value)
# # #       plot(dcfiles, type = "l") # la diferenciacion es util para estacionalizar la serie
# # #       # aplicamos test de raiz unitaria
# # #       adf.test(dcfiles)
# # #       
# # #       # arima
# # #       auto.arima(cfiles$Value) # diferenciacion y media movil
# # #       acfiles<- arima(cfiles$Value, order = c(0,1,0)) # order autoregresivo, diferenciacion, media movil
# # #       plot(acfiles$residuals, type="l") # un modelo donde el grafico es un poco mas estacionario sin mostrar una tendencia a la baja
# # #       
# # #       # Evaluar los residuales del modelo arima para conocer la estacionariedad
# # #       library(stats)
# # #       Box.test(acfiles$residuals, type ="Ljung-Box") # no se puede rechazar la Ho de estacionariedad
# # #       forecast(acfiles, 20)
# # #       old.par<- par(mfcol=c(2,1))
# # #       plot(forecast(acfiles, 20))
# # #       plot(cfiles$Value, type="l",ylab="Rendimientos",main= paste(countries[[c]]), ylim =  , add=TRUE)
# # #       par(old.par) 
# # #       
# # #       f<- as.data.frame(forecast(acfiles,35))
# # #       f$year<- seq(from=2016, to=2050, by=1)
# # #       
# # #       # test
# # #       test_i<-ts(cfiles[3], start = c(1961,1), end=c(2016,1),frequency = 1)
# # #       plot(test_i, type="l")
# # #       adf.test(test_i)
# # #       test1<- diff(test_i)
# # #       adf.test(test1)
# # #       plot(test1, type="l")
# # #       
# # #       
# # #       # arima
# # #       at<- arima(test_i)
# # #       acf(test_i) # correlograma
# # #       pacf(test_i)
# # #       
# # #       # eleccion de los MA(q), AR(p), ARIMA(p,d,q)
# # #       auto.arima(test_i) # provee el mejor 
# # #       proof<- arima(test_i, order = c(0,1,0)) # order autoregresivo, diferenciacion, media movil
# # #       
# # #       
# # #       Box.test(proof$residuals, type ="Ljung-Box") # no se puede rechazar la Ho de estacionariedad
# # #       forecast(proof, 4)
# # #       old.par<- par(mfcol=c(2,1))
# # #       plot(forecast(proof, 4))
# # #       plot(test_i, type="l",ylab="Rendimientos",main= paste(countries[[c]]), add=TRUE)
# # #       par(old.par) 
# # #       #par(new=T)
# # # })
# # 
# # # 
# # # 
# # # # creación de la función
# # # HWplot<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
# # #       
# # #       hw_object<- HoltWinters(ts_objet,beta = T, gamma = F)
# # #       forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
# # #       for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
# # #       fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
# # #       actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
# # #       
# # #       
# # #       # save(for_values, fitted_values, actual_values,file = paste(workdi,pots[[c]], ".RData", sep = ""))
# # #       
# # #       
# # #       graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
# # #       graphset<- merge(graphset, for_values, all=TRUE, by='time')
# # #       graphset[is.na(graphset$dev), ]$dev<-0
# # #       graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
# # #       
# # #       graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
# # #       p<- ggplot(graphset.met, aes(x=time, y=value))+
# # #             geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
# # #             geom_line(aes(colour=variable), size=line.size)+
# # #             geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
# # #       
# # #       return(p)
# # #       
# # # }

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

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")

############ Yields 
usda<- read.csv(file = paste("./psd_alldata_csv (1)/psd_alldata.csv",sep = "" ))
usda_rice<- filter(usda, Commodity_Description=="Rice, Milled")
usda_rice$Country_Name<- as.character(usda_rice$Country_Name)
usda_rice$Commodity_Description<- as.character(usda_rice$Commodity_Description)
usda_rice$Attribute_Description<- as.character(usda_rice$Attribute_Description)

deleTvariables<- c("Unit_ID" , "Month","Country_Code",  "Commodity_Code",  "Attribute_ID",  "Calendar_Year", "Commodity_Description")
usda_rice<- usda_rice[,!names(usda_rice) %in% deleTvariables]
colnames(usda_rice)[1]<- "Area"; colnames(usda_rice)[2]<- "Year"; colnames(usda_rice)[3]<- "Item"; colnames(usda_rice)[4]<- "Unit"
colnames(usda_rice)[5]<- "Val"


usda_rice<- filter(usda_rice,Item=="Yield")
usda_rice$Unit<-NULL
usda_rice$Item<- NULL
usda_rice<- filter(usda_rice, Area!="EU-15")



lac_usda<- c( "Argentina", "Mexico", "Venezuela","Paraguay", "Peru", "Brazil","Chile", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago",
              "Barbados","Panama", "El Salvador", "Suriname", "Uruguay", "Honduras","Martinique", "Nicaragua", "Haiti","Colombia", "Costa Rica", 
              "Aruba", "Belize","Ecuador", "Guatemala","Chile", "Bolivia", "Guyana", "Dominican Republic", "Bahamas")

###################
##    LAC      ####
###################
lacRice<- usda_rice %>% filter( Area %in% lac_usda)
pots<-unique(lacRice$Area)

###################
##    World    ####
###################
# WorldRice<- rice %>% filter(!Area %in% potsOut)  
pots<-unique(usda_rice$Area)

# c=1
####LAC----------------
lapply(1:length(pots), function(c){
      
      #################################################Country###############################################################
      workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/usda"
      cfiles<- filter(usda_rice, Area==pots[[c]]) #%>% filter(., Element=="Yield")
      
      test_i<- ts(cfiles[3], start =c(1961,1),frequency = 1 )
      
      #smooth using Holt Winter
      hw<-HoltWinters(x = test_i,beta = T, gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
      
      
      HWplot<- function(ts_objet, n.ahead=34, CI=0.95, error.ribbon="green", line.size=1){
            
            hw_object<- HoltWinters(ts_objet,beta = T, gamma = F)
            forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
            for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
            fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
            actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
            
            save(for_values, fitted_values, actual_values,file = paste(workdi,"/",pots[[c]], ".RData", sep = ""))
            
            
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
      
      
      HWplot(test_i) # usando la funcion 

      # cargar datos 
      load(paste("./RicePaper/usda/", pots[[c]], ".RData", sep = ""))
      
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
      write.csv(db,file = paste("./RicePaper/usda/", pots[[c]], "OutPutHW.csv", sep = ""))
      db<- filter(db,des!="forecast") %>%  filter(.,time<=2018) %>% spread(des, val)
      db$fitted[is.na(db$fitted)]<- db$actual
      
      localShock<- db$actual-db$fitted
      
      
      #smooth other techniques
      test_a<- ts(cfiles[3], start =c(1961,1),frequency = 1)
#       plot.ts(test_a)
      
      ## test seasonal
      fit<- tbats(test_a)
      seasonal<- !is.null(fit$seasonal.periods)
      cat(paste("The country ", pots[[c]], " has/hasnot seasonality ==", seasonal, ", done!!\n", sep = "")) 
      
      # decomposing non-seasonal data
      ## order 3
      test_a_sma<- SMA(test_a,n = 3) # smooth using simple movil average

      
      ## graph smooth using exponential movil average      
      test_a_ema<- EMA(test_a, n = ) # smooth using exponential movil average

      # dat frame     
      data_sma<- data.frame(time=time(test_a_sma), val=as.matrix(test_a_sma))
      data_sma$des<- "SMA"
      data_ema<- data.frame(time=time(test_a_ema), val=as.matrix(test_a_ema))
      data_ema$des<- "EMA"
      data_sma$time<- as.character(data_sma$time)
      data_ema$time<- as.character(data_ema$time)
      bdList_smooth<- list(data_sma,data_ema,j)
      db_smooth<- do.call(rbind, bdList_smooth)
      write.csv(db_smooth,file = paste("./RicePaper/usda/", pots[[c]], "OutPutSmooth.csv", sep = ""))
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
      brief<- data.frame(time= db_smooth$time,country= pots[[c]],localHV= localShock ,localsma=localShock_smooth_sma,localsma=localShock_smooth_sma, localema=localShock_smooth_ema)
      
      #average production
      cfilesPro1<- filter(usda, Attribute_Description=="Production") %>% 
            filter(., Commodity_Description=="Rice, Milled") %>% group_by(Market_Year)%>% summarise(totalQ=sum(Value))
      totalQ<- cfilesPro1$totalQ
      
      cfilesPro2<- filter(usda, Country_Name==pots[[c]])%>% filter(., Commodity_Description=="Rice, Milled") %>% 
            filter(., Attribute_Description=="Production") %>% select(Country_Name, Market_Year, Value)
      colnames(cfilesPro2)[1]<- "Area"
      colnames(cfilesPro2)[2]<- "Year"
      
      temQ<- cfilesPro2
      temQ<- temQ[c("Year", "Area", "Value")]
      temQ$Area<- as.character(temQ$Area)
      colnames(temQ)[1]<- "time"; temQ$time<- as.character(temQ$time); temQ$time<- as.numeric(temQ$time)
      colnames(cfilesPro1)[1]<- "time"
      
      # brief$totalQ<- cfilesPro1$totalQ
      brief$country<- as.character(brief$country)
      brief$time<- as.character(brief$time); brief$time<- as.numeric(brief$time)
      brief<- left_join(brief,temQ, by="time")
      brief<- brief[c("time","country","localHV", "localShock_smooth_sma","localShock_smooth_ema", "Value")]
      colnames(brief)[6]<- "Qave"
      
      cfilesPro1$time<- as.character(cfilesPro1$time); cfilesPro1$time<- as.numeric(cfilesPro1$time)
      brief<- left_join(brief,cfilesPro1, by="time")
      
      brief$localHV_W<- (brief$Qave/brief$totalQ)* brief$localHV
      brief$localsma_W<- (brief$Qave/brief$totalQ)* brief$localShock_smooth_sma
      brief$localema_W<- (brief$Qave/brief$totalQ)* brief$localShock_smooth_ema  
      
      
      write.csv(brief,file = paste("./RicePaper/usda/", pots[[c]], "briefDesy.csv", sep = ""))
      
      
      return(c)
      
      
}) 

### load data 
shockW<- list.files(path="./RicePaper/usda/", pattern = "briefDesy", all.files = T,full.names = T)
shockW<- lapply(shockW, read.csv ,header=T)
shockW<- do.call(rbind, shockW)
shockW$X<- NULL 
yr<-unique(shockW$time)

# c=1;y=20
lapply(1:length(pots), function(c){
      
      workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/usda/"
      foreign<- filter(shockW, country!=pots[[c]]) %>% group_by(time) %>% mutate(sumlocalHV_W=sum(localHV_W, na.rm=T),
                                                                                 sumlocalsma_W=sum(localsma_W, na.rm=T),
                                                                                 sumlocalema_W=sum(localema_W, na.rm=T)) 
      
      local<- filter(shockW, country==pots[[c]])
      yytemp<- list()
      for(y in 1:length(yr)){
            yy<- filter(foreign, time==yr[y])
            yy1<- unique(yy$sumlocalHV_W)
            yy2<- unique(yy$sumlocalsma_W)
            yy3<- unique(yy$sumlocalema_W)
            yytemp[[y]]<- data.frame(time=yr[y],country=pots[[c]],f_hvW=yy1,f_smaW=yy2,f_emaW= yy3)
      }
      xxtemp<- do.call(rbind, yytemp)
      
      local<- left_join(local,xxtemp, by = c("time", "country"))
      write.csv(local,file = paste("./RicePaper/usda/", pots[[c]], "_shocksLocal&Foreigh.csv", sep = ""))
      
      
      return(c)
      
})

### load data 
shockComplete<- list.files(path="./RicePaper/usda/", pattern = "shocksLocal&Foreigh", all.files = T,full.names = T)
shockComplete<- lapply(shockComplete, read.csv ,header=T)
shockComplete<- do.call(rbind, shockComplete)
shockComplete$X<- NULL 



#### getting zscore 
## local 

var<- c("localHV","localShock_smooth_sma","localShock_smooth_ema","f_hvW", "f_smaW","f_emaW")


# v=1; x=3
z_dataL<- list()
z_yL<- list()
for(v in 1:length(var)){
      
      tempZ<- shockComplete %>% select(., "time","country",var[[v]] )
      tempZ$time<- as.character(tempZ$time); tempZ$time<- as.numeric(tempZ$time)
      tempZ$country<- as.character(tempZ$country)
      
      for(x in 1:length(yr)){
            tt<- filter(tempZ, time==yr[[x]])
            sd_var<- sd(tt[,3])
            length_var<- length(tt[,3])
            a<- sd_var
            b<- mean(tt[,3])
            z<- (tt[,3]-b)/a
            z_dataL[[x]]<- data.frame(year=yr[[x]],country=unique(tt$country),zscore=z, Z_shock=var[[v]])
            z_temp<- do.call(rbind,z_dataL)
            
      }
      z_yL[[v]]<- z_temp
      
      cat(paste("Shock", var[[v]], "it was done!!!\n"))
      
}


gdata<- do.call(rbind,z_yL)
gdata<- as.data.frame(gdata)
gdata<- gdata %>% spread(Z_shock, zscore)
write.csv(gdata,file = paste("./RicePaper/usda/", "NumericCategoriesUSDA.csv", sep = ""))

temp_data<- list()
# v=1
for ( v in 1:length(var)){
      
      ## logica de los valores 
      hn<-  which(gdata[,var[v]]<=-1)
      nn<-  which(gdata[,var[v]]<0 & gdata[,var[v]]>-1)
      hp<-  which(gdata[,var[v]]>1)    
      mr<-  which(gdata[,var[v]]>0 & gdata[,var[v]]<1)
      
      # desempe�o 
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
      tanz<-tanz[,c("year","country","trend")]
      # tanz$shock<- var[v]
      colnames(tanz)[3]<- paste("cat_",var[v],sep = "")
      
      temp_data[[v]]<- tanz
      
      
      cat(paste("Shock", var[v], "it was done!!!\n"))
      
}


### processing 
sdata<- do.call(cbind, temp_data)
sdata<- sdata[,-c(4,5,7,8,10,11,13,14,16,17)]
cols<- c("cat_localHV","cat_localShock_smooth_sma","cat_localShock_smooth_ema","cat_f_hvW","cat_f_smaW","cat_f_emaW")
sdata<- sdata %>% mutate_each(funs(factor(.)), cols)
sdata<- na.omit(sdata)
rownames(sdata)<- 1:nrow(sdata)

sdata<- sdata %>%  mutate(., zone=ifelse((country %in% lac_usda),"LAC", "WORLD"))

### holt and winter
t1<- table(sdata$cat_localHV,sdata$cat_f_hvW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

hv<- prop.table(t1)
write.csv(hv,file = paste("./RicePaper/usda/","Years_hv_shocksLocal&ForeighUSDA.csv", sep = ""))

#### sma 
t1<-table(sdata$cat_localShock_smooth_sma,sdata$cat_f_smaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

sma_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/usda/","Years_sma_shocksLocal&ForeighUSDA.csv", sep = ""))

### ema
t1<-table(sdata$cat_localShock_smooth_ema,sdata$cat_f_emaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

ema_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/usda/","Years_ema_shocksLocal&ForeighUSDA.csv", sep = ""))



############ analysis just for LAC countries ######################
glac<- filter(sdata, country %in% lac_usda)

### holt and winter
t1<-table(glac$cat_localHV,glac$cat_f_hvW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

hv<- prop.table(t1)
write.csv(hv,file = paste("./RicePaper/usda/","Years_lac_hv_shocksLocal&ForeighUSDA.csv", sep = ""))

#### sma 
t1<-table(glac$cat_localShock_smooth_sma,glac$cat_f_smaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

sma_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/usda/","Years_lac_sma_shocksLocal&ForeighUSDA.csv", sep = ""))

### ema
t1<-table(glac$cat_localShock_smooth_ema,glac$cat_f_emaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

ema_t<- prop.table(t1)
write.csv(sma_t,file = paste("./RicePaper/usda/","Years_lac_ema_shocksLocal&ForeighUSDA.csv", sep = ""))


###################################### 

write.csv(sdata,file = paste("./RicePaper/usda/","Categories_shocksLocal&ForeighUSDA.csv", sep = ""))


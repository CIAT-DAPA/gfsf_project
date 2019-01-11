# Consumption and time series, rice Analisis destinado paper de arroz y otros usos. 
# carlos Eduardo

### clean files previous running
g=gc;rm(list = ls())
#libraries
# R options
options(warn = -1)
options(scipen = 999)

##### Load packages------------
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
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
suppressMessages(library(plm))
suppressMessages(library(texreg))
suppressMessages(library(foreign))
suppressMessages(library(car))
suppressMessages(library(gplots))
suppressMessages(library(tseries))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(lme4))
suppressMessages(library(RColorBrewer))

##### Principal directory-------
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")

##### Object of regions as vectors--------------
lac<- c( "Argentina", "Mexico", "Venezuela (Bolivarian Republic of)","Paraguay", "Peru", "Brazil","Chile", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago",
         "Barbados","Panama", "El Salvador", "Suriname", "Uruguay", "Honduras","Martinique", "Nicaragua", "Haiti","Colombia", "Costa Rica", 
         "Aruba", "Belize","Ecuador", "Guatemala","Chile", "Bolivia (Plurinational State of)", "Guyana", "Dominican Republic", "Bahamas")

potsOut<- c("Least Developed Countries" ,"Land Locked Developing Countries","Small Island Developing States",
            "Low Income Food Deficit Countries","Net Food Importing Developing Countries","European Union",
            "World","Africa","Eastern Africa","Middle Africa" ,"Northern Africa","Southern Africa","Western Africa",
            "Central America", "Caribbean","South America" ,"Asia", "Central Asia", "Eastern Asia" ,"Southern Asia",
            "Europe","Eastern Europe","Southern Europe" ,"Western Europe" , "Oceania","Australia & New Zealand","Melanesia",
            "Micronesia", "Americas","Northern America","South-Eastern Asia","Western Asia","China, Macao SAR",
            "EU(12)ex.int","EU(15)ex.int","EU(25)ex.int", "EU(27)ex.int",
            "European Union (exc intra-trade)","Northern Europe","Polynesia", "China")

lac_usda<- c( "Argentina", "Mexico", "Venezuela","Paraguay", "Peru", "Brazil","Chile", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago",
              "Barbados","Panama", "El Salvador", "Suriname", "Uruguay", "Honduras","Martinique", "Nicaragua", "Haiti","Colombia", "Costa Rica", 
              "Aruba", "Belize","Ecuador", "Guatemala","Chile", "Bolivia", "Guyana", "Dominican Republic", "Bahamas")

##### Shocks FAO& USDA. load data---------
#FAOcategoric
ShocksFAO_cat<- read.csv(file = paste("./RicePaper/fao/temporal/","Categories_shocksLocal&ForeighFAO.csv", sep = ""),header = T)
ShocksFAO_cat$X<- NULL
colnames(ShocksFAO_cat)[1]<- "Year"; colnames(ShocksFAO_cat)[2]<- "Area"

#USDAcategoric
ShocksUSDA_cat<- read.csv(file = paste("./RicePaper/usda/temporal/","Categories_shocksLocal&ForeighUSDA.csv", sep = ""))
ShocksUSDA_cat$X<- NULL
colnames(ShocksUSDA_cat)[1]<- "Year"; colnames(ShocksUSDA_cat)[2]<- "Area"

#networks
netw<- read.csv( "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/pic/dataCompleta.csv")
netw$X<- NULL

# adjusting 
netw$country<- plyr::revalue(netw$country, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                                 "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                 "Côte d'Ivoire"="Ivory Coast",
                                                 "China, Hong Kong SAR"="Hong Kong",
                                                 "China, Taiwan Province of"= "Taiwan",
                                                 "Venezuela (Bolivarian Republic of)"="Venezuela",
                                                "Bolivia (Plurinational State of)"="Bolivia" )) 
colnames(netw)[1]<- "Year"
colnames(netw)[2]<- "Area"


################################################## FAO #########################################################
########## consumption-------  
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")
faoStat<- read.csv(file = "./FoodBalanceSheets_E_All_Data_(Normalized)/FoodBalanceSheets_E_All_Data_(Normalized).csv", header = T)
str(faoStat)
faoStat$Item<- as.character(faoStat$Item); faoStat$Area<- as.character(faoStat$Area)
faoStat$Element<- as.character(faoStat$Element)
faoStat$Unit<- as.character(faoStat$Unit)
faoStat$Year<- as.character(faoStat$Year); faoStat$Year<- as.numeric(faoStat$Year)
faoStat$Area.Code<- NULL; faoStat$Item.Code<- NULL; faoStat$Element.Code<- NULL; faoStat$Year.Code<- NULL; faoStat$Flag<- NULL
faoStat$Unit<- NULL
d_faostat<- faoStat # subset
d_faostat<- filter(d_faostat, !Area %in% potsOut)

# adjusting 
d_faostat$Area<- plyr::revalue(d_faostat$Area, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                                 "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                 "Côte d'Ivoire"="Ivory Coast",
                                                 "China, Hong Kong SAR"="Hong Kong",
                                                 "China, Taiwan Province of"= "Taiwan")) 

# "China, Hong Kong SAR"='Hong Kong',
# "China, Taiwan Province of"='Taiwan',
# "China, mainland"="China

########## Imports-----------
faoStatTrade<- read.csv(file = "./Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv", header = T)
str(faoStatTrade)
faoStatTrade$Item<- as.character(faoStatTrade$Item); faoStat$Area<- as.character(faoStat$Area)
faoStatTrade$Element<- as.character(faoStatTrade$Element)
faoStatTrade$Unit<- as.character(faoStatTrade$Unit)
faoStatTrade$Year<- as.character(faoStatTrade$Year); faoStat$Year<- as.numeric(faoStat$Year)
faoStatTrade$Area.Code<- NULL; faoStatTrade$Item.Code<- NULL; faoStatTrade$Element.Code<- NULL; faoStatTrade$Year.Code<- NULL; faoStatTrade$Flag<- NULL
faoStatTrade$Unit<- NULL
d_faoStatTrade<- faoStatTrade # subset
d_faoStatTrade<- filter(d_faoStatTrade, !Area %in% potsOut)

d_faoStatTrade$Area<- plyr::revalue(d_faoStatTrade$Area, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                                           "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                           "Côte d'Ivoire"="Ivory Coast",
                                                           "China, Hong Kong SAR"="Hong Kong",
                                                           "China, Taiwan Province of"= "Taiwan")) 

########## domestic supply-------------
v<-c("Domestic supply quantity", "Import Quantity", "Stock Variation", "Export Quantity", "Production")
rice_all<- filter(d_faostat, Element %in% v) %>% filter(., Item=="Rice (Milled Equivalent)")
rice_all<- rice_all[!duplicated(rice_all),]
rice_all<- rice_all %>% spread(Element, Value)                


rice_exports<- filter(d_faoStatTrade,Element=="Export Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)") 
rice_exports$Year<- as.numeric(rice_exports$Year)
rice_exports$Area<- as.character(rice_exports$Area)
rice_exports<- rice_exports[!duplicated(rice_exports),]

########## Exports--------
rice_exports<- filter(d_faoStatTrade,Element=="Export Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)") 
rice_exports$Year<- as.numeric(rice_exports$Year)
rice_exports$Area<- as.character(rice_exports$Area)
rice_exports<- rice_exports[!duplicated(rice_exports),]

########## Stock----------
rice_stock<- filter(d_faostat, Element=="Stock Variation") %>% filter(., Item=="Rice (Milled Equivalent)")
rice_stock$Year<- as.numeric(rice_stock$Year)
rice_stock<- rice_stock[!duplicated(rice_stock),]

########## Demand Food----------
rice_demand<- filter(d_faostat,Element=="Food") %>% filter(. , Item=="Rice (Milled Equivalent)") 
rice_demand<- rice_demand[!duplicated(rice_demand),]
rice_demand<- rice_demand[!duplicated(rice_demand),]

########## Imports------------
rice_import<- filter(d_faoStatTrade,Element=="Import Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)")
rice_import$Year<- as.numeric(rice_import$Year)
rice_import$Area<- as.character(rice_import$Area)
rice_import<- rice_import[!duplicated(rice_import),]

########## Production-----------
rice_production<- filter(d_faostat,Element=="Production") %>% filter(. , Item=="Rice (Milled Equivalent)") 
rice_production$Year<- as.numeric(rice_production$Year)
rice_production$Area<- as.character(rice_production$Area)
rice_production<- rice_production[!duplicated(rice_production),]
rice_production<- rice_production %>% filter(.,Value!=0)

# Names changes------- 
colnames(rice_demand)[5]<-"Consumption";colnames(rice_import)[5]<- "Imports"; colnames(rice_exports)[5]<- "Exports"
colnames(rice_stock)[5]<-"Stock"; colnames(rice_production)[5]<- "Production"
mix<- left_join(rice_import,rice_demand,by = c("Area", "Year"))
mix<- mix[,c("Area", "Year", "Imports","Consumption")]

mix<- left_join(mix, rice_exports, by = c("Area", "Year"))
mix<- mix[,c("Area", "Year", "Imports","Consumption","Exports" )]

mix<- left_join(mix, rice_stock, by = c("Area","Year"))
mix<- mix[,c("Area","Year","Imports","Consumption", "Exports","Stock")]

mix<- left_join(mix, rice_production, by = c("Area","Year"))
mix<- mix[,c("Area","Year","Imports","Consumption", "Exports","Stock", "Production")]

# Join using shocks------------- 
mix<- dplyr::left_join(mix, ShocksFAO_cat, by = c("Area","Year"))
mix$Stock[is.na(mix$Stock)]<-0
mix$Exports[is.na(mix$Exports)]<-0
mix$Consumption[is.na(mix$Consumption)]<-0

# Eliminate missing data categories----- 
mix<- na.omit(mix)
row.names(mix)<- 1:nrow(mix)

# Adjusting measures units -----------
mix$Consumption<- (mix$Consumption)*1000 # before 1000 tonnes
mix$Stock<- (mix$Stock)*1000 # before 1000 tonnes
mix$Production<- (mix$Production)*1000 # before 1000 tonnes


# adjusting values stock negative values were addition, conversely, don't show decrease
for(i in 1:nrow(mix)){
      if(mix$Stock[i]<0){
            mix$Stock[i]<-  mix$Stock[i]*(-1)
      }else{}
}


# creating LAC variable 
cfiles<- mix %>%  mutate(., zone=ifelse((Area %in% lac),"LAC", "WORLD"))
fao_files<- cfiles
# creating  stocks domestic and foreign variables
fao_files$ds<- (fao_files$Stock/fao_files$Consumption) # stock as rate of consumption
fao_files$Imp_Con<- (fao_files$Imports/fao_files$Consumption)# consumption as rate of Imports
fao_files<- filter(fao_files, Imp_Con!=0)
fao_files<- fao_files %>% group_by(Area) %>% mutate(.,dtest= c(NA,diff(log(Imports/Consumption))))
fao_files<- fao_files %>% group_by(Area) %>% mutate(.,dtest2= c(NA,diff(log(Imports)/log(Consumption))))
fao_files$dtest[is.infinite(fao_files$dtest)]<-0
fao_files<- na.omit(fao_files)
fao_files$ds[is.infinite(fao_files$ds)]<-0
fao_files$dtest2[is.infinite(fao_files$dtest2)]<-0
fao_files<- filter(fao_files, Imports!=0)
summary(fao_files$dtest2)

# define a function to remove outliers
FindOutliers <- function(data) {
      lowerq = quantile(data)[2]
      upperq = quantile(data)[4]
      iqr = upperq - lowerq 
      #Or use IQR(data)
      # we identify extreme outliers
      extreme.threshold.upper = (iqr * 3) + upperq
      extreme.threshold.lower = lowerq - (iqr * 3)
      result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

# use the function to identify outliers
temp <- FindOutliers(fao_files$Imp_Con)
cfOut<- fao_files[temp,]
maxVal<- quantile(fao_files$Imp_Con,probs = 0.95)
# cfOut$Con_ImProof<- quantile(fao_files$Con_ImProof,probs = 0.95)
fao_files[temp,]$Imp_Con<-quantile(fao_files$Imp_Con,probs = 0.95)


### calculating diff of imports
fao_files<- fao_files %>% group_by(Area) %>% mutate(.,dffm_c = c(NA,diff(log(Imports/Consumption)))) 
fao_files<- na.omit(fao_files)
fao_files<- filter(fao_files, dffm_c!=0)

#number periods#Avaible data#eliminate countries with miss in years 
n.yrs<-length(unique(fao_files$Year))
ava.data<- fao_files %>% add_count(Area,Year)
rownames(ava.data)<- 1:nrow(ava.data)
sum.yrs.zone<- table(ava.data$Area, ava.data$n)
out.countries<- as.data.frame(sum.yrs.zone)
out.countries<- filter(out.countries,Freq<20) # con ausencia de mas de 35 años de datos
out.countries$Var1<- as.character(out.countries$Var1)
chao.countries<- out.countries[,1]

fao_files<- filter(fao_files, !Area %in% chao.countries)
# fao_files<- filter(fao_files, Year>=1980)

# calculating stock world by country
pots<- unique(fao_files$Area)

cf<- list()
# c=1
for(c in 1:length(pots)){
      
      
      cf[[c]]<- filter(fao_files, Area!=pots[[c]]) %>% group_by(Year) %>% select(Year,Stock, Consumption) %>%
            summarise(fs= (sum(Stock, na.rm=T)/sum(Consumption, na.rm=T))*100, Area=pots[c]) %>% 
            select(Year,Area,fs)
      
}

# Agrupando el stock y calculando variables de consumo
temp_cf<- do.call(rbind, cf)
fao_join<- left_join(fao_files,temp_cf, by=c("Year","Area"))

# Agrupando base de datos con stock y calculando variables rezagadas de consumo.  
fao_test<- fao_join
fao_test<- fao_test %>% group_by(Area) %>% mutate(.,cLag= lag(Consumption,k=1),Lconsumption= (Consumption/cLag))
fao_test<- na.omit(fao_test)

### Join completa 
# fao_test<- left_join(fao_test,ShocksUSDA_cat, by=c("Year","Area","zone"))

### Adjusting categories
fao_test$cat_localHV<- as.character(fao_test$cat_localHV)
fao_test$cat_localShock_smooth_sma<- as.character(fao_test$cat_localShock_smooth_sma)
fao_test$cat_f_hvW<- as.character(fao_test$cat_f_hvW)
fao_test$cat_localShock_smooth_ema<- as.character(fao_test$cat_localShock_smooth_ema)
fao_test$cat_f_smaW<- as.character(fao_test$cat_f_emaW)
fao_test$cat_f_emaW<- as.character(fao_test$cat_f_emaW)

#eliminate NAs
fao_test<- na.omit(fao_test)

# Ajusting smooth por domestic or extrangero
fao_test$hw.d<- paste("domestic", fao_test$cat_localHV)
fao_test$hw.f<- paste("foreign", fao_test$cat_f_hvW)
fao_test$sma.d<- paste("domestic", fao_test$cat_localShock_smooth_sma)
fao_test$sma.f<- paste("foreign", fao_test$cat_f_smaW)
fao_test$ema.d<- paste("domestic", fao_test$cat_localShock_smooth_ema)
fao_test$ema.f<- paste("foreign",fao_test$cat_f_emaW)

# Creating dummies HW---------------
ndnf<-  which(fao_test$hw.d=="domestic Negative" & fao_test$hw.f=="foreign Negative") #negative domestic and foreign / "Negative Negative" 
ndmrf<- which(fao_test$hw.d=="domestic Negative" & fao_test$hw.f=="foreign Middle_Range") #negative domestic and middle road / "Negative Middle_Range"
ndhpf<- which(fao_test$hw.d=="domestic Negative" & fao_test$hw.f=="foreign Positive") #negative domestic and high positive / "Negative Positive" 
mrdnd<- which(fao_test$hw.d=="domestic Middle_Range" & fao_test$hw.f=="foreign Negative") #middle foreign road and negative domestic  
mrdmrf<- which(fao_test$hw.d=="domestic Middle_Range" & fao_test$hw.f=="foreign Middle_Range") #middle road domestic road and middle foreign        
mrdhpf<- which(fao_test$hw.d=="domestic Middle_Range" & fao_test$hw.f=="foreign Positive") #middle road domestic and high positive foreign
hpdnf<- which(fao_test$hw.d=="domestic Positive" & fao_test$hw.f=="foreign Negative") #high positive domestic and negative foreign
hpdmrf<- which(fao_test$hw.d=="domestic Positive" & fao_test$hw.f=="foreign Middle_Range") #high positive domestic and middle roal foreign 
hpdhpf<- which(fao_test$hw.d=="domestic Positive" & fao_test$hw.f=="foreign Positive")#high positive domestic and high positive foreign 

# desempeño 
ndnf.d<-  c(ndnf);ndmrf.d<-  c(ndmrf);ndhpf.d<- c(ndhpf);mrdnd.d<- c(mrdnd);mrdmrf.d<- c(mrdmrf);mrdhpf.d<- c(mrdhpf)
hpdnf.d<- c(hpdnf);hpdmrf.d<- c(hpdmrf);hpdhpf.d<- c(hpdhpf) 

# copia
tanz<- fao_test
tanz$Dhw<- NA
tanz$Dhw[ndnf.d] <-  "domestic Negative & foreign Negative"
tanz$Dhw[ndmrf.d] <- "domestic Negative & foreign Middle_Range"
tanz$Dhw[ndhpf.d]<-  "domestic Negative & foreign Positive"
tanz$Dhw[mrdnd.d]<-  "domestic Middle_Range & foreign Negative"
tanz$Dhw[mrdmrf.d]<- "domestic Middle_Range & foreign Middle_Range"
tanz$Dhw[mrdhpf.d]<- "domestic Middle_Range & foreign Positive"
tanz$Dhw[hpdnf.d]<-  "domestic Positive & foreign Negative"
tanz$Dhw[hpdmrf.d]<- "domestic Positive & foreign Middle_Range"
tanz$Dhw[hpdhpf.d]<- "domestic Positive & foreign Positive"

# Creating dummies SMA------------
ndnf<-  which(fao_test$sma.d=="domestic Negative" & fao_test$sma.f=="foreign Negative") #negative domestic and foreign / "Negative Negative" 
ndmrf<- which(fao_test$sma.d=="domestic Negative" & fao_test$sma.f=="foreign Middle_Range") #negative domestic and middle road / "Negative Middle_Range"
ndhpf<- which(fao_test$sma.d=="domestic Negative" & fao_test$sma.f=="foreign Positive") #negative domestic and high positive / "Negative Positive" 
mrdnd<- which(fao_test$sma.d=="domestic Middle_Range" & fao_test$sma.f=="foreign Negative") #middle foreign road and negative domestic  
mrdmrf<- which(fao_test$sma.d=="domestic Middle_Range" & fao_test$sma.f=="foreign Middle_Range") #middle road domestic road and middle foreign        
mrdhpf<- which(fao_test$sma.d=="domestic Middle_Range" & fao_test$sma.f=="foreign Positive") #middle road domestic and high positive foreign
hpdnf<- which(fao_test$sma.d=="domestic Positive" & fao_test$sma.f=="foreign Negative") #high positive domestic and negative foreign
hpdmrf<- which(fao_test$sma.d=="domestic Positive" & fao_test$sma.f=="foreign Middle_Range") #high positive domestic and middle roal foreign 
hpdhpf<- which(fao_test$sma.d=="domestic Positive" & fao_test$sma.f=="foreign Positive")#high positive domestic and high positive foreign 

# desempeño 
ndnf.d<-  c(ndnf); ndmrf.d<-  c(ndmrf);ndhpf.d<- c(ndhpf);mrdnd.d<- c(mrdnd);
mrdmrf.d<- c(mrdmrf); mrdhpf.d<- c(mrdhpf); hpdnf.d<- c(hpdnf);hpdmrf.d<- c(hpdmrf)
hpdhpf.d<- c(hpdhpf)

# copia
tanz$Dsma<- NA
tanz$Dsma[ndnf.d] <- "domestic Negative & foreign Negative"
tanz$Dsma[ndmrf.d] <- "domestic Negative & foreign Middle_Range"
tanz$Dsma[ndhpf.d]<- "domestic Negative & foreign Positive"
tanz$Dsma[mrdnd.d]<- "domestic Middle_Range & foreign Negative"
tanz$Dsma[mrdmrf.d]<- "domestic Middle_Range & foreign Middle_Range"
tanz$Dsma[mrdhpf.d]<- "domestic Middle_Range & foreign Positive"
tanz$Dsma[hpdnf.d]<- "domestic Positive & foreign Negative"
tanz$Dsma[hpdmrf.d]<- "domestic Positive & foreign Middle_Range"
tanz$Dsma[hpdhpf.d]<- "domestic Positive & foreign Positive"

# creating dummies EMA------------
ndnf<-  which(fao_test$ema.d=="domestic Negative" & fao_test$ema.f=="foreign Negative") #negative domestic and foreign / "Negative Negative" 
ndmrf<- which(fao_test$ema.d=="domestic Negative" & fao_test$ema.f=="foreign Middle_Range") #negative domestic and middle road / "Negative Middle_Range"
ndhpf<- which(fao_test$ema.d=="domestic Negative" & fao_test$ema.f=="foreign Positive") #negative domestic and high positive / "Negative Positive" 
mrdnd<- which(fao_test$ema.d=="domestic Middle_Range" & fao_test$ema.f=="foreign Negative") #middle foreign road and negative domestic  
mrdmrf<- which(fao_test$ema.d=="domestic Middle_Range" & fao_test$ema.f=="foreign Middle_Range") #middle road domestic road and middle foreign        
mrdhpf<- which(fao_test$ema.d=="domestic Middle_Range" & fao_test$ema.f=="foreign Positive") #middle road domestic and high positive foreign
hpdnf<- which(fao_test$ema.d=="domestic Positive" & fao_test$ema.f=="foreign Negative") #high positive domestic and negative foreign
hpdmrf<- which(fao_test$ema.d=="domestic Positive" & fao_test$ema.f=="foreign Middle_Range") #high positive domestic and middle roal foreign 
hpdhpf<- which(fao_test$ema.d=="domestic Positive" & fao_test$ema.f=="foreign Positive")#high positive domestic and high positive foreign 

# desempeño 
ndnf.d<-  c(ndnf);ndmrf.d<-  c(ndmrf);ndhpf.d<- c(ndhpf);mrdnd.d<- c(mrdnd);mrdmrf.d<- c(mrdmrf)
mrdhpf.d<- c(mrdhpf);hpdnf.d<- c(hpdnf);hpdmrf.d<- c(hpdmrf);hpdhpf.d<- c(hpdhpf)

# copia
tanz$Dema<- NA
tanz$Dema[ndnf.d] <- "domestic Negative & foreign Negative"
tanz$Dema[ndmrf.d] <- "domestic Negative & foreign Middle_Range"
tanz$Dema[ndhpf.d]<- "domestic Negative & foreign Positive"
tanz$Dema[mrdnd.d]<- "domestic Middle_Range & foreign Negative"
tanz$Dema[mrdmrf.d]<- "domestic Middle_Range & foreign Middle_Range"
tanz$Dema[mrdhpf.d]<- "domestic Middle_Range & foreign Positive"
tanz$Dema[hpdnf.d]<- "domestic Positive & foreign Negative"
tanz$Dema[hpdmrf.d]<- "domestic Positive & foreign Middle_Range"
tanz$Dema[hpdhpf.d]<- "domestic Positive & foreign Positive"



############################# modelos 
fimports<- as.data.frame(tanz)
fimports$Area<- plyr::revalue(fimports$Area, c("Venezuela (Bolivarian Republic of)"="Venezuela",
                                               "Bolivia (Plurinational State of)"="Bolivia"))

### join data networks
fimports<- left_join(fimports, netw, by = c("Area", "Year"))
fimportsLAC<- fimports %>% filter(zone=="LAC")
# copia de la base de datos
# organizando lad dummies
fimports$Dhw<- factor(fimports$Dhw,levels=c("domestic Positive & foreign Negative","domestic Middle_Range & foreign Positive",
                                          "domestic Middle_Range & foreign Negative" ,"domestic Middle_Range & foreign Middle_Range",
                                          "domestic Negative & foreign Positive","domestic Positive & foreign Middle_Range" ,
                                          "domestic Negative & foreign Middle_Range"))

# graficas
# boxplot(fimports$dffm_c ~ as.factor(fimports$Dhw))
# boxplot(fimports$Imports ~ as.factor(fimports$Dhw))

###World--------
require(car)
fimports$porIMCon<- fimports$Imp_Con
fimports$Area<- unlist(fimports$Area)

all.fao<- pdata.frame(fimports, index = c("Area", "Year"))

# exploration data using regresions 
# boxplot(fimportsLAC$porIMCon ~ as.factor(fimportsLAC$Area))
# coplot(log(Imp_Con) ~ log(Production)+ Year|Area, panel=panel.car, col = "red", rows = 1 ,data = fimportsLAC)
# coplot(log(Production)~Year|Area, panel=panel.car, col = "red", rows = 1 ,data = fimportsLAC)
# coplot(log(Production)~Year|Area*log(Imp_Con), panel=panel.car, col = "red", rows = 1 ,data = fimportsLAC)

# grafico 1, production consumption and 
png(filename="./RicePaper/fao/temporal/WorldcoplotProd_Con_Import.png", 
    width = 10, height = 7, units = 'in', res = 100)

coplot(log(Production)~Year|log(Consumption)*log(Imports), 
       panel=panel.car, 
       col = "blue", pch = 16,
       overlap = 0.5,
       rows = 1 ,
       data = fimports)

dev.off()

# grafico 2, box plot
tiff(filename="./RicePaper/fao/temporal/WorldboxplotIMP_CON_Import.tiff", 
     width = 10, height = 10, units = 'in', res = 300)
ggplot(fimports,aes(x=Area,y=porIMCon,color=Area))  +
      geom_boxplot(alpha=0.4,stat="boxplot") + theme(aspect.ratio = 1)+ labs(y="Imports/Consumption")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme(legend.position = "none")
dev.off()

all.fao<- pdata.frame(fimports, index = c("Area", "Year"))

#############################
#####modelo agrupado#########
#############################
pool.fao.world.hw<- plm(diff(porIMCon)~ Dhw ,
                        data = all.fao,model ="pooling", 
                        na.action = na.exclude)

pool.fao.world.ema<- plm(diff(porIMCon)~ Dema ,
                        data = all.fao,model ="pooling", 
                        na.action = na.exclude)

pool.fao.world.sma<- plm(diff(porIMCon)~ Dsma ,
                        data = all.fao,model ="pooling", 
                        na.action = na.exclude)


pooled<- list(HW= pool.fao.world.hw,
              EMA= pool.fao.world.ema,
              SMA= pool.fao.world.sma)



SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/fao/temporal/FAOpool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)



################################################ Test de normalidad, homocedasticidad y autocorrelacion --------------
NormalDistribution<- lapply(pooled, function(p){
      
      # shapiro.test(residuals(p)) # evaluation NULL: normal distribution 
      ### comments: los residuos de los Pooled model muestran que no hay normalidad en la distribucion de los errores.
      shapiro.test(residuals(p))
})     
Homos<- lapply(pooled, function(p){
      # testing Homoskedasticity #Breusch-Pagan test
      # bptest(pooled$p,studentize = F) #NULL: homoskedasticity
      ### comments: los datos registra varianza que no es constante, es decir es heterogenea.
      Homos<- bptest(p,studentize = F)
})  

AutocResi<- lapply(pooled, function(p){
      #       # Autorrelation of the residuals
      AutocResi<- pdwtest(p,alternative="two.sided")
})    

testDataNorm<- data.frame(do.call(rbind,NormalDistribution), testing="Normalidad")
testHomos<- data.frame( do.call(rbind, Homos), testing="Homocedasticidad")
testAutocResi<- data.frame(do.call(rbind, AutocResi), testing="Autocorrelación")

teslist<- list(testAutocResi,testHomos, testDataNorm)

a<-lapply(teslist, function(l){
      l<- l %>% select("statistic","method","p.value", "testing")
})
# exportando resultados de los test de normalidad, autocorrelacion y homocedasticidad
test<- do.call(rbind,a)
test$model<- row.names(test)

test<- data.frame(statistic=unlist(test$statistic),
                  method= unlist(test$method),
                  p.value=unlist(test$p.value), 
                  model=unlist(test$model), 
                  testing=unlist(test$testing))
row.names(test)<- 1:nrow(test)
test<- as.data.frame(test)


##############################
##### modelo fix #############
##############################

all.fao<- pdata.frame(fimports, index = c("Area", "Year"))
fixed.fao.world.hw<- plm(diff(porIMCon)~ Dhw , data = all.fao,model ="within")
fixed.fao.world.ema<- plm(diff(porIMCon) ~ Dema , data = all.fao,model ="within")
fixed.fao.world.sma<- plm(diff(porIMCon) ~ Dsma , data = all.fao,model ="within")

fixed.world<- list(HW= fixed.fao.world.hw ,
                   EMA= fixed.fao.world.ema,
                   SMA= fixed.fao.world.sma)


SumPool<- lapply(fixed.world, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/fao/temporal/FAOfixed.doc",
                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                    head.tag = TRUE, body.tag = TRUE)



########################################### Testing for fixed effects, null: OLs better than fixed------------  
t1<-pFtest(fixed.world$HW,pooled$HW)
t2<-pFtest(fixed.world$EMA,pooled$EMA)
t3<-pFtest(fixed.world$SMA,pooled$SMA)


hw.models<- data.frame(statistic=unlist(t1$statistic),
                      method=unlist(t1$method),
                      p.value=unlist(t1$p.value),
                      model="HW",
                      testing="Select Fix or OLS")
ema.models<- data.frame(statistic=unlist(t2$statistic),
                       method=unlist(t2$method),
                       p.value=unlist(t2$p.value),
                       model="EMA",
                       testing="Select Fix or OLS")
sma.models<- data.frame(statistic=unlist(t3$statistic),
                       method=unlist(t3$method),
                       p.value=unlist(t3$p.value),
                       model="SMA",
                       testing="Select Fix or OLS")


testEfecFixOLS<- rbind(hw.models, ema.models, sma.models)
rownames(testEfecFixOLS)<- 1:nrow(testEfecFixOLS)
testjoin<- rbind(test,testEfecFixOLS)


rownames(testjoin)<- 1:nrow(testjoin)
testjoin<- data.frame(statistic=unlist(testjoin$statistic),
                      method=unlist(testjoin$method),
                      p.value=unlist(testjoin$p.value), 
                      model=unlist(testjoin$model), 
                      testing=unlist(testjoin$testing))

####  Exporting all test to csv
write.csv(x = testjoin,file = paste("./RicePaper/fao/temporal/testFAO.csv", sep = ""))



##############################
##### modelo random###########
############################## 
all.fao<- pdata.frame(fimports, index = c("Area", "Year"))
random.fao.world.hw<- plm(diff(porIMCon) ~ Dhw , data = all.fao,model ="random")
random.fao.world.ema<- plm(diff(porIMCon) ~ Dema , data = all.fao,model ="random")
random.fao.world.sma<- plm(diff(porIMCon) ~ Dsma , data = all.fao,model ="random")

random.world<- list(HW= random.fao.world.hw ,
                    EMA= random.fao.world.ema,
                    SMA= random.fao.world.sma)


SumPool<- lapply(random.world, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### Exportar las tablas OLS a doc. 
tables.models.random.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                     custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/fao/temporal/random.doc",
                                     inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                     head.tag = TRUE, body.tag = TRUE)

################################################## TEST random and fixed model-------------
hw.models <- phtest(fixed.world$HW, random.world$HW)
ema.models<- phtest(fixed.world$EMA, random.world$EMA)
sma.models<- phtest(fixed.world$SMA, random.world$SMA)

hw.models<- data.frame(statistic=unlist(hw.models$statistic), 
                       method=unlist(hw.models$method), 
                       p.value=unlist(hw.models$p.value),
                       model="HW",
                       testing="Select Fix or Ran")
ema.models<- data.frame(statistic=unlist(ema.models$statistic), 
                        method=unlist(ema.models$method), 
                        p.value=unlist(ema.models$p.value),
                        model="EMA",
                        testing="Select Fix or Ran")
sma.models<- data.frame(statistic=unlist(sma.models$statistic), 
                        method=unlist(sma.models$method), 
                        p.value=unlist(sma.models$p.value),
                        model="SMA",
                        testing="Select Fix or Ran")

testHusman<- rbind(hw.models, ema.models, sma.models)
rownames(testHusman)<- 1:nrow(testHusman)
testjoin<- rbind(testjoin,testHusman)

######################################################## TEST of serial correlation--------------------- 
hw.serial.correlation<- pbgtest(fixed.world$HW)
ema.serial.correlation<- pbgtest(fixed.world$EMA)
sma.serial.correlation<- pbgtest(fixed.world$SMA)
wool.hw.serial.correlation<- pwartest(fixed.world$HW)
wool.ema.serial.correlation<- pwartest(fixed.world$HW)
wool.sema.serial.correlation<- pwartest(fixed.world$HW)
### comments: we have serial correlation on our models.

shw.models<- data.frame(statistic=unlist(hw.serial.correlation$statistic), 
                        method=unlist(hw.serial.correlation$method), 
                        p.value=unlist(hw.serial.correlation$p.value),
                        model="HW", 
                        testing="Serial correlation")

sema.models<- data.frame(statistic=unlist(ema.serial.correlation$statistic), 
                         method=unlist(ema.serial.correlation$method), 
                         p.value=unlist(ema.serial.correlation$p.value),
                         model="EMA", 
                         testing="Serial correlation")

ssma.models<- data.frame(statistic=unlist(sma.serial.correlation$statistic), 
                         method=unlist(sma.serial.correlation$method), 
                         p.value=unlist(sma.serial.correlation$p.value),
                         model="SMA", 
                         testing="Serial correlation")


serial<- rbind(shw.models,sema.models,ssma.models)
rownames(serial)<- 1:nrow(serial)
testjoin<- rbind(testjoin,serial)

############################################### Test for cross-sectional dependence---------------
hw.models <- pcdtest(fixed.world$HW)
ema.models<- pcdtest(fixed.world$EMA)
sma.models<- pcdtest(fixed.world$SMA)

CDhw.models<- data.frame(statistic=unlist(hw.models$statistic), 
                         method=unlist(hw.models$method), 
                         p.value=unlist(hw.models$p.value),
                         model="HW",
                         testing="Cross-sectional dependence")
CDema.models<- data.frame(statistic=unlist(ema.models$statistic), 
                          method=unlist(ema.models$method), 
                          p.value=unlist(ema.models$p.value),
                          model="EMA",
                          testing="Cross-sectional dependence")

CDsma.models<- data.frame(statistic=unlist(sma.models$statistic), 
                          method=unlist(sma.models$method), 
                          p.value=unlist(sma.models$p.value),
                          model="SMA",
                          testing="Cross-sectional dependence")
CDtest<- rbind(CDhw.models, CDema.models, CDsma.models)
rownames(CDtest)<- 1:nrow(CDtest)
testjoin<- rbind(testjoin,CDtest)
rownames(testjoin)<- 1:nrow(testjoin)
testjoin<- data.frame(statistic=unlist(testjoin$statistic),
                      method=unlist(testjoin$method),
                      p.value=unlist(testjoin$p.value), 
                      model=unlist(testjoin$model), 
                      testing=unlist(testjoin$testing))


############################ ajusting model  + lag(log(Imports/Consumption),1)-----------
fixed.fao.world.hw2<- plm(diff(porIMCon)~ Dhw + lag(porIMCon) , 
                          data = all.fao,model ="within",effect = "individual")
fixed.fao.world.ema2<- plm(diff(porIMCon) ~ Dema + lag(porIMCon), 
                           data = all.fao,model ="within",effect = "individual")
fixed.fao.world.sma2<- plm(diff(porIMCon) ~ Dsma + lag(porIMCon), 
                           data = all.fao,model ="within",effect = "individual")

fixed.world.ajus<- list(HW= fixed.fao.world.hw2,
                   EMA= fixed.fao.world.ema2,
                   SMA= fixed.fao.world.sma2)

### corrigiendo los coeficientes 
robusCovaArellano<- lapply(fixed.world.ajus, function(m){
      robustCova <- coeftest(m)
      robustCova <- coeftest(m,vcovHC(m,method = "arellano",type = "HC3")) 
      
      
})


### exporting coefficientes 
coofExtractFix<- lapply(robusCovaArellano, function(a){
      coofExtract<- extract.coeftest(a)
      
})


tables.models<- htmlreg(l =list(coofExtractFix$HW),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-FAO"),file = "./RicePaper/fao/temporal/HWmodel.FAO.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)
tables.models<- htmlreg(l =list(coofExtractFix$EMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-FAO"),file = "./RicePaper/fao/temporal/EMAmodel.FAO.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)                       
tables.models<- htmlreg(l =list(coofExtractFix$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-FAO"),file = "./RicePaper/fao/temporal/SMAmodel.FAO.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)




### Exporting coefficientes 
SummaryFix<- lapply(fixed.world.ajus, function(a){
      summary(a, type="hc3",vcovHC(a,method = "arellano") )

})


### to extract to overall intercept-------- 
overallint.hw<- within_intercept(fixed.fao.world.hw2)
overallint.ema<- within_intercept(fixed.fao.world.ema2)
overallint.sma<- within_intercept(fixed.fao.world.sma2)

## intercepts
hwi<- within_intercept(fixed.world$HW,vcov = function(x)vcovHC(x,method="arellano", type = "HC0"))
smai<- within_intercept(fixed.world$SMA,vcov = function(x)vcovHC(x,method="arellano", type = "HC3"))
emai<- within_intercept(fixed.world$EMA,vcov = function(x)vcovHC(x,method="arellano", type = "HC3"))

datainter<- data.frame(coefficients=c(hwi,smai,emai), models=c("HW","SMA", "EMA"))
write.csv(datainter,"./RicePaper/fao/temporal/intercepsOVERALL.csv")

# fx_level<- fixef(fixed.world$HW, type = "level")
# fx_mean<-  fixef(fixed.fao.lac.hw2, type = "dmean")
# overallint<- within_intercept(fixed.fao.lac.hw2)
# all.equal(overallint + fx_mean, fx_level, check.attributes=F)



########################################################################
########################## LAC  ########################################
########################################################################
require(car)
fimportsLAC<- as.data.frame(fimportsLAC) # copia de la base de datos

fimportsLAC$porIMCon<- fimportsLAC$Imp_Con
fimportsLAC$Area<- unlist(fimportsLAC$Area)
fimportsLAC$Area<- plyr::revalue(fimportsLAC$Area, c("Venezuela (Bolivarian Republic of)"="Venezuela",
                                 "Bolivia (Plurinational State of)"="Bolivia"))

all.fao.lac<- pdata.frame(fimportsLAC, index = c("Area", "Year"))

# exploration data using regresions 
# boxplot(fimportsLAC$porIMCon ~ as.factor(fimportsLAC$Area))
# coplot(log(Imp_Con) ~ log(Production)+ Year|Area, panel=panel.car, col = "red", rows = 1 ,data = fimportsLAC)
# coplot(log(Production)~Year|Area, panel=panel.car, col = "red", rows = 1 ,data = fimportsLAC)
# coplot(log(Production)~Year|Area*log(Imp_Con), panel=panel.car, col = "red", rows = 1 ,data = fimportsLAC)

# grafico 1, production consumption and 
png(filename="./RicePaper/fao/temporal/coplotProd_Con_Import.png", 
    width = 10, height = 7, units = 'in', res = 100)

coplot(log(Production)~Year|log(Consumption)*log(Imports), 
       panel=panel.car, 
       col = "blue", pch = 16,
       overlap = 0.5,
       rows = 1 ,columns = 5,
       data = fimportsLAC)

dev.off()

# grafico 2, box plot
tiff(filename="./RicePaper/fao/temporal/boxplotIMP_CON_Import.tiff", 
     width = 10, height = 10, units = 'in', res = 300)
ggplot(fimportsLAC,aes(x=Area,y=porIMCon,color=Area))  +
      geom_boxplot(alpha=0.4,stat="boxplot") + theme(aspect.ratio = 1)+ labs(y="Imports/Consumption")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme(legend.position = "none")
dev.off()



#############################
#####modelo agrupado#########
#############################
pool.fao.lac.hw<- plm(diff(porIMCon)~ Dhw ,
                        data = all.fao.lac,model ="pooling", 
                        na.action = na.exclude)

pool.fao.lac.ema<- plm(diff(porIMCon)~ Dema ,
                         data = all.fao.lac,model ="pooling", 
                         na.action = na.exclude)

pool.fao.lac.sma<- plm(diff(porIMCon)~ Dsma ,
                         data = all.fao.lac,model ="pooling", 
                         na.action = na.exclude)


pooled<- list(HW= pool.fao.lac.hw,
              EMA= pool.fao.lac.ema,
              SMA= pool.fao.lac.sma)



SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/fao/temporal/LAC-FAOpool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)



################################################ Test de normalidad, homocedasticidad y autocorrelacion--------- 
NormalDistribution<- lapply(pooled, function(p){
      
      # shapiro.test(residuals(p)) # evaluation NULL: normal distribution 
      ### comments: los residuos de los Pooled model muestran que no hay normalidad en la distribucion de los errores.
      shapiro.test(residuals(p))
})     
Homos<- lapply(pooled, function(p){
      # testing Homoskedasticity #Breusch-Pagan test
      # bptest(pooled$p,studentize = F) #NULL: homoskedasticity
      ### comments: los datos registra varianza que no es constante, es decir es heterogenea.
      Homos<- bptest(p,studentize = F)
})  

AutocResi<- lapply(pooled, function(p){
      #       # Autorrelation of the residuals
      AutocResi<- pdwtest(p,alternative="two.sided")
})    

testDataNorm<- data.frame(do.call(rbind,NormalDistribution), testing="Normalidad")
testHomos<- data.frame( do.call(rbind, Homos), testing="Homocedasticidad")
testAutocResi<- data.frame(do.call(rbind, AutocResi), testing="Autocorrelación")

teslist<- list(testAutocResi,testHomos, testDataNorm)

a<-lapply(teslist, function(l){
      l<- l %>% select("statistic","method","p.value", "testing")
})
# exportando resultados de los test de normalidad, autocorrelacion y homocedasticidad
test<- do.call(rbind,a)
test$model<- row.names(test)

test<- data.frame(statistic=unlist(test$statistic),
                  method= unlist(test$method),
                  p.value=unlist(test$p.value), 
                  model=unlist(test$model), 
                  testing=unlist(test$testing))
row.names(test)<- 1:nrow(test)
test<- as.data.frame(test)



##############################
##### modelo fix #############
##############################

all.fao.lac<- pdata.frame(fimportsLAC, index = c("Area", "Year"))
fixed.lac.hw<- plm(diff(porIMCon)~ Dhw , data = all.fao.lac,model ="within")
fixed.lac.ema<- plm(diff(porIMCon) ~ Dema , data = all.fao.lac,model ="within")
fixed.lac.sma<- plm(diff(porIMCon) ~ Dsma , data = all.fao.lac,model ="within")

fixed.lac<- list(HW= fixed.lac.hw ,
                   EMA= fixed.lac.ema,
                   SMA= fixed.lac.sma)


SumPool<- lapply(fixed.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/fao/temporal/LAC-FAOfixed.doc",
                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                    head.tag = TRUE, body.tag = TRUE)



########################################### Testing for fixed effects, null: OLs better than fixed ---------- 
t1<-pFtest(fixed.lac$HW,pooled$HW)
t2<-pFtest(fixed.lac$EMA,pooled$EMA)
t3<-pFtest(fixed.lac$SMA,pooled$SMA)


hw.models<- data.frame(statistic=unlist(t1$statistic),
                       method=unlist(t1$method),
                       p.value=unlist(t1$p.value),
                       model="HW",
                       testing="Select Fix or OLS")
ema.models<- data.frame(statistic=unlist(t2$statistic),
                        method=unlist(t2$method),
                        p.value=unlist(t2$p.value),
                        model="EMA",
                        testing="Select Fix or OLS")
sma.models<- data.frame(statistic=unlist(t3$statistic),
                        method=unlist(t3$method),
                        p.value=unlist(t3$p.value),
                        model="SMA",
                        testing="Select Fix or OLS")


testEfecFixOLS<- rbind(hw.models, ema.models, sma.models)
rownames(testEfecFixOLS)<- 1:nrow(testEfecFixOLS)
testjoin<- rbind(test,testEfecFixOLS)


rownames(testjoin)<- 1:nrow(testjoin)
testjoin<- data.frame(statistic=unlist(testjoin$statistic),
                      method=unlist(testjoin$method),
                      p.value=unlist(testjoin$p.value), 
                      model=unlist(testjoin$model), 
                      testing=unlist(testjoin$testing))

####  Exporting all test to csv
write.csv(x = testjoin,file = paste("./RicePaper/fao/temporal/LAC-testFAO.csv", sep = ""))



##############################
##### modelo Random###########
############################## 
all.fao.lac<- pdata.frame(fimportsLAC, index = c("Area", "Year"))
random.fao.lac.hw<- plm(diff(porIMCon) ~ Dhw , data = all.fao.lac,model ="random")
random.fao.lac.ema<- plm(diff(porIMCon) ~ Dema , data = all.fao.lac,model ="random")
random.fao.lac.sma<- plm(diff(porIMCon) ~ Dsma , data = all.fao.lac,model ="random")

random.lac<- list(HW= random.fao.lac.hw ,
                    EMA= random.fao.lac.ema,
                    SMA= random.fao.lac.sma)


SumPool<- lapply(random.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### Exportar las tablas OLS a doc. 
tables.models.random.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                     custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/fao/temporal/LAC-random.doc",
                                     inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                     head.tag = TRUE, body.tag = TRUE)

################################################## TEST random and fixed model--------------
hw.models <- phtest(fixed.lac$HW, random.lac$HW)
ema.models<- phtest(fixed.lac$EMA, random.lac$EMA)
sma.models<- phtest(fixed.lac$SMA, random.lac$SMA)

hw.models<- data.frame(statistic=unlist(hw.models$statistic), 
                       method=unlist(hw.models$method), 
                       p.value=unlist(hw.models$p.value),
                       model="HW",
                       testing="Select Fix or Ran")
ema.models<- data.frame(statistic=unlist(ema.models$statistic), 
                        method=unlist(ema.models$method), 
                        p.value=unlist(ema.models$p.value),
                        model="EMA",
                        testing="Select Fix or Ran")
sma.models<- data.frame(statistic=unlist(sma.models$statistic), 
                        method=unlist(sma.models$method), 
                        p.value=unlist(sma.models$p.value),
                        model="SMA",
                        testing="Select Fix or Ran")

testHusman<- rbind(hw.models, ema.models, sma.models)
rownames(testHusman)<- 1:nrow(testHusman)
testjoin<- rbind(testjoin,testHusman)


######################################################## TEST of serial correlation-------------- 
hw.serial.correlation<- pbgtest(fixed.lac$HW)
ema.serial.correlation<- pbgtest(fixed.lac$EMA)
sma.serial.correlation<- pbgtest(fixed.lac$SMA)
wool.hw.serial.correlation<- pwartest(fixed.lac$HW)
wool.ema.serial.correlation<- pwartest(fixed.lac$HW)
wool.sema.serial.correlation<- pwartest(fixed.lac$HW)
### comments: we have serial correlation on our models.

shw.models<- data.frame(statistic=unlist(hw.serial.correlation$statistic), 
                        method=unlist(hw.serial.correlation$method), 
                        p.value=unlist(hw.serial.correlation$p.value),
                        model="HW", 
                        testing="Serial correlation")

sema.models<- data.frame(statistic=unlist(ema.serial.correlation$statistic), 
                         method=unlist(ema.serial.correlation$method), 
                         p.value=unlist(ema.serial.correlation$p.value),
                         model="EMA", 
                         testing="Serial correlation")

ssma.models<- data.frame(statistic=unlist(sma.serial.correlation$statistic), 
                         method=unlist(sma.serial.correlation$method), 
                         p.value=unlist(sma.serial.correlation$p.value),
                         model="SMA", 
                         testing="Serial correlation")


serial<- rbind(shw.models,sema.models,ssma.models)
rownames(serial)<- 1:nrow(serial)
testjoin<- rbind(testjoin,serial)

############################################### Test for cross-sectional dependence-------------
hw.models <- pcdtest(fixed.lac$HW)
ema.models<- pcdtest(fixed.lac$EMA)
sma.models<- pcdtest(fixed.lac$SMA)

CDhw.models<- data.frame(statistic=unlist(hw.models$statistic), 
                         method=unlist(hw.models$method), 
                         p.value=unlist(hw.models$p.value),
                         model="HW",
                         testing="Cross-sectional dependence")
CDema.models<- data.frame(statistic=unlist(ema.models$statistic), 
                          method=unlist(ema.models$method), 
                          p.value=unlist(ema.models$p.value),
                          model="EMA",
                          testing="Cross-sectional dependence")

CDsma.models<- data.frame(statistic=unlist(sma.models$statistic), 
                          method=unlist(sma.models$method), 
                          p.value=unlist(sma.models$p.value),
                          model="SMA",
                          testing="Cross-sectional dependence")
CDtest<- rbind(CDhw.models, CDema.models, CDsma.models)
rownames(CDtest)<- 1:nrow(CDtest)
testjoin<- rbind(testjoin,CDtest)
rownames(testjoin)<- 1:nrow(testjoin)
testjoin<- data.frame(statistic=unlist(testjoin$statistic),
                      method=unlist(testjoin$method),
                      p.value=unlist(testjoin$p.value), 
                      model=unlist(testjoin$model), 
                      testing=unlist(testjoin$testing))


############################ ajusting model  + lag(log(Imports/Consumption),1)----------

fixed.fao.lac.hw2<- plm(diff(porIMCon) ~ Dhw + lag(porIMCon), 
                        data = all.fao.lac,model ="within", effect = "individual") #,effect = "individual"
fixed.fao.lac.ema2<- plm(diff(porIMCon)~ Dema + lag(porIMCon),
                         data = all.fao.lac,model ="within",effect = "individual")
fixed.fao.lac.sma2<- plm(diff(porIMCon)~ Dsma + lag(porIMCon), 
                         data = all.fao.lac,model ="within",effect = "individual")
# overallint.hw<- within_intercept(fixed.fao.lac.hw2)
# overallint.ema<- within_intercept(fixed.fao.lac.ema2)
# overallint.sma<- within_intercept(fixed.fao.lac.sma2)
# 

fixed.lac.ajus<- list(HW= fixed.fao.lac.hw2,
                        EMA= fixed.fao.lac.ema2,
                        SMA= fixed.fao.lac.sma2)

### corrigiendo los coeficientes 
robusCovaArellano<- lapply(fixed.lac.ajus, function(m){
      robustCova <- coeftest(m)
      robustCova <- coeftest(m,vcovHC(m,method = "arellano",type = "HC3")) 
      
      
})

# within_intercept(fixed.fao.lac.hw2,vcov = function(x)vcovHC(x,method="arellano", type = "HC3"))
# fx_level<- fixef(fixed.fao.lac.hw2, type = "level")
# fx_mean<-  fixef(fixed.fao.lac.hw2, type = "dmean")
# overallint<- within_intercept(fixed.fao.lac.hw2)
# all.equal(overallint + fx_mean, fx_level, check.attributes=F)

### exporting coefficientes 
coofExtractFix<- lapply(robusCovaArellano, function(a){
      coofExtract<- extract.coeftest(a)
      
})


tables.models<- htmlreg(l =list(coofExtractFix$HW),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-FAO"),file = "./RicePaper/fao/temporal/LAC-HWmodel.FAO.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)
tables.models<- htmlreg(l =list(coofExtractFix$EMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-FAO"),file = "./RicePaper/fao/temporal/LAC-EMAmodel.FAO.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)                       
tables.models<- htmlreg(l =list(coofExtractFix$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-FAO"),file = "./RicePaper/fao/temporal/LAC-SMAmodel.FAO.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)




### Exporting coefficientes 
SummaryFix<- lapply(fixed.world.ajus, function(a){
      summary(a, type="hc3",vcovHC(a,method = "arellano") )
      
})



## intercepts
hwi<- within_intercept(fixed.lac.ajus$HW,vcov = function(x)vcovHC(x,method="arellano", type = "HC3"))
smai<- within_intercept(fixed.lac.ajus$SMA,vcov = function(x)vcovHC(x,method="arellano", type = "HC3"))
emai<- within_intercept(fixed.lac.ajus$EMA,vcov = function(x)vcovHC(x,method="arellano", type = "HC3"))

datainterLAC<- data.frame(coefficients=c(hwi,smai,emai), models=c("HW","SMA", "EMA"))
write.csv(datainterLAC,"./RicePaper/fao/temporal/LACintercepsOVERALL.csv")

# ###################################################### END CODE #####################################################################
all.fao$y<- (all.fao$Consumption/lag(all.fao$Consumption,k = 1))

##model comsuption 
p.world.c.hw2<- plm( log(y) ~ Dhw, data = all.fao,model ="pooling")
summary(p.world.c.hw2)


p.world.c.hw2<- plm( log(y) ~ Dhw*fs  + Dhw*ds , data = all.fao,model ="pooling")
p.world.c.ema2<- plm(log(y) ~ Dema*fs + Dema*ds , data = all.fao,model ="pooling")
p.world.c.sma2<- plm(log(y) ~ Dsma*fs + Dsma*ds , data = all.fao,model ="pooling")

fixed.world.c.ema2<- plm(porIMCon~ Dema , data = all.fao.lac,model ="within",effect = "individual")
fixed.world.c.sma2<- plm(porIMCon~ Dsma , data = all.fao.lac,model ="within",effect = "individual")

# ###################################################### EXPORT ####################################################################

write.csv(rice_all,paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/files/data/riceAllFAO.csv", sep = ""))
write.csv(rice_demand,paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/files/data/riceDemandFAO.csv", sep = ""))


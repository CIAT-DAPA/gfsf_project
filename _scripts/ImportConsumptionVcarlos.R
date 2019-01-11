# Consumption and time series, rice Analisis destinado paper de arroz y otros usos. 
# carlos Eduardo


#libraries
# R options
options(warn = -1)
options(scipen = 999)

# Load packages
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
ShocksFAO_cat<- read.csv(file = paste("./RicePaper/","Categories_shocksLocal&ForeighFAO.csv", sep = ""))
ShocksFAO_cat$X<- NULL
colnames(ShocksFAO_cat)[1]<- "Year"; colnames(ShocksFAO_cat)[2]<- "Area"
#FAOnumeric
ShocksFAO_num<- read.csv(file = paste("./RicePaper/","NumericCategoriesFAO.csv", sep = ""))
ShocksFAO_num$X<- NULL
colnames(ShocksFAO_num)[1]<- "Year"; colnames(ShocksFAO_num)[2]<- "Area"
ShocksFAO_num<- na.omit(ShocksFAO_num)

#USDAcategoric
ShocksUSDA_cat<- read.csv(file = paste("./RicePaper/usda/","Categories_shocksLocal&ForeighUSDA.csv", sep = ""))
ShocksUSDA_cat$X<- NULL
colnames(ShocksUSDA_cat)[1]<- "Year"; colnames(ShocksUSDA_cat)[2]<- "Area"

#USDAnumeric
ShocksUSDA_num<- read.csv(file = paste("./RicePaper/usda/","NumericCategoriesUSDA.csv", sep = ""))
ShocksUSDA_num$X<- NULL
colnames(ShocksUSDA_num)[1]<- "Year"; colnames(ShocksUSDA_num)[2]<- "Area"
ShocksUSDA_num<- na.omit(ShocksUSDA_num)

################################################## USDA data #############################################################################
usda<- read.csv(file = paste("./psd_alldata_csv (1)/psd_alldata.csv",sep = "" ))
usda_rice<- filter(usda, Commodity_Description=="Rice, Milled")
usda_rice$Country_Name<- as.character(usda_rice$Country_Name)
usda_rice$Commodity_Description<- as.character(usda_rice$Commodity_Description)
usda_rice$Attribute_Description<- as.character(usda_rice$Attribute_Description)

deleTvariables<- c("Unit_ID" , "Month","Country_Code",  "Commodity_Code",  "Attribute_ID",  "Calendar_Year", "Commodity_Description")
usda_rice<- usda_rice[,!names(usda_rice) %in% deleTvariables]
colnames(usda_rice)[1]<- "Area"; colnames(usda_rice)[2]<- "Year"; colnames(usda_rice)[3]<- "Item"; colnames(usda_rice)[4]<- "Unit"
colnames(usda_rice)[5]<- "Val"

# Items
item<- unique(usda_rice$Item)
f<- c("Ending Stocks", "Imports" ,"Exports", "Production", "Domestic Consumption", "Yield")
usda_rice<- filter(usda_rice, Item %in% f)
usda_rice$Unit<- NULL
usda_rice_wide<- usda_rice %>% spread(Item, Val)
usda_rice_wide<- filter(usda_rice_wide, Area!="EU-15")
colnames(usda_rice_wide)[3]<- "Consumption"; colnames(usda_rice_wide)[4]<- "Stock"

# adjusting measures units 
usda_rice_wide$Consumption<- (usda_rice_wide$Consumption)*1000 # before 1000 tonnes
usda_rice_wide$Stock<- (usda_rice_wide$Stock)*1000 # before 1000 tonnes
usda_rice_wide$Production<- (usda_rice_wide$Production)*1000 # before 1000 tonnes
usda_rice_wide$Exports<- (usda_rice_wide$Exports)*1000 # before 1000 tonnes
usda_rice_wide$Imports<- (usda_rice_wide$Imports)*1000 # before 1000 tonnes

# creating dummy LAC and World
# creating LAC variable 
usda_files<- usda_rice_wide %>%  mutate(., zone=ifelse((Area %in% lac_usda),"LAC", "WORLD"))

# creating  stocks domestic and foreign variables
usda_files$ds<- (usda_files$Stock/usda_files$Consumption) # stock as rate of consumption
usda_files$Con_ImProof<- (usda_files$Imports/usda_files$Consumption)# consumption as rate of Imports
usda_files<- na.omit(usda_files)
usda_files$ds[is.infinite(usda_files$ds)]<-0
usda_files<- filter(usda_files, Imports!=0)


### calculating diff of imports
usda_files<- usda_files %>% group_by(Area) %>% mutate(.,dffm_c = c(NA,diff(log(Imports/Consumption)))) 
usda_files<- na.omit(usda_files)
usda_files<- filter(usda_files, dffm_c!=0)

# number periods#Avaible data#eliminate countries with miss in years 
n.yrs<-length(unique(usda_files$Year))
ava.data<- usda_files %>% add_count(Area,Year)
rownames(ava.data)<- 1:nrow(ava.data)
sum.yrs.zone<- table(ava.data$Area, ava.data$n)
out.countries<- as.data.frame(sum.yrs.zone)
out.countries<- filter(out.countries,Freq<30) # con ausencia de mas de 35 años de datos
out.countries$Var1<- as.character(out.countries$Var1)
chao.countries<- out.countries[,1]

usda_files<- filter(usda_files, !Area %in% chao.countries)
# usda_files<- filter(usda_files, Year>=1980)


# calculating stock world by country
pots<- unique(usda_files$Area)

cf<- list()
# c=1
for(c in 1:length(pots)){
      
      
      cf[[c]]<- filter(usda_files, Area!=pots[[c]]) %>% group_by(Year) %>% select(Year,Stock, Consumption) %>%
            summarise(fs= (sum(Stock, na.rm=T)/sum(Consumption, na.rm=T))*100, Area=pots[c]) %>% 
            select(Year,Area,fs)
      
}

# Agrupando el stock y calculando variables de consumo
temp_cf<- do.call(rbind, cf)
usda_join<- left_join(usda_files,temp_cf, by=c("Year","Area"))

# Agrupando base de datos con stock y calculando variables rezagadas de consumo.  
usda_test<- usda_join
usda_test<- usda_test %>% group_by(Area) %>% mutate(.,cLag= lag(Consumption),Lconsumption= (Consumption/cLag))
usda_test<- na.omit(usda_test)

### Join completa 
usda_test<- left_join(usda_test,ShocksUSDA_cat, by=c("Year","Area","zone"))

### Adjusting categories
usda_test$cat_localHV<- as.character(usda_test$cat_localHV)
usda_test$cat_localShock_smooth_sma<- as.character(usda_test$cat_localShock_smooth_sma)
usda_test$cat_f_hvW<- as.character(usda_test$cat_f_hvW)
usda_test$cat_localShock_smooth_ema<- as.character(usda_test$cat_localShock_smooth_ema)
usda_test$cat_f_smaW<- as.character(usda_test$cat_f_emaW)
usda_test$cat_f_emaW<- as.character(usda_test$cat_f_emaW)

### Ajustando categorias
usda_test$cat_f_hvW<- plyr::revalue(usda_test$cat_f_hvW,replace =c("Negative_Negative"="Negative","High_Positive"="Positive", "High_Negative"="Negative"))
usda_test$cat_localShock_smooth_sma<- plyr::revalue(usda_test$cat_localShock_smooth_sma,replace =c("Negative_Negative"="Negative","High_Positive"="Positive", "High_Negative"="Negative"))
usda_test$cat_localShock_smooth_ema<- plyr::revalue(usda_test$cat_localShock_smooth_ema,replace =c("Negative_Negative"="Negative","High_Positive"="Positive", "High_Negative"="Negative"))
usda_test$cat_localHV<- plyr::revalue(usda_test$cat_localHV,replace =c("Negative_Negative"="Negative","High_Positive"="Positive", "High_Negative"="Negative"))
usda_test$cat_f_smaW<- plyr::revalue(usda_test$cat_f_smaW,replace =c("Negative_Negative"="Negative","High_Positive"="Positive", "High_Negative"="Negative"))
usda_test$cat_f_emaW<- plyr::revalue(usda_test$cat_f_emaW,replace =c("Negative_Negative"="Negative","High_Positive"="Positive", "High_Negative"="Negative"))

#eliminate NAs
usda_test<- na.omit(usda_test)

# Ajusting smooth por domestic or extrangero
usda_test$hw.d<- paste("domestic", usda_test$cat_localHV)
usda_test$hw.f<- paste("foreign", paste(usda_test$cat_f_hvW))
usda_test$sma.d<- paste("domestic", usda_test$cat_localShock_smooth_sma)
usda_test$sma.f<- paste("foreign", paste(usda_test$cat_f_smaW))
usda_test$ema.d<- paste("domestic", usda_test$cat_localShock_smooth_ema)
usda_test$ema.f<- paste("foreign", paste(usda_test$cat_f_emaW))

#### creating dummies HW---------------
ndnf<-  which(usda_test$hw.d=="domestic Negative" & usda_test$hw.f=="foreign Negative") #negative domestic and foreign / "Negative Negative" 
ndmrf<- which(usda_test$hw.d=="domestic Negative" & usda_test$hw.f=="foreign Middle_Range") #negative domestic and middle road / "Negative Middle_Range"
ndhpf<- which(usda_test$hw.d=="domestic Negative" & usda_test$hw.f=="foreign Positive") #negative domestic and high positive / "Negative Positive" 
mrdnd<- which(usda_test$hw.d=="domestic Middle_Range" & usda_test$hw.f=="foreign Negative") #middle foreign road and negative domestic  
mrdmrf<- which(usda_test$hw.d=="domestic Middle_Range" & usda_test$hw.f=="foreign Middle_Range") #middle road domestic road and middle foreign        
mrdhpf<- which(usda_test$hw.d=="domestic Middle_Range" & usda_test$hw.f=="foreign Positive") #middle road domestic and high positive foreign
hpdnf<- which(usda_test$hw.d=="domestic Positive" & usda_test$hw.f=="foreign Negative") #high positive domestic and negative foreign
hpdmrf<- which(usda_test$hw.d=="domestic Positive" & usda_test$hw.f=="foreign Middle_Range") #high positive domestic and middle roal foreign 
hpdhpf<- which(usda_test$hw.d=="domestic Positive" & usda_test$hw.f=="foreign Positive")#high positive domestic and high positive foreign 

# desempeño 
ndnf.d<-  c(ndnf);ndmrf.d<-  c(ndmrf);ndhpf.d<- c(ndhpf);mrdnd.d<- c(mrdnd);mrdmrf.d<- c(mrdmrf);mrdhpf.d<- c(mrdhpf)
hpdnf.d<- c(hpdnf);hpdmrf.d<- c(hpdmrf);hpdhpf.d<- c(hpdhpf)

# copia
tanz<- usda_test
tanz$Dhw<- NA
tanz$Dhw[ndnf.d] <- "domestic Negative & foreign Negative"
tanz$Dhw[ndmrf.d] <- "domestic Negative & foreign Middle_Range"
tanz$Dhw[ndhpf.d]<- "domestic Negative & foreign Positive"
tanz$Dhw[mrdnd.d]<- "domestic Middle_Range & foreign Negative"
tanz$Dhw[mrdmrf.d]<- "domestic Middle_Range & foreign Middle_Range"
tanz$Dhw[mrdhpf.d]<- "domestic Middle_Range & foreign Positive"
tanz$Dhw[hpdnf.d]<- "domestic Positive & foreign Negative"
tanz$Dhw[hpdmrf.d]<- "domestic Positive & foreign Middle_Range"
tanz$Dhw[hpdhpf.d]<- "domestic Positive & foreign Positive"

#### creating dummies SMA------------
ndnf<-  which(usda_test$sma.d=="domestic Negative" & usda_test$sma.f=="foreign Negative") #negative domestic and foreign / "Negative Negative" 
ndmrf<- which(usda_test$sma.d=="domestic Negative" & usda_test$sma.f=="foreign Middle_Range") #negative domestic and middle road / "Negative Middle_Range"
ndhpf<- which(usda_test$sma.d=="domestic Negative" & usda_test$sma.f=="foreign Positive") #negative domestic and high positive / "Negative Positive" 
mrdnd<- which(usda_test$sma.d=="domestic Middle_Range" & usda_test$sma.f=="foreign Negative") #middle foreign road and negative domestic  
mrdmrf<- which(usda_test$sma.d=="domestic Middle_Range" & usda_test$sma.f=="foreign Middle_Range") #middle road domestic road and middle foreign        
mrdhpf<- which(usda_test$sma.d=="domestic Middle_Range" & usda_test$sma.f=="foreign Positive") #middle road domestic and high positive foreign
hpdnf<- which(usda_test$sma.d=="domestic Positive" & usda_test$sma.f=="foreign Negative") #high positive domestic and negative foreign
hpdmrf<- which(usda_test$sma.d=="domestic Positive" & usda_test$sma.f=="foreign Middle_Range") #high positive domestic and middle roal foreign 
hpdhpf<- which(usda_test$sma.d=="domestic Positive" & usda_test$sma.f=="foreign Positive")#high positive domestic and high positive foreign 

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

#### creating dummies EMA???------------
ndnf<-  which(usda_test$ema.d=="domestic Negative" & usda_test$ema.f=="foreign Negative") #negative domestic and foreign / "Negative Negative" 
ndmrf<- which(usda_test$ema.d=="domestic Negative" & usda_test$ema.f=="foreign Middle_Range") #negative domestic and middle road / "Negative Middle_Range"
ndhpf<- which(usda_test$ema.d=="domestic Negative" & usda_test$ema.f=="foreign Positive") #negative domestic and high positive / "Negative Positive" 
mrdnd<- which(usda_test$ema.d=="domestic Middle_Range" & usda_test$ema.f=="foreign Negative") #middle foreign road and negative domestic  
mrdmrf<- which(usda_test$ema.d=="domestic Middle_Range" & usda_test$ema.f=="foreign Middle_Range") #middle road domestic road and middle foreign        
mrdhpf<- which(usda_test$ema.d=="domestic Middle_Range" & usda_test$ema.f=="foreign Positive") #middle road domestic and high positive foreign
hpdnf<- which(usda_test$ema.d=="domestic Positive" & usda_test$ema.f=="foreign Negative") #high positive domestic and negative foreign
hpdmrf<- which(usda_test$ema.d=="domestic Positive" & usda_test$ema.f=="foreign Middle_Range") #high positive domestic and middle roal foreign 
hpdhpf<- which(usda_test$ema.d=="domestic Positive" & usda_test$ema.f=="foreign Positive")#high positive domestic and high positive foreign 

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




############################################### IMPORTS #################################################
fimports<- tanz
fimportsLAC<- filter(tanz,zone=="LAC")

# # Data exploration ALL Countries
# png(filename="./RicePaper/usda/CON_IMP_USDA.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# scatterplot(dffm_c ~Year|Area, boxplots=F, smooth=T, reg.line=FALSE, 
#             data=fimports,xlab = "Year", 
#             legend.columns=1, legend.coords="topleft", legend.plot=F) #legend.coords="topright" # tanz_LAC
# 
# dev.off()
# 
# # Data exploration LAC countries
# png(filename="./RicePaper/usda/LAC_CON_IMP_USDA.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# scatterplot(dffm_c ~Year|Area, boxplots=F, smooth=T, reg.line=FALSE, 
#             data=fimportsLAC,xlab = "Year", 
#             legend.columns=1, legend.coords="topleft", legend.plot=F) #legend.coords="topright" # tanz_LAC
# 
# dev.off()
# 
# #### exploring heterogeneity countries: Plot group means and confidence intervals.
# png(filename="./RicePaper/usda/LACHeterogeneityCountryConsumptionUSDA.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# plotmeans(dffm_c ~Area, main="Heterogeneity across countries", data=fimportsLAC, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 
# png(filename="./RicePaper/usda/WorldHeterogeneityCountryConsumptionUSDA.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# plotmeans(dffm_c ~Area, main="Heterogeneity across countries", data=fimports, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 
# 
# #### exploring  heterogeneity year: Plot group means and confidence intervals.LAC
# png(filename="./RicePaper/usda/LACHeterogeneityConsumptionUSDA.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# plotmeans(dffm_c ~Year, main="Heterogeneity across years", data=fimportsLAC, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 
# #### exploring  heterogeneity year: Plot group means and confidence intervals.World
# png(filename="./RicePaper/usda/WorldHeterogeneityConsumptionUSDA.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# plotmeans(dffm_c ~Year, main="Heterogeneity across years", data=fimports, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 


###World--------
all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
pool.usda.world.hw<- plm(dffm_c ~ Dhw , data = all.usda,model ="pooling")
pool.usda.world.ema<- plm(dffm_c ~ Dema , data = all.usda,model ="pooling")
pool.usda.world.sma<- plm(dffm_c ~ Dsma , data = all.usda,model ="pooling")

pooled<- list(HW= pool.usda.world.hw ,
              EMA= pool.usda.world.ema,
              SMA= pool.usda.world.sma)


SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)

})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/pool.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                        head.tag = TRUE, body.tag = TRUE)


###Fixed and ramdom 
all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
fixed.usda.world.hw<- plm(dffm_c ~ Dhw , data = all.usda,model ="within")
fixed.usda.world.ema<- plm(dffm_c ~ Dema , data = all.usda,model ="within")
fixed.usda.world.sma<- plm(dffm_c ~ Dsma , data = all.usda,model ="within")

fixed.world<- list(HW= fixed.usda.world.hw ,
              EMA= fixed.usda.world.ema,
              SMA= fixed.usda.world.sma)


SumPool<- lapply(fixed.world, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)

})

### exportar las tablas OLS a doc. 
tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/fixed.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)


###Ramdom 
all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
random.usda.world.hw<- plm(dffm_c ~ Dhw , data = all.usda,model ="random")
random.usda.world.ema<- plm(dffm_c ~ Dema , data = all.usda,model ="random")
random.usda.world.sma<- plm(dffm_c ~ Dsma , data = all.usda,model ="random")

random.world<- list(HW= random.usda.world.hw ,
                   EMA= random.usda.world.ema,
                   SMA= random.usda.world.sma)


SumPool<- lapply(random.world, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.random.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/random.doc",
                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                    head.tag = TRUE, body.tag = TRUE)


### LAC----------
### filter LAC

lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
pool.usda.lac.hw<- plm(dffm_c ~ Dhw , data = lac.usda,model ="pooling")
pool.usda.lac.ema<- plm(dffm_c ~ Dema , data = lac.usda,model ="pooling")
pool.usda.lac.sma<- plm(dffm_c ~ Dsma , data = lac.usda,model ="pooling")

pooled<- list(HW= pool.usda.lac.hw ,
              EMA= pool.usda.lac.ema,
              SMA= pool.usda.lac.sma)


SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)

})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/LACpool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)


###Fixed and ramdom 
lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
fixed.usda.lac.hw<- plm(dffm_c ~ Dhw , data = lac.usda,model ="within")
fixed.usda.lac.ema<- plm(dffm_c ~ Dema , data = lac.usda,model ="within")
fixed.usda.lac.sma<- plm(dffm_c ~ Dsma , data = lac.usda,model ="within")

fixed.lac<- list(HW= fixed.usda.lac.hw ,
                   EMA= fixed.usda.lac.ema,
                   SMA= fixed.usda.lac.sma)


SumPool<- lapply(fixed.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/fixedALC.doc",
                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                    head.tag = TRUE, body.tag = TRUE)


###Ramdom 
lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
random.usda.lac.hw<- plm(dffm_c ~ Dhw , data = lac.usda,model ="random")
random.usda.lac.ema<- plm(dffm_c ~ Dema , data = lac.usda,model ="random")
random.usda.lac.sma<- plm(dffm_c ~ Dsma , data = lac.usda,model ="random")

random.lac<- list(HW= random.usda.lac.hw ,
                    EMA= random.usda.lac.ema,
                    SMA= random.usda.lac.sma)


SumPool<- lapply(random.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.random.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                     custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/randomALC.doc",
                                     inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                     head.tag = TRUE, body.tag = TRUE)


# 
# 
# #### TEST 
# # 1)Test cross-sectional dependence/contemporaneus correlation.
# ### World 
# pcdtest(fixed.usda.world, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(fixed.usda.world, test = c("cd"))
# 
# ### LAC
# pcdtest(fixed.usda.lac, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(fixed.usda.lac, test = c("cd"))
# 
# # 2)Testing for serial correlation
# ### World
# pbgtest(x = fixed.usda.world) # Null: is that there is not serial correlation 
# ### LAC 
# pbgtest(x = fixed.usda.lac) # Null: is that there is not serial correlation 
# 
# # 3)Testing Homoskedasticity
# ### World  
# bptest(log10(Con_Im) ~ Dema , data=all.usda,studentize = F) #NULL: homoskedasticity
# ### LAC
# bptest(log10(Con_Im) ~ Dema , data=lac.usda,studentize = F) #NULL: homoskedasticity
# 
# 
# 
# 
# ### Correction from correlation and hetero
# #### The original coefficients
# #World
# coeftest(fixed.usda.world,vcovHC) #heteros consistent coefficientes 
# coeftest(fixed.usda.lac.hw,vcovHC(fixed.usda.lac.hw,method = "arellano"))
# coeftest(fixed.usda.world,vcovHC(fixed.usda.world,type = "HC3")) # hetero consistent coefficients type =3
# # the following shows th Hc standar errors of the coefficients
# t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed.usda.world, type = x)))))
# 
# 
# 
# #LAC
# coeftest(fixed.usda.lac,vcovHC) #heteros consistent coefficientes 
# coeftest(fixed.usda.lac,vcovHC(fixed.usda.lac,method = "arellano"))
# coeftest(fixed.usda.lac,vcovHC(fixed.usda.lac,type = "HC3")) # hetero consistent coefficients type =3
# # the following shows th Hc standar errors of the coefficients
# t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed.usda.lac, type = x)))))
# 
# 
# ## Test de endogeneidad. correlacion entre los errores X y los errores del modelo
# phtest(fixed.usda.lac,fixed.usda.lac)
# 
# 
# # mco
# pooling.usda.world<- plm(log(Con_Im) ~ Dema , data = all.usda,model ="pooling")
# summary(pooling.usda.world)
# 
# # creating instrumentals variables 
# # pooling.usda.world<- plm(log(Con_Im) ~ Dema , data = all.usda,model ="pooling")
# # summary(pooling.usda.world)
# 
# 
# png(filename="./usda/WorldWImports/consumption.png", 
#         width = 10, height = 7, units = 'in', res = 100)
#     scatterplot(log(Con_Im)~Year|Area, boxplots=F, smooth=T, reg.line=FALSE, 
#                 data=all.usda,xlab = "Year", 
#                 legend.columns=1, legend.coords="topleft", legend.plot=F) #legend.coords="topright" # tanz_LAC
#     
#     dev.off()
# 
# png(filename="./usda/WorldHeterogeneityCountry.png", 
#   width = 10, height = 7, units = 'in', res = 100)
# plotmeans(log(Con_Im) ~Area, main="Heterogeneity across countries", data=all.usda, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 
# 
# 
# ### analisis de los residuos
# 
# pooling.usda.world<- lm(log(Con_Im) ~ Dema , data = all.usda)
# summary(pooling.usda.world)
# 
# pooling.usda.world$residuals
# plot(fitted.values(pooling.usda.world),pooling.usda.world$residuals)
# abline(h=0, col= "red")
# 
# hist(pooling.usda.world$residuals)
# hist(fitted.values(pooling.usda.world))    
# 
# resi.usda.all<- rstandard(pooling.usda.world) # standardized residual
# qqnorm(resi.usda.all, ylab="Standarized Residuals",
#        xlab = "Normal Scores",
#        main = "Old faithful model")
# qqline(resi.usda.all)
# 
# qqnorm(pooling.usda.world$residuals)
# qqline(pooling.usda.world$residuals)
# 
# residualPlot(pooling.usda.world)
# hist(residuals(pooling.usda.world))
# boxplot(residuals(pooling.usda.world))
# shapiro.test(residuals(pooling.usda.world)) # evaluation NULL: normal distribution 
# ### comments: los residuos de los Pooled model muestran que no hay normalidad en la distribucion de los errores.
# 
# 
# # testing Homoskedasticity #Breusch-Pagan test
# pooling.usda.world<- plm(log(Con_Im) ~ Dema , data = all.usda,model ="pooling") 
# bptest(log(Con_Im) ~ Dema, data=all.usda,studentize = F) #NULL: homoskedasticity
# ### comments: los datos registra varianza que no es constante, es decir es heterogenea.
# 
# 
# ### Fixed model
# 
# fx.usda.world1<- plm(log(Con_Im) ~ Dema , data = all.usda,model ="within") 
# fx.usda.world2<- plm(log(Con_Im) ~ Dsma , data = all.usda,model ="within") 
# fx.usda.world3<- plm(log(Con_Im) ~ Dhw , data = all.usda,model ="within") 
# 
# summary(fx.usda.world3)
# 
# fixef(fx.usda.world3, type = "dfirst")
# summary(fixef(fx.usda.world3, type = "dfirst"))
# 
# ## Random model
# ra.usda.world3<- plm(log(Con_Im) ~ Dhw , data = all.usda,model ="random") 
# summary(ra.usda.world3)
# 
# ## Hausmas test: fixed or random
# phtest(fx.usda.world3, ra.usda.world3) # if it is significant (p-value<0.05) then use fixed effects, 
# ## comments: el modelo de efectos fijos es mejor que el modelo de efectos aleatorios.
# 
# 
# ### Testing for cross-sectional dependence/contemporaneus correlation: using Breusch-pagan LM  of test of independence and Pasaran CD test
# pcdtest(fx.usda.world3, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(fx.usda.world3, test = c("cd"))
# ### comments: existe dependencia de los individuos transversales.  la inferencia/ estimacion muestra que 
# ### las unidades estan interrelacionadas.
# 
# 
# # testing for serial correlation
# pbgtest(x = fx.usda.world3) # Null: is that there is not serial correlation 
# adf.test(all.usda$Con_Im, k = 2) #la serie es estacionaria, p-value<0.05 then no units roots present
# ### comments: serial correlation in idiosyncratic errors
# 
# 
# ######## The original coefficients
# coeftest(fx.usda.world3,vcovHC) #heteros consistent coefficientes 
# coeftest(fx.usda.world3,vcovHC(fx.usda.world3,method = "arellano"))# 
# coeftest(fx.usda.world3,vcovHC(fx.usda.world3,type = "HC3")) # hetero consistent coefficients type =3
# #comments:: para nuestro problea de correlacion y heterocedasticidad + modelo de efectos fijos usamos "arellano"
# 
# ### modelo ajustado por robust covariance matrix estimators a la white for White panel models 
# HWmodel.all.usda<- coeftest(fx.usda.world3, vcov.=vcovHC(fx.usda.world3, method="arellano", type="HC0"))#,cluster = c("group")
# MAtrix.HWmodel.all.usda<- vcovHC(fx.usda.world3, method="arellano", type="HC0")
# 
# 
# ## extract results
# HWcoofModel.usda.all<- extract.coeftest(HWmodel.all.usda)
# tables.models<- screenreg(l =list(HWcoofModel.usda.all),stars = c(0.001,0.01, 0.05, 0.1),
#                           custom.model.names = c("All Countries-USDA"))
# 
# tables.models<- htmlreg(l =list(HWcoofModel.usda.all),stars = c(0.001,0.01, 0.05, 0.1),
#                           custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/HWmodel.usda.doc",
#                         inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                         head.tag = TRUE, body.tag = TRUE)
# 
# 
# 
# extPool<-extract.plm(HWmodel.all.usda,include.rsquared = F, include.adjrs = F)
# extOLs<- extract.lm(olsLAC, include.rsquared = F, include.adjrs = F)
# extFixed.dum<- extract.lm(fixed.dum, include.rsquared = F, include.adjrs = F)
# extFixed.plm<- extract.plm(model = fixed.plm,include.rsquared = F,include.adjrs = F)
# screenreg(l =list(extOLs,extPool, extFixed.dum,extFixed.plm ))
# 
# 

# 
# 
# 
# 
# 
# lac.usda<- pdata.frame(usda_test_LAC, index = c("Area", "Year"))
# fixed.usda.lac<- plm(Con_Im ~ Dhw , data = all.usda,model ="within")
# summary(fixed.usda.lac)
# 
# 
# 
# 
# 
# 
# 
# LACm_pooling<- plm(formula = log10(Consumption/lag(Consumption)) ~ factor(home_ema) + factor(out_ema) + ds + fs, data = pLACusda, model ="pooling")
# summary(LACm_pooling)
# 
# #### Modeling by LAC
# LAC.fixed<- plm(formula = log10(Consumption/lag(Consumption)) ~ home_ema + out_ema + ds + fs, data = pLAC, model ="within")
# summary(LAC.fixed)
# coeftest(LAC.fixed,vcovHC)
# 
# #### The original coefficients
# coeftest(LAC.fixed,vcovHC) #heteros consistent coefficientes 
# coeftest(LAC.fixed,vcovHC(LAC.fixed,method = "arellano"))
# coeftest(LAC.fixed,vcovHC(LAC.fixed,type = "HC3")) # hetero consistent coefficients type =3
# 
# 
# #### Modeling by World 
# pWord<- pdata.frame(tanz, index = c("Area", "Year"))
# Wordm_pooling<- plm(formula = log10(Consumption/lag(Consumption)) ~ factor(home_ema) + factor(out_ema) + ds + fs, data = pWord, model ="pooling")
# summary(Wordm_pooling)
# f.w<- plm(formula = log10(Consumption/lag(Consumption)) ~ home_ema + out_ema + ds + fs, data = pWord, model ="within")
# f.w<- plm(formula = log10(Consumption/lag(Consumption)) ~ home_ema + out_ema + 
#                 home_ema*fs + out_ema*ds + ds + fs, data = pWord, model ="within")
# 
# summary(f.w)
# 
# 
# #### Testing for cross-sectional dependence/contemporaneus correlation: using Breusch-pagan LM  of test of independence and Pasaran CD test
# pcdtest(f.w, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(f.w, test = c("cd"))
# 
# 
# # testing for serial correlation
# pbgtest(x = f.w) # Null: is that there is not serial correlation 
# panel.set<- plm.data(pWord, index=c("Area", "Year"))
# adf.test(panel.set$Lconsumption, k = 2) #la serie es estacionaria, p-value<0.05 then no units roots present
# 
# # testing Homoskedasticity 
# bptest(log10(Consumption) ~ factor(Area)-1+ home_ema + out_ema + ds + fs, data=pWord,studentize = F) #NULL: homoskedasticity
# # Alt: presence of heteroskedasticity, robust covariance matrix estimation 
# 
# 
# ### 
# coeftest(f.w,vcovHC) #heteros consistent coefficientes 
# coeftest(f.w,vcovHC(f.w,method = "arellano"))
# coeftest(f.w,vcovHC(f.w,type = "HC3")) # hetero consistent coefficients type =3
# 
# 
# 
# 
# 
# ################################################## FAO data #############################################################################
# 
# 
# 
# ############ consumption  
# setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")
# faoStat<- read.csv(file = "./FoodBalanceSheets_E_All_Data_(Normalized)/FoodBalanceSheets_E_All_Data_(Normalized).csv", header = T)
# str(faoStat)
# faoStat$Item<- as.character(faoStat$Item); faoStat$Area<- as.character(faoStat$Area)
# faoStat$Element<- as.character(faoStat$Element)
# faoStat$Unit<- as.character(faoStat$Unit)
# faoStat$Year<- as.character(faoStat$Year); faoStat$Year<- as.numeric(faoStat$Year)
# faoStat$Area.Code<- NULL; faoStat$Item.Code<- NULL; faoStat$Element.Code<- NULL; faoStat$Year.Code<- NULL; faoStat$Flag<- NULL
# faoStat$Unit<- NULL
# d_faostat<- faoStat # subset
# d_faostat<- filter(d_faostat, !Area %in% potsOut)
# 
# # adjusting 
# d_faostat$Area<- plyr::revalue(d_faostat$Area, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
#                                                  "Lao People's Democratic Republic"="Lao Democratic Republic", 
#                                                  "Côte d'Ivoire"="Ivory Coast",
#                                                  "China, Hong Kong SAR"="Hong Kong",
#                                                  "China, Taiwan Province of"= "Taiwan")) 
# 
# # "China, Hong Kong SAR"='Hong Kong',
# # "China, Taiwan Province of"='Taiwan',
# # "China, mainland"="China
# 
# 
# 
# ########## Imports
# faoStatTrade<- read.csv(file = "./Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv", header = T)
# str(faoStatTrade)
# faoStatTrade$Item<- as.character(faoStatTrade$Item); faoStat$Area<- as.character(faoStat$Area)
# faoStatTrade$Element<- as.character(faoStatTrade$Element)
# faoStatTrade$Unit<- as.character(faoStatTrade$Unit)
# faoStatTrade$Year<- as.character(faoStatTrade$Year); faoStat$Year<- as.numeric(faoStat$Year)
# faoStatTrade$Area.Code<- NULL; faoStatTrade$Item.Code<- NULL; faoStatTrade$Element.Code<- NULL; faoStatTrade$Year.Code<- NULL; faoStatTrade$Flag<- NULL
# faoStatTrade$Unit<- NULL
# d_faoStatTrade<- faoStatTrade # subset
# d_faoStatTrade<- filter(d_faoStatTrade, !Area %in% potsOut)
# 
# d_faoStatTrade$Area<- plyr::revalue(d_faoStatTrade$Area, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
#                                                            "Lao People's Democratic Republic"="Lao Democratic Republic", 
#                                                            "Côte d'Ivoire"="Ivory Coast",
#                                                            "China, Hong Kong SAR"="Hong Kong",
#                                                          "China, Taiwan Province of"= "Taiwan")) 
# 
# ######### domestic supply
# v<-c("Domestic supply quantity", "Import Quantity", "Stock Variation", "Export Quantity", "Production")
# rice_all<- filter(d_faostat, Element %in% v) %>% filter(., Item=="Rice (Milled Equivalent)")
# rice_all<- rice_all[!duplicated(rice_all),]
# rice_all<- rice_all %>% spread(Element, Value)                
# 
# 
# rice_exports<- filter(d_faoStatTrade,Element=="Export Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)") 
# rice_exports$Year<- as.numeric(rice_exports$Year)
# rice_exports$Area<- as.character(rice_exports$Area)
# rice_exports<- rice_exports[!duplicated(rice_exports),]
# 
# 
# ######### Exports
# rice_exports<- filter(d_faoStatTrade,Element=="Export Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)") 
# rice_exports$Year<- as.numeric(rice_exports$Year)
# rice_exports$Area<- as.character(rice_exports$Area)
# rice_exports<- rice_exports[!duplicated(rice_exports),]
# 
# 
# ########## Stock
# rice_stock<- filter(d_faostat, Element=="Stock Variation") %>% filter(., Item=="Rice (Milled Equivalent)")
# rice_stock$Year<- as.numeric(rice_stock$Year)
# rice_stock<- rice_stock[!duplicated(rice_stock),]
# 
# 
# ### Demand Food
# rice_demand<- filter(d_faostat,Element=="Food") %>% filter(. , Item=="Rice (Milled Equivalent)") 
# rice_demand<- rice_demand[!duplicated(rice_demand),]
# rice_demand<- rice_demand[!duplicated(rice_demand),]
# 
# 
# #### imports
# rice_import<- filter(d_faoStatTrade,Element=="Import Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)")
# rice_import$Year<- as.numeric(rice_import$Year)
# rice_import$Area<- as.character(rice_import$Area)
# rice_import<- rice_import[!duplicated(rice_import),]
# 
# ######### production
# rice_production<- filter(d_faostat,Element=="Production") %>% filter(. , Item=="Rice (Milled Equivalent)") 
# rice_production$Year<- as.numeric(rice_production$Year)
# rice_production$Area<- as.character(rice_production$Area)
# rice_production<- rice_production[!duplicated(rice_production),]
# 
# # names changes 
# colnames(rice_demand)[5]<-"Consumption";colnames(rice_import)[5]<- "Imports"; colnames(rice_exports)[5]<- "Exports"
# colnames(rice_stock)[5]<-"Stock"; colnames(rice_production)[5]<- "Production"
# mix<- left_join(rice_import,rice_demand,by = c("Area", "Year"))
# mix<- mix[,c("Area", "Year", "Imports","Consumption")]
# 
# mix<- left_join(mix, rice_exports, by = c("Area", "Year"))
# mix<- mix[,c("Area", "Year", "Imports","Consumption","Exports" )]
# 
# mix<- left_join(mix, rice_stock, by = c("Area","Year"))
# mix<- mix[,c("Area","Year","Imports","Consumption", "Exports","Stock")]
# 
# mix<- left_join(mix, rice_production, by = c("Area","Year"))
# mix<- mix[,c("Area","Year","Imports","Consumption", "Exports","Stock", "Production")]
# 
# # join using shocks 
# mix<- left_join(mix, Shocks, by = c("Area","Year"))
# mix$Stock[is.na(mix$Stock)]<-0
# mix$Exports[is.na(mix$Exports)]<-0
# mix$Consumption[is.na(mix$Consumption)]<-0
# 
# # eliminate missing data categories 
# mix<- na.omit(mix)
# row.names(mix)<- 1:nrow(mix)
# 
# # adjusting measures units 
# mix$Consumption<- (mix$Consumption)*1000 # before 1000 tonnes
# mix$Stock<- (mix$Stock)*1000 # before 1000 tonnes
# mix$Production<- (mix$Production)*1000 # before 1000 tonnes
# 
# 
# # adjusting values stock negative values were addition, conversely, don't show decrease
# for(i in 1:nrow(mix)){
#       if(mix$Stock[i]<0){
#             mix$Stock[i]<-  mix$Stock[i]*(-1)
#       }else{}
# }
# 
# 
# # creating LAC variable 
# cfiles<- mix %>%  mutate(., zone=ifelse((Area %in% lac),"LAC", "WORLD"))
# 
# 
# # creating  stocks domestic and foreign variables
# cfiles$ds<- (cfiles$Stock/cfiles$Consumption) # stock as rate of consumption
# cfiles$Con_Im<- (cfiles$Consumption/cfiles$Imports)# consumption as rate of Imports
# 
# cfiles$ds[is.infinite(cfiles$ds)]<-0
# cfiles$ds[is.na(cfiles$ds)]<-0
# 
# pots<- unique(cfiles$Area)
# 
# cf<- list()
# # c=1
# for(c in 1:length(pots)){
#       
#     
#       cf[[c]]<- filter(cfiles, Area!=pots[[c]]) %>% group_by(Year) %>% select(Year,Stock, Consumption) %>%
#             summarise(fs= sum(Stock, na.rm=T)/sum(Consumption, na.rm=T), Area=pots[c]) %>% 
#             select(Year,Area,fs)
#       
# }
# temp_cf<- do.call(rbind, cf)
# cfiles_F<- left_join(cfiles,temp_cf, by=c("Year","Area"))
# 
# # fill data using the dummies
# tanz<- cfiles_F
# tanz<-  filter(tanz, Consumption!=0)
# tanz<- tanz %>% group_by(Area) %>%mutate(.,cLag= lag(Consumption),Lconsumption= (Consumption/cLag))
# tanz<- na.omit(tanz)
# 
# # tanz<- tanz %>% mutate_each(funs(factor(.)), cols) # change the attributes variables to character to factor
# tanz$cat_localHV<- as.character(tanz$cat_localHV)
# tanz$cat_localShock_smooth_sma<- as.character(tanz$cat_localShock_smooth_sma)
# tanz$cat_f_hvW<- as.character(tanz$cat_f_hvW)
# tanz$cat_localShock_smooth_ema<- as.character(tanz$cat_localShock_smooth_ema)
# tanz$cat_f_smaW<- as.character(tanz$cat_f_emaW)
# tanz$cat_f_emaW<- as.character(tanz$cat_f_emaW)
# 
# 
# # adjusting variables dummy positive and negative = 1 and )
# pos<- c("Middle_Range","High_Positive")
# tanz<- tanz %>% mutate(., home_sma= ifelse(cat_localShock_smooth_sma %in% pos, "Positive", "Negative"))
# tanz<- tanz %>% mutate(., home_ema= ifelse(cat_localShock_smooth_ema %in% pos, "Positive", "Negative"))
# tanz<- tanz %>% mutate(., home_hw= ifelse(cat_localHV %in% pos, "Positive", "Negative"))
# 
# tanz<- tanz %>% mutate(., out_sma= ifelse( cat_f_smaW %in% pos, "Positive", "Negative"))
# tanz<- tanz %>% mutate(., out_ema= ifelse( cat_f_emaW %in% pos, "Positive", "Negative"))
# tanz<- tanz %>% mutate(., out_hw= ifelse( cat_f_hvW %in% pos, "Positive", "Negative"))
# 
# #####Subset LAC
# tanz_LAC<- filter(tanz,zone=="LAC")
# 
# ######################################################### Description Analisys #########################################################
# cor(tanz[,c("cLag","fs","ds")], use = "complete.obs", method = "pearson")
# 
# 
# plot(log10((tanz$Consumption)/tanz$cLag),tanz$ds)
# abline(h=0, col= "red")
# 
# 
# plot(log10((tanz$Consumption)/tanz$cLag),tanz$fs)
# abline(h=0, col= "red")
# 
# 
# plot(log10((tanz_LAC$Consumption)/tanz_LAC$cLag),tanz_LAC$ds)
# abline(h=0, col= "red")
# 
# 
# plot(log10((tanz_LAC$Consumption)/tanz_LAC$cLag),tanz_LAC$fs)
# abline(h=0, col= "red")
# 
# 
# 
# 
# 
# 
# 
# 
# # Exploring data ALC
# ## distribution 
# png(filename="./LACconsumption.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# scatterplot(Lconsumption ~Year|Area, boxplots=F, smooth=T, reg.line=FALSE, 
#             data=tanz_LAC,xlab = "Year", 
#             legend.columns=1, legend.coords="topleft", legend.plot=F) #legend.coords="topright" # tanz_LAC
# 
# dev.off()
# 
# #### exploring heterogeneity countries: Plot group means and confidence intervals.
# png(filename="./LACHeterogeneityCountryConsumption.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# plotmeans(Lconsumption ~Area, main="Heterogeneity across countries", data=tanz_LAC, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 
# 
# #### exploring  heterogeneity year: Plot group means and confidence intervals.
# png(filename="./LACHeterogeneityConsumption.png", 
#     width = 10, height = 7, units = 'in', res = 100)
# plotmeans(Lconsumption ~Year, main="Heterogeneity across years", data=tanz_LAC, bars = T,barwidth = 0.5, n.label = F)
# dev.off()
# 
#  
# 
# 
# ####################################################### Regresss ########################################################################
# 
# #regress Pooled
# # tanz_LAC<- filter(tanz_LAC, Area!=c("Suriname", "Guyana")) ##3 estos dos paises le faltan numeros
# pLAC<- pdata.frame(tanz_LAC, index = c("Area", "Year"))
# LACm_pooling<- plm(formula = log10(Consumption/lag(Consumption)) ~ factor(home_ema) + factor(out_ema) + ds + fs, data = pLAC, model ="pooling")
# summary(LACm_pooling)
# 
# #### Modeling by LAC
# LAC.fixed<- plm(formula = log10(Consumption/lag(Consumption)) ~ home_ema + out_ema + ds + fs, data = pLAC, model ="within")
# summary(LAC.fixed)
# coeftest(LAC.fixed,vcovHC)
# 
# #### The original coefficients
# coeftest(LAC.fixed,vcovHC) #heteros consistent coefficientes 
# coeftest(LAC.fixed,vcovHC(LAC.fixed,method = "arellano"))
# coeftest(LAC.fixed,vcovHC(LAC.fixed,type = "HC3")) # hetero consistent coefficients type =3
# 
# 
# #### Modeling by World 
# pWord<- pdata.frame(tanz, index = c("Area", "Year"))
# Wordm_pooling<- plm(formula = log10(Consumption/lag(Consumption)) ~ factor(home_ema) + factor(out_ema) + ds + fs, data = pWord, model ="pooling")
# summary(Wordm_pooling)
# f.w<- plm(formula = log10(Consumption/lag(Consumption)) ~ home_ema + out_ema + ds + fs, data = pWord, model ="within")
# f.w<- plm(formula = log10(Consumption/lag(Consumption)) ~ home_ema + out_ema + 
#           home_ema*fs + out_ema*ds + ds + fs, data = pWord, model ="within")
# 
# summary(f.w)
# 
# 
# #### Testing for cross-sectional dependence/contemporaneus correlation: using Breusch-pagan LM  of test of independence and Pasaran CD test
# pcdtest(f.w, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(f.w, test = c("cd"))
# 
# 
# # testing for serial correlation
# pbgtest(x = f.w) # Null: is that there is not serial correlation 
# panel.set<- plm.data(pWord, index=c("Area", "Year"))
# adf.test(panel.set$Lconsumption, k = 2) #la serie es estacionaria, p-value<0.05 then no units roots present
# 
# # testing Homoskedasticity 
# bptest(log10(Consumption) ~ factor(Area)-1+ home_ema + out_ema + ds + fs, data=pWord,studentize = F) #NULL: homoskedasticity
# # Alt: presence of heteroskedasticity, robust covariance matrix estimation 
# 
# 
# ### consistencia de co
# coeftest(f.w,vcovHC) #heteros consistent coefficientes 
# coeftest(f.w,vcovHC(f.w,method = "arellano"))
# coeftest(f.w,vcovHC(f.w,type = "HC3")) # hetero consistent coefficients type =3
# 
# 
# 
# 
# #regress OLS
# olsLAC<- lm( Lconsumption ~ factor(home_ema) + factor(out_ema)  + ds + fs, data=tanz_LAC) 
# summary(olsLAC)
# 
# # fixed effects using dummies 
# fixed.dum<- lm( formula = log10(Consumption/lag(Consumption)) ~ factor(Area)-1+ home_ema + out_ema  + ds + fs, data=tanz_LAC)
# summary(fixed.dum)
# 
# # fixed using plm model
# fixed.plm<- plm(formula = Lconsumption ~ factor(home_ema) + factor(out_ema)  + ds + fs, data = tanz_LAC, model ="within")
# summary(fixed.plm)
# fixef(fixed.plm) # para obtener los efectos fijos por unidad transversal
# fixed.plm2<- plm(formula = log10(Lconsumption) ~  factor(home_ema)*ds + factor(out_ema)*fs  + 
#                        factor(home_ema)*fs+ factor(out_ema)*ds + ds + fs, data = tanz_LAC, model ="within")
# summary(fixed.plm2)
# 
# # extract results
# extPool<-extract.plm(LACm_pooling,include.rsquared = F, include.adjrs = F)
# extOLs<- extract.lm(olsLAC, include.rsquared = F, include.adjrs = F)
# extFixed.dum<- extract.lm(fixed.dum, include.rsquared = F, include.adjrs = F)
# extFixed.plm<- extract.plm(model = fixed.plm,include.rsquared = F,include.adjrs = F)
# screenreg(l =list(extOLs,extPool, extFixed.dum,extFixed.plm ))
# 
# #testing for fixed effects, null: OLS better than fixed
# pFtest(fixed.plm, olsLAC) # p-value is < 0.05 then the fixed effects model is a better choice
# 
# # Random effects
# random.pl<-  plm(formula = log(Consumption) ~  home_ema + out_ema  + ds + fs, data = tanz_LAC, model ="random")
# summary(random.pl)
# extRandom.plm<-extract.plm(model = random.pl,include.rsquared = F,include.adjrs = F)
# screenreg(l =list(extOLs,extPool, extFixed.dum,extFixed.plm, extRandom.plm))
# 
# # Hausmas test: fixed or random
# phtest(fixed.plm, random.pl) # if it is significant (p-value<0.05) then use fixed effects, 
# 
# #### testing for time-fixed effects
# fixed.plm_yr<- plm(formula = log(Consumption) ~ factor(Year)+ home_ema + out_ema  + ds + fs, data = tanz_LAC, model ="within")
# summary(fixed.plm_yr)
# 
# extFixedYear.plm<- extract.plm(model= fixed.plm.yr,include.rsquared = F,include.adjrs = F)
# screenreg(l =list(extOLs,extPool, extFixed.dum,extFixed.plm, extRandom.plm,extFixedYear.plm))
# 
# # testing  tiem-fixed effects. the null is that no time-fixed effects needed
# pFtest(fixed.plm_yr,fixed.plm) # if p-value< 0.05 then use time-fixed effects
# plmtest(fixed.plm_yr, c("time"), type = ("bp")) # test of Breusch-Pagan
# 
# # testing for cross-sectional dependence/contemporaneus correlation: using Breusch-pagan LM  of test of independence and Pasaran CD test
# pcdtest(fixed.plm, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(fixed.plm, test = c("cd"))
# 
# 
# # testing for serial correlation
# pbgtest(x = fixed.plm) # Null: is that there is not serial correlation 
# panel.set<- plm.data(tanz_LAC, index=c("Area", "Year"))
# adf.test(panel.set$Consumption, k = 2) #la serie es estacionaria, p-value<0.05 then no units roots present
# 
# # testing Homoskedasticity 
# bptest(log(Consumption) ~ factor(Area)-1+ home_ema + out_ema + ds + fs, data=tanz_LAC,studentize = F) #NULL: homoskedasticity
# # Alt: presence of heteroskedasticity, robust covariance matrix estimation 
# 
# ## when it have both heteroskedasticity  ans serial correlation. Recommened for fixed effects 
# # controlling for hetero: fixed effects
# coeftest(fixed.plm)
# coeftest(fixed.plm2)
# summary(fixed.plm2)
# fixed.plm$residuals
# plot(fitted.values(fixed.plm2),fixed.plm2$residuals)
# abline(h=0, col= "red")
# fitted.values(fixed.plm2)
# # the original coefficients
# coeftest(fixed.plm,vcovHC) #heteros consistent coefficientes 
# coeftest(fixed.plm,vcovHC(fixed.plm,method = "arellano"))
# coeftest(fixed.plm,vcovHC(fixed.plm,type = "HC3")) # hetero consistent coefficients type =3
# 
# # the following shows th Hc standar errors of the coefficients
# t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed.plm, type = x)))))
# 
# 
# 
# 
# # 
# # ##model pool
# # m_pooling<- plm(formula = log(Consumption/lag(Consumption)) ~ cat_localHV| + cat_f_hvW| +ds + fs, data = pfiles, model ="pooling")
# # summary(m_pooling)
# # ##model fixed
# # m_fixed<- plm(formula = Lconsumption ~ cat_localHV| + cat_f_hvW| + ds + fs, data = pfiles, model ="within")
# # summary(m_fixed)
# # ## random
# # m_random<- plm(formula = Lconsumption ~ cat_localHV| + cat_f_hvW| + ds + fs, data = pfiles, model ="random")
# # summary(m_random)
# # 
# # ### which model is more appropiate? / we use hausman test Null: random effect model is appropiate/ Alt: fixed effect model is appropiate
# # phtest(m_fixed,m_random)
# # 
# # ### which model is more appropiate?/ Random model and Pooled OLS model, Null: Pooled model is appropiate/ Alt: Random affect model is appropiate
# # ### Breusch-pagan
# # plmtest(m_pooling, type = c("bp"))
# # 
# # ### Pesaran CD test for cross0sectional dependence in panels NUll: there is no serial correlation /alt: there is serial correlation
# # pcdtest(m_fixed,test = "cd")
# # 
# # 
# # #### LAC
# # 
# # pLAC<- pdata.frame(tanz_LAC, index = c("Area", "Year"))
# # LACm_pooling<- plm(formula = log(Consumption/lag(Consumption))~ cat_localHV| + cat_f_hvW| + ds + fs, data = pLAC, model ="pooling")
# # summary(LACm_pooling)
# # 
# # LACm_fe<- plm(formula = Lconsumption ~ cat_localHV + cat_f_hvW + ds + fs, data = pLAC, model ="within")
# # summary(LACm_fe)
# # LACm_re<- plm(formula = Lconsumption ~ cat_localHV + cat_f_hvW + ds + fs, data = pLAC, model ="random")
# # summary(LACm_re)
# # 
# # fixef(LACm_fe,type = "dmean") # deviations fro the overall mean / "dfirst" deviations from the firts individual
# # fixef(LACm_fe, type = "dfirst")
# # 
# # summary(fixef(LACm_fe, type="dmean"))
# # summary(fixef(LACm_fe, type="dfirst"))
# # summary(fixef(LACm_fe))
# # 
# # 
# # #### case two ways
# # LACm_twfe<- plm(formula = Lconsumption ~ cat_localHV + cat_f_hvW + ds + fs, data = pLAC, model ="within", effect = "twoways")
# # summary(LACm_twfe)
# # fixef(LACm_twfe,effect = "time")
# # 
# # 
# # ### general method of moments estimador
# # 
# # lac_gmm<- pgmm( Lconsumption ~ cat_localHV + cat_f_hvW + ds + fs, data = pLAC, model ="twostep", effect = "twoways")



# codes for eliminated

# ### adjusting variables dummy positive and negative = 1 and )-----------
# pos<- c("Middle_Range","High_Positive")
# usda_test<- usda_test %>% mutate(., home_sma= ifelse(cat_localShock_smooth_sma %in% pos, "Positive", "Negative"))
# usda_test<- usda_test %>% mutate(., home_ema= ifelse(cat_localShock_smooth_ema %in% pos, "Positive", "Negative"))
# usda_test<- usda_test %>% mutate(., home_hw= ifelse(cat_localHV %in% pos, "Positive", "Negative"))
# 
# usda_test<- usda_test %>% mutate(., out_sma= ifelse( cat_f_smaW %in% pos, "Positive", "Negative"))
# usda_test<- usda_test %>% mutate(., out_ema= ifelse( cat_f_emaW %in% pos, "Positive", "Negative"))
# usda_test<- usda_test %>% mutate(., out_hw= ifelse( cat_f_hvW %in% pos, "Positive", "Negative"))
# usda_test<- na.omit(usda_test)



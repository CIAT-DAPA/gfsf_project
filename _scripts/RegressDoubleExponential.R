# Consumption and time series, rice Analisis destinado paper de arroz y otros usos. 
# carlos Eduardo

### clean files previous running
g=gc;rm(list = ls())
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
ShocksFAO_cat<- read.csv(file = paste("./RicePaper/temporal/","Categories_shocksLocal&ForeighFAO.csv", sep = ""),header = T)
ShocksFAO_cat$X<- NULL
colnames(ShocksFAO_cat)[1]<- "Year"; colnames(ShocksFAO_cat)[2]<- "Area"

#USDAcategoric
ShocksUSDA_cat<- read.csv(file = paste("./RicePaper/usda/temporal/","Categories_shocksLocal&ForeighUSDA.csv", sep = ""))
ShocksUSDA_cat$X<- NULL
colnames(ShocksUSDA_cat)[1]<- "Year"; colnames(ShocksUSDA_cat)[2]<- "Area"


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
usda_files<- usda_files %>% group_by(Area) %>% mutate(.,dtest= c(NA,diff(log(Imports/Consumption))))
usda_files<- na.omit(usda_files)
usda_files$ds[is.infinite(usda_files$ds)]<-0
usda_files$dtest[is.infinite(usda_files$dtest)]<-0
usda_files<- filter(usda_files, Imports!=0)


### Calculating diff of imports
usda_files<- usda_files %>% group_by(Area) %>% mutate(.,dffm_c = c(NA,diff(log(Imports/Consumption)))) 
usda_files<- na.omit(usda_files)
usda_files<- filter(usda_files, dffm_c!=0)

### Number periods
#Avaible data#eliminate countries with miss in years 
n.yrs<-length(unique(usda_files$Year))
ava.data<- usda_files %>% add_count(Area,Year)
rownames(ava.data)<- 1:nrow(ava.data)
sum.yrs.zone<- table(ava.data$Area, ava.data$n)
out.countries<- as.data.frame(sum.yrs.zone)
out.countries<- filter(out.countries,Freq<20) # con ausencia de mas de 35 años de datos
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

#### creating dummies EMA------------
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

###World--------
all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
pool.usda.world.hw<- plm(dtest ~ Dhw , data = all.usda,model ="pooling")
pool.usda.world.ema<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dema , data = all.usda,model ="pooling")
pool.usda.world.sma<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dsma , data = all.usda,model ="pooling")

pooled<- list(HW= pool.usda.world.hw ,
              EMA= pool.usda.world.ema,
              SMA= pool.usda.world.sma)


SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/pool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)

# creating object list
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

# eliminate files
rm(testvar,testAutocResi, teslist, testHomos,hpdhp.f,hpdhpf.d,hpdmr.f,hpdmrf.d,hpdn.f,hpdnf.d,AutocResi)

### test for individual and time effects
testEffects<- lapply(pooled, function(p){
      g<- p
      plmtest(g, effect = "twoways", type="bp")
})    
testEffects<- data.frame(do.call(rbind,testEffects), testing="Test Effects")
testEffects$model<- rownames(testEffects)
testEffects<-  testEffects %>% select("statistic","method","p.value", "testing")
testEffects$model<- row.names(testEffects)
row.names(testEffects)<- 1:nrow(testEffects)
testjoin <- rbind(testEffects, test)


###Fixed 
all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
fixed.usda.world.hw<- plm(dtest ~ Dhw , data = all.usda,model ="within")
fixed.usda.world.ema<- plm(dtest ~ Dema , data = all.usda,model ="within")
fixed.usda.world.sma<- plm(dtest ~ Dsma , data = all.usda,model ="within")

fixed.world<- list(HW= fixed.usda.world.hw ,
                   EMA= fixed.usda.world.ema,
                   SMA= fixed.usda.world.sma)


SumPool<- lapply(fixed.world, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/fixed.doc",
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

### Exportar las tablas OLS a doc. 
tables.models.random.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                     custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/random.doc",
                                     inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                     head.tag = TRUE, body.tag = TRUE)

### TEST Fixed model or OLS
pFtest(fixed.world$HW,pooled$HW )
pFtest(fixed.world$EMA,pooled$EMA )
pFtest(fixed.world$SMA,pooled$HW)

### TEST random and fixed model
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


### TEST of serial correlation 
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

### test for cross-sectional dependence
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

### Raiz Unitaria
# testing for serial correlation
# pbgtest(x = f.w) # Null: is that there is not serial correlation 
all.usda$y<- diff(log(all.usda$Imports/all.usda$Consumption)) 

panel.set<- plm.data(all.usda, index=c("Area", "Year"))
rootUnit<- adf.test(all.usda$Lconsumption, k = 2,alternative = "stationary") #la serie es estacionaria, p-value<0.05 then no units roots present

rootUnit<- data.frame(statistic=unlist(rootUnit$statistic),
                      method=unlist(rootUnit$method),
                      p.value=unlist(rootUnit$p.value), 
                      model= "ALL", 
                      testing="Unit root tests for panel data")
row.names(rootUnit)<- 1:nrow(rootUnit)
testjoin<- rbind(testjoin,rootUnit)

####  Exporting all test to csv
write.csv(x = testjoin,file = paste("./RicePaper/usda/temporal/testUsda.csv", sep = ""))


### corrigiendo los coeficientes 
robusCovaArellano<- lapply(fixed.world, function(m){
      robustCova <- coeftest(m)
      robustCova <- coeftest(m,vcovHC(m,method = "arellano", type="HC3")) 
      
      
})

### exporting coefficientes 
coofExtractFix<- lapply(robusCovaArellano, function(a){
      coofExtract<- extract.coeftest(a)
   
})


tables.models<- htmlreg(l =list(coofExtractFix$HW),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/temporal/HWmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)
tables.models<- htmlreg(l =list(coofExtractFix$EMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/temporal/EMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)                       
tables.models<- htmlreg(l =list(coofExtractFix$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/temporal/SMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)


#################################################### LAC----------
### filter LAC
lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
pool.usda.lac.hw<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dhw , data = lac.usda,model ="pooling")
pool.usda.lac.ema<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dema , data = lac.usda,model ="pooling")
pool.usda.lac.sma<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dsma , data = lac.usda,model ="pooling")

pooled<- list(HW= pool.usda.lac.hw ,
              EMA= pool.usda.lac.ema,
              SMA= pool.usda.lac.sma)


SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/LACpool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)


# creating object list
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

# eliminate files
rm(testvar,testAutocResi, teslist, testHomos,hpdhp.f,hpdhpf.d,hpdmr.f,hpdmrf.d,hpdn.f,hpdnf.d,AutocResi)

### test for individual and time effects
testEffects<- lapply(pooled, function(p){
      g<- p
      plmtest(g, effect = "twoways", type="bp")
})    
testEffects<- data.frame(do.call(rbind,testEffects), testing="Test Effects")
testEffects$model<- rownames(testEffects)
testEffects<-  testEffects %>% select("statistic","method","p.value", "testing")
testEffects$model<- row.names(testEffects)
row.names(testEffects)<- 1:nrow(testEffects)
testjoin <- rbind(testEffects, test)


###Fixed 
lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
fixed.usda.lac.hw<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dhw , data = lac.usda,model ="within")
fixed.usda.lac.ema<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dema , data = lac.usda,model ="within")
fixed.usda.lac.sma<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dsma , data = lac.usda,model ="within")

fixed.lac<- list(HW= fixed.usda.lac.hw ,
                 EMA= fixed.usda.lac.ema,
                 SMA= fixed.usda.lac.sma)


SumPool<- lapply(fixed.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                  custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/fixedALC.doc",
                                  inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                  head.tag = TRUE, body.tag = TRUE)



###Ramdom 
lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
random.usda.lac.hw<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dhw , data = lac.usda,model ="random")
random.usda.lac.ema<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dema , data = lac.usda,model ="random")
random.usda.lac.sma<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dsma , data = lac.usda,model ="random")

random.lac<- list(HW= random.usda.lac.hw ,
                  EMA= random.usda.lac.ema,
                  SMA= random.usda.lac.sma)


SumPool<- lapply(random.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.random.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                   custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/randomALC.doc",
                                   inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                   head.tag = TRUE, body.tag = TRUE)


### TEST Fixed model or OLS
pFtest(fixed.lac$HW,pooled$HW )
pFtest(fixed.lac$EMA,pooled$EMA )
pFtest(fixed.lac$SMA,pooled$HW)

### TEST random and fixed model
hw.models <- phtest(fixed.lac$HW, random.world$HW)
ema.models<- phtest(fixed.lac$EMA, random.world$EMA)
sma.models<- phtest(fixed.lac$SMA, random.world$SMA)

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


### TEST of serial correlation 
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

### test for cross-sectional dependence
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

### Raiz Unitaria
# testing for serial correlation
# pbgtest(x = f.w) # Null: is that there is not serial correlation 
lac.usda$y<- diff(log(lac.usda$Imports/lac.usda$Consumption)) 

panel.set<- plm.data(lac.usda, index=c("Area", "Year"))
rootUnit<- adf.test(lac.usda$Lconsumption, k = 2,alternative = "stationary") #la serie es estacionaria, p-value<0.05 then no units roots present

rootUnit<- data.frame(statistic=unlist(rootUnit$statistic),
                      method=unlist(rootUnit$method),
                      p.value=unlist(rootUnit$p.value), 
                      model= "ALL", 
                      testing="Unit root tests for panel data")
row.names(rootUnit)<- 1:nrow(rootUnit)
testjoin<- rbind(testjoin,rootUnit)

####  Exporting all test to csv
write.csv(x = testjoin,file = paste("./RicePaper/usda/temporal/LACtestUsda.csv", sep = ""))


### corrigiendo los coeficientes 
robusCovaArellano<- lapply(fixed.world, function(m){
      robustCova <- coeftest(m)
      robustCova <- coeftest(m,vcovHC(m,method = "arellano", type="HC3")) 
      
      
})

### exporting coefficientes 
coofExtractFix<- lapply(robusCovaArellano, function(a){
      coofExtract<- extract.coeftest(a)
      
})


tables.models<- htmlreg(l =list(coofExtractFix$HW),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/temporal/LACHWmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)
tables.models<- htmlreg(l =list(coofExtractFix$EMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/temporal/LACEMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)                       
tables.models<- htmlreg(l =list(coofExtractFix$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-USDA"),file = "./RicePaper/usda/temporal/LACSMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)


####################################################### END CODE #################################################################
















###Ramdom 



# ################################################# FAO #########################################################

########### consumption  
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



########## Imports
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

######### domestic supply
v<-c("Domestic supply quantity", "Import Quantity", "Stock Variation", "Export Quantity", "Production")
rice_all<- filter(d_faostat, Element %in% v) %>% filter(., Item=="Rice (Milled Equivalent)")
rice_all<- rice_all[!duplicated(rice_all),]
rice_all<- rice_all %>% spread(Element, Value)                


rice_exports<- filter(d_faoStatTrade,Element=="Export Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)") 
rice_exports$Year<- as.numeric(rice_exports$Year)
rice_exports$Area<- as.character(rice_exports$Area)
rice_exports<- rice_exports[!duplicated(rice_exports),]


######### Exports
rice_exports<- filter(d_faoStatTrade,Element=="Export Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)") 
rice_exports$Year<- as.numeric(rice_exports$Year)
rice_exports$Area<- as.character(rice_exports$Area)
rice_exports<- rice_exports[!duplicated(rice_exports),]


########## Stock
rice_stock<- filter(d_faostat, Element=="Stock Variation") %>% filter(., Item=="Rice (Milled Equivalent)")
rice_stock$Year<- as.numeric(rice_stock$Year)
rice_stock<- rice_stock[!duplicated(rice_stock),]


### Demand Food
rice_demand<- filter(d_faostat,Element=="Food") %>% filter(. , Item=="Rice (Milled Equivalent)") 
rice_demand<- rice_demand[!duplicated(rice_demand),]
rice_demand<- rice_demand[!duplicated(rice_demand),]


#### imports
rice_import<- filter(d_faoStatTrade,Element=="Import Quantity") %>% filter(. , Item=="Rice - total  (Rice milled equivalent)")
rice_import$Year<- as.numeric(rice_import$Year)
rice_import$Area<- as.character(rice_import$Area)
rice_import<- rice_import[!duplicated(rice_import),]

######### production
rice_production<- filter(d_faostat,Element=="Production") %>% filter(. , Item=="Rice (Milled Equivalent)") 
rice_production$Year<- as.numeric(rice_production$Year)
rice_production$Area<- as.character(rice_production$Area)
rice_production<- rice_production[!duplicated(rice_production),]

# names changes 
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

# join using shocks 
mix<- left_join(mix, ShocksFAO_cat, by = c("Area","Year"))
mix$Stock[is.na(mix$Stock)]<-0
mix$Exports[is.na(mix$Exports)]<-0
mix$Consumption[is.na(mix$Consumption)]<-0

# eliminate missing data categories 
mix<- na.omit(mix)
row.names(mix)<- 1:nrow(mix)

# adjusting measures units 
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
fao_files$Con_ImProof<- (usda_files$Imports/usda_files$Consumption)# consumption as rate of Imports
fao_files<- na.omit(fao_files)
fao_files$ds[is.infinite(fao_files$ds)]<-0
fao_files<- filter(fao_files, Imports!=0)

### calculating diff of imports
fao_files<- fao_files %>% group_by(Area) %>% mutate(.,dffm_c = c(NA,diff(log(Imports/Consumption)))) 
fao_files<- na.omit(fao_files)
fao_files<- filter(fao_files, dffm_c!=0)

# number periods#Avaible data#eliminate countries with miss in years 
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
fao_test<- fao_test %>% group_by(Area) %>% mutate(.,cLag= lag(Consumption),Lconsumption= (Consumption/cLag))
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

#### creating dummies HW---------------
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

#### creating dummies EMA------------
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


# ############################################### IMPORTS #################################################
# fimports<- tanz
# fimportsLAC<- filter(tanz,zone=="LAC")
# 
# 
# ###World--------
# all.fao<- pdata.frame(fimports, index = c("Area", "Year"))
# pool.fao.world.hw<- plm(diff(log(Imports/Consumption), lag = 3,differences = 1) ~ Dhw , data = all.fao,model ="pooling")
# pool.fao.world.ema<- plm(dffm_c ~ Dema , data = all.fao,model ="pooling")
# pool.fao.world.sma<- plm(dffm_c ~ Dsma , data = all.fao,model ="pooling")
# 
# pooled<- list(HW= pool.fao.world.hw ,
#               EMA= pool.fao.world.ema,
#               SMA= pool.fao.world.sma)
# 
# 
# SumPool<- lapply(pooled, function(p){
#       extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
#       
# })
# 
# ### exportar las tablas OLS a doc. 
# tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
#                              custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/temporal/FAOpool.doc",
#                              inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                              head.tag = TRUE, body.tag = TRUE)
# 
# 
# ###Fixed and ramdom 
# all.fao<- pdata.frame(fimports, index = c("Area", "Year"))
# fixed.fao.world.hw<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dhw , data = all.usda,model ="within")
# fixed.fao.world.ema<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dema , data = all.usda,model ="within")
# fixed.fao.world.sma<- plm(diff(log(Imports/Consumption), lag =1,differences = 1) ~ Dsma , data = all.usda,model ="within")
# 
# fixed.world<- list(HW= fixed.fao.world.hw ,
#                    EMA= fixed.fao.world.ema,
#                    SMA= fixed.fao.world.sma)
# 
# 
# SumPool<- lapply(fixed.world, function(p){
#       extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
#       
# })
# 
# ### exportar las tablas OLS a doc. 
# tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
#                                     custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/temporal/FAOfixed.doc",
#                                     inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                                     head.tag = TRUE, body.tag = TRUE)
# 
# 
# ###Ramdom 
# all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
# random.usda.world.hw<- plm(dffm_c ~ Dhw , data = all.usda,model ="random")
# random.usda.world.ema<- plm(dffm_c ~ Dema , data = all.usda,model ="random")
# random.usda.world.sma<- plm(dffm_c ~ Dsma , data = all.usda,model ="random")
# 
# random.world<- list(HW= random.usda.world.hw ,
#                     EMA= random.usda.world.ema,
#                     SMA= random.usda.world.sma)
# 
# 
# SumPool<- lapply(random.world, function(p){
#       extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
#       
# })
# 
# ### exportar las tablas OLS a doc. 
# tables.models.random.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
#                                      custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/temporal/random.doc",
#                                      inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                                      head.tag = TRUE, body.tag = TRUE)
# 
# 
# ### LAC----------
# ### filter LAC
# 
# lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
# pool.usda.lac.hw<- plm(dffm_c ~ Dhw , data = lac.usda,model ="pooling")
# pool.usda.lac.ema<- plm(dffm_c ~ Dema , data = lac.usda,model ="pooling")
# pool.usda.lac.sma<- plm(dffm_c ~ Dsma , data = lac.usda,model ="pooling")
# 
# pooled<- list(HW= pool.usda.lac.hw ,
#               EMA= pool.usda.lac.ema,
#               SMA= pool.usda.lac.sma)
# 
# 
# SumPool<- lapply(pooled, function(p){
#       extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
#       
# })
# 
# ### exportar las tablas OLS a doc. 
# tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
#                              custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/temporal/LACpool.doc",
#                              inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                              head.tag = TRUE, body.tag = TRUE)
# 
# 
# ###Fixed and ramdom 
# lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
# fixed.usda.lac.hw<- plm(dffm_c ~ Dhw , data = lac.usda,model ="within")
# fixed.usda.lac.ema<- plm(dffm_c ~ Dema , data = lac.usda,model ="within")
# fixed.usda.lac.sma<- plm(dffm_c ~ Dsma , data = lac.usda,model ="within")
# 
# fixed.lac<- list(HW= fixed.usda.lac.hw ,
#                  EMA= fixed.usda.lac.ema,
#                  SMA= fixed.usda.lac.sma)
# 
# 
# SumPool<- lapply(fixed.lac, function(p){
#       extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
#       
# })
# 
# ### exportar las tablas OLS a doc. 
# tables.models.fixed.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
#                                   custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/temporal/fixedALC.doc",
#                                   inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                                   head.tag = TRUE, body.tag = TRUE)
# 
# 
# ###Ramdom 
# lac.usda<-  pdata.frame(fimportsLAC, index = c("Area", "Year"))
# random.usda.lac.hw<- plm(dffm_c ~ Dhw , data = lac.usda,model ="random")
# random.usda.lac.ema<- plm(dffm_c ~ Dema , data = lac.usda,model ="random")
# random.usda.lac.sma<- plm(dffm_c ~ Dsma , data = lac.usda,model ="random")
# 
# random.lac<- list(HW= random.usda.lac.hw ,
#                   EMA= random.usda.lac.ema,
#                   SMA= random.usda.lac.sma)
# 
# 
# SumPool<- lapply(random.lac, function(p){
#       extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
#       
# })
# 
# ### exportar las tablas . 
# tables.models.random.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
#                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/temporal/randomALC.doc",
#                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
#                                    head.tag = TRUE, body.tag = TRUE)
# 
# test.lac.usda
# 
# ############################################################## TEST ###################################################
# 
# ###USDA
# #### TEST 
# # 1)Test cross-sectional dependence/contemporaneus correlation.
# ### World 
# 
# pcdtest(fixed.fao.world.hw, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(fixed.fao.world.hw, test = c("cd"))
# 
# 
# ### LAC
# pcdtest(fixed.usda.lac, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# pcdtest(fixed.usda.lac, test = c("cd"))
# 
# # # 2)Testing for serial correlation
# # ### World
# # pbgtest(x = fixed.usda.world) # Null: is that there is not serial correlation 
# # ### LAC 
# # pbgtest(x = fixed.usda.lac) # Null: is that there is not serial correlation 
# # 
# # # 3)Testing Homoskedasticity
# # ### World  
# # bptest(log10(Con_Im) ~ Dema , data=all.usda,studentize = F) #NULL: homoskedasticity
# # ### LAC
# # bptest(log10(Con_Im) ~ Dema , data=lac.usda,studentize = F) #NULL: homoskedasticity
# # 
# # 
# 
# 
# 
# #World
# #LAC
# ###FAO
# 
# #World
# #LAC
# 
# # #### TEST 
# # # 1)Test cross-sectional dependence/contemporaneus correlation.
# # ### World 
# # pcdtest(fixed.usda.world, test = c("lm")) # Null: No cross-sectional dependence / Alt: Cross-sectinal dependence
# # pcdtest(fixed.usda.world, test = c("cd"))
# # 

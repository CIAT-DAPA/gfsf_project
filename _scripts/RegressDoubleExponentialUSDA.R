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
suppressMessages(library(lme4))





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
usda_files$Imp_Con<- (usda_files$Imports/usda_files$Consumption)# consumption as rate of Imports
usda_files<- filter(usda_files, Imp_Con!=0)
usda_files<- usda_files %>% group_by(Area) %>% mutate(.,dtest= c(NA,diff(log(Imports/Consumption))))
usda_files<- usda_files %>% group_by(Area) %>% mutate(.,dtest2= c(NA,diff(log(Imports)/log(Consumption))))
usda_files$dtest[is.infinite(usda_files$dtest)]<-0
usda_files<- na.omit(usda_files)
usda_files$ds[is.infinite(usda_files$ds)]<-0
usda_files$dtest2[is.infinite(usda_files$dtest2)]<-0
usda_files<- filter(usda_files, Imports!=0)
usda_files<- filter(usda_files, Production!=0)

summary(usda_files$dtest2)

# # define a function to remove outliers
# FindOutliers <- function(data) {
#       lowerq = quantile(data)[2]
#       upperq = quantile(data)[4]
#       iqr = upperq - lowerq 
#       #Or use IQR(data)
#       # we identify extreme outliers
#       extreme.threshold.upper = (iqr * 3) + upperq
#       extreme.threshold.lower = lowerq - (iqr * 3)
#       result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
# }
# 
# # use the function to identify outliers
# temp <- FindOutliers(usda_files$Imp_Con)
# cfOut<- usda_files[temp,]
# maxVal<- quantile(usda_files$Imp_Con,probs = 0.95)
# # cfOut$Con_ImProof<- quantile(fao_files$Con_ImProof,probs = 0.95)
# usda_files[temp,]$Imp_Con<-quantile(usda_files$Imp_Con,probs = 0.95)
# 
# 

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


############################# modelos 
fimports<- as.data.frame(tanz) # copia de la base de datos
# organizando lad dummies
fimports$Dhw<- factor(fimports$Dhw,levels=c("domestic Positive & foreign Negative","domestic Middle_Range & foreign Positive",
                                            "domestic Middle_Range & foreign Negative" ,"domestic Middle_Range & foreign Middle_Range",
                                            "domestic Negative & foreign Positive","domestic Positive & foreign Middle_Range" ,
                                            "domestic Negative & foreign Middle_Range"))
# sub set LAC
fimportsLAC<- filter(tanz,zone=="LAC")
# vito<-data.frame(Area=fimports$Area, Year=fimports$Year,Dhw=fimports$Dhw,
#                  Imports=fimports$Imports,Consumption=fimports$Consumption)


# graficas
# boxplot(fimports$dffm_c ~ as.factor(fimports$Dhw))
# boxplot(fimports$Imports ~ as.factor(fimports$Dhw))

###World--------
require(car)
fimports$porIMCon<- fimports$Imp_Con*100
fimports$Area<- unlist(fimports$Area)
fimports$Area<- plyr::revalue(fimports$Area, c("Venezuela (Bolivarian Republic of)"="Venezuela",
                                               "Bolivia (Plurinational State of)"="Bolivia"))
# grafico 1, production consumption and 
png(filename="./RicePaper/usda/temporal/WorldcoplotProd_Con_Import.png", 
    width = 10, height = 7, units = 'in', res = 100)

coplot(log(Production)~Year|log(Consumption)*log(Imports), 
       panel=panel.car, 
       col = "blue", pch = 16,
       overlap = 0.5,
       rows = 1 ,
       data = fimports)

dev.off()

# grafico 2, box plot
tiff(filename="./RicePaper/usda/temporal/WorldboxplotIMP_CON_Import.tiff", 
     width = 10, height = 10, units = 'in', res = 300)
ggplot(fimports,aes(x=Area,y=porIMCon,color=Area))  +
      geom_boxplot(alpha=0.4,stat="boxplot") + theme(aspect.ratio = 1)+ labs(y="Imports/Consumption")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme(legend.position = "none")
dev.off()


all.usda<- pdata.frame(fimports, index = c("Area", "Year"))


#############################
#####modelo agrupado#########
#############################
pool.usda.world.hw<- plm(porIMCon~ Dhw ,
                        data = all.usda,model ="pooling", 
                        na.action = na.exclude)

pool.usda.world.ema<- plm(porIMCon~ Dema ,
                         data = all.usda,model ="pooling", 
                         na.action = na.exclude)

pool.usda.world.sma<- plm(porIMCon~ Dsma ,
                         data = all.usda,model ="pooling", 
                         na.action = na.exclude)


pooled<- list(HW= pool.usda.world.hw,
              EMA= pool.usda.world.ema,
              SMA= pool.usda.world.sma)



SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/usdapool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)



################################################ Test de normalidad, homocedasticidad y autocorrelacion 
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

all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
fixed.usda.world.hw<- plm(porIMCon ~ Dhw , data = all.usda,model ="within")
fixed.usda.world.ema<- plm(porIMCon ~ Dema , data = all.usda,model ="within")
fixed.usda.world.sma<- plm(porIMCon ~ Dsma , data = all.usda,model ="within")

fixed.world<- list(HW= fixed.usda.world.hw ,
                   EMA= fixed.usda.world.ema,
                   SMA= fixed.usda.world.sma)


SumPool<- lapply(fixed.world, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/usdafixed.doc",
                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                    head.tag = TRUE, body.tag = TRUE)



########################################### Testing for fixed effects, null: OLs better than fixed  
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
write.csv(x = testjoin,file = paste("./RicePaper/usda/temporal/testusda.csv", sep = ""))



##############################
##### modelo random###########
############################## 
all.usda<- pdata.frame(fimports, index = c("Area", "Year"))
random.usda.world.hw<- plm(porIMCon ~ Dhw , data = all.usda,model ="random")
random.usda.world.ema<- plm(porIMCon ~ Dema , data = all.usda,model ="random")
random.usda.world.sma<- plm(porIMCon ~ Dsma , data = all.usda,model ="random")

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

################################################## TEST random and fixed model
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


######################################################## TEST of serial correlation 
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

############################################### Test for cross-sectional dependence
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


############################ ajusting model  + lag(log(Imports/Consumption),1)
fixed.usda.world.hw2<- plm(porIMCon ~ Dhw , data = all.usda,model ="within",effect = "individual")
fixed.usda.world.ema2<- plm(porIMCon ~ Dema , data = all.usda,model ="within",effect = "individual")
fixed.usda.world.sma2<- plm(porIMCon ~ Dsma , data = all.usda,model ="within",effect = "individual")

fixed.world.ajus<- list(HW= fixed.usda.world.hw2,
                        EMA= fixed.usda.world.ema2,
                        SMA= fixed.usda.world.sma2)

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
                        custom.model.names = c("All Countries-usda"),file = "./RicePaper/usda/temporal/HWmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)
tables.models<- htmlreg(l =list(coofExtractFix$EMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-usda"),file = "./RicePaper/usda/temporal/EMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)                       
tables.models<- htmlreg(l =list(coofExtractFix$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-usda"),file = "./RicePaper/usda/temporal/SMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)




### Exporting coefficientes 
SummaryFix<- lapply(fixed.world.ajus, function(a){
      summary(a, type="hc3",vcovHC(a,method = "arellano") )
      
})


########################################################################
########################## LAC  ########################################
########################################################################
require(car)
fimportsLAC$porIMCon<- fimportsLAC$Imp_Con*100
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
png(filename="./RicePaper/usda/temporal/coplotProd_Con_Import.png", 
    width = 10, height = 7, units = 'in', res = 100)

coplot(log(Production)~Year|log(Consumption)*log(Imports), 
       panel=panel.car, 
       col = "blue", pch = 16,
       overlap = 0.5,
       rows = 1 ,columns = 5,
       data = fimportsLAC)

dev.off()

# grafico 2, box plot
tiff(filename="./RicePaper/usda/temporal/boxplotIMP_CON_Import.tiff", 
     width = 10, height = 10, units = 'in', res = 300)
ggplot(fimportsLAC,aes(x=Area,y=porIMCon,color=Area))  +
      geom_boxplot(alpha=0.4,stat="boxplot") + theme(aspect.ratio = 1)+ labs(y="Imports/Consumption")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme(legend.position = "none")
dev.off()


all.usda.lac<- pdata.frame(fimportsLAC, index = c("Area", "Year"))

#############################
#####modelo agrupado#########
#############################
pool.usda.lac.hw<- plm(porIMCon~ Dhw ,
                      data = all.usda.lac,model ="pooling", 
                      na.action = na.exclude)

pool.usda.lac.ema<- plm(porIMCon~ Dema ,
                       data = all.usda.lac,model ="pooling", 
                       na.action = na.exclude)

pool.usda.lac.sma<- plm(porIMCon~ Dsma ,
                       data = all.usda.lac,model ="pooling", 
                       na.action = na.exclude)


pooled<- list(HW= pool.usda.lac.hw,
              EMA= pool.usda.lac.ema,
              SMA= pool.usda.lac.sma)



SumPool<- lapply(pooled, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.pool<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                             custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/LAC-usdapool.doc",
                             inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                             head.tag = TRUE, body.tag = TRUE)



################################################ Test de normalidad, homocedasticidad y autocorrelacion 
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

all.usda.lac<- pdata.frame(fimportsLAC, index = c("Area", "Year"))
fixed.lac.hw<- plm(porIMCon~ Dhw , data = all.usda.lac,model ="within")
fixed.lac.ema<- plm(porIMCon ~ Dema , data = all.usda.lac,model ="within")
fixed.lac.sma<- plm(porIMCon ~ Dsma , data = all.usda.lac,model ="within")

fixed.lac<- list(HW= fixed.lac.hw ,
                 EMA= fixed.lac.ema,
                 SMA= fixed.lac.sma)


SumPool<- lapply(fixed.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### exportar las tablas OLS a doc. 
tables.models.fixed.world<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                    custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/LAC-usdafixed.doc",
                                    inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                    head.tag = TRUE, body.tag = TRUE)



########################################### Testing for fixed effects, null: OLs better than fixed  
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
write.csv(x = testjoin,file = paste("./RicePaper/usda/temporal/LAC-testusda.csv", sep = ""))



##############################
##### modelo random###########
############################## 
all.usda.lac<- pdata.frame(fimportsLAC, index = c("Area", "Year"))
random.usda.lac.hw<- plm(porIMCon ~ Dhw , data = all.usda.lac,model ="random")
random.usda.lac.ema<- plm(porIMCon ~ Dema , data = all.usda.lac,model ="random")
random.usda.lac.sma<- plm(porIMCon ~ Dsma , data = all.usda.lac,model ="random")

random.lac<- list(HW= random.usda.lac.hw ,
                  EMA= random.usda.lac.ema,
                  SMA= random.usda.lac.sma)


SumPool<- lapply(random.lac, function(p){
      extpool<- extract.plm(p,include.rsquared = F, include.adjrs = F)
      
})

### Exportar las tablas OLS a doc. 
tables.models.random.lac<- htmlreg(l = list(SumPool$HW, SumPool$EMA, SumPool$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                                   custom.model.names = c("HW", "EMA", "SMA"),file ="./RicePaper/usda/temporal/LAC-random.doc",
                                   inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
                                   head.tag = TRUE, body.tag = TRUE)

################################################## TEST random and fixed model
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


######################################################## TEST of serial correlation 
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

############################################### Test for cross-sectional dependence
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


############################ ajusting model  + lag(log(Imports/Consumption),1)
fixed.usda.lac.hw2<- plm(porIMCon ~ Dhw , data = all.usda.lac,model ="within",effect = "individual")
fixed.usda.lac.ema2<- plm(porIMCon ~ Dema , data = all.usda.lac,model ="within",effect = "individual")
fixed.usda.lac.sma2<- plm(porIMCon ~ Dsma , data = all.usda.lac,model ="within",effect = "individual")

fixed.lac.ajus<- list(HW= fixed.usda.lac.hw2,
                      EMA= fixed.usda.lac.ema2,
                      SMA= fixed.usda.lac.sma2)

### corrigiendo los coeficientes 
robusCovaArellano<- lapply(fixed.lac.ajus, function(m){
      robustCova <- coeftest(m)
      robustCova <- coeftest(m,vcovHC(m,method = "arellano",type = "HC3")) 
      
      
})


### exporting coefficientes 
coofExtractFix<- lapply(robusCovaArellano, function(a){
      coofExtract<- extract.coeftest(a)
      
})


tables.models<- htmlreg(l =list(coofExtractFix$HW),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-usda"),file = "./RicePaper/usda/temporal/LAC-HWmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)
tables.models<- htmlreg(l =list(coofExtractFix$EMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-usda"),file = "./RicePaper/usda/temporal/LAC-EMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)                       
tables.models<- htmlreg(l =list(coofExtractFix$SMA),stars = c(0.001,0.01, 0.05, 0.1),
                        custom.model.names = c("All Countries-usda"),file = "./RicePaper/usda/temporal/LAC-SMAmodel.usda.doc",
                        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                        head.tag = TRUE, body.tag = TRUE)




### Exporting coefficientes 
SummaryFix<- lapply(fixed.world.ajus, function(a){
      summary(a, type="hc3",vcovHC(a,method = "arellano") )
      
})


# ###################################################### END CODE #####################################################################
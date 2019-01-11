
#### RTB 
#### Autor Carlos Eduardo Gonzalez 
#### pronostico, shock de rendimientos y analisis de redes

### directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles")

### libreria-----------

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
suppressMessages(library(networkD3))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(forecast))
suppressMessages(library(TTR))
suppressMessages(library(stats))


#### Regions

sacar<- c("Br. Indian Ocean Terr.", "World", "So. African Customs Union", 
          "Free Zones", "Fr. South Antarctic Terr.", "Fr. South Antarctic Terr.", 
          "Antarctica", "Special Categories", "US Misc. Pacific Isds", "Br. Indian Ocean Terr.", "Neutral Zone",
          "Cocos Isds", "Guam", "Tokelau", "Norfolk Isds", "Pitcairn", "Western Sahara",
          "United States Minor Outlying Islands")
cc<- cc %>% dplyr::filter(., !Partner %in% sacar) %>% dplyr::filter(., !Reporter %in% sacar) 

#### Regions---------------

##AMERICA
caribbean<- c("Cuba", "Dominican Rep.", "Haiti", "Jamaica", "Dominican Rep.","N. Mariana Isds",
              "Aruba","Saint Lucia","Dominica", "Saint Vincent and the Grenadines", "Puerto Rico",
              "Anguilla", "Antigua and Barbuda", "Saint Kitts and Nevis", "Montserrat",
              "Martinique", "Barbados", "Trinidad and Tobago", "Montserrat", "Grenada", 
              "Neth. Antilles", "Bahamas", "Bermuda", "Turks and Caicos Isds", "Bunkers" , 
              "Cayman Isds", "Br. Virgin Isds", "CuraÃ§ao", "Saint Maarten", "Bonaire","Saint BarthÃ©lemy" )
Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico",
                     "Nicaragua","Panama","El Salvador" )
Northern_America<-c("Canada","Greenland","USA", "Saint Pierre and Miquelon")
South_America<- c("Argentina","Bolivia (Plurinational State of)","Brazil","Chile","Colombia","Ecuador","Guyana","Peru",
                  "Paraguay","Uruguay","Venezuela","Suriname", "Falkland Isds (Malvinas)")


### ASIA
Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")
Eastern_Asia<- c("China","Japan","Rep. of Korea","Mongolia","North Korea", "Taiwan","China, Hong Kong SAR", 
                 "China, Macao SAR","China, mainland", "Dem. People's Rep. of Korea")
South_Eastern_Asia<- c("Indonesia","Cambodia","Lao People's Dem. Rep.","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "Timor-Leste","Viet Nam", "Singapore", "Brunei Darussalam")
Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Sri Lanka","Maldives", "Sri Lanka",
                  "Nepal","Other Indian Ocean","Pakistan")
Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", "Armenia",
                "State of Palestine", "Rest of Arabia","Saudi Arabia","Iran","Bahrain","Kuwait","Oman", "Bahrain", "Qatar","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates", "Georgia", "South Ossetia",
                "Lebanon", "Lebanon")



##OCENIA
Australia_and_New_Zealand<- c( "New Zealand", "Australia", "New Caledonia")

micro_poli_melanesia<- c("Fiji", "Papua New Guinea","Solomon Isds","Vanuatu", "Tonga","Palau", "Kiribati", "French Polynesia", 
                         "FS Micronesia", "Wallis and Futuna Isds", "Tuvalu", "Samoa", "Cook Isds", "Christmas Isds",
                         "American Samoa", "Marshall Isds", "Nauru", "Niue")


## EUROPA
Eastern_Europe<- c("Bulgaria","Belarus","Czechia","Hungary","Rep. of Moldova","Other Balkans","Poland",
                   "Russia","Slovakia","Ukraine", "Russian Federation", "Latvia",
                   "TFYR of Macedonia", "Kosovo", "Poland", "Serbia", "Romania", "Lithuania")
Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia", "Serbia and Montenegro", "Andorra",
                    "Malta", "San Marino", "Montenegro", "Bosnia Herzegovina", "Gibraltar")
Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","United Kingdom", "Estonia", 
                    "Sweden", "Faeroe Isds")
Western_Europe<- c("Austria","Belgium-Luxembourg","Switzerland","Germany","France","Belgium","Netherlands","Other Atlantic", "Luxembourg")

## AFRICA

Middle_Africa<- c("Angola","Central African Rep.","Cameroon","Dem. Rep. of the Congo","Congo","Gabon","Equatorial Guinea","Chad",
                  "São Tomé and Príncipe")
Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Sudan (former)","Tunisia", "South Sudan", "Sudan", "Fmr Sudan") #"Sudan,"
Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")
Western_Africa<- c("Benin","Burkina Faso","CÃ´te d'Ivoire","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia", "Sierra Leone",
                   "Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leon","Togo", "Saint Helena", 
                   "Sao Tome and Principe", "Cabo Verde")
Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar",
                   "Mozambique","Malawi","Rwanda","Somalia", "United Rep. of Tanzania","Comoros",
                   "Uganda","Zambia","Zimbabwe", "Mauritius", "Seychelles", "Mayotte")


#### Load Data-------------
files<- list.files(path = "./", 
                   pattern = "appendDataComercioCOMTRADECassavaStarch",all.files = T)
files<- lapply(files, read.csv)
files<- do.call(rbind, files)


#### function----- 
HWplot<- function(ts_objet, n.ahead=24, CI=0.95, error.ribbon="green", line.size=1){
      
      hw_object<- HoltWinters(ts_objet,gamma = F)
      forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
      for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
      fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
      actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
      
      save(for_values, fitted_values, actual_values,file = paste(workdi,pots[[c]], ".RData", sep = ""))
      
      
      graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
      graphset<- merge(graphset, for_values, all=TRUE, by='time')
      graphset[is.na(graphset$dev), ]$dev<-0
      graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
      
      graphset.met<- reshape::melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
      p<- ggplot(graphset.met, aes(x=time, y=value))+
            geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
            geom_line(aes(colour=variable), size=line.size)+ labs(title= paste(pots[[c]], ", Time series",sep=""))+
            geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
      
      return(p)
      
}


## Processing-----
ca<- files
ca<- ca %>% dplyr::select(Year,Trade.Flow,Reporter,Trade.Value..US..,Partner, Alt.Qty.Unit, Commodity)
colnames(ca)[3]<- "Reporter.Countries"
colnames(ca)[5]<- "Partner.Countries"
colnames(ca)[2]<- "flow"
colnames(ca)[4]<- "money"
colnames(ca)[6]<- "tons"

ca<- ca[!is.na(ca$money),] ## eliminando missing on 

#### eliminate special zones
ca$Reporter.Countries<- as.character(ca$Reporter.Countries)
ca$Partner.Countries<- as.character(ca$Partner.Countries)
nesReports<- ca[grepl(pattern =" nes",x = ca$Reporter.Countries),]
nesReports<- unique(nesReports$Reporter.Countries)
nesPartner<- ca[grepl(pattern =" nes",x = ca$Partner.Countries),] 
nesPartner<- unique(nesPartner$Partner.Countries)
nesout<- c(nesPartner, nesReports, "World")

###  filter without nes
ca<- ca %>% dplyr::filter(., !Reporter.Countries %in% nesout) %>% dplyr::filter(.,!Partner.Countries %in% nesout)
crops<- unique(ca$Commodity)
starc<- c("Starch; manioc (cassava)",
          "Manioc (cassava) starch")

ca<- ca %>% dplyr::filter(., Commodity %in% starc)
ca$Commodity<- plyr::revalue(ca$Commodity, c("Starch; manioc (cassava)" ="Starch",
                                       "Manioc (cassava) starch"="Starch"))
str(ca)
ca$flow<- as.character(ca$flow)
ca$Commodity<- as.character(ca$Commodity)
test<- ca %>% group_by(Year,flow,Reporter.Countries)%>% summarise(dinero=sum(money), cantidad=sum(tons))

### filter using feature of money and Exports
testm<- test %>% dplyr::filter(.,flow=="Export") %>% select(Year, flow, Reporter.Countries, dinero )
# testm<- testm %>% group_by(Reporter.Countries) %>%spread(.,"Year","dinero")

#### creando sub regiones ----------
###### Creación de objectos regiones correspondientes a la red filtrada
### America
treal1<- testm %>% dplyr::filter(Reporter.Countries %in% caribbean) %>% dplyr::mutate(., zone="caribbean")
treal2<- testm %>% dplyr::filter(Reporter.Countries %in% Central_America) %>% dplyr::mutate(., zone="Central_America")
treal3<- testm %>% dplyr::filter(Reporter.Countries %in% South_America) %>% dplyr::mutate(., zone="South_America")
treal4<- testm %>% dplyr::filter(Reporter.Countries %in% Northern_America) %>% dplyr::mutate(., zone="Northern_America")

##Asia
treal5<- testm %>% dplyr::filter(Reporter.Countries %in% Eastern_Asia) %>% dplyr::mutate(., zone="Eastern_Asia")
treal6<- testm %>% dplyr::filter(Reporter.Countries %in% South_Eastern_Asia) %>% dplyr::mutate(., zone="South_Eastern_Asia")
treal7<- testm %>% dplyr::filter(Reporter.Countries %in% Southern_Asia) %>% dplyr::mutate(., zone="Southern_Asia")
treal8<- testm %>% dplyr::filter(Reporter.Countries %in% Western_Asia) %>% dplyr::mutate(., zone="Western_Asia")
treal9<- testm %>% dplyr::filter(Reporter.Countries %in% Central_Asia) %>% dplyr::mutate(., zone="Central_Asia")


##Africa
treal10<- testm %>% dplyr::filter(Reporter.Countries %in% Eastern_Africa) %>% dplyr::mutate(., zone="Eastern_Africa")
treal11<- testm %>% dplyr::filter(Reporter.Countries %in% Middle_Africa) %>% dplyr::mutate(., zone="Middle_Africa")
treal12<- testm %>% dplyr::filter(Reporter.Countries %in% Northern_Africa) %>% dplyr::mutate(., zone="Northern_Africa")
treal13<- testm %>% dplyr::filter(Reporter.Countries %in% Southern_Africa) %>% dplyr::mutate(., zone="Southern_Africa")
treal14<- testm %>% dplyr::filter(Reporter.Countries %in% Western_Africa) %>% dplyr::mutate(., zone="Western_Africa")

##Oceania
treal15<- testm %>% dplyr::filter(Reporter.Countries %in% Australia_and_New_Zealand) %>% dplyr::mutate(., zone="Australia_and_New_Zealand")
treal16<- testm %>% dplyr::filter(Reporter.Countries %in% micro_poli_melanesia) %>% dplyr::mutate(., zone="micro_poli_melanesia")


##EUROPA
treal17<- testm %>% dplyr::filter(Reporter.Countries %in% Eastern_Europe) %>% dplyr::mutate(., zone="Eastern_Europe")
treal18<- testm %>% dplyr::filter(Reporter.Countries %in% Northern_Europe) %>% dplyr::mutate(., zone="Northern_Europe")
treal19<- testm %>% dplyr::filter(Reporter.Countries %in% Southern_Europe) %>% dplyr::mutate(., zone="Southern_Europe")
treal20<- testm %>% dplyr::filter(Reporter.Countries %in% Western_Europe) %>% dplyr::mutate(., zone="Western_Europe")

##### apilan los subsets por regiones
r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,treal15,
          treal16,treal17,treal18,treal19, treal20)

rr<- r %>% group_by(Year,flow,zone)%>% summarise(val=sum(dinero))


pots<- unique(rr$zone)

# c=7

##### Running ---------
c=19
# tryCatch(lapply(1:length(pots), function(c){

############Country -------------- 
      workdi<- "./forecasts/"
      cfiles<- filter(rr, zone==pots[c])  %>% as.data.frame()
      cfiles$val<- log10(cfiles$val)
      nf<- cfiles[nrow(cfiles),]
      #nf$Year
      ns<- cfiles[1,]
      # ns$Year
      test_i<- ts(cfiles[4], start =c(ns$Year), end =c(nf$Year) ) # , end =c()
      plot(test_i)
      
      #smooth using Holt Winter
      hw<-HoltWinters(x = test_i,gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
            plot(hw)
            plot(fitted(hw))
             
      forecast<- predict(hw,n.ahead = 10, prediction.interval = T,level = 0.95 )
      plot(hw,forecast)
      
    
      # Aplicar la funcion 
      tiff(filename= paste(workdi,pots[[c]],"_graphHW.tiff",sep=""), 
           width = 10, height = 7, units = 'in', res = 100)
      HWplot(test_i) # usando la funcion 
      dev.off()
      
      
      # cargar datos 
       load(paste("./forecasts/", pots[[c]], ".RData", sep = ""))
      
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
      db$zone<- pots[[c]] 
      write.csv(db,file = paste("./forecasts/", pots[[c]], "OutPutHW.csv", sep = ""))


      #smooth other techniques
      test_a<- ts(cfiles[4], start =c(ns$Year), end=c(nf$Year))
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
#             plot.ts(test_a_sma)
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
      db_smooth$zone<- pots[[c]]
      write.csv(db_smooth,file = paste("./forecasts/", pots[[c]], "OutPutSmooth.csv", sep = ""))
     
      
      
#       return(c)
      
      
# })) 


### load data------ 
shockW<- list.files(path="./forecasts/", pattern = "OutPutSmooth", all.files = T,full.names = T)
shockW<- lapply(shockW, read.csv ,header=T)
shockW<- do.call(rbind, shockW)
shockW$X<- NULL 
shockW<- shockW  %>% spread(des,val)



shockH<- list.files(path="./forecasts/", pattern = "OutPutHW.csv", all.files = T,full.names = T)
shockH<- lapply(shockH, read.csv ,header=T)
shockH<- do.call(rbind, shockH)
shockH$X<- NULL 
par<- c("fitted", "forecast")
shockH<- shockH %>% dplyr::filter(des %in% par)
shockH<- shockH  %>% spread(time,val)
shockH$mbase<- rowMeans(shockH[c(18:20)],na.rm=T)
shockH$npredic<- rowMeans(shockH[c(41:44)],na.rm=T)
shockH<- shockH %>% select(zone, mbase,npredic)
shockH$zone<- as.character(shockH$zone)
b1<- shockH[c("zone", "mbase")]
b2<- shockH[c("zone", "npredic")]

ct<- left_join(b1,b2, by=c("zone"))
ct<- ct[!is.na(ct$npredic),]

### to find the time series exports-imports-re for each country

#### foresight to exports future
###### projections by suregion
#### SNA analysis

yr<- unique(rice$Year)
yr<- sort(yr)
f<- unique(rice$flow)
rr<- rice %>% split(rice$flow)

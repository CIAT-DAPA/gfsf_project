### RTB crops una vez mas, Carlos Edo.

### Carlos Eduardo Gonzalez R. 
### RTB Analysis
g=gc;rm(list = ls())

# librerias------------
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
# suppressMessages(library(dplyr))
# suppressMessages(library(tidyverse)) 
# suppressMessages(library(modelr)) 
# suppressMessages(library(purrr)) 
# suppressMessages(library(broom)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(corrplot)) 
suppressMessages(library(FactoMineR)) 
suppressMessages(library(factoextra)) 
suppressMessages(library(cluster)) 
# suppressMessages(library(RCurl)) 
suppressMessages(library(ggthemes)) 
# suppressMessages(library(tidyquant))
suppressMessages(library(devtools))
suppressMessages(library(mvoutlier))
suppressMessages(library(R.utils))
suppressMessages(library(RColorBrewer))



# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
# 

abbreviate("percentage") 
options(warn = -1); options(scipen = 999) 
options(digits=3) 



############################################################# BIG Regions ####################################################################

rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
#rdsFiles<-c("/mnt/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
# Big Regions
r<- c("EAP", "EUR","LAC", "MEN", "NAM", "SAS", "SSA")


s<- c("SSP2-HGEM-HiYld2","SSP2-HGEM-RegYld2","SSP2-HGEM-HiNARS2", "SSP2-HGEM-MMEFF2","SSP2-HGEM2")

# Parametro 2 All Countries.
r2<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA", "Africa","Americas","DVD", "DVG")
r3<- c("Africa","Americas", "Asia","Europe", "Oceania")
r4<- c("Australia and New Zealand","Caribbean","Central America", "Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia",
       "Middle Africa","Northern Africa","Northern America","Northern Europe","South America","South-Eastern Asia","Southern Africa","Southern Asia",
       "Southern Europe","Western Africa","Western Asia", "Western Europe", "Western and Central Asia")
r5<- c("MENg","EAPg")
rall<- c(r, r2,r3,r4, r5)
jrtb<- c("jbana","jcass", "jpota", "jswpt","jyams","jorat")

africa<- c("MEN-Algeria","MEN-Egypt","MEN-Libya", "MEN-Mauritania","MEN-Morocco","MEN-Tunisia",
           "SSA-Angola", "SSA-Benin",  "SSA-Botswana","SSA-Burkina Faso",
           "SSA-Burundi", "SSA-Cameroon", "SSA-Central African Rep.", "SSA-Chad","SSA-Congo", "SSA-Djibouti",
           "SSA-DRC","SSA-Equatorial Guinea", "SSA-Eritrea", "SSA-Ethiopia","SSA-Gabon",
           "SSA-Gambia","SSA-Ghana", "SSA-Guinea", "SSA-Guinea-Bissau", "SSA-Ivory Coast",
           "SSA-Kenya", "SSA-Lesotho",  "SSA-Liberia","SSA-Madagascar","SSA-Malawi",
           "SSA-Mali", "SSA-Mozambique", "SSA-Namibia",  "SSA-Niger", "SSA-Nigeria", "SSA-Rwanda",
           "SSA-Senegal", "SSA-Sierra Leon", "SSA-Somalia","SSA-South Africa",
           "SSA-Sudan", "SSA-Swaziland",  "SSA-Tanzania", "SSA-Togo",  "SSA-Uganda", "SSA-Zambia",
           "SSA-Zimbabwe")

asia<- c("EAP-Cambodia", "EAP-China","EAP-Indonesia","EAP-Japan",
         "EAP-Laos",  "EAP-Malaysia", "EAP-Mongolia", "EAP-Myanmar",
         "EAP-North Korea","EAP-Other Indian Ocean", "EAP-Other Southeast Asia","EAP-Philippines",
         "EAP-South Korea",  "EAP-Thailand", "EAP-Timor L'Este","EAP-Vietnam",
         "EUR-Cyprus", "FSU-Armenia","FSU-Azerbaijan", "FSU-Georgia",
         "FSU-Kazakhstan", "FSU-Kyrgyzstan",   "FSU-Tajikistan","FSU-Turkmenistan",
         "FSU-Uzbekistan", "MEN-Iran","MEN-Iraq","MEN-Israel",
         "MEN-Jordan", "MEN-Lebanon", "MEN-Palestine","MEN-Rest of Arabia",
         "MEN-Saudi Arabia", "MEN-Syria", "MEN-Turkey","MEN-Yemen",
         "SAS-Afghanistan","SAS-Bangladesh", "SAS-Bhutan", "SAS-India",
         "SAS-Nepal","SAS-Pakistan","SAS-Sri Lanka")

other<- c("EUR-Albania", "EUR-Austria","EUR-Baltic States",
        "EUR-Belgium-Luxembourg",  "EUR-Bulgaria",
        "EUR-Croatia",  "EUR-Czech Republic","EUR-Denmark",
        "EUR-Finland", "EUR-France", "EUR-Germany",
        "EUR-Greece", "EUR-Hungary","EUR-Iceland",  "EUR-Ireland", "EUR-Italy",
        "EUR-Netherlands", "EUR-Norway","EUR-Other Balkans","EUR-Poland",
        "EUR-Portugal","EUR-Romania", "EUR-Slovakia", "EUR-Slovenia",
        "EUR-Spain", "EUR-Sweden","EUR-Switzerland", "EUR-UK",
        "FSU-Belarus","FSU-Moldova", "FSU-Russia", "FSU-Ukraine",
        "SSA-Other Atlantic", "NAM-Canada","NAM-Greenland","NAM-USA")

t<- c(2010, 2030,2050)
# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Cassava","F&V-Banana", "F&V-Plantain") #"R&T-Other Roots"
rtb2<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Cassava","F&V-Banana", "F&V-Plantain","R&T-Other Roots")

# cargo datos de area y rendimiento
cfiles<-list.files(path = rdsFiles, pattern = "datatotal.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
cdata<- do.call(rbind,cdata)

# quitar comillas
cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
cdata$Commodity<-  gsub("'", '',cdata$Commodity)
cdata$Regions<-  gsub("'", '',cdata$Regions)
cdata$Year<-  gsub("'",'',cdata$Year)

cdata$parameter<- as.character( cdata$parameter)
cdata$Scenarios<- as.character( cdata$Scenarios)
cdata$Commodity<- as.character( cdata$Commodity)
cdata$Regions<- as.character( cdata$Regions) 

cdata<- dplyr::filter(cdata, Scenarios %in% s)


##### Graph area, rendimiento----------------
yfiled<- cdata
yfiled<- dplyr::filter(yfiled, !Regions %in% rall) 
yfiled<- dplyr::filter(yfiled, Commodity %in% rtb)
yfiled<- dplyr::filter(yfiled, Year==2050)
yfiled<- yfiled %>% spread(Scenarios, Val)


# Asunto banana
require(dplyr)
banana<- filter(yfiled, Commodity=="F&V-Banana" |  Commodity=="F&V-Plantain")
colnames(banana)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
banana$Year<- NULL
banana<- banana %>% gather(Scenarios, Val, 4:ncol(banana)) %>% 
      spread(parameter, Val)

proof<- banana %>% group_by(Regions,Scenarios) %>% summarise(TYldXAgg= sum(QSupXAgg,na.rm=TRUE)/sum(TAreaXAgg,na.rm=TRUE),TAreaXAgg= sum(TAreaXAgg,na.rm=TRUE),QSupXAgg=sum(QSupXAgg) )
proof$Commodity<- "Banana"
proof<- proof %>%  gather(parameter,Val, 3:5)
proof<- proof %>%  spread(Scenarios,Val)
proof<- proof[c("Commodity", "Regions", "parameter", "HIGH+NARS", "HIGH", "RMM", "REGION","REF")]
      
################################# Regions #############################

#africa
proof_africa<- filter(proof,Regions %in% africa )
proof_africa$reg<- "Africa"
# asia
proof_asia<- filter(proof,Regions %in% asia)
proof_asia$reg<- "Asia"

#LAC
proof_lac<- proof[grep(pattern ="LAC",x = proof$Regions, ignore.case = T),]
proof_lac$reg<- "Latin America & Caribbean"

#Euro and NortA
proof_other<- filter(proof,Regions %in% other)
proof_other$reg<- "Europe & N.America"

proof_world<- filter(proof, Regions=="WLD")
proof_world$reg<- "World"

# stack many dataframe
cbaba<- do.call("rbind", list(proof_africa,proof_asia,proof_lac,proof_other,proof_world))
cbaba<- cbaba[c("Commodity","Regions","reg","parameter","HIGH+NARS","HIGH","RMM","REGION","REF")]
cbaba<- cbaba %>%  gather(Scenarios,Val, 5:ncol(cbaba)) %>% spread(parameter,Val)
cbaba<- cbaba %>% group_by(reg,Scenarios) %>% 
      summarise(TYldXAgg= sum(QSupXAgg,na.rm=TRUE)/sum(TAreaXAgg,na.rm=TRUE),TAreaXAgg= sum(TAreaXAgg,na.rm=TRUE),QSupXAgg=sum(QSupXAgg) )

cbaba<- cbaba %>% gather(parameter,Val, 3:ncol(cbaba))%>% spread(Scenarios,Val)

#agregacion por regiones 

cbaba$HIGH.NARSdif<- ((cbaba$`HIGH+NARS`- cbaba$REF)/cbaba$REF)*100
cbaba$HIGHdif<- ((cbaba$HIGH- cbaba$REF)/cbaba$REF)*100
cbaba$RMMdif<- ((cbaba$RMM- cbaba$REF)/cbaba$REF)*100
cbaba$REGIONdif<- ((cbaba$REGION- cbaba$REF)/cbaba$REF)*100
cbaba<- cbaba[,-c(3:7)]
cbaba$Commodity<-"Banana"
cbaba<- cbaba[c( "Commodity","reg","parameter", "HIGH.NARSdif", "HIGHdif","RMMdif", "REGIONdif")]
colnames(cbaba)<- c("Commodity", "Region","parameter", "HIGH+NARS","HIGH","RMM", "REGION")
cbaba<- as.data.frame(cbaba)
cbaba<- cbaba %>% gather(Scenarios, Percentage, 4:ncol(cbaba))

# continuo  anothers crops
yfiled<- yfiled[which(yfiled$Commodity!="F&V-Banana" ),]
yfiled<- yfiled[which(yfiled$Commodity!="F&V-Plantain" ),]
colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )

#africa
p_africa<- filter(yfiled,Regions %in% africa )
p_africa$reg<- "Africa"
# asia
p_asia<- filter(yfiled,Regions %in% asia)
p_asia$reg<- "Asia"

#LAC
p_lac<- yfiled[grep(pattern ="LAC",x = yfiled$Regions, ignore.case = T),]
p_lac$reg<- "Latin America & Caribbean"

#Euro and NortA
p_other<- filter(yfiled,Regions %in% other)
p_other$reg<- "Europe & N.America"

p_world<- filter(yfiled, Regions=="WLD")
p_world$reg<- "World"

# stack many dataframe
cy<- do.call("rbind", list(p_africa,p_asia,p_lac,p_other,p_world))
cy<- cy[c("Commodity","Regions","reg","parameter","HIGH+NARS","HIGH","RMM","REGION","REF")]
cy<- cy %>%  gather(Scenarios,Val, 5:ncol(cy)) %>% spread(parameter,Val)
cy<- cy %>% group_by(reg,Scenarios,Commodity) %>% 
      summarise(TYldXAgg= sum(QSupXAgg,na.rm=TRUE)/sum(TAreaXAgg,na.rm=TRUE),TAreaXAgg= sum(TAreaXAgg,na.rm=TRUE),QSupXAgg=sum(QSupXAgg) )

cy<- cy %>% gather(parameter,Val, 4:ncol(cy))%>% spread(Scenarios,Val)

cy$HIGH.NARSdif<- ((cy$`HIGH+NARS`- cy$REF)/cy$REF)*100
cy$HIGHdif<- ((cy$HIGH- cy$REF)/cy$REF)*100
cy$RMMdif<- ((cy$RMM- cy$REF)/cy$REF)*100
cy$REGIONdif<- ((cy$REGION- cy$REF)/cy$REF)*100

cy<- cy[,-c(4:8)]
cy<- cy[c( "Commodity","reg","parameter", "HIGH.NARSdif", "HIGHdif","RMMdif", "REGIONdif")]
colnames(cy)<- c("Commodity", "Region","parameter", "HIGH+NARS","HIGH","RMM", "REGION")
cy<- as.data.frame(cy)
cy<- cy %>% gather(Scenarios, Percentage, 4:ncol(cy))

# yfiled<- filter(yfiled, parameter!="QSupXAgg")


# join both datasets
cf<- rbind(cbaba,cy)


# # jam
zones<- c("Asia","Europe & N.America")
b1<- cf[which(cf$Commodity=="R&T-Yams"),]
b1<- filter(b1, !Region %in% zones) 


# # sweet potato
b2<- cf[which(cf$Commodity=="R&T-Sweet Potato"),]
b2<- filter(b2,Region!="Europe & N.America") 

## cassava
b3<- cf[which(cf$Commodity=="R&T-Cassava"),]
b3<- filter(b3,Region!="Europe & N.America") 


##bana&plantain
b4<- cf[which(cf$Commodity=="Banana"),]
b4<- filter(b4,Region!="Europe & N.America") 


# join
cf<- filter(cf, Commodity!="R&T-Sweet Potato") 
cf<- filter(cf, Commodity!="R&T-Yams") 
cf<- filter(cf, Commodity!="R&T-Cassava") 
cf<- filter(cf, Commodity!="Banana") 

efiles<- rbind(cf,b1)
efiles<- rbind(efiles,b2)
efiles<- rbind(efiles,b3)
efiles<- rbind(efiles,b4)

efiles<- filter(efiles, Region!="World")

write.csv(cf,paste(rdsFiles,"Version2YieldDiffReference_HeapMapPP.csv",sep=""))

## grafico yield
efiles$Commodity<-  gsub("R&T-", '',efiles$Commodity)
efiles$Commodity<- plyr::revalue(efiles$Commodity, c("Yams"="Yam","Sweet Potato"="Sweetpotato"))
efiles$parameter<- plyr::revalue(efiles$parameter, c("QSupXAgg"="Supply","TAreaXAgg"="Area","TYldXAgg"="Yield"))
efiles$parameter <- factor(efiles$parameter, levels = c("Supply","Yield","Area"))


breaks <- seq(from=min(range(efiles$Percentage)), to=max(range(efiles$Percentage)), length.out=100)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab') 


png(filename=paste(rdsFiles,"YieldDiffReference_HeapMapTest.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =efiles, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Percentage), colour = "white")+  facet_grid(Commodity~parameter,drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100), limits=c(-52,52))+ labs(x = "",y = "") +  
      coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  


##### Food Ava--------------

# cargo datos de area y rendimiento
cfiles<-list.files(path = rdsFiles, pattern = "TradeFood.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
cdata<- do.call(rbind,cdata)
cdata<- filter(cdata, parameter=="PerCapKCalCXAgg")

# quitar comillas
cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
cdata$Commodity<-  gsub("'", '',cdata$Commodity)
cdata$Regions<-  gsub("'", '',cdata$Regions)
cdata$Year<-  gsub("'",'',cdata$Year)

cdata$parameter<- as.character( cdata$parameter)
cdata$Scenarios<- as.character( cdata$Scenarios)
cdata$Commodity<- as.character( cdata$Commodity)
cdata$Regions<- as.character( cdata$Regions) 

cdata<- filter(cdata, Scenarios %in% s)

#filtros mas especificos 
y<- cdata
y<- filter(y, Regions %in% r) 
yall<- filter(y, Commodity=="All")
yrtb<- filter(y, Commodity %in% rtb2)
# filtros años
yall<- filter(yall, Year==2050)
yrtb<- filter(yrtb, Year==2050)

##all
yall<- yall %>% spread(Scenarios, Val)
colnames(yall)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
yall$HIGH.NARSdif<- ((yall$`HIGH+NARS`- yall$REF)/yall$REF)*100
yall$HIGHdif<- ((yall$HIGH- yall$REF)/yall$REF)*100
yall$RMMdif<- ((yall$RMM- yall$REF)/yall$REF)*100
yall$REGIONdif<- ((yall$REGION- yall$REF)/yall$REF)*100
all<- yall
all<- all[,-c(5:9)]
colnames(all)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION")
all$Year<- NULL
all<- all %>% gather(Scenarios, Percentage, 4:ncol(all))
all$parameter<- NULL

##rtb
yrtb<- yrtb %>% group_by(Regions,Scenarios)%>% summarise(totalPer=sum(Val,na.rm=TRUE))
yrtb$Commodity<- "RTB"

yrtb<- yrtb %>% spread(Scenarios, totalPer)
colnames(yrtb)<- c("Regions","Commodity", "HIGH+NARS","HIGH","RMM", "REGION", "REF")
yrtb<- as.data.frame(yrtb)

yrtb$HIGH.NARSdif<- ((yrtb$`HIGH+NARS`- yrtb$REF)/yrtb$REF)*100
yrtb$HIGHdif<- ((yrtb$HIGH- yrtb$REF)/yrtb$REF)*100
yrtb$RMMdif<- ((yrtb$RMM- yrtb$REF)/yrtb$REF)*100
yrtb$REGIONdif<- ((yrtb$REGION- yrtb$REF)/yrtb$REF)*100

yt<- yrtb
yt<- yt[,-c(3:7)]
colnames(yt)<- c("Regions","Commodity", "HIGH+NARS","HIGH","RMM", "REGION")
# yt$Year<- NULL
yt<- yt %>% gather(Scenarios, Percentage, 3:ncol(yt))
yt<- yt[c("Commodity", "Regions","Scenarios","Percentage")]

food<- rbind(all, yt)
write.csv(food,paste(rdsFiles,"CaloriesPERDiffReference_HeapMap.csv",sep=""))

food$parameter<- plyr::revalue(food$parameter, c("QSupXAgg"="Total Supply","TAreaXAgg"="Total Area", "TYldXAgg"="Total Yield"))
efiles$parameter <- factor(efiles$parameter, levels = c("Total Yield","Total Area","Total Supply"))
efiles$Commodity<-  gsub("R&T-", '',efiles$Commodity)

## grafico yield

breaks <- seq(from=min(range(food$Percentage)), to=max(range(food$Percentage)), length.out=100)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(rdsFiles,"CaloriesPERDiffReference_HeapMapTest.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =food, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Percentage), colour = "white")+  facet_grid(Commodity~., drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100),limits=c(-12,12))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

### Ratios

yrtb<- as.data.frame(yrtb)
yrtb<- yrtb[,c("Commodity", "Regions", "HIGH+NARS", "HIGH", "RMM", "REGION", "REF")]
yall$parameter<- NULL
yall$Year<- NULL
yall<- yall[,c("Commodity", "Regions","HIGH+NARS","HIGH", "RMM","REGION","REF")]

ration<- rbind(yall, yrtb)
ration<- ration %>% gather(Sce, Val, 3:ncol(ration))
ration<- ration %>% spread(Commodity, Val)
ration$Rate<- (ration$RTB/ration$All)*100

ration$Sce <- factor(ration$Sce, levels = c("REF","HIGH","HIGH+NARS","REGION","RMM"))
write.csv(ration,paste(rdsFiles,"ratioCalories_HeapMap.csv",sep=""))



hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename= paste(rdsFiles,"ratioCalories_HeapMap.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =ration, aes(Sce, Regions)) + 
      geom_tile(aes(fill = Rate), colour = "white")+  #facet_grid(Commodity~., drop = T)+
      labs(x=NULL, y=NULL, title=paste("Ratios in per capita kilocalories available ( RTB crops/All Crops) per investment scenario\n by 2050 compared to baseline results (HGEM) ",sep = "")) +
      scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

##### Demand, supply-----------
# cargo datos de area y rendimiento
# cfiles<-list.files(path = rdsFiles, pattern = "TradeFood.rds",full.names = T)
# cfiles<- lapply(cfiles, readRDS)
# cdata<-cfiles
# cdata<- do.call(rbind,cdata)
# variables<- c("QDXAgg", "QSupXAgg")
# cdata<- filter(cdata, parameter %in% variables)
# cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
# cdata$Commodity<-  gsub("'", '',cdata$Commodity)
# cdata$Regions<-  gsub("'", '',cdata$Regions)
# cdata$Year<-  gsub("'",'',cdata$Year)
# 
# cdata$parameter<- as.character( cdata$parameter)
# cdata$Scenarios<- as.character( cdata$Scenarios)
# cdata$Commodity<- as.character( cdata$Commodity)
# cdata$Regions<- as.character( cdata$Regions) 
# 
# cdata<- filter(cdata, Scenarios %in% s)
# cdata<- filter(cdata, Commodity %in% rtb)
# cdata<- filter(cdata, Regions %in% r) 
# 
# cdata<- filter(cdata, Year==2050)
# yfiled<- cdata %>% spread(Scenarios, Val)
# 
# 
# # filter banana
# banana<- filter(yfiled, Commodity=="F&V-Banana" |  Commodity=="F&V-Plantain")
# colnames(banana)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
# banana$Year<- NULL
# banana<- banana %>% gather(Scenarios, Val, 4:ncol(banana)) %>% 
#       spread(parameter, Val)
# 
# proof<- banana %>% group_by(Regions,Scenarios) %>% summarise(QDXAgg= sum(QDXAgg,na.rm=TRUE),QSupXAgg= sum(QSupXAgg,na.rm=TRUE))
# proof$Commodity<- "Banana&Plantain"
# proof<- proof %>%  gather(parameter,Val, 3:4)
# proof<- proof %>%  spread(Scenarios,Val)
# 
# 
# proof$HIGH.NARSdif<- ((proof$`HIGH+NARS`- proof$REF)/proof$REF)*100
# proof$HIGHdif<- ((proof$HIGH- proof$REF)/proof$REF)*100
# proof$RMMdif<- ((proof$RMM- proof$REF)/proof$REF)*100
# proof$REGIONdif<- ((proof$REGION- proof$REF)/proof$REF)*100
# proof<- proof[,-c(4:8)]
# colnames(proof)<- c("Regions","Commodity", "parameter", "HIGH+NARS","HIGH","RMM", "REGION")
# 
# proof<- proof %>% gather(Scenarios, Percentage, 4:ncol(proof))
# proof<- as.data.frame(proof)
# proof<- proof[c("Commodity","Regions", "parameter","Scenarios","Percentage")]
# 
# # continuo
# yfiled<- yfiled[which(yfiled$Commodity!="F&V-Banana" ),]
# yfiled<- yfiled[which(yfiled$Commodity!="F&V-Plantain" ),]
# colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
# yfiled$HIGH.NARSdif<- ((yfiled$`HIGH+NARS`- yfiled$REF)/yfiled$REF)*100
# yfiled$HIGHdif<- ((yfiled$HIGH- yfiled$REF)/yfiled$REF)*100
# yfiled$RMMdif<- ((yfiled$RMM- yfiled$REF)/yfiled$REF)*100
# yfiled$REGIONdif<- ((yfiled$REGION- yfiled$REF)/yfiled$REF)*100
# yfiled<- yfiled[,-c(5:9)]
# colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION")
# yfiled$Year<- NULL
# yfiled<- yfiled %>% gather(Scenarios, Percentage, 4:ncol(yfiled))
# 
# #unir datos
# cf<- rbind(yfiled,proof)
# cf<- na.omit(cf)
# 
# write.csv(cf,paste(rdsFiles,"SupplyDemandDiffReference_HeapMapPP.csv",sep=""))
# ## grafico yield
# 
# hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
# png(filename=paste(rdsFiles,"SupplyDemandDiffReference_HeapMapPP.png",sep=""), 
#     width = 12, height = 12, units = 'in', res = 300)
# 
# 
# n<- ggplot(data =cf, aes(Scenarios, Regions)) + 
#       geom_tile(aes(fill = Percentage), colour = "white")+  facet_grid(Commodity~parameter, drop = T)+
#       labs(x=NULL, y=NULL, title=paste("Changes in Demand and Supply per investment scenario\n by 2050 compared to baseline results (HGEM) ",sep = "")) +
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
# 
# 
# plot(n)
# dev.off()  

##### Graficos RTB crops like group---------
# cargo datos de area y rendimiento
cfiles<-list.files(path = rdsFiles, pattern = "TradeFood.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
cdata<- do.call(rbind,cdata)
variables<- c("QDXAgg", "QSupXAgg", "FoodAvailXAgg")
cdata<- filter(cdata, parameter %in% variables)
cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
cdata$Commodity<-  gsub("'", '',cdata$Commodity)
cdata$Regions<-  gsub("'", '',cdata$Regions)
cdata$Year<-  gsub("'",'',cdata$Year)

cdata$parameter<- as.character( cdata$parameter)
cdata$Scenarios<- as.character( cdata$Scenarios)
cdata$Commodity<- as.character( cdata$Commodity)
cdata$Regions<- as.character( cdata$Regions) 

cdata<- filter(cdata, Scenarios %in% s)
cdata<- filter(cdata, Commodity %in% rtb2)
cdata<- filter(cdata, Regions %in% r) 

cdata<- filter(cdata, Year==2050)
yfiled<- cdata %>% spread(Scenarios, Val)


# filter banana
banana<- filter(yfiled, Commodity=="F&V-Banana" |  Commodity=="F&V-Plantain")
colnames(banana)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
banana$Year<- NULL
banana<- banana %>% gather(Scenarios, Val, 4:ncol(banana)) %>% 
      spread(parameter, Val)

proof<- banana %>% group_by(Regions,Scenarios) %>% summarise(QDXAgg= sum(QDXAgg,na.rm=TRUE),QSupXAgg= sum(QSupXAgg, na.rm=TRUE),FoodAvailXAgg=sum(FoodAvailXAgg,na.rm=TRUE) )
proof$Commodity<- "Banana"
proof<- proof %>%  gather(parameter,Val, 3:5)
proof<- proof %>%  spread(Scenarios,Val)
proof<- as.data.frame(proof)

# continuo
yfiled<- yfiled[which(yfiled$Commodity!="F&V-Banana"),]
yfiled<- yfiled[which(yfiled$Commodity!="F&V-Plantain"),]
colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
yfiled$Year<- NULL

#organizar los datos
proof<- proof[c("Commodity","Regions","parameter", "HIGH+NARS","HIGH","RMM", "REGION","REF" )]
proof<- as.data.frame(proof)

#unir datos
cf<- rbind(yfiled,proof)
rownames(cf)<-  1:nrow(cf)

cf<- cf %>% gather(Scenarios,Val,4:ncol(cf)) %>% spread(parameter, Val) %>%
      group_by(Regions,Scenarios) %>% 
      summarise(FoodAvailXAgg=sum(FoodAvailXAgg,na.rm=TRUE),QDXAgg=sum(QDXAgg,na.rm=TRUE), QSupXAgg =sum(QSupXAgg,na.rm=TRUE)) %>% 
      gather(parameter, Val, 3:ncol(.)) %>% 
      spread(Scenarios, Val)
cf<- as.data.frame(cf)


cf$HIGH.NARSdif<- ((cf$`HIGH+NARS`- cf$REF)/cf$REF)*100
cf$HIGHdif<- ((cf$HIGH- cf$REF)/cf$REF)*100
cf$RMMdif<- ((cf$RMM- cf$REF)/cf$REF)*100
cf$REGIONdif<- ((cf$REGION- cf$REF)/cf$REF)*100
cf<- cf[,-c(3:7)]
cf<- cf[c("Regions","parameter","HIGH.NARSdif","HIGHdif","RMMdif","REGIONdif")]
colnames(cf)<- c("Regions","parameter","HIGH+NARS","HIGH","RMM","REGION")
cf<- cf %>% gather(Sce, Percentage, 3:ncol(.))

cf<- na.omit(cf)
write.csv(cf,paste(rdsFiles,"SupplyDemandFoodDiffReference_HeapMapPPV1V2.csv",sep=""))

## grafico yield
cf$parameter<- plyr::revalue(cf$parameter, c("FoodAvailXAgg"="Food Availability","QDXAgg"="Total Demand", "QSupXAgg"="Supply"))
cf$parameter <- factor(cf$parameter, levels = c("Food Availability","Total Demand","Supply"))

breaks <- seq(from=min(range(cf$Percentage)), to=max(range(cf$Percentage)), length.out=100)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab') 


##V1
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(rdsFiles,"SupplyDemandFoodDiffReference_HeapMapTESTV1.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =cf, aes(Sce, Regions)) + 
      geom_tile(aes(fill = Percentage) , colour = "white")+  facet_grid(.~parameter, drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100), limits=c(-40,40))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

##V2
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(rdsFiles,"SupplyDemandFoodDiffReference_HeapMapTESTV2.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =cf, aes(Sce, Regions)) + 
      geom_tile(aes(fill = Percentage) , colour = "white")+  facet_grid(parameter~., drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100), limits=c(-40,40))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

#### Cereals---------
cereals<- c("CER-Barley","CER-Maize", "CER-Millet", "CER-Rice", "CER-Sorghum", "CER-Wheat", "CER-Other Cereals")


# cargo datos de area y rendimiento
cfiles<-list.files(path = rdsFiles, pattern = "TradeFood.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
cdata<- do.call(rbind,cdata)
cdata<- filter(cdata, parameter=="PerCapKCalCXAgg")

# quitar comillas
cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
cdata$Commodity<-  gsub("'", '',cdata$Commodity)
cdata$Regions<-  gsub("'", '',cdata$Regions)
cdata$Year<-  gsub("'",'',cdata$Year)

cdata$parameter<- as.character( cdata$parameter)
cdata$Scenarios<- as.character( cdata$Scenarios)
cdata$Commodity<- as.character( cdata$Commodity)
cdata$Regions<- as.character( cdata$Regions) 

cdata<- filter(cdata, Scenarios %in% s)

#filtros mas especificos 
y<- cdata
y<- filter(y, Regions %in% r) 
yall<- filter(y, Commodity=="All")
yrtb<- filter(y, Commodity %in% cereals)
# filtros años
yall<- filter(yall, Year==2050)
yrtb<- filter(yrtb, Year==2050)

##all
yall<- yall %>% spread(Scenarios, Val)
colnames(yall)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
yall$HIGH.NARSdif<- ((yall$`HIGH+NARS`- yall$REF)/yall$REF)*100
yall$HIGHdif<- ((yall$HIGH- yall$REF)/yall$REF)*100
yall$RMMdif<- ((yall$RMM- yall$REF)/yall$REF)*100
yall$REGIONdif<- ((yall$REGION- yall$REF)/yall$REF)*100
all<- yall
all<- all[,-c(5:9)]
colnames(all)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION")
all$Year<- NULL
all<- all %>% gather(Scenarios, Percentage, 4:ncol(all))
all$parameter<- NULL

##rtb
yrtb<- yrtb %>% group_by(Regions,Scenarios)%>% summarise(totalPer=sum(Val))
yrtb$Commodity<- "Cereals"

yrtb<- yrtb %>% spread(Scenarios, totalPer)
colnames(yrtb)<- c("Regions","Commodity", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )

yrtb$HIGH.NARSdif<- ((yrtb$`HIGH+NARS`- yrtb$REF)/yrtb$REF)*100
yrtb$HIGHdif<- ((yrtb$HIGH- yt$REF)/yrtb$REF)*100
yrtb$RMMdif<- ((yrtb$RMM- yt$REF)/yrtb$REF)*100
yrtb$REGIONdif<- ((yrtb$REGION- yrtb$REF)/yrtb$REF)*100

yt<- yrtb
yt<- yt[,-c(3:7)]
colnames(yt)<- c("Regions","Commodity", "HIGH+NARS","HIGH","RMM", "REGION")
# yt$Year<- NULL
yt<- yt %>% gather(Scenarios, Percentage, 3:ncol(yt))
yt<- yt[c( "Commodity", "Regions","Scenarios","Percentage")]

yt<- as.data.frame(yt)
food<- rbind(all, yt)


write.csv(food,paste(rdsFiles,"CerealsCaloriesPERDiffReference_HeapMap.csv",sep=""))
## grafico yield

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(rdsFiles,"CaloriesPERDiffReference_HeapMap.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =food, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Percentage), colour = "white")+  facet_grid(Commodity~., drop = T)+
      labs(x=NULL, y=NULL, title=paste("Changes in per capita kilocalories available (All and RTB crops) per investment scenario\n by 2050 compared to baseline results (HGEM) ",sep = "")) +
      scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

###

yrtb<- as.data.frame(yrtb)
yrtb<- yrtb[c("Commodity", "Regions","HIGH+NARS", "HIGH", "RMM", "REGION", "REF", "HIGH.NARSdif", "HIGHdif", "RMMdif", "REGIONdif")]
yrtb<- yrtb[,c("Commodity", "Regions", "HIGH+NARS", "HIGH", "RMM", "REGION", "REF")]
yall$parameter<- NULL
yall$Year<- NULL
yall<- yall[,c("Commodity", "Regions","HIGH+NARS","HIGH", "RMM","REGION","REF")]

ration<- rbind(yall, yrtb)
ration<- ration %>% gather(Sce, Val, 3:ncol(ration))
ration<- ration %>% spread(Commodity, Val)
ration$Rate<- (ration$Cereals/ration$All)*100

ration$Sce <- factor(ration$Sce, levels = c("REF","HIGH","HIGH+NARS","REGION","RMM"))

write.csv(ration,paste(rdsFiles,"CerealRatioCalories_HeapMap.csv",sep=""))

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename= paste(rdsFiles,"ratioCalories_HeapMap.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =ration, aes(Sce, Regions)) + 
      geom_tile(aes(fill = Rate), colour = "white")+  #facet_grid(Commodity~., drop = T)+
      labs(x=NULL, y=NULL, title=paste("Ratios in per capita kilocalories available ( RTB crops/All Crops) per investment scenario\n by 2050 compared to baseline results (HGEM) ",sep = "")) +
      scale_fill_gradientn(colours = hm.palette(100))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  

#### Adjust graph supply total-----

# cargo datos de area y rendimiento
cfiles<-list.files(path = rdsFiles, pattern = "TradeFood.rds",full.names = T)
cfiles<- lapply(cfiles, readRDS)
cdata<-cfiles
cdata<- do.call(rbind,cdata)
variables<- c("QDXAgg", "QSupXAgg", "FoodAvailXAgg")
cdata<- filter(cdata, parameter %in% variables)
cdata$Scenarios<-  gsub("'",'',cdata$Scenarios)
cdata$Commodity<-  gsub("'", '',cdata$Commodity)
cdata$Regions<-  gsub("'", '',cdata$Regions)
cdata$Year<-  gsub("'",'',cdata$Year)

cdata$parameter<- as.character( cdata$parameter)
cdata$Scenarios<- as.character( cdata$Scenarios)
cdata$Commodity<- as.character( cdata$Commodity)
cdata$Regions<- as.character( cdata$Regions) 

cdata<- filter(cdata, Scenarios %in% s)
cdata<- filter(cdata, Commodity %in% rtb2)
cdata<- filter(cdata, Regions %in% r) 

cdata<- filter(cdata, Year==2050)
yfiled<- cdata %>% spread(Scenarios, Val)


# filter banana
banana<- filter(yfiled, Commodity=="F&V-Banana" |  Commodity=="F&V-Plantain")
colnames(banana)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
banana$Year<- NULL
banana<- banana %>% gather(Scenarios, Val, 4:ncol(banana)) %>% 
      spread(parameter, Val)

proof<- banana %>% group_by(Regions,Scenarios) %>% summarise(QDXAgg= sum(QDXAgg,na.rm=T),QSupXAgg= sum(QSupXAgg,na.rm=T),FoodAvailXAgg=sum(FoodAvailXAgg,na.rm=T) )
proof$Commodity<- "Banana"
proof<- proof %>%  gather(parameter,Val, 3:5)
proof<- proof %>%  spread(Scenarios,Val)

proof$HIGH.NARSdif<- ((proof$`HIGH+NARS`- proof$REF)/proof$REF)*100
proof$HIGHdif<- ((proof$HIGH- proof$REF)/proof$REF)*100
proof$RMMdif<- ((proof$RMM- proof$REF)/proof$REF)*100
proof$REGIONdif<- ((proof$REGION- proof$REF)/proof$REF)*100
proof<- proof[,-c(4:8)]
colnames(proof)<- c("Regions","Commodity", "parameter", "HIGH+NARS","HIGH","RMM", "REGION")

proof<- proof %>% gather(Scenarios, Percentage, 4:ncol(proof))
proof<- as.data.frame(proof)
proof<- proof[c("Commodity","Regions", "parameter","Scenarios","Percentage")]
proof<- na.omit(proof)

# continuo
yfiled<- yfiled[which(yfiled$Commodity!="F&V-Banana" ),]
yfiled<- yfiled[which(yfiled$Commodity!="F&V-Plantain" ),]
colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION", "REF" )
yfiled$HIGH.NARSdif<- ((yfiled$`HIGH+NARS`- yfiled$REF)/yfiled$REF)*100
yfiled$HIGHdif<- ((yfiled$HIGH- yfiled$REF)/yfiled$REF)*100
yfiled$RMMdif<- ((yfiled$RMM- yfiled$REF)/yfiled$REF)*100
yfiled$REGIONdif<- ((yfiled$REGION- yfiled$REF)/yfiled$REF)*100
yfiled<- yfiled[,-c(5:9)]
colnames(yfiled)<- c("Commodity", "Regions","Year","parameter", "HIGH+NARS","HIGH","RMM", "REGION")
yfiled$Year<- NULL
yfiled<- yfiled %>% gather(Scenarios, Percentage, 4:ncol(yfiled))

#unir datos
cf<- rbind(yfiled,proof)
cf<- na.omit(cf)


## filter regions 
# # jam
b1<- cf[which(cf$Commodity=="R&T-Yams"),]
b1<- filter(b1,Regions=="LAC" | Regions=="SSA" ) 


# # sweet potato
b2<- cf[which(cf$Commodity=="R&T-Sweet Potato"),]
b2<- filter(b2,Regions=="EAP"|Regions=="LAC"| Regions=="SAS"|Regions=="SSA" ) 

## cassava
b3<- cf[which(cf$Commodity=="R&T-Cassava"),]
b3<- filter(b3,Regions=="EAP"|Regions=="LAC"| Regions=="SSA" ) 


##bana&plantain
b4<- cf[which(cf$Commodity=="Banana"),]
b4<- filter(b4,Regions=="EAP"|Regions=="LAC"| Regions=="SSA" | Regions=="SSA") 


# join
cf<- filter(cf, Commodity!="R&T-Sweet Potato") 
cf<- filter(cf, Commodity!="R&T-Yams") 
cf<- filter(cf, Commodity!="R&T-Cassava") 
cf<- filter(cf, Commodity!="Banana") 

efiles<- rbind(cf,b1)
efiles<- rbind(efiles,b2)
efiles<- rbind(efiles,b3)
efiles<- rbind(efiles,b4)

breaks <- seq(from=min(range(efiles$Percentage)), to=max(range(efiles$Percentage)), length.out=100)

write.csv(cf,paste(rdsFiles,"SupplyDemandFoodDiffReference_HeapMapPPV3V4.csv",sep=""))
efiles$parameter<- plyr::revalue(efiles$parameter, c("FoodAvailXAgg"="Food Availability","QDXAgg"="Total Demand", "QSupXAgg"="Supply"))
efiles$parameter <- factor(efiles$parameter, levels = c("Food Availability","Total Demand","Supply"))
efiles$Commodity<-  gsub("R&T-", '',efiles$Commodity)
efiles$Commodity<-  plyr::revalue(efiles$Commodity,c("Sweet Potato"="Sweetpotato", "Yams"="Yam"))

efiles<- filter(efiles, Commodity!="Other Roots")

## V3
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(rdsFiles,"SupplyDemandFoodDiffReference_HeapMapTESTV3.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =efiles, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Percentage) , colour = "white")+  facet_grid(Commodity~parameter, drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100),limits=c(-55,55))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  


## V4

hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
png(filename=paste(rdsFiles,"SupplyDemandFoodDiffReference_HeapMapTESTV4.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =efiles, aes(Scenarios, Regions)) + 
      geom_tile(aes(fill = Percentage) , colour = "white")+  facet_grid(parameter~Commodity, drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100),limits=c(-55,55))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  


##otros graficos--------------------

#g1
th<- read.csv(paste(rdsFiles,"g1.csv", sep = ""))
colnames(th)[2]<- "2050 Baseline"
colnames(th)[3]<- "HIGH+NARS"

th<- th %>% gather(Sce,Val, 2:ncol(th))

th$Sce <- factor(th$Sce, levels = c("2050 Baseline","HIGH+NARS","HIGH","RMM","REGION"))

png(filename=paste(rdsFiles,"G1.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<-ggplot(data=th,aes(x=Regions,y=Val,fill=Sce))+geom_bar(stat="identity", position = "dodge",colour="black")+
      # scale_fill_brewer(palette = "Dark2") +
      scale_fill_brewer( type = "div" , palette = "RdBu" )+
      # theme(axis.text.x=element_text(size=14, angle = 0))+
      theme(axis.text.y=element_text(size=16, angle = 0))+
      # theme(strip.text.x = element_text(angle = 0,size = 30, face = "bold.italic"))+
      # theme(strip.text.y = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(aspect.ratio = 1)+ theme(legend.text=element_text(size=16))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 16),legend.title=element_blank())+
      labs(y="Million tons", x="")+ 
      theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
      # theme(axis.title.x = element_text(size = rel(1.8), angle = 0))

plot(n)
dev.off()  


#g2
th1<- read.csv(paste(rdsFiles,"g2.csv", sep = ""))
th1<- th1 %>% gather(Crops,Val, 2:ncol(th1))
th1$change<- (th1$Val)*100


png(filename=paste(rdsFiles,"G2.png",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<-ggplot(data=th1,aes(x=regions,y=change,fill=Crops))+geom_bar(stat="identity", position = "dodge",colour="black")+
      scale_fill_brewer(palette = "Dark2") +
      #scale_fill_brewer( type = "div" , palette = "RdBu")+
      theme(axis.text.x=element_text(size=14, angle = 0))+
      theme(axis.text.y=element_text(size=16, angle = 0))+
      theme(strip.text.x = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(aspect.ratio = 1)+ theme(legend.text=element_text(size=16))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 18),legend.title=element_blank())+
      labs(y="Net yield change\n(%)",x="")+
      theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
      theme(axis.title.x = element_text(size = rel(1.8), angle = 0)) + geom_hline(aes(yintercept=100), color="black", linetype="dashed") +
      scale_y_continuous(limits = c(0,120)) 
      

      

plot(n)
dev.off()  

#g3
th3<- read.csv(paste(rdsFiles,"g3.csv", sep = ""))
th3<- th3 %>% gather(Crops,Val, 2:ncol(th3))


th3$Crops <- factor(th3$Crops, levels = c("Yam","Banana","Cassava","Sweetpotato","Potato"))
th3$Regions <- factor(th3$Regions, levels = c("Asia","Africa","Latin America & Caribbean","Europe & North America"))

png(filename=paste(rdsFiles,"G3.png",sep=""), 
    width = 22, height = 16, units = 'in', res = 300)


n<- ggplot(data=th3,aes(x=Regions,y=Val,fill=Crops))+ geom_bar(stat='identity')+
      scale_fill_brewer(palette = "Dark2") + 
      #scale_fill_brewer( type = "div" , palette = "RdBu")+
      theme(axis.text.x=element_text(size=16, angle = 45))+
      theme(axis.text.y=element_text(size=24, angle = 0))+
      theme(strip.text.x = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(aspect.ratio = 1)+ theme(legend.text=element_text(size=26))+
      theme(axis.text.x = element_text(angle =0, hjust = 0.5,vjust=0, size = 20),legend.title=element_blank())+
      labs(y="million tons",x="")+
      theme(axis.title.y = element_text(size = rel(3), angle = 90))+
      theme(axis.title.x = element_text(size = rel(2), angle = 90)) +
      scale_y_continuous(limits = c(0,420), breaks=seq(0,420, by = 40)) 


plot(n)
dev.off()  

#g4
th4<- read.csv(paste(rdsFiles,"g4.csv", sep = ""))
colnames(th4)[5]<- "North America"
th4<- th4 %>% gather(Regions,Val, 2:(ncol(th4)-1))
th4$Regions <- factor(th4$Regions, levels = c("Africa","Asia","Europe","North America", "Oceania", "LAC"))

th4$parameter<- as.character(th4$parameter)
parameter<- unique(th4$parameter)
i=1 # production

a<- th4 %>% filter(., parameter==parameter[i])
a$Regions <- factor(a$Regions, levels = c("Africa","Asia","Europe","North America", "Oceania", "LAC"))

png(filename=paste(rdsFiles,"G4.1.png",sep=""), 
    width = 16, height = 16, units = 'in', res = 300)

a1<- ggplot(data=a,aes(x=yr,y=Val,fill=Regions))+ geom_area()+
      labs(y="million tons",x="")+ scale_fill_brewer(palette = "Dark2")+
      theme( legend.direction="horizontal",legend.position="bottom", legend.box = "horizontal",legend.title = element_blank())+
      theme(axis.text.x=element_text(size=16, angle = 45))+
      theme(axis.text.y=element_text(size=24, angle = 0))+
      theme(strip.text.x = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(aspect.ratio = 1)+ theme(legend.text=element_text(size=26))+
      theme(axis.text.x = element_text(angle =0, hjust = 0.5,vjust=0, size = 20),legend.title=element_blank())+
      labs(y="million tons",x="")+
      theme(axis.title.y = element_text(size = rel(3), angle = 90))+
      theme(axis.title.x = element_text(size = rel(2), angle = 90))+
      scale_x_continuous(limits = c(1960,2014), breaks=seq(1960,2014, by = 4))+
      scale_y_continuous(limits = c(0,1000), breaks=seq(0,1000, by = 100))
      

plot(a1)
dev.off()  


a2<- th4 %>% filter(., parameter=="RTB Total harvested areas")
a2$Regions <- factor(a2$Regions, levels = c("Africa","Asia","Europe","North America", "Oceania", "LAC"))

png(filename=paste(rdsFiles,"G4.2.png",sep=""), 
    width = 16, height = 16, units = 'in', res = 300)

a2<- ggplot(data=a2,aes(x=yr,y=Val,fill=Regions))+ geom_area()+
      labs(y="million hectares",x="")+ scale_fill_brewer(palette = "Dark2")+
      theme( legend.direction="horizontal",legend.position="bottom", legend.box = "horizontal",legend.title = element_blank())+
      theme(axis.text.x=element_text(size=16, angle = 45))+
      theme(axis.text.y=element_text(size=24, angle = 0))+
      theme(strip.text.x = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 30, face = "bold.italic"))+
      theme(aspect.ratio = 1)+ theme(legend.text=element_text(size=26))+
      theme(axis.text.x = element_text(angle =0, hjust = 0.5,vjust=0, size = 20),legend.title=element_blank())+
      theme(axis.title.y = element_text(size = rel(3), angle = 90))+
      theme(axis.title.x = element_text(size = rel(2), angle = 90))+
      scale_x_continuous(limits = c(1960,2014), breaks=seq(1960,2014, by = 4))+
      scale_y_continuous(limits = c(0,75), breaks=seq(0,75, by = 10))


plot(a2)
dev.off()  




g <- arrangeGrob(a1, a2, nrow=1) #generates g
plot(g)
ggsave(file=paste(rdsFiles,"G4.png", sep=''),g, width=28, height=20, units='in') 

      
      
      

## graficos version 16------------
v16<- read.csv(file = paste0(rdsFiles,"ThanasisData.csv"),header = t)
v16<-  v16 %>% gather(Sce, Percentage, 3:ncol(v16)) %>% mutate(Percentage2= Percentage*100)
v16$Percentage<- NULL
colnames(v16)[4]<- "Percentage"
v16$Sce<-revalue(v16$Sce, c( "HIGH.NARS"="HIGH+NARS"))
                                         

breaks <- seq(from=min(range(v16$Percentage)), to=max(range(v16$Percentage)), length.out=100)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab') 



hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
jpeg(filename=paste(rdsFiles,"Version16SupplyDemandFoodDiffReference_HeapMapTESTV.jpeg",sep=""), 
    width = 12, height = 12, units = 'in', res = 300)


n<- ggplot(data =v16, aes(Sce, Region)) + 
      geom_tile(aes(fill = Percentage) , colour = "white")+  facet_grid(.~Parameter, drop = T)+
      labs(x=NULL, y=NULL) +
      scale_fill_gradientn(colours = hm.palette(100), limits=c(-40,40))+ labs(x = "",y = "") +  #  theme_grey() 
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme_grey() + labs(x = "",y = "")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))+
      theme(axis.text.y = element_text(hjust = 1, size = 11))+
      theme(strip.text.x = element_text(size = 9, face = "bold.italic"))+
      theme(strip.text=element_text(size=8))+
      theme(strip.text.y = element_text(angle = 0,size = 11)) 


plot(n)
dev.off()  
# Yields RTB crops # Autor Carlos Eduardo Gonzalez CIAT. 
# 050/08/2017 Cali Colombia

# librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(dplyr)
library(RColorBrewer)
library(png)

# manejo de digitos
options(digits=3) 
options(scipen = 999)

#################################################### Load files ###########################################
rdsFiles<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/USAIDForGFSF/RTB_files/")
cfiles<- readRDS(file = paste(rdsFiles,"TradeFood.rds", sep = ""))

#  Cambiar la categoria de las variables  & corregir single quotes importadas de .csv
cfiles$Scenarios<- as.character(cfiles$Scenarios);cfiles$Commodity<- as.character(cfiles$Commodity)
cfiles$Regions<- as.character(cfiles$Regions) 

cfiles$Scenarios<-  gsub("'", '',cfiles$Scenarios)
cfiles$Commodity<-  gsub("'", '',cfiles$Commodity)
cfiles$Regions<-  gsub("'", '',cfiles$Regions)
cfiles$Year<-  gsub("'", '',cfiles$Year)


# List crops, creación de subgrupos por regiones usando patrones
zone<- unique(cfiles$Regions)
alc<- cfiles[grep(pattern = "LAC",x = cfiles$Regions, ignore.case = T),]
sas<- cfiles[grep(pattern = "SAS",x = cfiles$Regions, ignore.case = T),]
eap<- cfiles[grep(pattern = "EAP",x = cfiles$Regions, ignore.case = T),]
eur<- cfiles[grep(pattern = "EUR",x = cfiles$Regions, ignore.case = T),]
fsu<- cfiles[grep(pattern = "FSU",x = cfiles$Regions, ignore.case = T),]
men<- cfiles[grep(pattern = "MEN",x = cfiles$Regions, ignore.case = T),]
nam<- cfiles[grep(pattern = "NAM",x = cfiles$Regions, ignore.case = T),]
ssa<- cfiles[grep(pattern = "SSA",x = cfiles$Regions, ignore.case = T),]



#lista de base de datos como vectores para ser usandos en proceso mas grandes
pots<- list(alc,sas,eap,eur,fsu,men,nam,ssa)
#r<- c("EAP", "EUR","FSU", "LAC", "MEN", "NAM", "SAS", "SSA")


# Escenarios IMPACT model y grupo de tuberculos
sce<- unique(cfiles$Scenarios)
crops<- unique(cfiles$Commodity)

# Vector con los cultivos para RTB incluyendo Bananas
rtb<- c("R&T-Potato","R&T-Sweet Potato","R&T-Yams","R&T-Other Roots","R&T-Cassava","F&V-Banana") 

# Eliminacion de los escenarios de cambio climatico. NoCC
sceNoCC<-  c("SSP2-NoCC", "SSP2-NoCC-SWHC2", "SSP2-NoCC-IRREXP-WUE2", "SSP2-NoCC-Pangloss2", "SSP2-NoCC-IRREXP2")

# Data con solo roots and tubers
z<- list()
for(i in 1:length(rtb)){
      #filtros por crops and Scenario
      z[[i]]<- alc[which(alc$Commodity==rtb[i]),]
      z[[i]]<- subset(z[[i]],Scenarios!="SSP2-NoCC" & 
                            Scenarios!="SSP2-NoCC-SWHC2" & 
                            Scenarios!="SSP2-NoCC-IRREXP-WUE2" & 
                            Scenarios!="SSP2-NoCC-Pangloss2" &
                            Scenarios!="SSP2-NoCC-IRREXP2") 
      #reshape 
      require(plyr)
      require(tidyr)
      z[[i]]<- z[[i]] %>% spread(Year, Val)
      rownames(z[[i]])<- 1:nrow(z[[i]])
      cat(paste("running this is crop= ", rtb[i]," it's done\n", sep = ""  ))
      
}

# Apilando los datos de una lista, datos completos 
rtbCrops<- do.call(rbind,z)

# Quitar la palabra SSP2.
rtbCrops$Scenarios<-  gsub("SSP2-", "",rtbCrops$Scenarios)
# Quitar la palabar LAC.
rtbCrops$Regions<-  gsub("^LAC-", "",rtbCrops$Regions)


#Net Trade y  filtros logicos-----------------
TradeFood<- c("QNXAgg", "FoodAvailXAgg")
tznet<- rtbCrops[which(rtbCrops$parameter=="QNXAgg"),]
row.names(tznet)<- 1:nrow(tznet)

# caso Belize
Be<-tznet[which(tznet$Regions=="Belize"),] 
Be<-Be[which(Be$Commodity=="R&T-Yams" | Be$Commodity=="R&T-Sweet Potato"),]

tznet<- tznet[!which(tznet$Regions=="Belize" & tznet$Commodity=="R&T-Yams"),] 
 
## logica de los valores 
#Bel
nn1<- which(is.na(tznet[,"2005"]) & tznet[,"2050"]<0 & tznet[,"2010"]<0)
pp1<- which(is.na(tznet[,"2005"]) & tznet[,"2050"]>0 & tznet[,"2010"]>0)
np1<- which(is.na(tznet[,"2005"]) & tznet[,"2050"]<0 & tznet[,"2010"]>0) 
pn1<- which(is.na(tznet[,"2005"]) & tznet[,"2050"]>0 & tznet[,"2010"]<0) 

nn<-  which(!is.na(tznet[,"2005"]) & tznet[,"2050"]<0 & tznet[,"2005"]<0)
pn<-  which(!is.na(tznet[,"2005"]) & tznet[,"2050"]>0 & tznet[,"2005"]<0)
np<-  which(!is.na(tznet[,"2005"]) & tznet[,"2050"]<0 & tznet[,"2005"]>0)
pp<-  which(!is.na(tznet[,"2005"]) & tznet[,"2050"]>0 & tznet[,"2005"]>0)



## Desempeño 
export<-   c(pp,pp1)
import <-  c(nn,nn1)
tran_XtoM<- c(np,np1) # inicia exportador termina importador
tran_MtoX<- c(pn,pn1) # inicia importador termina exportador

# hard<- c(nn,pp,pn,np)
# 
# tznet$impacto<- NA 
# 
# #loops 
# for(j in 1:nrow(tznet)){
#       if(j %in% hard){
#       tznet$impacto[j] <- ((tznet$`2050`[j] - tznet$`2005`[j])/tznet$`2005`[j]) * 100
#       } else {  }
#       
# }


# copia
tanz<- tznet
tanz$Trend<- NA
tanz$Trend[import] <- "Negative, Importer net"
tanz$Trend[export] <- "Positive, exporter net"
tanz$Trend[tran_MtoX]<- "Transition from M to X"
tanz$Trend[tran_XtoM]<- "Transition from X to M"

##Copia de seguridad cambios relativos y vectores de desempeño
write.csv(tanz,paste(rdsFiles,"NETTradeRTBcrops.csv", sep = ""), row.names = FALSE)

crops<- c(unique(tanz$Commodity))
sce<- c(unique(tanz$Scenarios))
fpu<- c(unique(tanz$Regions))


# i=j=f=1
# grafico 1. 
for(f in 1:length(fpu)){ 
                        z <- tanz[which(tanz$Regions==fpu[f]),]
                        rownames(z)<-1:nrow(z)
                      
                        z<- z[,c("Scenarios", "Commodity", "trend", "Regions", "parameter", "2005", "2010","2015","2020",
                                 "2025", "2030","2035","2040",  "2045", "2050")]
                        
                        z<- z %>% gather("Val", "Year", 6:ncol(z))
                        
                        png(filename=paste(rdsFiles,"RTB_crops_NetTrade",fpu[f],".png",sep=""), 
                            width = 12, height = 12, units = 'in', res = 300)
                        
                         c<-ggplot(data=z, aes(Val,Year,group=Scenarios,color=Scenarios)) +
                               geom_line(linetype="dashed",size=1)+ facet_grid(Commodity~., scales = "free") +
                               labs(title=paste("Net Trade by FPU= ",fpu[f], " ,RTB Crops\n All scenarios",sep = ""),y="Net trade (000 mt)",x="Year")+
                               theme(strip.text.y = element_text(angle = 0,size = 12))+
                               geom_hline(aes(yintercept=0)) 

                         plot(c)
                         dev.off()  
                         cat(paste( ' running in ', fpu[f], ' It has been complete\n', sep=''))
                  }
                

# grafico 2

      png(filename=paste(rdsFiles,"RTB_crops_NetTrade",fpu[f],".png",sep=""), 
          width = 12, height = 12, units = 'in', res = 300)
      

      n<- ggplot(data = tanz, aes(Scenarios,Regions)) + 
            geom_tile(aes(fill = Trend), colour = "white")+  facet_grid(~Commodity, scales = "free")+
            labs(x=NULL, y=NULL, title=("R&T crops, parameter= Net Trade, by all scenarios \n  Trends (2005-2050)")) +
            theme_grey() + labs(x = "",y = "") + 
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      plot(n)
      dev.off()  
   


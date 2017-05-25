# Codigos para el procesamiento y organizacion de los graficos para el CSA. Climate Smart Acgriculture
# Harold and Carlos 

#librerias------------
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(lattice)
library(latticeExtra)
library(colorspace)
library(Rcpp)


#Definir directorio de trabajo-------------
setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.2/OutputFiles/Aggregation/")
#Direción graficos
grd<-"C:/Users/CEGONZALEZ/Documents/CSA/"
#Direción copias seguridad
copy<-"C:/Users/CEGONZALEZ/Documents/CSA/copy/"
# country
country<- "Vietnam"

# manejo de digitos
options(digits=3) 
options(scipen = 999)

#Cargar marco de datos principal-------------
md<-read.csv("CSA_VIET2.csv",header=T)
phi<- md


#Hacer un subconjunto que sólo contenga las variables de mi interés----------------
mdsub_phi<-subset(phi,phi$impactparameter=="TAreaXAgg -- Total Area" |
                      phi$impactparameter== "QNXAgg -- Net Trade" | 
                      phi$impactparameter== "TYldXAgg -- Total Yield"|
                      phi$impactparameter=="AnmlNumXAgg -- Animal Numbers"|
                      phi$impactparameter=="AnmlYldXAgg -- Animal Yield")

mdsub_phi$impactparameter<-revalue(mdsub_phi$impactparameter, c("QNXAgg -- Net Trade"="Net Trade",
                                                                    "TAreaXAgg -- Total Area"="Total Area",
                                                                    "TYldXAgg -- Total Yield"= "Yield",
                                                                    "AnmlNumXAgg -- Animal Numbers"="Animal Numbers",
                                                                    "AnmlYldXAgg -- Animal Yield"="Animal Yield"))

sp<- unique(mdsub_phi$commodity)
# convertir  a variable caracter
mdsub_phi$impactparameter <- as.character(mdsub_phi$impactparameter)
mdsub_phi$scenario <- as.character(mdsub_phi$scenario)
mdsub_phi$region <- as.character(mdsub_phi$region)
mdsub_phi$commodity <- as.character(mdsub_phi$commodity)
mdsub_phi$productiontype <- as.character(mdsub_phi$productiontype)


#Hacer un subconjunto que sólo contenga los cinco cultivos analizados
mdsub_phi<-subset(mdsub_phi,mdsub_phi$commodity=="ccafe"| mdsub_phi$commodity=="jcafe" |
                      mdsub_phi$commodity=="cfv" | mdsub_phi$commodity=="jfv" | 
                      mdsub_phi$commodity=="cmaiz" | mdsub_phi$commodity=="jmaiz" | 
                      mdsub_phi$commodity=="cmeat" | mdsub_phi$commodity=="jmeat" |
                      mdsub_phi$commodity=="cpork" | mdsub_phi$commodity=="jpork" |
                      mdsub_phi$commodity=="crice" | mdsub_phi$commodity=="jrice" |
                      mdsub_phi$commodity=="csubf" | mdsub_phi$commodity=="jsubf" |
                      mdsub_phi$commodity=="ccass" |  mdsub_phi$commodity=="jcass" |
                      mdsub_phi$commodity=="cothr" | mdsub_phi$commodity=="jothr" |
                      mdsub_phi$commodity=="crt" | mdsub_phi$commodity=="jrt" |
                      mdsub_phi$commodity=="jteas" | mdsub_phi$commodity=="cteas"|
                      mdsub_phi$commodity=="jsugc" | mdsub_phi$commodity=="csugc")


  

mdsub_phi$productiontype<- NULL
row.names(mdsub_phi)<- 1: nrow(mdsub_phi)

# cambio de nombre de los cultivos 
mdsub_phi$commodity<-revalue(mdsub_phi$commodity, c( "ccafe"="Coffe",
                                                     "jcafe"="Coffe",
                                                     "cmaiz"="Maize",
                                                     "jmaiz"="Maize",
                                                     "cmeat"="Meat",
                                                     "jmeat"="Meat",
                                                     "cpork"="Pork",
                                                     "jpork"="Pork",
                                                     "crice"="Rice",
                                                     "jrice"="Rice",
                                                     "csubf"="sub-Tropical fruit",
                                                     "jsubf"="sub-Tropical fruit",
                                                     "jteas"="Tea",
                                                     "cteas"="Tea",
                                                     "cothr"="Other Crops",
                                                     "jothr"="Other Crops",
                                                     "crt"= "Root and Tubers",
                                                     "jrt"= "Root and Tubers",
                                                     "jsugc"= "Sugarcane",
                                                     "csugc"= "Sugarcane",
                                                     "ccass"= "Cassava",
                                                     "jcass"= "Cassava"))

mdwide_phi<- mdsub_phi %>%
  spread ("year", "Val")


# categorias de climas
mdwide_phi<-data.frame(mdwide_phi,"Cat"=ifelse(mdwide_phi$scenario=="NoCC","NoCC","CC"))
mdwide_phi<-mdwide_phi[,-c(5:19)]
rownames(mdwide_phi)<- 1: nrow(mdwide_phi)

#Copia de seguridad pais filtrado------
write.csv(mdwide_phi,paste(copy, country,"Vietnam2Total.csv", sep = ""), row.names = FALSE)

#Net Trade y  filtros logicos-----------------
Phinet<- mdwide_phi[which(mdwide_phi$impactparameter=="Net Trade"),]


## logica de los valores 
nn<-  which(Phinet$X2050<0 & Phinet$X2020<0) # net trade negativo  importador neto
pn<-  which(Phinet$X2050>0 & Phinet$X2020<0) # impacto positivo inicia como importador termina como exportador
np<-  which(Phinet$X2050<0 & Phinet$X2020>0) # impacto negativo inicia como exportador termina como importador
pp<-  which(Phinet$X2050>0 & Phinet$X2020>0) # net trade positivo  exportador neto

# No se tienen datos de tendencia en el que 

# desempeño 
export<-   c(pp)
import <-  c(nn)
tran_XtoM<- c(np) # inicia exportador termina importador
tran_MtoX<- c(pn) # inicia importador termina exportador

hard<- c(nn,pp,np)

Phinet$impacto<- NA # para poner el impacto  cambio relativo

#loops 
for(j in 1:nrow(Phinet)){
  
  if(j %in% hard){
    Phinet$impacto[j] <- abs(Phinet$X2050[j] - Phinet$X2020[j])/max(abs(Phinet$X2050[j]), abs(Phinet$X2020[j]), na.rm=TRUE) * 100
  } else {  }
  
}

##Copia de seguridad cambios relativos y vectores de desempeño
write.csv(Phinet,paste(copy, country, "Vietnam2RelativosTOTAL.csv", sep = ""), row.names = FALSE)

# # copia ojo con el orden
phill<- Phinet
phill<- as.data.frame(phill)
# phill$trend[tran_MtoX]<- "Transition M to X"
phill$trend <- NA
phill$trend[export] <- "Positive"
phill$trend[tran_XtoM]<- "Transition X to M"
phill$trend[import] <- "Negative"

#list por crops
crops<- unique(phill$commodity)
crops<- list( "Cassava","Coffe", "Maize", "Meat", "Other Crops","Pork",              
    "Rice","Root and Tubers", "sub-Tropical fruit", "Tea" ) 

crops<- unique(phill$commodity)
cxx <- list()

for (i in 1:length(crops)){
  
  if(length(which(phill$commodity==crops[[i]]))>0){
    z <- phill[which(phill$commodity==crops[[i]]),]
    x <- table(z$trend[z$Cat=='CC'])
    x <- x[which.max(x)]
    z <- z[,c("impactparameter","scenario", "commodity", "region", "Cat", "impacto")] %>% spread("scenario","impacto")
    if(x >= 3){
      z$trend <- names(x)
    }
    cxx[[i]] <- z
  } else{
    cat(paste('Commodity:', crops[i], 'does not have data\n', sep=''))
    
   }
  
}

##Unir los cultivos
cxx <- do.call(rbind, cxx)  

##Calcular la media de los impactos
cxx$CC_mean <- rowMeans(x=cxx[,5:8], na.rm=TRUE)
cxx$NoCC_mean <- cxx[,9]
cxx<- cxx[,c("impactparameter","commodity","region","trend","CC_mean","NoCC_mean")]
write.csv(cxx, paste(copy, country, "Vietnam2NettradeTOTAL.csv", sep = ""),row.names = FALSE)




#Area  y rendimientos---------------------------------------
Phill2<- mdwide_phi[which(mdwide_phi$impactparameter=="Total Area" | 
                            mdwide_phi$impactparameter=="Yield" |
                            mdwide_phi$impactparameter=="Animal Yield" | 
                            mdwide_phi$impactparameter=="Total Production"|
                            mdwide_phi$impactparameter=="Animal Numbers"),]

Phill2$Percentage_Change<- ((Phill2$X2050-Phill2$X2020)/Phill2$X2020)*100

##Eliminamos las columnas que no necesitamos 
Phill2<- data.frame(Phill2[1:4], Phill2[,36:37])

##Reshape
Phill2_t<-Phill2 %>%
  spread("scenario","Percentage_Change")
##Calculo de la media
Phill2_t$mean <- rowMeans(x=Phill2_t[,5:ncol(Phill2_t)], na.rm=TRUE)
Phill2_t <- Phill2_t[,c("impactparameter", "commodity", "region", "Cat", "mean", "NoCC")]
Phill2_t$NoCC<- NULL
#Reshape
Phill2_t <- Phill2_t %>% spread(Cat, mean)
write.csv(Phill2_t, paste(copy,country,"Vietnam2TableSumm.csv", sep=""), row.names = FALSE)

#Solicitud datos CSA-------------
mdwide_PhilCSA<- mdwide_phi
mdwide_PhilCSA<- mdwide_PhilCSA[,c("impactparameter", "scenario","commodity","region","X2020","X2025","X2030", "X2035","X2040","X2045", "X2050", "Cat" )]
mdwide_PhilCSA<- mdwide_PhilCSA[-which(mdwide_PhilCSA=="Net Trade"),]
rownames(mdwide_PhilCSA)<- 1:nrow(mdwide_PhilCSA)


#Listas para desarrollar el proceso
#cultivos
#periodos
k<- list()
testyear<- list()
y<- c("X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050")
v<- c("Total Area", "Yield" )

library(dplyr)
library(tidyr)
#library(Rcpp)
#library(Rcpp11)
csa <- mdwide_PhilCSA[,c("impactparameter", "commodity", paste("X", seq(from=2020, to=2050, by=5), sep = ""), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
csa <- csa %>% gather(Year, Value, X2020:X2050)
csa <- csa %>% spread(Cat, Value)
csa$Year <- as.numeric(gsub(pattern = "X", replacement = "", x = csa$Year))
csa$percentual_change <- (csa$CC-csa$NoCC)/csa$NoCC * 100

write.csv(csa, paste(copy, country, "Vietnam2CSAAreaYield.csv", sep = ""), row.names = FALSE)



library(ggplot2)

#grafico Area Cultivos
tiff(filename=paste(grd, country, "Vietnam2AreaCommodities.tiff", sep=""), width = 10, height = 10, units = 'in', res =800)

n<- ggplot(csa[which(csa$impactparameter=="Total Area"),], aes(x=Year, y=percentual_change ))
n<- n + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n<- n + geom_area(position = "stack", alpha = 0.4)
n<- n + ggtitle("Area ") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n<- n + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n<- n + coord_cartesian(ylim = c(-15, 2)) #+  scale_y_continuous( breaks=seq(-15, 2,0.5 ))
n<- n + coord_cartesian(xlim = c(2020, 2050)) +   scale_x_continuous( breaks=seq(2020,2050,5))

n
dev.off()


library(ggplot2)
#grafico numero de Animales 
tiff(filename=paste(grd,"Numer Animales Vietnam2.tiff", sep=""), width = 10, height = 10, units = 'in', res =800)

n1<- ggplot(csa[which(csa$impactparameter=="Animal Numbers"),], aes(x=Year, y=percentual_change ))
n1<- n1 + geom_line(aes(colour = commodity),  size = 0.5) + facet_wrap( ~ commodity,ncol=2)
n1<- n1 + geom_area(position = "stack", alpha = 0.4)
n1<- n1 + ggtitle("Animal Numbers") + theme_bw() + theme(plot.title=element_text(size=15, face = 'bold'))
n1<- n1 + xlab('Years') + ylab('Percentual change') + coord_equal() + theme(legend.position="none")
n1<- n1 + coord_cartesian(ylim = c(-0.5, 0.5)) #+ scale_y_continuous( breaks=seq(-0.5,0.5,0.2))
n1<- n1 + coord_cartesian(xlim = c(2020, 2050)) #+   scale_x_continuous( breaks=seq(2020,2050,5))

n1
dev.off()

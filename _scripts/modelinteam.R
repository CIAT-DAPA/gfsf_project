g=gc;rm(list = ls())




#Cargar librerias-----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)


#Limitar numero de decimales-----
options(digits=3) 


###SSP2setwd("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/")
cc<- read.csv("./Maize3.1.csv")
str(cc)
cc$impactparameter<- as.character(cc$impactparameter)
cc$scenario<- as.character(cc$scenario)
cc$commodity<- as.character(cc$commodity)
cc$region<- as.character(cc$region)
cc$productiontype<- as.character(cc$productiontype)


#### 
cc$commodity<- revalue(cc$commodity, c("cmaiz"="Maize", "jmaiz"="Maize"))
# ccTemp<-data.frame(cc,"Cat"=ifelse(cc$scenario=="NoCC_SSP","NoCC","CC"))
ccTemp<- cc %>% spread(year, Val)
ccTemp<- ccTemp[,-c(28:ncol(ccTemp))]
ccTemp<- ccTemp %>% dplyr::filter(., region=="LAC-Colombia")
# ccTemp$Percentage_Change<-((ccTemp$`2030`-ccTemp$`2005`)/ccTemp$`2005`)*100
# ccTemp<- ccTemp %>% dplyr::select(.,impactparameter, scenario, commodity, region,          
#                                   productiontype,Cat,Percentage_Change )
parametros<- unique(ccTemp$impactparameter)
aa<- c("QSXAgg -- Total Production", "TYldXAgg -- Total Yield"  ,"TAreaXAgg -- Total Area"   )
################
ccTemp<- ccTemp %>% dplyr::filter(., impactparameter %in% aa )
sce<-c( "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

i=1
for(i in 1:length(sce)){
      m2<- ccTemp[grep(pattern = sce[i], x = ccTemp$scenario),]
      m2<-data.frame(m2,"Cat"=ifelse(m2$scenario==paste("NoCC_",sce[i], sep = ""),"NoCC","CC"))
# m2<- m2[,-c(29:ncol(m2)-1)]
      m2$Percentage_Change<-((m2$X2030-m2$X2005)/m2$X2005)*100
      m2<- m2 %>% dplyr::select(.,impactparameter, scenario, commodity, region,Cat,Percentage_Change )
      m2<- m2[c("impactparameter", "scenario", "commodity", "Cat" ,"region","Percentage_Change")]
      m2<- aggregate(m2[,"Percentage_Change"],
                       by=list(m2$region,m2$impactparameter,
                               m2$commodity,m2$Cat),
                       FUN=median)

      m2<- m2 %>% spread(Group.4, x)
      m2$diff<-  m2$CC- m2$NoCC
      m2$sce<-sce[i]
      write.csv(m2, paste(copy, sce[i], ".csv", sep = ""))
      
}

cfiles<- list.files(copy, pattern = "SSP", full.names = T)
cfiles<- lapply(cfiles, read.csv)
cfiles<- do.call(rbind, cfiles)

write.csv(cfiles, paste(copy,"resumen.csv", sep = ""))





#       
# ###SSP3
# m3<- maiz[grep(pattern = "SSP3", x = maiz$scenario),]
# anal_datag<- aggregate(ccTemp[,"Percentage_Change"],
#                        by=list(ccTemp$region,ccTemp$impactparameter,
#                                ccTemp$commodity,ccTemp$Cat),
#                        FUN=median)
# 
# ###SSP4
# m4<- maiz[grep(pattern = "SSP4", x = maiz$scenario),]
# anal_datag<- aggregate(ccTemp[,"Percentage_Change"],
#                        by=list(ccTemp$region,ccTemp$impactparameter,
#                                ccTemp$commodity,ccTemp$Cat),
#                        FUN=median)
# 
# ###SSP1
# m1<- maiz[grep(pattern = "SSP1", x = maiz$scenario),]
# anal_datag<- aggregate(ccTemp[,"Percentage_Change"],
#                        by=list(ccTemp$region,ccTemp$impactparameter,
#                                ccTemp$commodity,ccTemp$Cat),
#                        FUN=median)
# 
# ###SSP5
# m5<- maiz[grep(pattern = "SSP5", x = maiz$scenario),]
# anal_datag<- aggregate(ccTemp[,"Percentage_Change"],
#                        by=list(ccTemp$region,ccTemp$impactparameter,
#                                ccTemp$commodity,ccTemp$Cat),
#                        FUN=median)
# 
# 
# 
# 
# ###SSP2
# m2<- maiz[grep(pattern = "SSP2", x = maiz$scenario),]
# m2$scenario <- factor(m2$scenario, levels = c( "NoCC_SSP2" ,"IPSL_SSP2_4.5", "HGEM_SSP2_4.5",
#                                                "GFDL_SSP2_4.5", "NORE_SSP2_4.5"))
# m2$esc<- "ssp2"
# ###SSP3
# m3<- maiz[grep(pattern = "SSP3", x = maiz$scenario),]
# m3$scenario <- factor(m3$scenario, levels = c( "NoCC_SSP3" ,"IPSL_SSP3_4.5", "HGEM_SSP3_4.5",
#                                                "GFDL_SSP3_4.5", "NORE_SSP3_4.5"))
# m3$esc<- "ssp3"
# 
# ###SSP4
# m4<- maiz[grep(pattern = "SSP4", x = maiz$scenario),]
# m4$scenario <- factor(m4$scenario, levels = c( "NoCC_SSP4" ,"IPSL_SSP4_4.5", "HGEM_SSP4_4.5",
#                                                "GFDL_SSP4_4.5", "NORE_SSP4_4.5"))
# m4$esc<- "ssp4"
# 
# ###SSP1
# m1<- maiz[grep(pattern = "SSP1", x = maiz$scenario),]
# m1$scenario <- factor(m1$scenario, levels = c( "NoCC_SSP1" ,"IPSL_SSP1_4.5", "HGEM_SSP1_4.5",
#                                                "GFDL_SSP1_4.5", "NORE_SSP1_4.5"))
# m1$esc<- "ssp1"
# ###SSP5
# m5<- maiz[grep(pattern = "SSP5", x = maiz$scenario),]
# m5$scenario <- factor(m5$scenario, levels = c( "NoCC_SSP5" ,"IPSL_SSP5_4.5", "HGEM_SSP5_4.5",
#                                                "GFDL_SSP5_4.5", "NORE_SSP5_4.5"))
# m5$esc<- "ssp5"
# 
# #### todos datos yield
# xx<- rbind(m1,m2)
# xx<- rbind(xx,m3)
# xx<- rbind(xx,m4)
# xx<- rbind(xx,m5)
# 
# #Evolution of Net Trade by crop and by region
# datos<- xx %>% dplyr::filter(., impactparameter=="TYldXAgg -- Total Yield") 
# 
# 
# 
# 
# dev.off()
# tt<- unique(xx$esc)
# 
# for(i in 1:length(tt)){
#       datos<- xx %>% dplyr::filter(., impactparameter=="TYldXAgg -- Total Yield") 
#       
#       
#       png(filename=paste(tt[[i]],"Yields.png",sep=""), width = 7, height = 7, units = 'in', res = 300)
#       
#       aa<- ggplot(datos[which(datos$esc==tt[[i]]),], aes(x=year,y=Val, color=scenario))+ 
#             geom_line(size=1) + labs(x="Year", y="Val" )+ 
#             facet_grid(.~impactparameter)+ ggtitle(paste(tt[[i]]))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
#             theme(axis.text.y = element_text(hjust = 1, size = 11))+
#             theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#             theme(strip.text=element_text(size=8))+
#             theme(strip.text.y = element_text(angle = 0,size = 11)) 
#       
#       plot(aa)
#       
#       dev.off()
#       return() 
#       
# }
# 
# 
# for(i in 1:length(tt)){
#       datos<- xx %>% dplyr::filter(., impactparameter=="QSXAgg -- Total Production" ) 
#       
#       
#       png(filename=paste(tt[[i]],"Production.png",sep=""), width = 7, height = 7, units = 'in', res = 300)
#       
#       aa<- ggplot(datos[which(datos$esc==tt[[i]]),], aes(x=year,y=Val, color=scenario))+ 
#             geom_line(size=1) + labs(x="Year", y="Val" )+ 
#             facet_grid(.~impactparameter)+ ggtitle(paste(tt[[i]]))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
#             theme(axis.text.y = element_text(hjust = 1, size = 11))+
#             theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#             theme(strip.text=element_text(size=8))+
#             theme(strip.text.y = element_text(angle = 0,size = 11)) 
#       
#       plot(aa)
#       
#       dev.off()
#       
# }
# 
# 
# 
# for(i in 1:length(tt)){
#       datos<- xx %>% dplyr::filter(., impactparameter=="TAreaXAgg -- Total Area" ) 
#       
#       
#       png(filename=paste(tt[[i]],"Area.png",sep=""), width = 7, height = 7, units = 'in', res = 300)
#       
#       aa<- ggplot(datos[which(datos$esc==tt[[i]]),], aes(x=year,y=Val, color=scenario))+ 
#             geom_line(size=1) + labs(x="Year", y="Val" )+ 
#             facet_grid(.~impactparameter)+ ggtitle(paste(tt[[i]]))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#             theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
#             theme(axis.text.y = element_text(hjust = 1, size = 11))+
#             theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#             theme(strip.text=element_text(size=8))+
#             theme(strip.text.y = element_text(angle = 0,size = 11)) 
#       
#       plot(aa)
#       
#       dev.off()
#       
# }
# 

# codigo para presentar el docunmento en un formato apropiado

#directorio datos
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/TasasCrecimiento/")
pic<-("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/GraphGrownRate/")
      
# Limitar numero de decimales
options(digits=3) 
options(scipen=999)

#Librerias
library(reshape)
library(ggplot2)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)

#Lista de tipos de sistemas
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize") # Soybean

#Lista de cultivos
crops.en<-c("rice","maiz","soyb","bean","whea")
crops.enj<-c("jrice","jbean","jwhea","jmaiz" ) #"jsoyb",


# load files 
c_files<- list.files()
c_files<- lapply(c_files, read.csv)
c_files<- do.call(rbind, c_files)

colnames(c_files)[1]<- "fpu"
colnames(c_files)[2]<- "j"
colnames(c_files)[3]<- "lnd"


# ajustando nombres
c_files$j<-revalue(c_files$j, c("Bean"="jbean",
                                "Rice"="jrice",
                                "Maize"="jmaiz",
                                "Wheat"="jwhea"))
c_files$lnd<- revalue(c_files$lnd, c("IRRI"="air",
                                "RA"="arf"))
# formato adecuado para IMPACT                                                   
require(plyr)
require(tidyr)
c_files<- c_files %>% 
      gather(gcm, val, 4:ncol(c_files))

#organizar
rownames(c_files)<-1:nrow(c_files)


#Los NAs los reemplazo por ceros
c_files[is.na(c_files)==TRUE]=0

write.csv(c_files,file='//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/LongFormat.csv',row.names=F)


#comparaciones con el archivo original
data1<- read.csv("C:/Users/CEGONZALEZ/Documents/Scripts/BID/CCPROCESSING/InputFiles/LongFormat.csv")
data2<- c_files


#Sin soya
data1<- data1[which(data1$j!="jsoyb"),]
row.names(data1)<- 1:nrow(data1)

data1$fpu<- as.character(data1$fpu)
data2$fpu<- as.character(data2$fpu)

data1$j<- as.character(data1$j)
data2$j<- as.character(data2$j)

data1$lnd<- as.character(data1$lnd)
data2$lnd<- as.character(data2$lnd)


data1$gcm<- as.character(data1$gcm)
data2$gcm<- as.character(data2$gcm)


total <- merge(data1, data2,by=c("fpu","j","lnd","gcm"))



colnames(total)[5]<-"initial"
colnames(total)[6]<-"updated"


#Just for wheat
totalwheat<- total
totalwheat<- totalwheat[which(totalwheat$j=="jwhea"),]



png(filename = paste(pic,"WheathTc.png",sep=""), 
    width = 15, height = 15, units = 'in', res = 100)
w<- ggplot(data = totalwheat,aes(x=totalwheat$initial,y=totalwheat$updated,group=gcm))+ geom_point(aes(color=gcm),size=3) +
      facet_grid(lnd~.) + 
      theme(strip.text.x = element_text(size = 14))+
      theme(strip.text.y = element_text(size = 14, angle = 270))+
      theme(axis.text.x=element_text(size=14))+
      theme(axis.text.y=element_text(size=14))+
      theme(legend.title = element_text(size=12,face="bold"))+
      theme(legend.text = element_text(size=9))+
      guides(color=guide_legend("Scenarios"))+ geom_hline(aes(yintercept=0))+
      geom_vline(aes(xintercept=0)) + ylab('Updated') +  xlab('Initial') +  ggtitle("Comparasion between original & updated")


plot(w)
dev.off()

# para conocer la igualdad de datos entre la nueva base datos y la antigua.
#identical(sort(unique(total$fpu)), sort(unique(data1$fpu)))
Total_test<- left_join(data1,data2, by=c("fpu","j","lnd","gcm"))
is.na(Total_test)

g=gc;rm(list = ls())



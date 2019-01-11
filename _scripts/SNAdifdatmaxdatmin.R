### procesamiento rangos de datos, datos minimos, maximos
g=gc;rm(list = ls())

### decimals
options(warn = -1)
options(scipen = 999)

### directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/Paper SAN")

### decimals
options(warn = -1)
options(scipen = 999)

### librerias
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(gridExtra))

############################################ rangos #################################
### extract data
cfiles<- list.files("dataTotal_sce_", path = "./Results",all.files = T,full.names = T)
cfiles<- lapply(cfiles, read.csv)
cfiles<- do.call(rbind,cfiles)
cfiles$X<- NULL
cfiles$MODEL<- as.character(cfiles$MODEL)
cfiles$REGION<- as.character(cfiles$REGION)
cfiles$Crop<- as.character(cfiles$Crop)
# cfiles$UNIT<- as.character(cfiles$UNIT)
cfiles$parametro<- as.character(cfiles$parametro)
cfiles$cat<- as.character(cfiles$cat)
cfiles$sce<- as.character(cfiles$sce)

### vectores
var<- unique(cfiles$parametro)
esc<- unique(cfiles$sce)
catt<- unique(cfiles$cat)
pots<- unique(cfiles$REGION)
model<- unique(cfiles$MODEL)


xfiles<- cfiles
# xfiles$Crop<- plyr::revalue(xfiles$Crop,c("wheat"="Wheat"))

xfiles$dif<- xfiles$datmax- xfiles$datamin

# boxplot(dif ~ sce, data=xfiles)
i=1
for(i in 1:length(catt)){
            varout<- c("Land", "Price", "Yield")

      xtest<- filter(xfiles, cat==catt[i]) %>% filter(!parametro %in% varout)
      png(filename= paste("./pic/",catt[i],"_DiferenciasDatminDatmaxGCMs.png",sep=""), 
          width = 20, height = 11, units = 'in', res = 300)
      
      p<- ggplot(xtest,aes(MODEL,dif))+
            geom_bar(stat="identity", aes(fill=parametro)) +
            facet_grid(REGION~sce)+ #coord_equal()+
            theme(strip.text.x = element_text(angle = 0,size = 12, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 12, face = "bold.italic"))+
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=12,face="bold"))+
            labs(title=catt[i])
      
      
      plot(p)
      dev.off()  
      
}
# p<- ggplot(xfiles,aes(MODEL,dif))+
#       geom_bar(stat="identity", aes(fill=parametro)) +
#       facet_grid(REGION~sce)+
#       theme(strip.text.x = element_text(angle = 0,size = 12, face = "bold.italic"))+
#       theme(strip.text.y = element_text(angle = 0,size = 12, face = "bold.italic"))+
#       theme(axis.text=element_text(size=12),
#             axis.title=element_text(size=12,face="bold"))
# 
#       
# p

##### CONSUMO
t<- c("Energy", "Food", "Feed", "All", "Other", "Maize", "Rice", "wheat", "Sugar crops", "Fiber crops", "Roots Tuber")
cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
consumo<- xfiles %>% filter(.,parametro=="Consumption" ) %>% filter(., Crop %in% cult)

png(filename= paste("./pic/DiffConsumoCrops.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

      ggplot(data = consumo, aes(x=Crop, y=dif)) + geom_boxplot(aes(fill=Crop)) + facet_grid(sce~cat)

dev.off()  

con_food<- xfiles[grep("Food", x = xfiles$Crop),]
png(filename= paste("./pic/DiffConsumoFood.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = con_food, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=REGION)) + 
      facet_grid(sce~cat)+ labs(x="Category Food", y="Desviation on TM")+
      theme(strip.text.x = element_text(angle = 0,size = 14, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 14, face = "bold.italic"))+
      theme(axis.text.x=element_text(size=10, angle=90))


dev.off()

### sin escenario raro
png(filename= paste("./pic/DiffConsumoFoodOutSceWeirdPhoAllCrops.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = con_food %>% filter(., sce!="sce_cfe_tx") %>% filter(.,Crop!="Food all crops") %>%
             filter(., MODEL=="Phoenix_6LA") %>% filter(.,dif!=100), aes(x=REGION, y=dif)) + 
      geom_boxplot(aes(fill=cat)) + 
      facet_grid(~sce)+ labs(x="Category Food", y="Desviation on TM")+ labs(title="Food all Crops")
      theme(strip.text.x = element_text(angle = 0,size = 14, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 14, face = "bold.italic"))+
      theme(axis.text.x=element_text(size=10, angle=90))

dev.off()




##### PRODUCTION
t<- c("Energy", "Food", "Feed", "All", "Other", "Maize", "Rice", "wheat", "Sugar crops", "Fiber crops", "Roots Tuber")
cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
production<- xfiles %>% filter(.,parametro=="Production") %>% filter(., Crop %in% cult)

png(filename= paste("./pic/DiffProduction.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = production, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation on TM")+ labs(title="Producion")
      

dev.off() 

#### Exports
gross<- xfiles[grep("gross", xfiles$Crop),]
cult<- c( "Maize gross" , "Oil crops gross" ,"Other grain gross","Sugar crops gross" ,
          "Wheat gross","Energy crops gross" ,"Fiber crops gross","Rice gross",
          "Roots tuber gross","Root tuber gross")

exports<- gross %>% filter(.,parametro=="Exports") %>% filter(., Crop %in% cult)


png(filename= paste("./pic/DiffExports.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = exports, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation on TM")+ labs(title="Exports")+
      theme(axis.text.x=element_text(size=10, angle=90))


dev.off() 

#### imports
gross<- xfiles[grep("gross", xfiles$Crop),]
cult<- c( "Maize gross" , "Oil crops gross" ,"Other grain gross","Sugar crops gross" ,
          "Wheat gross","Energy crops gross" ,"Fiber crops gross","Rice gross",
          "Roots tuber gross","Root tuber gross")

imports<- gross %>% filter(.,parametro=="Imports") %>% filter(., Crop %in% cult)


png(filename= paste("./pic/Diffimports.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = imports, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation on TM")+ labs(title="Imports")+
      theme(axis.text.x=element_text(size=10, angle=90))

dev.off() 

##### PRODUCTION
t<- c("Energy", "Food", "Feed", "All", "Other", "Maize", "Rice", "wheat", "Sugar crops", "Fiber crops", "Roots Tuber")
cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
production<- xfiles %>% filter(.,parametro=="Production") %>% filter(., Crop %in% cult)

png(filename= paste("./pic/DiffProduction.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = production, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation on TM")+ labs(title="Producion")


dev.off() 


##### unir datos 

cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
pr<- xfiles
# pr<- xfiles %>% filter(Crop %in% cult)

pr$Crop<- sub("gross","", pr$Crop)
w<- c("Exports", "Imports")
x<- pr %>% filter(parametro %in% w) 
x$Crop<-  plyr::revalue(x$Crop, c("Wheat "="Wheat", 
                                  "Maize "="Maize",
                                  "Roots tuber "="Root tuber",
                                  "Root tuber "="Root tuber",
                                  "Rice "="Rice",
                                  "Sugar crops "="Sugar crops",
                                  "Fiber crops "="Fiber crops",
                                  "Oil crops "="Oil crops",
                                  "Other grain "="Other grain"))

x<- x %>% filter(., Crop %in% cult) 

#### prices
t<- c("Energy", "Food", "Feed", "All", "Other", "Maize", "Rice", "wheat", "Sugar crops", "Fiber crops", "Roots Tuber")
cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
precio<- xfiles %>% filter(.,parametro=="Price") %>% filter(., Crop %in% cult)

png(filename= paste("./pic/DiffPrice.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = production, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation US$2005")+ labs(title="Price")


dev.off() 


##### land
cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
tierra<- xfiles %>% filter(.,parametro=="Land") %>% filter(., Crop %in% cult)

png(filename= paste("./pic/Difftierra.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = production, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation millions HA")+ labs(title="Land")

dev.off() 

##### yield
cult<- c("Maize", "Rice", "Wheat", "Sugar crops", "Fiber crops", "Roots Tuber", "Other grain","Oil crops")
rend<- xfiles %>% filter(.,parametro=="Yield") %>% filter(., Crop %in% cult)

png(filename= paste("./pic/DiffYield.png",sep=""), 
    width = 20, height = 11, units = 'in', res = 300)

ggplot(data = production, aes(x=Crop, y=dif)) + 
      geom_boxplot(aes(fill=MODEL)) + facet_grid(sce~cat)+
      labs(x="Crops", y="Desviation ")+ labs(title="Yield")

dev.off() 

########################################### join ################################################
comer<- rbind(x,production)
comer<- rbind(comer,consumo)
comer<- rbind(comer, tierra)
comer<- rbind(comer, rend)
comer<- rbind(comer, precio)

write.csv(comer,"./Results/totalrangeVariables.csv")
comer<- comer[c("MODEL" ,"REGION","Crop", "yr", "parametro","cat","sce", "dif" )]

test<- comer %>% group_by(MODEL,REGION,Crop,yr, cat, sce)%>%spread(parametro,dif)
write.csv(test,"./Results/totalrangeVariablesDIFF.csv")
vito<- comer %>% group_by(MODEL,Crop, yr, parametro, cat, sce ) %>% summarise(mean=mean(dif))

######################################## outliers ###################################

cfiles<- list.files("Outliers_", path = "./Results",all.files = T,full.names = T)
cfiles<- lapply(cfiles, read.csv)
cfiles<- do.call(rbind,cfiles)
cfiles$X<- NULL
cfiles$MODEL<- as.character(cfiles$MODEL)
cfiles$REGION<- as.character(cfiles$REGION)
cfiles$Crop<- as.character(cfiles$Crop)
# cfiles$UNIT<- as.character(cfiles$UNIT)
cfiles$parametro<- as.character(cfiles$parametro)
cfiles$cat<- as.character(cfiles$cat)
cfiles$sce<- as.character(cfiles$sce)
cfiles<- cfiles[!is.infinite(cfiles$mean),]

write.csv(cfiles,"./Results/TodosRaros.csv")

t1<- table(cfiles$MODEL, cfiles$parametro)
write.csv(t1,"./Results/tabla1.csv")
t2<-table(cfiles$sce,cfiles$MODEL)
write.csv(t2,"./Results/tabla2.csv")

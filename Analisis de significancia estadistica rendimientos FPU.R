#1) Programa para analizar la significancia estadistica de la diferencia de medias entre los escenarios analizados----
# y la linea bas a nivel de FPU
#Por: Jesús Rodríguez
options(scipen = 999)
setwd("D:/ToBackup/Jesús Rodríguez/BID/Resultados_agregados")
crops<-c("arroz_riego_","arroz_secano_","frijol_riego_","frijol_secano_","maiz_riego_","maiz_secano_","soya_riego_","soya_secano_","trigo_riego_","trigo_secano_")
models <- read.csv("D:/ToBackup/Jesús Rodríguez/BID/ModelosGCM.csv",header=T,stringsAsFactors=F)
models = rbind(models,'WFD')

# Crear matriz que tendra los valores p del WILCOX test
pv<-matrix(ncol=length(crops),nrow=dim(models)[1]+1)
colnames(pv)<-crops
rownames(pv)<-c(models$Models,'Y_Prom')

for (c in 1:length(crops)) {
  #Definir dimensiones de mi matriz vacia (dado que todos los modelos para cada cultivo y sistema de riego tienen el 
  # mismo numero de fpus fijare el J= 1 para obtener esta informacion) y el numero de col lo obtendre de la dimension de models
  z<-read.csv(paste(crops[c],models[1,],'_','FPU.csv',sep=''),header=T,stringsAsFactors = F)

  #z<-z[complete.cases((z[,"num_pixels"]) == TRUE),]
  
  y<-matrix(nrow=dim(z)[1], ncol=dim(models)[1])
  #Calcular los rendiminetos promedios en cada GCM y WFD
  for (j in 1:dim(models)[1]){
    md<-read.csv(paste(crops[c],models[j,],'_','FPU.csv',sep=''),header=T,stringsAsFactors = F)
    #md<-md[complete.cases((md[,"num_pixels"]) == TRUE),]
    zz<-matrix(nrow=dim(md)[1],ncol=1)
    zz<-apply(md[,paste('Rend_fpu_20',22:45,sep='')],1,function(x){mean(x,na.rm = T)})
    y[,j]<-zz
    rownames(y)<-md$X
    colnames(y)<-models$Models
    
  }
  y<-data.frame(y,y_prom=apply(y[,1:10],1,function(x){mean(x,na.rm = T)}))
  
  x<-matrix(ncol=1,nrow=dim(y)[2])
  rownames(x)<-c(models$Models,"y_prom")
  colnames(x)<-"P value"
  for (j in 1:dim(y)[2]) {
    x[j,1]<-as.numeric(wilcox.test(y[,j],y[,"WFD"],paired = T,exact=F)["p.value"])
  }
  
  x<-round(x,5)
  
  pv[,c]<-x
  print(c)
}


write.csv(pv,paste('D:/ToBackup/Jesús Rodríguez/BID/Graficos/p_value_menor_0.025_paired_wilcox_test__all_crops_all systems_FPU.csv',sep=''),row.names=T)

rm(list=ls())

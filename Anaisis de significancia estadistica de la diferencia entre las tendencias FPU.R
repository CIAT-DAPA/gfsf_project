#Programa para analizar la significancia estadistica de la diferencia entre las tendencias de los escenarios analizados----
# y la linea bas a nivel de FPU
#Por: Jesús Rodríguez
options(scipen = 999)
#Computador personal
setwd("D:/ToBackup/Jesús Rodríguez/BID/Resultados_agregados")
#Dapadfs

crops<-c("arroz_riego_","arroz_secano_","frijol_riego_","frijol_secano_","maiz_riego_","maiz_secano_","soya_riego_","soya_secano_","trigo_riego_","trigo_secano_")
models <- read.csv("D:/ToBackup/Jesús Rodríguez/BID/ModelosGCM.csv",header=T,stringsAsFactors=F)
models = rbind(models,'WFD')

# Crear matriz que tendra los valores p del WILCOX test
pv_lm<-matrix(ncol=length(crops),nrow=dim(models)[1])
rownames(pv_lm)<-models$Models
colnames(pv_lm)<-crops

for (c in 1:length(crops)) {
  #Definir dimensiones de mi matriz vacia (dado que todos los modelos para cada cultivo y sistema de riego tienen el 
  # mismo numero de fpus fijare el J= 1 para obtener esta informacion) y el numero de col lo obtendre de la dimension de models
  z<-read.csv(paste(crops[c],models[1,],'_','FPU.csv',sep=''),header=T,stringsAsFactors = F)
  
  est_lm<-matrix(0,nrow = dim(z)[1],ncol=dim(models)[1])
  rownames(est_lm)<-z$X
  colnames(est_lm)<-models$Models
  
  #Calcular los rendiminetos promedios en cada GCM y WFD
  for (j in 1:dim(models)[1]){
    
    md<-read.csv(paste(crops[c],models[j,],'_','FPU.csv',sep=''),header=T,stringsAsFactors = F)
    md2<-t(md[,paste('Rend_fpu_',2022:2045,sep='')])
    colnames(md2)<-md$X
    t<-2022:2045
    est_lm[which(complete.cases(md)==FALSE),j]=NA
    est_lm[which(complete.cases(md)==TRUE),j]<-apply(md2[,which(complete.cases(md)==TRUE)], 2, function(md2.col) lm(md2.col~t)$coef [2])
  }
  
  x_lm<-matrix(ncol=1,nrow=dim(est_lm)[2])
  rownames(x_lm)<-c(models$Models)
  colnames(x_lm)<-"P value"
  for (h in 1:dim(est_lm)[2]) {
    x_lm[h,1]<-as.numeric(wilcox.test(est_lm[,h],est_lm[,11],paired = T,exact=F)["p.value"])
  }
  
  x_lm<-round(x_lm,5)
  pv_lm[,c]<-x_lm
  
  print(c)
  
}

pv_lm<-pv_lm<0.025

write.csv(pv_lm,paste('D:/ToBackup/Jesús Rodríguez/BID/Graficos/trend_p_value__menor_0.025_paired_wilcox_test_all_crops_all_systems_FPU.csv',sep=''),row.names=T)

rm(list=ls())

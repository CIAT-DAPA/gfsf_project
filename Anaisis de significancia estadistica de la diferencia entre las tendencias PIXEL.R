#Programa para analizar la significancia estadistica de la diferencia de las tendencias entre los escenarios analizados
# y la linea bas a nivel de PIXEL.
#Por: Jesús Rodríguez
options(scipen = 999)
#Especificar cultivo
cultivos = c('arroz','maiz','soya','frijol','trigo')
cultivos.en = c('rice','maize','soybean','bean','wheat')

#¡Aqui debes especificar MANUALMENTE el sistema riego / secano!
treat = 'riego'  #riego o secano

#Cargar lista de modelos de circulacion general 
#models = read.table('Z:/_documentos/ModelosGCM.csv',header=T,sep=',',stringsAsFactors=F)
models <- read.csv("D:/ToBackup/Jesús Rodríguez/BID/ModelosGCM.csv",header=T,stringsAsFactors=F)
models = rbind(models,'WFD')

# Crear matriz que tendra los valores p del WILCOX test
pv<-matrix(ncol=length(cultivos),nrow=dim(models)[1])
colnames(pv)<-paste(cultivos.en,treat)
rownames(pv)<-models$Models

for ( c in 1:length(cultivos)) {
  
  #Cargar información de latitud, longitud, area de spam, fpu, etc.
  load(paste('D:/ToBackup/Jesús Rodríguez/BID/08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_',treat,'.RDat',sep=''))
  #load(paste('Z:/08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_',treat,'.RDat',sep=''))
  # Establdcer dimenciones de la matriz que contendra información de rendimientos a nivel de FPU para 
  # todos los GCMs y WFD
  # se predefine j=1 de forma fija en el codigo. Ver linea siguiente models[1,]
  load(paste('D:/ToBackup/Jesús Rodríguez/BID/12-Resultados/',cultivos[c],'/',cultivos[c],'-ENTREGA3','/','_',cultivos[c],'_',treat,'_',models[1,],'.RDat',sep=''))
  # Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
  rend<-matrix(nrow=length(Run), ncol=24)
  #crear vector que contendra información acerca del rendimiento promedio en los 24 años analizados
  y<-matrix(nrow=dim(rend)[1],ncol=1)
  x<-matrix(nrow=dim(rend)[1],ncol=dim(models)[1])
  colnames(x)<-t(models)
  t<-2022:2045
  #Ciclo por GCM y WFD
  for (j in 1:dim(models)[1])  {
    #Cargar información de los resultados de los modelos de cultivo
    load(paste('D:/ToBackup/Jesús Rodríguez/BID/12-Resultados/',cultivos[c],'/',cultivos[c],'-',treat,'-ENTREGA2','/','_',cultivos[c],'_',treat,'_',models[j,],'.RDat',sep=''))
    
    for (i in 1:length(Run)){
      rend[i,]<- Run[[i]] [,"HWAH"][1:24]  #solo incluir años 1 a 24
    }
    #descartar pixeles con demasiadas fallas
    rend[rend==-99] = 0  #convertir -99 a 0
    #find areas where consistently failing and discard these from aggregation
    zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
    ind.falla = which(zeros.wfd.r>=13)
    
    # Descartar pixeles con más de 13 años con fallas en la linea base
    if(sum(ind.falla) == 0)
    {
      rend<-rend
      
      
    } else {
      rend[ind.falla,]=NA
      
    }
    
    #¡Atención! en las siguientes lineas voy a reemplazar NAs con zeros. Sé que parece que estoy desaciendo lo que hice en las lineas anteriores... En parte es cierto, pero tengo una buena razón.
    # Voy a estimar las lineas de tendencia para cada pixel. La cuestion es que en los distintos "j" GCMs los pixeles que cumplen la condición zeros.wfd.r>=13 son distintos. Para 
    # tener claro la ubicación de los pixeles llenos de NAs voy a reemplazarlos por ceros, luego estimar las lineas de tendencias y luego las tendencias iguales a ceros, que son las correspondientes
    # a los NAs iniciales, las igualare a NAs
    rend[is.na(rend)]<-0
    #Asignar nombres a el data frame de rendimientos
    colnames(rend)<-paste0("Rend_20",22:45)
    y<-apply(rend, 1, function(rend.row) lm(rend.row~t)$coef [2])
    x[,j]<-y
    
    
  }
  
  x[ which(x == 0)]=NA
  
  z<-matrix(nrow=dim(models)[1],ncol=1)
  rownames(z)<-models$Models
  colnames(z)<-"P value"
  for (h in 1:dim(models)[1]) {
    z[h,1]<-as.numeric(wilcox.test(x[,h],x[,11],paired = T,exact=F)["p.value"])
    
  }
  
  z<-round(z,3)
  
  pv[,c]<-z
  print(c)
  
}

write.csv(pv,paste('D:/ToBackup/Jesús Rodríguez/BID/Graficos/trend_p_value_paired_wilcox_test_',treat,'_all_crops.csv',sep=''),row.names=T)

rm(list=ls())

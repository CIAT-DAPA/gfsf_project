#Programa para analizar la significancia estadistica de la diferencia de medias entre los escenarios analizados
# y la linea bas a nivel de PIXEL.
#Por: Jesús Rodríguez
options(scipen = 999)
#Especificar cultivo
cultivos = c('arroz','maiz','soya','frijol','trigo')
cultivos.en = c('rice','maize','soybean','bean','wheat')

#¡Aqui debes especificar MANUALMENTE el sistema riego / secano!
treat<-c("riego","secano")  #riego o secano

#Cargar lista de modelos de circulacion general 
#models = read.table('Z:/_documentos/ModelosGCM.csv',header=T,sep=',',stringsAsFactors=F)
models <- read.csv("D:/ToBackup/Jesús Rodríguez/BID/ModelosGCM.csv",header=T,stringsAsFactors=F)
models = rbind(models,'WFD')

for (t in 1:length(treat)) {

  # Crear matriz que tendra los valores p del WILCOX test
  pv<-matrix(ncol=length(cultivos),nrow=dim(models)[1])
  colnames(pv)<-paste(cultivos.en,treat[t])
  rownames(pv)<-models$Models
  
  for ( c in 1:length(cultivos)) {
    
    #Cargar información de latitud, longitud, area de spam, fpu, etc.
    load(paste('D:/ToBackup/Jesús Rodríguez/BID/08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_',treat[t],'.RDat',sep=''))
    #load(paste('Z:/08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_',treat[t],'.RDat',sep=''))
    # Establdcer dimenciones de la matriz que contendra información de rendimientos a nivel de FPU para 
    # todos los GCMs y WFD
    # se predefine j=1 de forma fija en el codigo. Ver linea siguiente models[1,]
    load(paste('D:/ToBackup/Jesús Rodríguez/BID/12-Resultados/',cultivos[c],'/',cultivos[c],'-ENTREGA3','/','_',cultivos[c],'_',treat[t],'_',models[1,],'.RDat',sep=''))
    x<-matrix(nrow=length(Run),ncol=dim(models)[1])
    colnames(x)<-t(models)
    # Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
    rend<-matrix(nrow=length(Run), ncol=24)
    #crear vector que contendra información acerca del rendimiento promedio en los 24 años analizados
    y<-matrix(nrow=length(Run),ncol=1)
    rm(Run)
    #Ciclo por GCM y WFD
    for (j in 1:dim(models)[1])  {
      #Cargar información de los resultados de los modelos de cultivo
      load(paste('D:/ToBackup/Jesús Rodríguez/BID/12-Resultados/',cultivos[c],'/',cultivos[c],'-',treat[t],'-ENTREGA2','/','_',cultivos[c],'_',treat[t],'_',models[j,],'.RDat',sep=''))
      
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
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("Rend_20",22:45)
      y<-t(t(as.numeric(apply(rend,1,function(x){mean(x,na.rm = T)}))))
      x[,j]<-y
      
      
    }
    
    z<-matrix(nrow=dim(models)[1],ncol=1)
    rownames(z)<-models$Models
    colnames(z)<-"P value"
    for (h in 1:dim(models)[1]) {
      z[h,1]<-as.numeric(wilcox.test(x[,h],x[,"WFD"],paired = T,exact=F)["p.value"])
      
    }
    
    z<-round(z,3)
    
    pv[,c]<-z
    print(c)
    
  }

  
  write.csv(pv,paste('D:/ToBackup/Jesús Rodríguez/BID/Graficos/p_value_menor_0.025__paired_wilcox_test_',treat[t],'_all_crops.csv',sep=''),row.names=T)
  
}


#### Escenarios historicos WFD

copyRice<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Rice/")
copyWheat<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Wheat/")
copyBean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Bean/")
copyMaize<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Maize/")
copySoybean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Soybean/")

###########################################ProofByPixel####################################################

#RICE --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/historical/final/RICE_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Rice/historical/final/RICE_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyRice<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Rice/")

###GCMs y variedades de trigo
variedades<- c("IR8", "IR64","IR72")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Rice_riego",'.RDat',sep=''))



#loop para cargar datos 
for (v in 1:length(variedades)){
      
      load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = ""))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
    
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                              Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                              v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys", "sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyRice,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running RICE IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Rice_secano",'.RDat',sep=''))


###GCMs y variedades de trigo
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedades<- c("IR8", "IR64","IR72")

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD", sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys", "sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyRice,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running RICE RA ", variedades[v], " of gcm WFD", " Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#BEAN --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/historical/final/BEAN_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Bean/historical/final/BEAN_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyBean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Bean/")

###GCMs y variedades de FRIJOL
variedades<- c("A193", "BAT881","BRSRadiante","Carioca", "ICTAOstua", "Manitou", "Perola")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Bean_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyBean,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running BEAN IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  



#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Bean_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyBean,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running BEAN RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#MAIZE --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/historical/final/MAIZE_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Maize/historical/final/MAIZE_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyMaize<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Maize/")

###GCMs y variedades de FRIJOL
variedades<-c("H6","FM6","MCCURDY6714")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Maize_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyMaize,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running MAIZE IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Maize_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyMaize,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running MAIZE RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#WHEAT --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/historical/final/WHEAT_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Wheat/historical/final/WHEAT_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copyWheat<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Wheat/")

###GCMs y variedades de FRIJOL
variedades<-c("BrigadierBA","DonErnestoBA","Gerek79BA","HalconsnaBA" ,"Seri82BA","TajanBA")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Wheat_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyWheat,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running WHEAT IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Wheat_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="RA", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copyWheat,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running WHEAT RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


g=gc;rm(list = ls())

#SOYBEAN --------------------
setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/future/final/")
gdr1<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/historical/final/SOY_irrigation_")
gdr2<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis/Soybean/historical/final/SOY_rainfed_")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")
copySoybean<-  c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/Soybean/")

###GCMs y variedades de FRIJOL
variedades<-c("DONMARIO","Hutcheson")


#### IRRIGATED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Soybeans_riego",'.RDat',sep=''))


#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr1,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_riego[,"x"],lat=crop_riego[,"y"],
                            Area=crop_riego[,"riego.area"],FPU=crop_riego[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copySoybean,"IRRI",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running SOy IRRI ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  

#### RAINFED
##Cargar información de latitud, longitud, area de spam, fpu, etc.
load(paste(grd3,"Soybeans_secano",'.RDat',sep=''))

### variedades de trigo

#loop para cargar datos 
for (v in 1:length(variedades)){
      
      try(load(paste(gdr2,variedades[v],"_","WFD",".RDat",sep = "")))
      
      #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada año (columnas)
      rend<-matrix(nrow=length(Run), ncol=28)
      for (i in 1:length(Run))
      {rend[i,]<- Run[[i]][,"HWAH"][1:28]}  #solo incluir años 1 a 28
      
      #descartar pixeles con demasiadas fallas
      rend[rend==-99] = 0  #convertir -99 a 0
      
      #Convertir rend en un data.frame
      rend<-data.frame(rend)
      
      #Asignar nombres a el data frame de rendimientos
      colnames(rend)<-paste0("20",22:49)
      
      #Creo un dataframe
      eval(parse(text=paste('md<-data.frame(long=crop_secano[,"x"],lat=crop_secano[,"y"],
                            Area=crop_secano[,"secano.area"],FPU=crop_secano[,"New_FPU"],
                            v=variedades[v],sce="WFD",sys="IRRI", rend)',sep='')))      
      md$pix<- NA
      for (row in 1:nrow(md)){
            md$pix<- paste("Pix_" ,"long", md$long,"_","lat", md$lat, sep = "")
      }
      
      md<- md[,c("long","lat","pix","Area","FPU","v","sys","sce", paste0("X20",22:49))]
      
      #Computador personal
      write.csv(md,paste(copySoybean,"RA",'_',variedades[v],'_',"WFD",'_FPU.csv',sep=''),row.names=T)
      cat(paste("Running SOY RA ", variedades[v], " of gcm WFD"," Done!!\n", sep = "" ))
      
}  


 g=gc;rm(list = ls())
#Datos de SPAM
#carlos edo

library(dplyr)
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(maptools)
library(sp)
library(maptools)
library(maps)
library(raster)
library(ncdf4)
# install.packages(c("ncdf4", "RNetCDF"),
#                  configure.args = c(RNetCDF = "--with-netcdf-include=/usr/include/udunits2"))

#directorios y shapeALC
path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/07-SPAM_data/SPAM2005/yield/'
r_wheat<- '//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/DataAll/Raster/'
map <- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Shape_files/"
alc <- shapefile(paste0(map,"Latino_America1.shp"))
plot(alc)


############################################################## WHEAT----
#Wheat Irrigated----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
wheat_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_wheat_irrigated.nc'))
y_r_i_wheat<- raster(paste0(r_wheat,'Wheat_ncc_noresm1_m_Irrigated.tif'))

writeRaster(wheat_ra_spam, filename=paste("C:/Users/CEGONZALEZ/Documents/BIDCarlos/raster/","WheatSPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)

spam_wheat_ra_re[spam_wheat_ra_re>=1]<-1
plot(spam_wheat_ra_re)
s<- spam_wheat_ra_re[spam_wheat_ra_re==1]
sum(s)
sum(spam_wheat_ra_re[spam_wheat_ra_re==1])

plot(wheat_ra_spam)
plot(alc,add=T)

# crop using Extent
extent(wheat_ra_spam) #extension de spam
extent(alc) #extension de crop modeling ciat
crs(y_r_i_wheat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(wheat_ra_spam) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#Recorte primero por extent y luego mascara
cr <- crop(wheat_ra_spam, extent(alc))
cr <- mask(cr, mask = alc)

cr[cr>0]<-1
cr<- cr[cr==1]
sum(cr)



plot(cr)
plot(alc, add=T)



#Hacer el resample para organizar los pixeles del mismo tamaño
spam_wheat_ra_re<-resample(cr,y_r_i_wheat)
plot(spam_wheat_ra_re)


#Exportar  tipo raster
writeRaster(spam_wheat_ra_re, filename=paste(r_wheat,"Wheat_resample","_","IRR_","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_wheat, filename=paste(r_wheat,"Wheat_cropmodeling","_","IRR_","ncc_noresm1_m_", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
spam<- spam_wheat_ra_re # spam
spam[spam>0]<- 1
plot(spam)

ciat<- y_r_i_wheat # ciat
ciat[ciat>0]<-1


spat<-ciat+spam
plot(spat)
spat[spat==2]<-1
plot(spat)
plot(alc,add=T)
writeRaster(spat, filename=paste("C:/Users/CEGONZALEZ/Documents/BIDCarlos/raster/","Wheat_spatial_coindicence","_","IRR_","ncc_noresm1_m_","BrigadierBA_",".tif",sep=""), format="GTiff", overwrite=TRUE)

#tasa de coincidencia espacial
#crop modeling
ciat<- ciat[ciat>=1]
dapa<- sum(ciat[ciat>=1])
px<- sum(spat[spat>=1])
spam<-sum(spam[spam>=1])
# tasa de uso de pixeles
rate<-px/spam 

rm(wheat_ra_spam,cr,y_r_i_wheat)

#Wheat Rainfed----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
wheat_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_wheat_rainfed.nc'))
y_r_i_wheat<- raster(paste0(r_wheat,'Wheat_ncc_noresm1_m_rainfed.tif'))


# crop using Extent
extent(wheat_ra_spam) #extension de spam
extent(alc) #extension de crop modeling ciat
crs(y_r_i_wheat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(wheat_ra_spam) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


#Recorte primero por extent y luego mascara
cr <- crop(wheat_ra_spam, extent(alc))
cr <- mask(cr, mask = alc)
plot(cr)
plot(alc, add=T)



#Hacer el resample para organizar los pixeles del mismo tamaño
spam_wheat_ra_re<-resample(cr,y_r_i_wheat)
plot(spam_wheat_ra_re)


#Exportar  tipo raster
writeRaster(spam_wheat_ra_re, filename=paste(r_wheat,"Wheat_resample","_","RA_","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_wheat, filename=paste(r_wheat,"Wheat_cropmodeling","_","RA_","ncc_noresm1_m_", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
spam<- spam_wheat_ra_re # spam
spam[spam>0]<- 1
plot(spam)

ciat<- y_r_i_wheat # ciat
ciat[ciat>0]<-1

spat<-ciat+spam
plot(spat)
spat[spat==2]<-1
plot(spat)
plot(alc,add=T)
writeRaster(spat, filename=paste("C:/Users/CEGONZALEZ/Documents/BIDCarlos/raster/","Wheat_spatial_coindicence","_","RA_","ncc_noresm1_m_","BrigadierBA_",".tif",sep=""), format="GTiff", overwrite=TRUE)

#tasa de coincidencia espacial
#crop modeling
ciat<- ciat[ciat>=1]
dapa<- sum(ciat[ciat>=1])
px<- sum(spat[spat>=1])
spam<-sum(spam[spam>=1])
# tasa de uso de pixeles
rate<-px/spam 

rm(wheat_ra_spam,cr,y_r_i_wheat)

############################################################## BEAN----

#Bean Irrigated----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
bean_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_bean_irrigated.nc'))
y_r_i_bean<- raster(paste0(r_wheat,'Bean_ncc_noresm1_m_Irrigated.tif'))

# crop using Extent
extent(bean_ra_spam) #extension de spam
extent(alc) #extension de crop modeling ciat
crs(y_r_i_bean) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(bean_ra_spam) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


#Recorte primero por extent y luego mascara
bean_ra_spam <- crop(bean_ra_spam, extent(alc))
bean_ra_spam <- mask(cr, mask = alc)
plot(cr)
plot(alc, add=T)

#exportar  tipo raster
writeRaster(bean_ra_spam, filename=paste(r_wheat,"Bean_resample","_","IRR","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_bean, filename=paste(r_wheat,"Bean_cropmodeling","_","IRR_","ncc_noresm1_m_","Perola", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)

#cohindicendia spatial/ algegra de raster
spam<- bean_ra_spam # spam
spam[spam>0]<- 1
plot(spam)

ciat<- y_r_i_bean # ciat
ciat[ciat>0]<-1

spat<-ciat+spam
plot(spat)
spat[spat==2]<-1
plot(spat)
plot(alc,add=T)
writeRaster(spat, filename=paste("C:/Users/CEGONZALEZ/Documents/BIDCarlos/raster/","Wheat_spatial_coindicence","_","RA_","ncc_noresm1_m_","BrigadierBA_",".tif",sep=""), format="GTiff", overwrite=TRUE)

#tasa de coincidencia espacial
#crop modeling
ciat<- ciat[ciat>=1]
dapa<- sum(ciat[ciat>=1])
px<- sum(spat[spat>=1])
spam<-sum(spam[spam>=1])
# tasa de uso de pixeles
rate<-px/spam 

rm(wheat_ra_spam,cr,y_r_i_wheat)

#Bean Rainfed----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
bean_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_bean_rainfed.nc'))
y_r_i_bean<- raster(paste0(r_wheat,'Bean_ncc_noresm1_m_rainfed.tif'))


# extraer pixeles de spam para ALC
extent(bean_ra_spam) #extension de spam
extent(y_r_i_bean) #extension de crop modeling ciat

# extraer los datos de spam con la capa de crop modeling
cp1<-crop(bean_ra_spam,alc)
cp1
plot(cp1)


#Hacer el resample para organizar los pixeles del mismo tamaño
crs(y_r_i_bean) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(spam_bean_ra_re) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
spam_bean_ra_re<-resample(cp1,y_r_i_bean)

#exportar  tipo raster
writeRaster(spam_bean_ra_re, filename=paste(r_wheat,"Bean_resample","_","RA","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_bean, filename=paste(r_wheat,"Bean_cropmodeling","_","RA_","ncc_noresm1_m_", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
# spat<- spam_wheat_ra_re + y_r_i_wheatv
r<- spam_bean_ra_re
r[r>0]<- 1
r
w<- y_r_i_bean
w[w>0]<-1
w
spat<-w+r
plot(spat)

writeRaster(spat, filename=paste(r_wheat,"Bean_spatial_coindicence","_","RA_","ncc_noresm1_m_",".tif",sep=""), format="GTiff", overwrite=TRUE)

#tasa de coincidencia
spat[spat>1]<-1
spat<- spat[spat>=1]
sum(spat)
sum(spat)/sum(spam)

############################################################## RICE----

#Rice Irrigated----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
rice_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_rice_irrigated.nc'))
y_r_i_rice<- raster(paste0(r_wheat,'Rice_ncc_noresm1_m_Irrigated.tif'))


# extraer pixeles de spam para ALC
extent(rice_ra_spam) #extension de spam
extent(y_r_i_rice) #extension de crop modeling ciat

# extraer los datos de spam con la capa de crop modeling
cp1<-crop(rice_ra_spam,alc)
cp1
plot(cp1)

extent(cp1) #extension de spam
extent(alc) #extension de crop modeling ciat


#Hacer el resample para organizar los pixeles del mismo tamaño
crs(y_r_i_rice) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(rice_ra_spam) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
spam_rice_ra_re<-resample(cp1,y_r_i_rice)

#exportar  tipo raster
writeRaster(spam_rice_ra_re, filename=paste(r_wheat,"Rice_resample","_","IRR","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_bean, filename=paste(r_wheat,"Rice_cropmodeling","_","IRR_","ncc_noresm1_m_","Perola", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
# spat<- spam_wheat_ra_re + y_r_i_wheatv
r<- spam_rice_ra_re
r[r>0]<- 1
r
w<- y_r_i_rice
w[w>0]<-1
w
spat<-w+r
plot(spat)

writeRaster(spat, filename=paste(r_wheat,"Rice_spatial_coindicence","_","IRR_","ncc_noresm1_m_","IR8",".tif",sep=""), format="GTiff", overwrite=TRUE)


#tasa de coincidencia espacial
#crop modeling
modelling<- w[w>=1]
sum(modelling)
#spam
spam<- r[r>=1]
sum(spam)

#tasa de coincidencia
spat[spat>1]<-1

spat<- spat[spat>=1]
sum(spat)

sum(spat)/sum(spam)

#tasa de coincidencia
spat[spat>1]<-1
spat<- spat[spat>=1]
sum(spat)
sum(spat)/sum(spam)

#Rice Rainfed----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
rice_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_rice_rainfed.nc'))
y_r_i_rice<- raster(paste0(r_wheat,'Rice_ncc_noresm1_m_rainfed.tif'))



# extraer los datos de spam con la capa de crop modeling
cp1<-crop(rice_ra_spam,alc)
cp1
plot(cp1)


#Hacer el resample para organizar los pixeles del mismo tamaño
crs(y_r_i_rice) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(spam_rice_ra_re) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
spam_rice_ra_re<-resample(cp1,y_r_i_rice)

#exportar  tipo raster
writeRaster(spam_rice_ra_re, filename=paste(r_wheat,"Rice_resample","_","RA","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_rice, filename=paste(r_wheat,"Rice_cropmodeling","_","RA_","ncc_noresm1_m_", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
# spat<- spam_wheat_ra_re + y_r_i_wheatv
r<- spam_rice_ra_re
r[r>0]<- 1
r
w<- y_r_i_rice
w[w>0]<-1
w
spat<-w+r
plot(spat)

writeRaster(spat, filename=paste(r_wheat,"Rice_spatial_coindicence","_","RA_","ncc_noresm1_m_",".tif",sep=""), format="GTiff", overwrite=TRUE)

#tasa de coincidencia
spat[spat>1]<-1
spat<- spat[spat>=1]
sum(spat)
sum(spat)/sum(spam)

############################################################## MAIZE----

#MAIZE Irrigated----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
maize_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_maize_irrigated.nc'))
y_r_i_maize<- raster(paste0(r_wheat,'Maize_ncc_noresm1_m_Irrigated.tif'))


# extraer pixeles de spam para ALC
extent(maize_ra_spam) #extension de spam
extent(y_r_i_maize) #extension de crop modeling ciat

# extraer los datos de spam con la capa de crop modeling
cp1<-crop(maize_ra_spam,alc)
cp1
plot(cp1)



#Hacer el resample para organizar los pixeles del mismo tamaño
crs(y_r_i_maize) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(maize_ra_spam) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
spam_maize_ra_re<-resample(cp1,y_r_i_maize)

#exportar  tipo raster
writeRaster(spam_maize_ra_re, filename=paste(r_wheat,"Maize_resample","_","IRR","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_maize, filename=paste(r_wheat,"Maize_cropmodeling","_","IRR_","ncc_noresm1_m_","Perola", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
# spat<- spam_wheat_ra_re + y_r_i_wheatv
r<- spam_maize_ra_re
r[r>0]<- 1
r
w<- y_r_i_maize
w[w>0]<-1
w
spat<-w+r
plot(spat)

writeRaster(spat, filename=paste(r_wheat,"Maize_spatial_coindicence","_","IRR_","ncc_noresm1_m_","H6",".tif",sep=""), format="GTiff", overwrite=TRUE)


#tasa de coincidencia espacial
#crop modeling
modelling<- w[w>=1]
sum(modelling)
#spam
spam<- r[r>=1]
sum(spam)

#tasa de coincidencia
spat[spat>1]<-1

spat<- spat[spat>=1]
sum(spat)

sum(spat)/sum(spam)

#tasa de coincidencia
spat[spat>1]<-1
spat<- spat[spat>=1]
sum(spat)
sum(spat)/sum(spam)

#Maize Rainfed----------------------
#Raster de smap y raster de los rendimientos obtenidos en la modelacion de cultivos
maize_ra_spam<- raster(paste0(path,'spam2005v2r0_yield_maize_rainfed.nc'))
y_r_i_maize<- raster(paste0(r_wheat,'Maize_ncc_noresm1_m_rainfed.tif'))



# extraer los datos de spam con la capa de crop modeling
cp1<-crop(maize_ra_spam,alc)
cp1
plot(cp1)


#Hacer el resample para organizar los pixeles del mismo tamaño
crs(y_r_i_maize) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(maize_ra_spam) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
maize_ra_spam<-resample(cp1,y_r_i_maize)

#exportar  tipo raster
writeRaster(spam_maize_ra_re, filename=paste(r_wheat,"Maize_resample","_","RA","SPAM",".tif",sep=""), format="GTiff", overwrite=TRUE)
writeRaster(y_r_i_maize, filename=paste(r_wheat,"Maize_cropmodeling","_","RA_","ncc_noresm1_m_", "CIAT",".tif",sep=""), format="GTiff", overwrite=TRUE)


#cohindicendia spatial/ algegra de raster
# spat<- spam_wheat_ra_re + y_r_i_wheatv
r<- spam_maize_ra_re
r[r>0]<- 1
r
w<- y_r_i_maize
w[w>0]<-1
w
spat<-w+r
plot(spat)

writeRaster(spat, filename=paste(r_wheat,"Maize_spatial_coindicence","_","RA_","ncc_noresm1_m_",".tif",sep=""), format="GTiff", overwrite=TRUE)

#tasa de coincidencia
spat[spat>1]<-1
spat<- spat[spat>=1]
sum(spat)
sum(spat)/sum(spam)

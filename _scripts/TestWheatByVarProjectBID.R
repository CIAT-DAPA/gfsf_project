# Exploracion de los resultados de BID  por variedades modeladas
# Carlos Eduardo Gonzalez III

##########################  Importa, filtra, organiza, agrega y calcula los rendimeintos ponderados.------------
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/EvaluaciónWheatBID.R")


########################## Procesamiento de datos trigo pasado y futuro IRRIGATED---------
# file Future, load and processing -------------

setwd("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BIDsecundVersion/Wheat_IRR/Wheat_Future")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")

wheat_files <- list.files()
wheat_files <- lapply(wheat_files, read.csv)

# Apilar resultados
wheat_files <- do.call(rbind, wheat_files)

# Procesar
wheat_files$sce<- as.character(wheat_files$sce)
wheat_files$var<- as.character(wheat_files$var)
wheat_files$pixels.original.Var1<- as.character(wheat_files$pixels.original.Var1)
wheat_files$X<- NULL

# Analisis 
names(wheat_files)[1]<- paste("FPU")  # cambio nombre de columna
names(wheat_files)[6:33]<- paste0("20",22:49)

# Reshape para graficar
require(plyr)
require(tidyr)
data_all<- wheat_files
data_all<- data_all %>% 
    gather(year,val, 6:33) 

# crear lista de FPUS
fpu<- unique(data_all$FPU)


# graficas
require(ggplot2)

g<-ggplot(data=data_all, aes(x=year,y=val))+ 
    geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
    ylab("Wighted Yield ") +
    xlab("Years") +
    theme(axis.text.x=element_text(size=7, angle=90))+
    guides(color=guide_legend("FPUs"))

tiff(filename=paste(copy,"IRRI_WheatVarietiesGCMs.tiff",sep=""), 
     width = 18, height = 12, units = 'in', res = 100)

plot(g)

dev.off()
    

# Using function
by_fpu<- list()
for(i in 1:length(fpu)){
        by_fpu[[i]]<- data_all[which(data_all$FPU==fpu[[i]]),]
    }

pic<-list()
grap<- function(x){
    ggplot(data=x, aes(x=year,y=val))+ 
        geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
        ylab("Wighted Yield ") + ggtitle(x)+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("FPUs"))
    
}
require(ggplot2)
pic<-lapply(by_fpu, grap)
require(gridExtra)
h<-do.call(grid.arrange,pic)


g=gc; rm(list = ls())
# file Historical, load and processing -------------
setwd("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BIDsecundVersion/Wheat_IRR/Wheat_Historical")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")

wheat_files <- list.files()
wheat_files <- lapply(wheat_files, read.csv)

# Apilar resultados
wheat_files <- do.call(rbind, wheat_files)

# Procesar
wheat_files$sce<- as.character(wheat_files$sce)
wheat_files$var<- as.character(wheat_files$var)
wheat_files$pixels.original.Var1<- as.character(wheat_files$pixels.original.Var1)
wheat_files$X<- NULL

# Analisis 
names(wheat_files)[1]<- paste("FPU")  # cambio nombre de columna
names(wheat_files)[6:33]<- paste0("20",22:49)

# Reshape para graficar
require(plyr)
require(tidyr)
data_all<- wheat_files
data_all<- data_all %>% 
    gather(year,val, 6:33) 


# graficas
require(ggplot2)

g1<-ggplot(data=data_all, aes(x=year,y=val))+ 
    geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
    ylab("Wighted Yield ") +
    xlab("Years") +
    theme(axis.text.x=element_text(size=7, angle=90))+
    guides(color=guide_legend("FPUs"))

tiff(filename=paste(copy,"IRR_WheatVarietiesWFD.tiff",sep=""), 
     width = 18, height = 12, units = 'in', res = 100)

plot(g1)

dev.off()


g=gc; rm(list = ls())


# wheat_files <- sapply(name_files, function(x){
#   load(file =x,  )
#   get(ls()[ls()!="wheat_files"])
# })


########################## Procesamiento de datos trigo pasado y futuro RAINFED---------
# file Future, load and processing -------------

setwd("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BIDsecundVersion/Wheat_RA/Wheat_Future")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")

wheat_files <- list.files()
wheat_files <- lapply(wheat_files, read.csv)

# Apilar resultados
wheat_files <- do.call(rbind, wheat_files)

# Procesar
wheat_files$sce<- as.character(wheat_files$sce)
wheat_files$var<- as.character(wheat_files$var)
wheat_files$pixels.original.Var1<- as.character(wheat_files$pixels.original.Var1)
wheat_files$X<- NULL

# Analisis 
names(wheat_files)[1]<- paste("FPU")  # cambio nombre de columna
names(wheat_files)[6:33]<- paste0("20",22:49)

# Reshape para graficar
require(plyr)
require(tidyr)
data_all<- wheat_files
data_all<- data_all %>% 
    gather(year,val, 6:33) 

# crear lista de FPUS
fpu<- unique(data_all$FPU)


# graficas
require(ggplot2)

g<-ggplot(data=data_all, aes(x=year,y=val))+ 
    geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
    ylab("Wighted Yield ") +
    xlab("Years") +
    theme(axis.text.x=element_text(size=7, angle=90))+
    guides(color=guide_legend("FPUs"))

tiff(filename=paste(copy,"RA_WheatVarietiesGCMs.tiff",sep=""), 
     width = 18, height = 12, units = 'in', res = 100)

plot(g)

dev.off()


# Using function
by_fpu<- list()
for(i in 1:length(fpu)){
    by_fpu[[i]]<- data_all[which(data_all$FPU==fpu[[i]]),]
}

pic<-list()
grap<- function(x){
    ggplot(data=x, aes(x=year,y=val))+ 
        geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
        ylab("Wighted Yield ") + ggtitle(x)+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("FPUs"))
    
}
require(ggplot2)
pic<-lapply(by_fpu, grap)
require(gridExtra)
h<-do.call(grid.arrange,pic)


g=gc; rm(list = ls())
# file Historical, load and processing -------------

setwd("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BIDsecundVersion/Wheat_RA/Wheat_Historical")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")

wheat_files <- list.files()
wheat_files <- lapply(wheat_files, read.csv)

# Apilar resultados
wheat_files <- do.call(rbind, wheat_files)

# Procesar
wheat_files$sce<- as.character(wheat_files$sce)
wheat_files$var<- as.character(wheat_files$var)
wheat_files$pixels.original.Var1<- as.character(wheat_files$pixels.original.Var1)
wheat_files$X<- NULL

# Analisis 
names(wheat_files)[1]<- paste("FPU")  # cambio nombre de columna
names(wheat_files)[6:33]<- paste0("20",22:49)

# Reshape para graficar
require(plyr)
require(tidyr)
data_all<- wheat_files
data_all<- data_all %>% 
    gather(year,val, 6:33) 


# graficas
require(ggplot2)

g1<-ggplot(data=data_all, aes(x=year,y=val))+ 
    geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
    ylab("Wighted Yield ") +
    xlab("Years") +
    theme(axis.text.x=element_text(size=7, angle=90))+
    guides(color=guide_legend("FPUs"))

tiff(filename=paste(copy,"RA_WheatVarietiesWFD.tiff",sep=""), 
     width = 18, height = 12, units = 'in', res = 100)

plot(g1)

dev.off()


g=gc; rm(list = ls())



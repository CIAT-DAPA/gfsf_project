# Prcesamiento/Validación/Análisis de los resultados de la modelación de cultivo.
# Benjamin & Carlos 


##########################  Importa, filtra, organiza, agrega y calcula los rendimeintos ponderados.------------
#setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2")


#Directorios generales------------------
github<-("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/")
copy<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/graphs/")
archivo<- ("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BIDsecundVersion/")
grp<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
crops<- c("Bean", "Wheat", "Maize", "Rice" )  # "Rice", "Soybean"
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
               "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
time<- c("Future", "Historical")
sys_q<- c("IRR","RA")



#Funciones para graficar---------------
# Grafico1
grap1<-function(x){
    ggplot(data=x, aes(x=year,y=val,colour=var))+ 
        geom_line(aes(group=var))+
        geom_point()+ facet_grid(var~sce)+
        ylab("Wighted Yield ") + ggtitle("Scatter plot")+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("Varieties"))
}    
    
    
    
# Grafico 2
grap2<- function(x){
    ggplot(data=x, aes(x=val))+ 
                geom_density(aes(x=val, colour=var))+ facet_grid(var~sce)+
                ylab("Density") + ggtitle("Density")+
                xlab("Wighted Yiel") +
                theme(axis.text.x=element_text(size=10, angle=90))+
                guides(color=guide_legend("FPUs"))
}
             
# Grafico 3
grap3<- function(x){
    ggplot(data=x, aes(x=val))+ 
        geom_density(aes(x=val, colour=var), position = "stack" )+
         ylab("Density") + ggtitle("Density")+
         xlab("Wighted Yiel") +
         theme(axis.text.x=element_text(size=10, angle=90))+
         guides(color=guide_legend("Var"))
            
}
             
             

#Desarrollo del loop-----------------


for(c in 1:length(crops)){
    for(s in 1:length(sys_q)){
        for(t in 1:length(time)){
                             c_files <- list.files(path= paste(grp, "/", crops[c], "_", sys_q[s], "/", crops[c], "_", time[t], sep = ""), full.names = T)
                             c_files <- lapply(c_files, read.csv)
                             c_files <- do.call(rbind,c_files)
                             # Procesar 
                             c_files$sce<- as.character(c_files$sce)
                             c_files$var<- as.character(c_files$var)
                             c_files$pixels.original.Var1<- as.character(c_files$pixels.original.Var1)
                             c_files$X<- NULL
         
                             # Analisis 
                             names(c_files)[1]<- paste("FPU")  # cambio nombre de columna
                             names(c_files)[6:33]<- paste0("20",22:49)
                             
                             
                             # Reshape para graficar
                             require(plyr)
                             require(tidyr)
                             c_files<- c_files
                             c_files<- c_files %>% 
                                 gather(year,val, 6:33) 
                             
                             # crear lista de FPUS
                             #c_files$crop<- paste(crops[c],"_", sys_q[s],sep = "")
                             fpu<- unique(c_files$FPU)
                             
                             #crear listas de Variedades
                             variedades<- unique(c_files$var)
                             #crear lista de Scenarios
                             sce<- unique(c_files$sce)
                             
                             #################################### Using function by FPU
                             by_fpu <- list()
                             for(i in 1:length(fpu)){
                                 by_fpu[[i]] <- c_files[which(c_files$FPU==fpu[[i]]),]
                             }
                             
                             
                             ################################### Using Function by variedades
                             by_var <- list()
                             for(v in 1:length(variedades)){
                                 by_var[[v]]<- c_files[which(c_files$var==variedades[[v]]),]
                             }
                             
                             ################################### Using function by Scenarios
                             by_scer<- list()
                             for(g in 1:length(sce)){
                                 by_scer[[g]]<- c_files[which(c_files$var==sce[[g]]),]
                             }
                             
                             
                             ################################## Definiciond de variedades
                             require(ggplot2)
                           
                             ########################### Correr graficos
                             #####FPU
                             pic1 <-list()
                             pic1 <-lapply(by_fpu, grap1)
                             for(m in 1:length(pic1)){
                                 tiff(filename = paste(copy,crops[c],"_",sys_q[s],"_var_", fpu[m],"_",time[t],"_", "trend",".tiff",sep=""), 
                                      width = 18, height = 12, units = 'in', res = 100)
                                 plot(pic1[[m]])
                                 dev.off()
                             }
                             

#                              #####FPU
#                              pic3<-list()
#                              pic3<-lapply(by_fpu, grap3)
#                              
#                              for(m in 1:length(pic3)){
#                                  for(f in 1:length(fpu)){
#                                  tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_var_", fpu[m],"_","Density",".tiff",sep=""), 
#                                       width = 18, height = 12, units = 'in', res = 100)
#                                  plot(pic3[[m]])
#                                  dev.off()
#                                  }
                             }
                     
        }
    } 
    
         
#         #####Var
#             pic2<-list()
#             pic2<-lapply(by_var, grap2)
#             require(gridExtra)
#             z<-do.call(grid.arrange,pic2)
#             
#             tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density_Var",".tiff",sep=""), 
#                  width = 18, height = 12, units = 'in', res = 100)
#             
#             plot(z)
#             
#             dev.off()
#             
#             rm(z)
#          
#          #####Sce
#             pic3<-list()
#             pic3<-lapply(by_scer, grap3)
#             require(gridExtra)
#             z<-do.call(grid.arrange,pic3)
#             
#             tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density_Sce",".tiff",sep=""), 
#                  width = 18, height = 12, units = 'in', res = 100)
#             
#             plot(z)
#             
#             dev.off()
#             
#             rm(z)


g=gc;rm(list = ls())


###################################################################################################+




require(ggplot2)
pic1<-list()
by_fpu[1]
grap1<-function(x){
    ggplot(data=x, aes(x=year,y=val,colour=var))+ 
        geom_line(aes(group=var))+
        geom_point()+ facet_grid(var~.)+
        ylab("Wighted Yield ") + ggtitle("Scatter plot")+
        xlab("Years") +
        theme(axis.text.x=element_text(size=7, angle=90))+
        guides(color=guide_legend("Varieties"))
    
}

pic1<-lapply(by_fpu[1], grap1)

for(m in 1:length(pic1[1])){
    tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_var_fpu",".tiff",sep=""), 
         width = 18, height = 12, units = 'in', res = 100)
    plot(pic1[[m]])
    dev.off()
    
}


# Prcesamiento/Validación/Análisis de los resultados de la modelación de cultivo.
# Benjamin & Carlos 


##########################  Importa, filtra, organiza, agrega y calcula los rendimeintos ponderados.------------
#setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2")


#Directorios generales---------
github<-("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/")
copy<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/graphs/")
archivo<- ("C:/Users/CEGONZALEZ/Documents/BIDCarlos/BIDsecundVersion/")
grp<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/")
crops<- c("Rice", "Bean", "Wheat", "Soybean", "Maize" )
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
               "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
time<- c("Future", "Historical")
sys_q<- c("IRR","RA")




for(c in length(crops)) {
    #correr codigo base
#     source(paste(github,"Evaluación",crops[c],"BID.R", sep=""))

    #procesar y cargar los archivos por cada cultivo y por categoria historical and future
    
    for(t in length(time)){     
        
        for(s in length(sys_q))
         c_files<- list.files(path= paste(grp, "/", crops[c], "_", sys_q[s], "/", crops[c], "_", time[t], sep = ""), full.names = T)
         c_files<- lapply(c_files, read.csv)
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
         data_all<- c_files
         data_all<- data_all %>% 
             gather(year,val, 6:33) 
         
         # crear lista de FPUS
         data_all$crop<- paste(crops[c],"_", sys_q[s],sep = "")
         fpu<- unique(data_all$FPU)
         
         #crear listas de Variedades
         variedades<- unique(data_all$var)
         #crear lista de Scenarios
         sce<- unique(data_all$sce)
         
         #################################### Using function by FPU
         by_fpu<- list()
         for(i in 1:length(fpu)){
             by_fpu[[i]]<- data_all[which(data_all$FPU==fpu[[i]]),]
         }
         
         
         ################################### Using Function by variedades
         by_var<- list()
         for(v in 1:length(variedades)){
             by_var[[v]]<- data_all[which(data_all$var==variedades[[v]]),]
         }
         
         ################################### Using function by Scenarios
         by_scer<- list()
         for(g in 1:length(sce)){
             by_scer[[g]]<- data_all[which(data_all$var==sce[[g]]),]
         }
         
         
         ################################## Definiciond de variedades
         require(ggplot2)
         # Grafico1
         grap1<-function(x){
             ggplot(data=x, aes(x=year,y=val))+ 
                 geom_point(aes(colour = factor(FPU)),shape=15,size = 1.5)+ facet_grid(var~sce)+
                 ylab("Wighted Yield ") + ggtitle("Scatter plot")+
                 xlab("Years") +
                 theme(axis.text.x=element_text(size=7, angle=90))+
                 guides(color=guide_legend("FPUs"))
             
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
         
         
         ########################### Correr graficos
         #####FPU
         pic1<-list()
         pic1<-lapply(by_fpu, grap1)
         require(gridExtra)
         z<-do.call(grid.arrange,pic1)
         
         tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density_FPU",".tiff",sep=""), 
              width = 18, height = 12, units = 'in', res = 100)
         
         plot(z)
         
         dev.off()
         
            rm(z)
        #####Var
            pic2<-list()
            pic2<-lapply(by_var, grap2)
            require(gridExtra)
            z<-do.call(grid.arrange,pic2)
            
            tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density_Var",".tiff",sep=""), 
                 width = 18, height = 12, units = 'in', res = 100)
            
            plot(z)
            
            dev.off()
            
            rm(z)
         
         #####Sce
            pic3<-list()
            pic3<-lapply(by_scer, grap3)
            require(gridExtra)
            z<-do.call(grid.arrange,pic3)
            
            tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density_Sce",".tiff",sep=""), 
                 width = 18, height = 12, units = 'in', res = 100)
            
            plot(z)
            
            dev.off()
            
            rm(z)

        
    }
}     
  

         
g=gc;rm(list = ls())


# 
# grap4<- function(x){
#     ggplot(data=x, aes(x=val))+ 
#         geom_density(aes(x=val, colour=sce), position = "stack" )+
#         ylab("Density") + ggtitle("Density")+ facet_grid(~var)+
#         xlab("Wighted Yiel") +
#         theme(axis.text.x=element_text(size=10, angle=90))+
#         guides(color=guide_legend("Varieties"))
#     
# }
# 
# 
# # Doing graphs density
# 
# # graph2
# pic<-lapply(by_fpu, grap2)
# require(gridExtra)
# z<-do.call(grid.arrange,pic)
# 
# tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density",".tiff",sep=""), 
#      width = 18, height = 12, units = 'in', res = 100)
# 
# plot(z)
# 
# dev.off()
# 
# # graph3
# pic<-lapply(by_fpu, grap3)
# require(gridExtra)
# z<-do.call(grid.arrange,pic)
# 
# tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density",".tiff",sep=""), 
#      width = 18, height = 12, units = 'in', res = 100)
# 
# plot(z)
# 
# dev.off()
# 
# # graph4
# pic<-lapply(by_fpu, grap4)
# require(gridExtra)
# z<-do.call(grid.arrange,pic)
# 
# tiff(filename=paste(copy,crops[c],"_",sys_q[s],"_Density",".tiff",sep=""), 
#      width = 18, height = 12, units = 'in', res = 100)
# 
# plot(z)
# 
# dev.off()
# 
# 
# # prueba 1
# by_fpu[1]
# 
# grap2<- function(x){
#     ggplot(data=x, aes(x=val))+ 
#         geom_density(aes(x=val, colour=var))+ facet_grid(var~sce)+
#         ylab("Density") + ggtitle("Density")+
#         xlab("Wighted Yiel") +
#         theme(axis.text.x=element_text(size=10, angle=90))+
#         guides(color=guide_legend("FPUs"))
#     
# }
# 
# pic<- lapply(by_fpu[1], grap2)
# pic
# 
# # prueba 2
# 
# by_fpu[1]
# 
# grap3<- function(x){
#     ggplot(data=x, aes(x=val))+ 
#         geom_density(aes(x=val, colour=var), position = "stack" )+
#         ylab("Density") + ggtitle("Density")+
#         xlab("Wighted Yiel") +
#         theme(axis.text.x=element_text(size=10, angle=90))+
#         guides(color=guide_legend("Var"))
#     
# }
# 
# pic<- lapply(by_fpu[1], grap3)
# pic
# 
# 
# 
# # prueba 3
# 
# # prueba 2
# 
# by_fpu[1]
# 
# grap4<- function(x){
#     ggplot(data=x, aes(x=val))+ 
#         geom_density(aes(x=val, colour=sce), position = "stack" )+
#         ylab("Density") + ggtitle("Density")+ facet_grid(~var)+
#         xlab("Wighted Yiel") +
#         theme(axis.text.x=element_text(size=10, angle=90))+
#         guides(color=guide_legend("Varieties"))
#     
# }
# 
# pic<- lapply(by_fpu[1], grap4)
# pic

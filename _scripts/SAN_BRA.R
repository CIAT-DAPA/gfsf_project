# codigo de agregacion version carlos  caso SAN_BRA frijol RAINFED

library(RColorBrewer)
#GCMs----------------------
#directorios
grd1<-("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
copy<- ("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/FPUWeight/")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
resum<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/")
weird<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/SelectVar_Lowest_ExtremeDataBean_RA.csv")
pic<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/sum/hist/")

reg<- read.csv("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ListFPUs.csv",header = T,stringsAsFactors = F )
pots<- reg[,1]

#objetos
sys<- c( "IRRI", "RA")
crops<- c("Rice","Bean","Wheat","Maize","Soybean") 

c=1
s=p=1
for(c in 1:length(crops)){
                   for(s in 1:length(sys)){
                         for(p in 1:length(pots)){

                              dataF<-list.files(path =grd1,pattern = paste(crops[c],"_",sys[s],"_.csv",sep=""),full.names = T)
                              dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
                              DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL

                                  
                              ### Por FPU
                              DataFiles<- DataFiles[which(DataFiles$FPU==pots[p]),]

                              #### Datos y analisis 
                              require(plyr)
                              require(tidyr)
                              DataFiles<- DataFiles %>% gather(year,val, 8:35) 
                              DataFiles$year<- sub(pattern = "X", replacement = "", x = DataFiles$year, ignore.case = T)
                              #rownames(Dafilesy)<- 1:nrow(Dafilesy)
                              #is.na(DataFiles$val)
                              
                              #test
                              test<- sum(DataFiles$val)
                              
                              if(test>0){
                                    
                                    DataNoZero<- DataFiles[which(DataFiles$val!=0),]
                                    q1<- quantile(DataNoZero$val,probs = 0.05,na.rm = T)
                                    inf<- DataFiles
                                    
                                    inf$status<- NA
                                    for(i in 1:nrow(inf)){
                                          if(((inf$val[i]<=q1)+(inf$val[i]!=0))>=2) {inf$status[i]<- "lowest Yield (lower than 5% probs)"}else{} 
                                          if(inf$val[i]==0){inf$status[i]<- "Fallo total (yield==0)"}else{}
                                          if(((inf$val[i]!=0)+(inf$val[i]>q1))>=2){inf$status[i]<- "Normal"}else{} 
                                    }
                                    
                                    #tabla resumen datos
                                    tab<-as.data.frame(table(inf$sce, inf$status))
                                    colnames(tab)[1]<- 'Scenario';colnames(tab)[2]<- 'Status';  colnames(tab)[3]<- 'NoFre'
                                    tab<- tab %>% spread(Scenario,NoFre);  tab$Total<- rowSums(tab[,c(2:10)],na.rm = T)
                                    tab$pots<- pots[p]; tab$crop<- crops[c]; tab$sys<- sys[s]; tab$analysis<- sum(tab$Total)
                                    tab$ratio<- (tab$Total/tab$analysis) *100
                                    #Reporte esta de cada FPU
                                    write.csv(tab,paste(resum,"Report_Cases_Yields_",crops[c],"_",pots[p],"_",sys[s],"_", "_FPU.csv", sep = ""))
                                    cat(paste("termino reporte general ",crops[c], " ", pots[p], " ", sys[s], " proceso continua proceso\n", sep = " "))
                                    }else{cat(paste(pots[p]," no hay dato\n", sep = ""))}
                                    
                              } 
                         }
                   }
rm(DataFiles, DataNoZero, inf, tab, weird)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% agregacion por FPU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
library(RColorBrewer)

c=1
for(p in 1:length(crops)){
            dataF<- list.files(path =resum,pattern = paste("Report_Cases_Yields_",crops[c],sep=""),full.names = T)
            dataF<- lapply(dataF,read.csv,stringsAsFactors = F)
            }

DataFiles<- do.call(rbind,dataF)
DataFiles$X<- NULL
write.csv(DataFiles,paste(resum,"overviewComplete",crops[c],"_",".csv",sep = ""))


#graph ALC Yield Total-----
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  

for(c in 1:length(crops)){
      by_crops <- list()
      by_crops[[c]] <- DataFiles[which(DataFiles$crop==crops[[c]]),]
      
      #hacer graficas
      png(filename = paste(pic,crops[c],"_","behaviour_Data",".png",sep=""), 
          width = 20, height = 12, units = 'in', res = 100)
      
      require(ggplot2)
      n<- ggplot(data = by_crops[[c]], aes(pots,Status)) + 
            geom_tile(aes(fill = ratio), colour = "white")+ 
            labs(x=NULL, y=NULL, title="") +
            scale_fill_gradientn(colours = hm.palette(100))+ theme_grey() + 
            labs(x = "",y = "") + labs(title = paste("Crop = ",unique(by_crops[[c]]$crop), ""))+
            scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_equal()+ 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            facet_grid(sys~., scales = "fixed", space = "fixed") +theme(strip.text=element_text(size=8))+
            theme(strip.text.y = element_text(angle = 0,size = 12))
            

plot(n)
dev.off()    
      
}



# for(p in 1:length(pots)){
#       by_pots <- list()
#       
#       #### filter
#       by_pots[[p]] <- DataFiles[which(DataFiles$pots==pots[[p]]),]
#       
#       if(nrow(by_pots[[p]]) >= 1){
# 
#             #hacer graficas
# #             png(filename = paste(copyPix,pots[p],"_","trend",".png",sep=""), 
# #                 width = 20, height = 12, units = 'in', res = 100)
#             require(ggplot2)
#             n<-ggplot(data=by_pots[[p]], aes(x=year,y=val,colour=pix))+ 
#                   geom_bar()+
#                   geom_point()+ facet_grid(v~sce)+
#                   ylab("Yield ") + ggtitle("Scatter plot")+
#                   xlab("Years") +
#                   theme(axis.text.x=element_text(size=7, angle=90))+
#                   guides(color=guide_legend("Varieties"))+theme(legend.position="none")
#             plot(n)
#             dev.off()
#       } else {
#             cat(paste(" SOY IRRI FPU: ", pots[[p]], " does not have varieties\n", sep = ""))
#       }
#       
# }



##############################################################################################################

for(c in 1:length(crops)){
      
            for(s in 1:length(sys)){
                  
                  for(p in 1:length(pots)){
                        
                        dataF<-list.files(path =grd1,pattern = paste(crops[c],"_",sys[s],"_.csv",sep=""),full.names = T)
                        dataF<-lapply(dataF,read.csv,stringsAsFactors = F)
                        DataFiles<- do.call(rbind,dataF); DataFiles$X<- NULL
                        gcm<- unique(DataFiles$sce); pots<- unique(DataFiles$FPU)
                        
                        #por FPU
                        DataFiles<- DataFiles[which(DataFiles$FPU==pots[p]),]

                        #Agregar columnas de producción de 2022 a 2049
                        DataFiles[,paste0("Prod_20",22:49)]<-DataFiles[,"Area"]*DataFiles[,paste0("X20",22:49)]
                        DataFiles[,'ones'] = 1
                  
               
                        #Eliminar las columnas de los rendimientos
                        DataFiles<-DataFiles[,!names(DataFiles) %in% (paste0("X20",22:49))]
                        
                        
                        #Agregar producción y area a nivel de fpu
                        DataFiles_fpu<-aggregate(DataFiles[,c("ones","Area",paste0("Prod_20",22:49))],by=list(DataFiles[,"FPU"]),FUN= function(x) {sum(x, na.rm=TRUE)} )
                        
                        #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
                        DataFiles_fpu[,paste0("Rend_fpu_20",22:49)]<-DataFiles_fpu[,paste0("Prod_20",22:49)]/DataFiles_fpu[,"Area"]
                        
                        #Crear un data frame con sólo FPU y rend a nivel de fpu 
                        DataFiles_fpu<- DataFiles_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:49))]
                        
                        #Asignar nombres apropiados a las columnas
                        colnames(DataFiles_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:49))

                        #Ordenar datos
                        DataFiles_fpu<- DataFiles_fpu[,c("FPU","num_pixels", "sce", "sys", paste0("Rend_fpu_20",22:49))]
                        
                        #Exportar resultados
                        write.csv(DataFiles_fpu,paste(resum,crops[c],"_",sys[s],"_FPU.csv", sep = ""))
                        
                        cat(paste("Running Yields weight and aggregate for ", crops[c]," ", sys[s], " ",  gcm[g], " it's done\n", sep = "" ))
                        
            }      
      }      
}





g=gc;rm(list = ls())

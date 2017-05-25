# Seleccion de mayores rendimientos por prixel por Año

########################################################### GCMs -------------------------------

#directorios
gdr1<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
grdw<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/WFD/")

########### PARTE B
#cargar files
crops<- c("Rice","Bean","Wheat","Maize", "Soybean") 
sys<- c("IRRI","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

#c=3;s=2;g=9
for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running select to max value for yield: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
                  c_files <- list.files(path=paste(gdr1,"/", crops[c],sep = ""),pattern = sys[s],full.names = T)
                  c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
                  c_files <- do.call(rbind,c_files)
                  c_files$X<- NULL
                  #reshape corregir año
                  require(plyr)
                  require(tidyr)
                  c_files<- c_files %>% 
                        gather(year,val, 9:36) 
                  c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
                  
                  # rendimeintos por pixel/variedad/año
                  c_files<- c_files %>% 
                        spread(v,val) 
                  c_files[,"ymax"] <- apply(c_files[, 9:ncol(c_files)], 1, max)
                  #Eliminar columnas de variedades
                  c_files<- c_files[ ,c(1:8,ncol(c_files):ncol(c_files))]
                  is.na(c_files)
                  #reshape
                  c_files<- c_files %>% 
                        spread(year,ymax) 
                  write.csv(c_files,paste(copy,"YieldMax_",crops[c],"_",sys[s],"_", ".csv",sep = "")) 
                  
       }
     }




########################################################### Datos historicos -------------------------------
#directorios
#cargar files
gcm <- "WFD"


for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            cat(paste("Running select to max value for yield: ",crops[c], " ", sys[s], " done!!!\n", sep = ""))
            
            c_files <- list.files(path=paste(grdw,crops[c],sep = ""),pattern = sys[s],full.names = T)
            c_files <- lapply(c_files, read.csv, stringsAsFactors = F)
            c_files <- do.call(rbind,c_files)
            c_files$X<- NULL
            #reshape corregir año
            require(plyr)
            require(tidyr)
            c_files<- c_files %>% 
                  gather(year,val, 9:36) 
            c_files$year<- sub(pattern = "X", replacement = "", x = c_files$year, ignore.case = T)
            
            # rendimeintos por pixel/variedad/año
            c_files<- c_files %>% 
                  spread(v,val) 
            c_files[,"ymax"] <- apply(c_files[, 9:ncol(c_files)], 1, max)
            #Eliminar columnas de variedades
            c_files<- c_files[ ,c(1:8,ncol(c_files):ncol(c_files))]
            #reshape
            c_files<- c_files %>% 
                  spread(year,ymax) 
            write.csv(c_files,paste(copy,"YieldMax_",crops[c],"_",sys[s],"_WFD", ".csv",sep = "")) 
            
      }
}

g=gc;rm(list = ls())

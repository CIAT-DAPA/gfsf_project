# Pasar al formato base de para agregacion de pixeles a FPU
# separar por GCM y luego hacer un marge con los datos que traen la informacion general para obtener el area
#

#directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels")
copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/VariesSelect/")
copyrun<-c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Pixels/FilesRunPIX/")
grd3<- c("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/")

########### Objetos generales
#cargar files
crops<- c("Rice","Bean","Wheat","Maize") # Soybean
sys<- c("IRRI","RA")
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")



# c=3
# s=1
# g=1
# p=1
pix<-list()

for(c in 1:length(crops)){
      for(s in 1:length(sys)){
            df<-read.csv(paste(copy,"YieldMax_", crops[c],"_", sys[s],"_.csv",sep = ""), stringsAsFactors = F,header = T)
            df$X<- NULL
            #df$Area<-NULL
            gcm<- unique(df$sce)
            pixel<- unique(df$pix)
            dfile<- df
            for(p in 1:length(pixel)){
                  for(g in 1:length(gcm)){
                        pix[[p]]<- dfile[which(dfile$pix==pixel[[p]]),]
                        pix[[p]]<- pix[[p]][which(pix[[p]]$sce==gcm[[g]]),]
                        require(plyr)
                        require(tidyr)
                        pix[[p]]<- pix[[p]] %>% 
                              gather(year,val, 7:34)
                        pix[[p]]$year<- sub(pattern = "X", replacement = "", x =   pix[[p]]$year, ignore.case = T)
                        
                        
                        #save all dataframes inside list and after to export to .RDAT files
                        save(pix,file =paste(copyrun, crops[c],sys[s],"pix.RDat",sep = ""))
                        cat(paste( crops[c]," ", sys[s],"processing", pixel[[p]], "for this GCM ", gcm[[g]], "done!!!\n",sep="" ))
                  }
            }
      }
}





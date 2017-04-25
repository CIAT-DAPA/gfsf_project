
## Operacion "Codigos BID, el final esta cerca programar o morir".
## Autores: Los sobrevivientes Harold A. & Carlos Edo


#Objetos de uso general--------------------------------------------------------------------------
gcm <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
         "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

variedadesWheat<- c("BrigadierBA","DonErnestoBA","Gerek79BA","HalconsnaBA" ,"Seri82BA","TajanBA")
variedadesBean<- c("A193", "BAT881","BRSRadiante","Carioca", "ICTAOstua", "Manitou", "Perola")
variedadesRice<- c("IR8", "IR64","IR72")
variedadesMaize<- c("H6","FM6","MCCURDY6714")
variedadesSoybean<- c("DONMARIO", "Hutcheson")

sys_q<- c("IRR", "RA")
crops<- c( "Rice","Wheat", "Bean", "Soybean","Maize")
Time<- c( "Future", "Historical")


#############################################Codes Github
# Para replicar los codigos aca presentados se debe acceder al github re-running 
github<- "C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  END     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#Parte A. Revision a nivel de FPU---------------------------------------------------------------
#Este codigo permite hacer agregaciones por Variedades/gcm a nivel de FPU para evualar desemeño
# github<- "C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/"
# files <- list.files(github,full.names=TRUE,pattern="Evaluación")
# sapply(files, source)

# #### Source for each crop, treating individual 
# source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/EvaluaciónRiceBID.R")
# source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/EvaluaciónWheatBID.R")
# source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/EvaluaciónBeanBID.R")
# source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/EvaluaciónMaizeBID.R")
# #source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/EvaluaciónSoybeanBID.R")

#Parte B Analisis  y procesamiento de datos a nivel de PIXEL:---------------------------------- 
#Este codigo tiene el proposito de realizar analisis sobre el comportamiento de los rendimientos a nivel de pixel por FPU
#usando variedades,gcms,etc. El resultado final son files tipo .csv 
### "Codigo para crear files por pixeles que corresponden a los FPU y visualizar su comportamiento"
# github<- "C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/"
# files <- list.files(github,full.names=TRUE,pattern="Proof")
# sapply(files, source)
# 


#### Source for each crop, treating individual
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelBean.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelWheat.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelRice.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelMaize.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelWheat.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelSoybean.R")
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProofByPixelWFD.R")


#Review min, max, range mean for pixels data
### Este codigo permite por cultivo/gcm obtener la distribución de los rendimientos 
### para evitar datos muy bajos o muy elevados
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ReviewDataWeirAllCrops.R")

#Parte C.----------------------------------------------------------------------------------------
### "codigo para calcular los rendimientos maximos por variedad por cada pixel/gcm"
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/SelectYieldByVarietiesAtPixel.R")

#Agregacion de rendimientos a nivel de pixel a FPU
#Este codigo toma los rendimientos y los agrega a nivel FPU, calculando una rendimiento ponderado
### "codigo para calcular lso rendimientos poderados y agregados por FPU"
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/AgreggationVersionCarlos.R")


### "Presentacion del formato adecuado para ser leido por IMPACT"
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/CalculoTasasCrecimientoAnualizadas.R")

#Presentacion del file en el formato requerido por CCProcessing.gms
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/FormatoLongAppropiateIMPACT.R")

#Parte D.---------------------------------------------------------------------------------------
#Procesamiento de los resultados de IMPACT para presentar reportes y graficos.
source("C:/Users/CEGONZALEZ/Documents/GitHub/gfsf_project/_scripts/ProcesamientoResultadosIMMPACTBIDVersionCarlos.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




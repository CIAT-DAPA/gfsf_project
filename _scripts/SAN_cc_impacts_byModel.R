### Paper SAN

### Directorio de bases de datos
setwd("C:/Users/User/Dropbox/SAN")

## decimales
options(warn = -1)
options(scipen = 999)

### Librerias
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)

### Crear archivo madre
cfiles <- read.csv("lamp_native_20180917-181537.csv")
### Crear un archivo de respaldo para trabajar llamado fs (food security)
fs <- cfiles

### Revisar tipo de variables
# length(fs) ## N° de columnas
# ls(fs) ## Nombre columnas
# str(fs) ## Estructura del objeto

### Convertir variables factor a caracter
fs$MODEL<-as.character(fs$MODEL)
fs$SCENARIO<- as.character(fs$SCENARIO)
fs$REGION<- as.character(fs$REGION)
fs$VARIABLE<- as.character(fs$VARIABLE)
fs$UNIT<- as.character(fs$UNIT)
# str(fs) ## revisar

### Crar una variable con solo la columna VARIABLE ####
#var<-unique(fs$VARIABLE)

### Corregir y unificar nombres
fs$REGION<- plyr::revalue(fs$REGION, c("Colombia"="COL", "clm"="COL",
                                       "Brazil"="BR", "BRA"="BR","bra"="BR",
                                       "Central America and Caribbean"="CAC", "cac"="CAC",
                                       "Argentina"="ARG",
                                       "Mexico"="MEX", "mex"="MEX",
                                       "South America_Northern"="SUR_AN",
                                       "South America_Southern"="SUR_AS",
                                       "XLM"="RestLAC_ADAGE",
                                       "ola"="OtherLAC_PHOX",
                                       "ven"="VEN"))

### Crear una variable con solo la variable REGION
reg<- unique(fs$REGION) 
# length(reg)

### Eliminar países innecesarios ####
## 1_Crear un objeto con los países de LAC
lac<- c("ARG", "BR", "CAC", "COL", "MEX", "SUR_AN", "SUR_AS",
        "OtherLAC_PHOX","RestLAC_ADAGE", "VEN")

## 2_Eliminar países innecesarios
fsa<- fs %>% dplyr::filter(REGION%in%lac)
# unique(fsa$REGION) ## Verificar los países

### Eliminar columnas innecesarias o seleccionar variables ####
# fsa$X1990<-NULL ## Elimina columnas

fsa<- fsa %>% select(MODEL,SCENARIO,REGION,VARIABLE,
                     X2010,X2015,X2020,X2025,X2030,
                     X2035,X2040,X2045,X2050) ### Seleccionar variables

### Cambiar nombre a variables de seg. alimentaria ####
# unique(fsa$VARIABLE) ## Listar nombres
fsa$VARIABLE<-plyr::revalue(fsa$VARIABLE, c("Food Security|Dietary Adequacy|Share US 2010 cal/cap/day"="Dietary Adequacy",
                                            "Food Security|Food Expenditure Share Income"="Food Expenditure Share Income",
                                            "Food Security|Food Import Share Merchandise Export"="Food Import Share Merch Export",
                                            "Food Security|Food Production Value|Agricultural Sector"="Food Production Value",
                                            "Food Security|Grain Import Ratio"="Grain Import Ratio",
                                            "Food Security|Protein Animal"="Protein Animal",
                                            "Food Security|Protein Total"="Protein Total",
                                            "Food Security|Share Staple Calories"="Share Staple Calories",
                                            "GDP|PPP"="GDP PPP"
))

### Filtrar por variables de seguridad alimentaria ####
## 1_Crear objeto con variables de seg. alim.
foods<- c("Dietary Adequacy","Food Expenditure Share Income",
          "Food Import Share Merch Export","Food Production Value",
          "Grain Import Ratio","Protein Animal","Protein Total",
          "Share Staple Calories","GDP PPP")

## 2_Eliminar variables de seguridad alimentaria
food<- fsa %>% dplyr::filter(VARIABLE%in%foods)

### Cambiar nombre de variables
names(food)<-c("Model","Scenario","Region","Variable","2010","2015",
               "2020","2025","2030","2035","2040","2045","2050")

### Subsets por variable ####
# Variables con unidad mil_2005USD
food_prod_value<- food[grep("Food Production Value", food$Variable),]
#Variables con unidad pct
diet_adeq<- food[grep("Dietary Adequacy", food$Variable),]
food_exp_share<- food[grep("Food Expenditure Share Income", food$Variable),]
share_staples<- food[grep("Share Staple Calories", food$Variable),]
grain_imp_ratio<- food[grep("Grain Import Ratio", food$Variable),]
food_imp_merch_exp<- food[grep("Food Import Share Merch Export", food$Variable),]
# Variables con unidad g_pcap_pday
protein_total<- food[grep("Protein Total", food$Variable),]
protein_animal<- food[grep("Protein Animal", food$Variable),]
# Variables con unidad thous_2005USD
gdp_cap_ppp<- food[grep("GDP PPP", food$Variable),]

### Homogeneizar valores ####
## diet_adeq
# Separar modelo GCAM
diet_GCAM<- diet_adeq %>%filter(Model=="GCAM_LAMP")
diet_others<- diet_adeq %>%filter(!Model=="GCAM_LAMP")
# Multiplicar por 100 para %
diet_GCAM$`2010`<- (diet_GCAM$`2010`)*100
diet_GCAM$`2015`<- (diet_GCAM$`2015`)*100
diet_GCAM$`2020`<- (diet_GCAM$`2020`)*100
diet_GCAM$`2025`<- (diet_GCAM$`2025`)*100
diet_GCAM$`2030`<- (diet_GCAM$`2030`)*100
diet_GCAM$`2035`<- (diet_GCAM$`2035`)*100
diet_GCAM$`2040`<- (diet_GCAM$`2040`)*100
diet_GCAM$`2045`<- (diet_GCAM$`2045`)*100
diet_GCAM$`2050`<- (diet_GCAM$`2050`)*100
# Combinar bases
diet_adeq<- rbind(diet_GCAM, diet_others)

## diet_adeq
# Separar modelo MEG4C
diet_GCAM<- diet_adeq %>%filter(Model=="MEG4C")
diet_others<- diet_adeq %>%filter(!Model=="MEG4C")
# Multiplicar por 100 para %
diet_GCAM$`2010`<- (diet_GCAM$`2010`)*100
diet_GCAM$`2015`<- (diet_GCAM$`2015`)*100
diet_GCAM$`2020`<- (diet_GCAM$`2020`)*100
diet_GCAM$`2025`<- (diet_GCAM$`2025`)*100
diet_GCAM$`2030`<- (diet_GCAM$`2030`)*100
diet_GCAM$`2035`<- (diet_GCAM$`2035`)*100
diet_GCAM$`2040`<- (diet_GCAM$`2040`)*100
diet_GCAM$`2045`<- (diet_GCAM$`2045`)*100
diet_GCAM$`2050`<- (diet_GCAM$`2050`)*100
# Combinar bases
diet_adeq<- rbind(diet_GCAM, diet_others)

## share_staples
# Separar modelo GCAM
share_staples_GCAM<- share_staples %>%filter(Model=="GCAM_LAMP")
share_staples_others<- share_staples %>%filter(!Model=="GCAM_LAMP")
# Multiplicar por 100 para %
share_staples_GCAM$`2010`<- (share_staples_GCAM$`2010`)*100
share_staples_GCAM$`2015`<- (share_staples_GCAM$`2015`)*100
share_staples_GCAM$`2020`<- (share_staples_GCAM$`2020`)*100
share_staples_GCAM$`2025`<- (share_staples_GCAM$`2025`)*100
share_staples_GCAM$`2030`<- (share_staples_GCAM$`2030`)*100
share_staples_GCAM$`2035`<- (share_staples_GCAM$`2035`)*100
share_staples_GCAM$`2040`<- (share_staples_GCAM$`2040`)*100
share_staples_GCAM$`2045`<- (share_staples_GCAM$`2045`)*100
share_staples_GCAM$`2050`<- (share_staples_GCAM$`2050`)*100
# Combinar bases
share_staples<- rbind(share_staples_GCAM, share_staples_others)

## food_exp_share
# Separar modelo Phoenix
fexp_phx<- food_exp_share %>%filter(Model=="Phoenix_6LA")
fexp_others<- food_exp_share %>%filter(!Model=="Phoenix_6LA")
# Multiplicar por 100 para %
fexp_phx$`2010`<- (fexp_phx$`2010`)*100
fexp_phx$`2015`<- (fexp_phx$`2015`)*100
fexp_phx$`2020`<- (fexp_phx$`2020`)*100
fexp_phx$`2025`<- (fexp_phx$`2025`)*100
fexp_phx$`2030`<- (fexp_phx$`2030`)*100
fexp_phx$`2035`<- (fexp_phx$`2035`)*100
fexp_phx$`2040`<- (fexp_phx$`2040`)*100
fexp_phx$`2045`<- (fexp_phx$`2045`)*100
fexp_phx$`2050`<- (fexp_phx$`2050`)*100
# Combinar bases
food_exp_share<- rbind(fexp_phx, fexp_others)

### Crear objeto con combinación de escenarios para 4.5 ####
## Ver nombre de escenarios

## Agrupar escenarios
sce_cfe_tx<-c("ccsm_4p5_cfe_ffict",
              "gfdl_4p5_cfe_ffict",
              "hadgem_4p5_cfe_ffict")

sce_cfe_notx<-c("hadgem_4p5_cfe_nopol",
               "ccsm_4p5_cfe_nopol",
               "gfdl_4p5_cfe_nopol")

sce_nocfe_notx<- c("gfdl_4p5_nocfe_nopol",
                   "hadgem_4p5_nocfe_nopol",
                   "ccsm_4p5_nocfe_nopol")

sce_nocfe_tx<- c("gfdl_4p5_nocfe_ffict",
                 "hadgem_4p5_nocfe_ffict",
                 "ccsm_4p5_nocfe_ffict")

core_nocfe_tx<-c("core_4p5_noimpacts_ffict")

ref_nocfe_notx<-c("core_ref_noimpacts_nopol")

# ********** CLimate change impacts 4p5/ref ************
# Guardar las bases de datos
# write.csv(proof,"./Results/gdp.csv")

### food_prod_value ####
## CC impacts ref_nocfe_notx - sce_nocfe_notx
food_prod_value_imp<- food_prod_value %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
food_prod_value_imp2<- food_prod_value %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
fpv_imp<-rbind(food_prod_value_imp, food_prod_value_imp2)
#Filtrar por modelo
fpv_imp<- fpv_imp %>%filter(Model=="ADAGE")
# Calcular variación % 2010 a 2050
fpv_imp$var<- ((fpv_imp$`2050` - fpv_imp$`2010`)/ fpv_imp$`2050`)*100
# Gráfico de densidad
proof<- fpv_imp %>%  select(Model,Region,Scenario,var)

gf1<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="Food production value", x="pct")
plot(gf1)

### diet_adeq ####
## CC impacts ref_nocfe_nopol - sce_nocfe_tx
imp<- diet_adeq %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
imp2<- diet_adeq %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
tot_imp<-rbind(imp, imp2)
#Filtrar por modelo
tot_imp<- tot_imp %>%filter(Model=="ADAGE")
# Calcular variación % 2010 a 2050
tot_imp$var<- (tot_imp$`2050` - tot_imp$`2010`)
# Gráfico de densidad
proof<- tot_imp %>%  select(Model,Region,Scenario,var)

gf3<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="Dietary energy supply adequacy", x="pct")
plot(gf3)

### food_exp_share ####
## CC impacts ref_nocfe_nopol - sce_nocfe_tx
imp<- food_exp_share %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
imp2<- food_exp_share %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
tot_imp<-rbind(imp, imp2)
#Filtrar por modelo
tot_imp<- tot_imp %>%filter(Model=="ADAGE")
# Calcular variación % 2010 a 2050
tot_imp$var<- (tot_imp$`2050` - tot_imp$`2010`)
# Gráfico de densidad
proof<- tot_imp %>%  select(Model,Region,Scenario,var)

gf5<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="Food Expenditure Share of Income", x="pct")
plot(gf5)

### share_staples ####
## CC impacts ref_nocfe_nopol - sce_nocfe_tx
imp<- share_staples %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
imp2<- share_staples %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
tot_imp<-rbind(imp, imp2)
#Filtrar por modelo
tot_imp<- tot_imp %>%filter(Model=="ADAGE")
# Calcular variación % 2010 a 2050
tot_imp$var<- (tot_imp$`2050` - tot_imp$`2010`)
# Gráfico de densidad
proof<- tot_imp %>%  select(Model,Region,Scenario,var)

gf7<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="Share of dietary energy supply", x="pct")
plot(gf7)

### protein_total ####
## CC impacts ref_nocfe_nopol - sce_nocfe_tx
imp<- protein_total %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
imp2<- protein_total %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
tot_imp<-rbind(imp, imp2)
#Filtrar por modelo
tot_imp<- tot_imp %>%filter(Model=="ADAGE")
# Calcular variación % 2010 a 2050
tot_imp$var<- ((tot_imp$`2050` - tot_imp$`2010`) /tot_imp$`2050`)*100
# Gráfico de densidad
proof<- tot_imp %>%  select(Model,Region,Scenario,var)

gf9<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="Protein total", x="pct")
plot(gf9)

### protein_animal ####
## CC impacts ref_nocfe_nopol - sce_nocfe_tx
imp<- protein_animal %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
imp2<- protein_animal %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
tot_imp<-rbind(imp, imp2)
#Filtrar por modelo
tot_imp<- tot_imp %>%filter(Model=="ADAGE")
# Calcular variación % 2010 a 2050
tot_imp$var<- ((tot_imp$`2050` - tot_imp$`2010`) /tot_imp$`2050`)*100
# Gráfico de densidad
proof<- tot_imp %>%  select(Model,Region,Scenario,var)

gf11<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="Protein animal", x="pct")
plot(gf11)

### gdp_cap_ppp ####
## CC impacts ref_nocfe_nopol - sce_nocfe_tx
imp<- gdp_cap_ppp %>% dplyr::filter(Scenario%in%sce_nocfe_tx)
imp2<- gdp_cap_ppp %>% dplyr::filter(Scenario%in%ref_nocfe_notx)
tot_imp<-rbind(imp, imp2)
#Filtrar por modelo
tot_imp<- tot_imp %>%filter(Model=="GCAM_LAMP")
# Calcular variación % 2010 a 2050
tot_imp$var<- ((tot_imp$`2050` - tot_imp$`2010`) /tot_imp$`2050`)*100
# Gráfico de densidad
proof<- tot_imp %>%  select(Model,Region,Scenario,var)

gf13<- ggplot(proof, aes(var)) + geom_density(aes(color=Scenario),alpha = 0.5) +
  labs(title="GDP per capita PPP", x="pct")
plot(gf13)

### Combinar gráficos ####
png(filename = paste("./pic/","cc_imp-pol_ADAGE.png", sep=""), width = 12, height = 6,
    units = 'in', res = 400)

gg <- ggarrange(gf1,gf3,gf5,gf7,gf9,gf11, ncol = 3, nrow = 2,common.legend = T,
                legend ="bottom")
plot(gg)
dev.off()


## Resultados de los impactos del cambio Climatico por pais
## Datos tomados de las simulaciones del BID
## Analaisis de Vulnerabilidad para el Sector Agricola en America Latina

### 
library(dplyr)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(latticeExtra)
library(gridExtra)


path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/'
names_crop <- c('bean', 'Maize', 'Rice', 'Soybean', 'wheat')
names_crop_to_graph <- c('Drybean', 'Maize', 'Rice', 'Soybean', 'wheat')
variable <- 'HWAH' ## HWAH, NDCH, Stress_nitrogen_all, Stress_water_all, TMAXA, TMINA, YPEM
type_analysis <- 'Change_Porcentaje'

#### Mapas por pais

names_crop <- c('bean', 'Maize', 'Rice', 'Soybean', 'wheat') ##
names_crop_to_graph <- c('bean', 'Maize', 'Rice', 'Soybean', 'wheat')
system <- 'Secano' ## Secano, Riego
system_ <- 'Rainfed'



# by_country <- c('Argentina', 'Bolivia', 'Guatemala')
# save_in <- '//dapadfs/workspace_cluster_6/ALPACAS/Varios/Pedidos_Jeimar/'  ## Donde se quiere guardar la informacion

save_in <- '//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/'
# grdmaps<-"//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/"

by_country <- c('Honduras', 'El Salvador')

rng <- c(-100, 100)
my.at <- pretty(rng, 15)
shape <- readOGR(paste0(path, '03-Map_LatinAmerica/Latino_America1.shp'), 'Latino_America1')
plot(shape)

# 
# for(j in 1:length(by_country)){
#       
#       for(i in 1:length(names_crop)){
#         
#         crop_s <- raster(paste0(path, '/12-Resultados/raster_resultados/', names_crop[i], '_', variable, '_', type_analysis, '_', system, '.asc'))
#         ## plot(crop)
#         filter_country <- shape[shape$COUNTRY == by_country[j], ]
#         crop_s <- crop(crop_s, filter_country)
#         crop_s <- mask(crop_s, filter_country)
        my.palette <- colorRampPalette(c('red','orange','white','green','forestgreen'), space="rgb")(15)
#       
#           pdf(file = paste0(save_in, names_crop[i], "_", by_country[j], "_mapa_DSSAT_", system, ".pdf"))
#         grafico <- spplot(crop_s, sp.layout = filter_country, col.regions = my.palette, cuts = 10, 
#                           main = paste('Percentage Changes of', names_crop_to_graph[i], "for Irrigated"), scales = list(draw = TRUE), 
#                           colorkey = list(height=0.3, at = c(-100,-80,-60,-40,-20,0,20,40,60,80,100)), sub = paste(by_country[j]), col = 'transparent',
#                           xlim = bbox(filter_country)[1, ],
#                           ylim= bbox(filter_country)[2, ]) +
#           layer(sp.polygons(filter_country)) 
#         
#         
#         print(grafico)
#         dev.off()
#         
#       }
# 
#   
#   
# }


j=1
i=1

# for(j in 1:length(by_country)){
#       print(j)
#       tryCatch({
#             
            for(i in 1:length(names_crop)){
                  print(i)
                  tryCatch({
                        
                        crop_s <- raster(paste0(path, '/12-Resultados/raster_resultados/', names_crop[i], '_', variable, '_', type_analysis, '_', system, '.asc'))
                        ## plot(crop)
#                         filter_country <- shape[shape$COUNTRY == by_country[j], ]
                        filter_country<- shape
#                         crop_s <- crop(crop_s, filter_country)
#                         crop_s <- mask(crop_s, filter_country)
#                         my.palette <- colorRampPalette(c('red','orange','white','green','forestgreen'), space = "rgb")(15)
                        
                        png(file = paste0(save_in, names_crop[i], "_mapa_DSSAT_", system_, ".png"))
                        grafico <- spplot(crop_s, sp.layout = filter_country, col.regions = my.palette, cuts = 10, 
                                          main = paste('Percentage Changes of', names_crop_to_graph[i], "for ", system_), scales = list(draw = TRUE), 
                                          colorkey = list(height=0.3, at = c(-100,-80,-60,-40,-20,0,20,40,60,80,100)), col = 'transparent',
                                          xlim = bbox(filter_country)[1, ],
                                          ylim= bbox(filter_country)[2, ]) +
                              layer(sp.polygons(filter_country)) 
                        
                        
                        print(grafico)
                        dev.off()
                        
                  }, error = function(ei) {} )
                  
                  
            }
            
            
#       }, error = function(e) {} )
#       
#       
# }
# 








## Calcular los resultados por Region para EcoCrop
### Esto es importante

library(dplyr)
path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/'

load(paste0(path, 'change_suitable_areas_latam_country.RData'))
ls()
out_reg
regions <- c('JAM', 'GTM', 'ARG', 'BOL', 'DOM', 'NIC', 'ECU', 'HND', 'SLV')

climate_models <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "gfld_esm2g", "inm_cm4",
                    "ipsl_cm5a_lr", "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m" )
region <- 'Honduras'

## seleccionado una region 
i = 4

head(out_reg)
tdata <- filter(out_reg, region == regions[i]) %>%
      mutate(change_area = (-hc_area+fc_area)*100/hc_area) %>%
      filter(change_area != 'NaN') %>%
      #select(change_area) %>%
      group_by(crop, gcm) %>%
      droplevels() %>%
      summarise(change_area = mean(change_area, na.rm = T)) %>%
      # filter(crop != 'coffea_arabica') %>%
      droplevels()

tdata <- filter(tdata, !crop %in% 'wheat')%>%  ## Filtrar solo los cultivos necesarios
      droplevels() ## solo si es necesario

# tdata
# boxplot(tdata$change_area ~ tdata$crop, outcol="red",medcol="red",boxcol="blue",col="white",
#         border="black", horizontal=F)
# boxplot(tdata$change_area ~ tdata$crop, outcol="red",medcol="red",boxcol="blue",col="white",
#         border="black", horizontal=F, ylim = c(-100, 100))
# grid(lwd=1)

png(file = paste0(save_in, regions[i], '_EcoCrop.png'))
par(las=1, mar=(c(4,8,5,5)))
boxplot(tdata$change_area ~ tdata$crop, horizontal=T,axes=F,
        xlab="", main = paste0('Suitability Change ',   regions[i]), 
        outcol="red",medcol="red",boxcol="blue",col="white",border="black")

axis(side=1,at=seq(-100,200,by=25))
axis(side=2,at=1:length(levels(tdata$crop)),labels=paste(levels(tdata$crop)),las=1)
box()
grid(lwd=1)
abline(v=0, lwd=1)
dev.off()



#### Mapas EcoCrop 16 Mayo 2016 ####
## Estos Graficos son utilizando ya los mapas categorizados creados por Patricia
recategorizar <- function(data){ 
      
      # color <- 0
      
      if(data == 5){
            
            data <- 'Becomes Suitable'
            # color <- 'Blue'
            
      }
      
      if(data == 4){
            
            data <- 'More Suitable'
            # color <- 'Green'
            
      }
      
      if(data == 3){
            
            data <- 'Remains Suitable'
            # color <- 'burlywood1'
            
      }
      
      if(data == 2){
            
            data <- 'Less but Still Suitable'
            # color <- 'Orange'
            
      }
      
      if(data == 1){
            
            data <- 'Becomes Unsuitable'
            # color <- 'Red'
            
      }
      
      if(data == 0){
            
            data <- 'NA'
      }
      
      # return(data.frame(categoria = data, color = color))
      return(data)
}



library(dplyr)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(latticeExtra)
library(gridExtra)


path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/'

names_crop <- c('yam', 'sugarcane', 'potato', 'coffea_robusta', 'cassava', 'banana') 
names_crop_to_graph <- c('Yam', 'Sugarcane', 'Potato', 'Coffee Robusta', 'Cassava', 'Banana') 


by_country <- c('Honduras', 'Argentina', 'Bolivia', 'Guatemala', 
                'Dominican Republic', 'Ecuador', 'Nicaragua','Honduras', 'El Salvador')
save_in <- '//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ECOcropSteve/'

shape <- readOGR(paste0(path, '03-Map_LatinAmerica/Latino_America1.shp'), 'Latino_America1')

path_data <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/Raster visualizador Ecocrop/Future/'


# crop_sC <- raster(paste0(path_data, 'sugarcane_change_forest_exc_suit_categories.tif'))

i=6
j=3
for(i in 1:length(names_crop)) {
      
      crop_sC <- raster(paste0(path_data, names_crop[i], '_change_forest_exc_suit_categories.tif'))
      
      # crop_sF <- raster()
      
      # filter_country <- shape[shape$COUNTRY == by_country, ]
      filter_country <- shape[shape$COUNTRY == by_country[j], ]
      # filter_country <- shape
#       allo<- alc[alc@data$COUNTRY==lugares[l],]
#       map_allo<- fortify(allo)
#       
      crop_sC <- crop( crop_sC, filter_country)
      # crop_sF <- crop( crop_sF, filter_country)
      
      crop_sC <- mask( crop_sC, filter_country)
      # crop_sF <- mask( crop_sF, filter_country)
      crop_sC <- rasterToPoints(crop_sC) 
      
      # unlist(lapply(crop_sC[, 3], recategorizar))
      # do.call('rbind', lapply(crop_sC[, 3], recategorizar))
      crop_sC_re <- data.frame(crop_sC, categoria = unlist(lapply(crop_sC[, 3], recategorizar)) )
      
      colors_by_categoria <- data.frame(categoria = c('Becomes Unsuitable', 
                                                      'Less but Still Suitable', 
                                                      'Remains Suitable', 
                                                      'More Suitable',
                                                      'Becomes Suitable'
      ), 
      color = c('#e41a1c',
                '#ff7f00',
                '#fb9a99',
                '#984ea3',
                '#377eb8'
      ))
      
      
      levels_col_cat <- colors_by_categoria %>%
            filter(categoria %in% unique(crop_sC_re$categoria))
      
      crop_sC_re <- crop_sC_re %>%
            filter(categoria != 'NA' ) 
      
      crop_sC_re$categoria <- factor(crop_sC_re$categoria, levels = as.character(levels_col_cat$categoria))
      
      # levels(crop_sC_re$categoria) <- as.character(levels_col_cat$categoria)
      # levels(crop_sC_re$categoria) <- c('Becomes Unsuitable', 'Less by Still Suitable', 'Becomes Suitable', 'Remains Suitable', 'More Suitable')
      
      x <- crop_sC_re
      filter_country1 <- fortify(filter_country)
      
      #Coropleta 
      # ggsave(y, filename = paste(save_in, 'EcoCrop_atrr_', names_crop_to_graph[i], '_', by_country, '.pdf', sep=""))
      
    
      
      png(filename = paste(save_in,'EcoCrop_atrr_', names_crop_to_graph[i],"_","CarlosVcoropleta.png", sep=""), width = 9, height = 6, units = 'in', res = 400)
      
      y <- ggplot() +
            geom_polygon( data = filter_country1, aes(x=long, y=lat, group = group), colour="red", fill="white", alpha = 0.7 )+
            geom_raster(data=x, aes(x, y, fill = categoria)) +
            geom_path(data = filter_country1, aes(x=long, y=lat, group=group), colour="black", size = 0.25)+
            coord_equal() +
            ggtitle(paste(names_crop_to_graph[i])) +
            # ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],'): \n',models[m,],sep=''))+
            # scale_fill_gradientn(colours=color_scale,limits=limits2,na.value = "grey50")+ # limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
            theme_bw()+
            labs(fill='')+
            theme(
                  legend.text = element_text(size=14),
                  legend.title = element_text(face="bold",size=14),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  plot.title = element_text(face="bold", size=18),
                  panel.border = element_blank(),
                  axis.ticks = element_blank())+
            scale_fill_manual(values = as.character(levels_col_cat$color))
      
      
      plot(y)
      dev.off() 
      
      
}




### Para realizar los panales los objetos son out_reg para EcoCrop y mdata para DSSAT

path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/'
names_crop <- c('bean', 'Maize', 'Rice', 'Soybean', 'wheat')
names_crop_to_graph <- c('Drybean', 'Maize', 'Rice', 'Soybean', 'wheat')


system <- c('Riego', 'Secano')
variable <- 'HWAH' ## HWAH, NDCH, Stress_nitrogen_all, Stress_water_all, TMAXA, TMINA, YPEM
type_analysis <- 'Change_Porcentaje'

## Zona de Estudio
shape <- readOGR(paste0(path, '03-Map_LatinAmerica/Latino_America.shp'), 'Latino_America')
country <- shape[, 'COUNTRY'] ## Seleccionar el Layer Country por pais, o si se requiere el analisis por FPU

## Si se requiere de una analisis particular por algunas de las regiones
## ya sea por Pais o FPU
by_ <- T

# regions <- 'Colombia|Peru|Argentina|Paraguay|Bolivia|Mexico|Chile|Guatemala|Honduras|El Salvador'
regions <- 'Honduras|El Salvador'


stored <- list()
stored_system <- list()

## Proof
## i = 1 
## j = 1


for(j in 1:length(system)){
      
      
      for(i in 1:length(names_crop)){
            
            crop_s <- raster(paste0(path, '/12-Resultados/raster_resultados/', names_crop[i], '_', variable, '_', type_analysis, '_', system[j], '.asc'))
            ## plot(crop)
            
            values <- rasterToPoints(crop_s)
            
            xy <- cbind(values[, 'x'], values[, 'y'])
            colnames(xy) <- c('x', 'y')
            occ <- SpatialPoints(xy)
            proj4string(occ) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") ## Mismo sistema de coordenadas que el shape
            
            # Extract Country categories
            
            if(by_ == T){
                  
                  overCountry <- sp::over(occ, country)
                  overCountry <- data.frame(df_country = overCountry, ch = values[, 3])
                  overCountry <- overCountry[grep(paste(regions), overCountry[, 1]), ] 
                  
            }else{
                  
                  overCountry <- sp::over(occ, country)
                  overCountry <- data.frame(df_country = overCountry, ch = values[, 3])
                  
            }
            
            
            
            mean_by_country <- overCountry %>%
                  group_by(COUNTRY) %>%
                  summarise_each(funs(mean(., na.rm=TRUE))) %>%
                  na.omit() %>%
                  arrange(desc(ch))
            
            #colnames(mean_by_country)[2] <- paste(names_crop_to_graph[i], system[j], sep = '_')
            colnames(mean_by_country)[2] <- paste(names_crop_to_graph[i])
            
            
            ## Cuando se requiera exportar los datos
            ##write.csv(mean_by_country, file = paste0(save_in, names_crop_to_graph[i], '_', variable, '_', type_analysis, '_', system[j], '.csv'), row.names = F)
            
            
            # change <- ggplot(mean_by_country, aes(x = COUNTRY, y =  paste(names_crop_to_graph[i], system[j], sep = '_'))) +
            #             geom_bar(stat = "identity")+ 
            #             scale_x_discrete(limits = mean_by_country$COUNTRY) +
            #             labs(x = paste(title_x), y = paste(title_y)) +
            #             ggtitle(paste(names_crop_to_graph[i], system[j])) +
            #             coord_flip()
            #   
            # ggsave(change, file= paste0(save_in, names_crop_to_graph[i], '_', variable, '_', type_analysis, '_', system[j], '.PDF'))
            # By default, uses stat="bin", which gives the count in each category
            
            stored[[i]] <- mean_by_country
      }
      
      merge_change <- function(x) Reduce(function(u, v) merge(u, v, by = 'COUNTRY', all = T), x)
      stored_system[[j]] <- data.frame(merge_change(stored), type = paste(system[j]))
      
      
}

all_data <- Reduce(function(u, v) rbind(u, v), stored_system)
mdata <- melt(all_data, id.vars= c('COUNTRY', 'type'))




## Nombre de los Ejes
title_x <- 'Crop'
title_y <- 'Percentage Change'
save_in <- 'C:/Users/jmesa/Desktop/graficas/'  ## Donde se quiere guardar la informacion

# country_filter <- c('Colombia', 'Peru', 'Argentina', 'Paraguay', 'Bolivia', 'Mexico', 'Chile')
# country_filter <- c('Jamaica', 'Guatemala', 'Argentina', 'Bolivia', Honduras|El Salvador)
country_filter <- c('Honduras', 'El Salvador')


############################# EcoCrop Jeison 

path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/'

load(paste0(path, 'change_suitable_areas_latam_country.RData'))
ls()

# out_reg #Datos obtenidos en otros procesos 

regions <- c('JAM', 'GTM', 'ARG', 'BOL', 'DOM', 'NIC', 'ECU', 'HND', 'SLV')

climate_models <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "gfld_esm2g", "inm_cm4",
                    "ipsl_cm5a_lr", "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m" )



region <- 'Ecuador'

i = 9

head(out_reg)
tdata <- filter(out_reg, region == regions[i]) %>%
      mutate(change_area = (-hc_area+fc_area)*100/hc_area) %>%
      filter(change_area != 'NaN') %>%
      #select(change_area) %>%
      group_by(crop, gcm) %>%
      droplevels() %>%
      summarise(change_area = mean(change_area, na.rm = T)) %>%
      # filter(crop != 'coffea_arabica') %>%
      group_by(crop) %>%
      summarise(average = mean(change_area, na.rm = T)) %>%
      droplevels()



tdata <- tdata %>%
      filter(crop != 'cassava') %>%
      mutate(model = rep('EcoCrop', length(variable))) 



# y <- filter(mdata, COUNTRY == paste(country_filter[1])) %>%
# mutate(model = rep('DSSAT', length(variable)))



x <- ggplot(tdata)+geom_bar(stat = 'identity', aes(x = crop, y = average))+
      ggtitle(region)+
      ylim(c(-100, 100))+
      ylab('Percentage Change in Suitability')+
      theme(text = element_text(size=19))
# axis.text.x = element_text(angle = 45, hjust = 1))



### Para realizar los panales los objetos son out_reg para EcoCrop y mdata para DSSAT
tdata


i = 1  ## Cambiar bien


y <-    ggplot(filter(mdata, COUNTRY == paste(country_filter[i]), variable != 'wheat', variable != 'Soybean'), aes(x = variable, y =  value)) +
      geom_bar(aes(fill = type), position = 'dodge', stat = "identity")+
      labs(x = title_x, y = title_y) +
      ggtitle(country_filter[i]) +
      scale_fill_manual(values=c("#0000FF", "#00FF00"),
                        name="Type",
                        labels=c("Irrigated", "rainfed"))+
      ylim(c(-100, 100))+
      theme(text = element_text(size=19))


p <- grid.arrange(x, y, ncol=2)
# ggsave("mtcars.pdf", width = 20, height = 20, units = "cm")
ggsave(paste0(save_in, 'Honduras_EcoCrop_DSSAT.pdf'), p, width = 50, height = 30, units = "cm")




############## codigo carlos 


path <- '//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/11-EcoCrop_runs/impacts/'

load(paste0(path, 'change_suitable_areas_latam_country.RData'))
ls()

# out_reg #Datos obtenidos en otros procesos 

regions <- c('JAM', 'GTM', 'ARG', 'BOL', 'DOM', 'NIC', 'ECU', 'HND', 'SLV')

climate_models <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "gfld_esm2g", "inm_cm4",
                    "ipsl_cm5a_lr", "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m" )

r=8
for(r in 1:length(regions)){
      
      tdata <- filter(out_reg, region == regions[r]) %>%
            mutate(change_area = (-hc_area+fc_area)*100/hc_area) %>%
            filter(change_area != 'NaN') %>%
            group_by(crop, gcm) %>%
            droplevels() %>%
            summarise(change_area = mean(change_area, na.rm = T)) %>%
            group_by(crop) %>%
            summarise(average = mean(change_area, na.rm = T)) %>%
            droplevels()
      
      tdata$crop<- as.character(tdata$crop)
      tdata$crop<- plyr::revalue(tdata$crop, c("banana"="Banana","cassava"="Cassava",
                                               "coffea_robusta"="Coffee\nRobusta", 
                                               "sugarcane"="Sugarcane", 
                                               "wheat"= "Wheat",
                                               "yam"="Yam",
                                               "coffea_arabica"="Coffee\nArabica",
                                               "potato"="Potato"))

      tdata$average[is.na(tdata$average)==TRUE]<- NULL
      #nocrops<-c("Coffee\nArabica","Wheat")
      tdata<- filter(tdata, average!=Inf) %>% filter(., average!=0)  %>% 
            filter(., average<=100)%>% 
            filter(.,average>= -100) #%>% filter(., !crop %in% nocrops)
      
      png(filename= paste(save_in,"Suitability_",regions[r],"_bar.png", sep = ""), 
          width = 9, height = 6, units = 'in', res = 300)
      labs2 = 'Type'
      
      
      x <- ggplot(tdata)+geom_bar(stat = 'identity', aes(x = crop, y = average, fill=crop),position=position_dodge(width=1))+
            ylim(c(-100, 100))+ scale_fill_brewer(palette = "Reds") +
            ylab('Percentage Change in Suitability')+ 
            labs(fill=labs2)+
            theme_light()+ 
            theme(text = element_text(size=11))+
            theme(legend.position = "none")+
            theme(axis.text.x=element_text(size=11))+
            theme(axis.text.y=element_text(size=11))+
            theme(strip.text.x = element_text(angle = 45,size = 9, face = "bold.italic"))+
            theme(strip.text.y = element_text(angle = 0,size = 9, face = "bold.italic"))+
            theme(aspect.ratio = 1)+
            theme(legend.text = element_text(size=11),
                  legend.title = element_text(face="bold",size=10),
                  legend.key = element_blank(),
                  strip.text.y = element_text(size=9, face="bold"),
                  plot.title = element_text(face="bold", size=10),
                  axis.ticks = element_blank(),
                  strip.background = element_rect(colour="white", fill="white")) 
      

      plot(x)
      dev.off()
      
      print(r)
      
      
}



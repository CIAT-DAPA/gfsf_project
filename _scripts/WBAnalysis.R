#### Codigo para procesar resultados de WB

### libraries 
## limpiar consola
g=gc;rm(list = ls())

### librerias 
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(rgdal))
suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools))
suppressMessages(library(sp))
suppressMessages(library(maps))
suppressMessages(library(raster))
suppressMessages(library(cumplyr))
suppressMessages(library(scales))



### directories
#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB")
gen<- c("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/")

### Load data
### cargar los datos area
### groups scenarios
g1<- c("_SSP1"); g2<- c("_SSP2"); g3<- c("_SSP3"); g4<- c("_SSP4");g5<- c("_SSP5")
rcp<- list(g1,g2,g3,g4,g5)


### groups of variables 
# datos por sistema de produccion  riego y secano
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )
# datos categorias totales
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area")
# datos categorias agregados 
dataagg<- c("FoodAvailXAgg", "QDXAgg -- Total Demand",
            "QEXAgg -- Export","QINTXAgg -- Intermediate Demand","QMXAgg -- Import",
            "QFXAgg -- Household Demand","QSupXAgg -- Commodity Supply",
            "QSXAgg -- Total Production")


# datos categorias animales
dataanimal<- c("AnmlNumXAgg -- Animal Numbers" , "AnmlYldXAgg -- Animal Yield")
# data tratamiento especial
dataespecial<- c("QNXAgg -- Net Trade")


##################  GENERALITIES   #########################################
# r=2 ###SSP
lapply(1:length(rcp), function(r){
     xx<- list.files( path = gen,pattern ="csv", full.names = T)
     kk<- unlist(xx)
     kk<- kk[grep(pattern=unlist(rcp[r]), x = kk)]
     # kk<- list(kk)
     pp<- lapply(kk,read.csv) ### load data
     
     ### convirtiendo en archivo data
     xx<- do.call(rbind, pp)

     
     xx$impactparameter<- as.character(xx$impactparameter)
     xx$scenario<- as.character(xx$scenario)
     xx$commodity<- as.character(xx$commodity)
     xx$region<- as.character(xx$region)
     xx$productiontype<- as.character(xx$productiontype)
     
     ## Definicion de cultivos
     crops<- unique(xx$commodity)
     cropsout<- c("Other", "Other Pulses", "Other meals","Other Cereals", "Other Oilseeds",
                  "Other Oils", "Other Roots", "Soybean Meal", "Temperate Fruit" ,"Rapeseed Oil" ,
                  "Groundnut Oil", "Groundnut meal",  "Rapeseed Meal", "Soybean Oil","Soybean Meal",  
                  "Palm Kernel Oil" ,"Palm Kernel Meal", "Sunflower Meal" )
     # transformar  factor to character
     phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )
     
     # grupos de cultivos. Este se debe ajustar por pais analizado
     # Data.frame no tiene en cuenta los sistemas de producción
     phi$productiontype<- NULL
     
     crops<- unique(phi$commodity)
     cultivations<-  crops[-1]
     cultivationsTrade<- crops[-1]
     
     animals<- c( "Beef", "Sheep" ,"Dairy Production")
     other<- c("-")
     row.names(phi)<- 1: nrow(phi)
     
     k<- list()
 
           i=1 ## variables
          for(i in seq_along(datatotal)){
           
           # selecciono el cultivo
           k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
           # filtro por numero de ref
           ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
           ref<- unique(ref)
           #filtro
           nocc<- k[[i]] %>% filter(scenario %in% ref)
           df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                         "commodity","year" ,"Val")]),]
           df$year<- as.numeric(df$year)
           #Eliminando todos los NoCC
           k[[i]]<- k[[i]] %>% filter(!scenario %in% ref)
           #apilando solo un NoCC
           k[[i]]<- rbind(k[[i]], df)
           df<- df %>% spread(year,Val)
           #reordeno los datos
           rownames(k[[i]])<- 1: nrow(k[[i]])
           #reshape a lo ancho  
           k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,commodity,region)%>%
                 spread ("year","Val")
           
           ##creando variable de scenario
           sce<- unique(k[[i]]$scenario)
           sce<- substring(sce, 9)
           s<- as.data.frame(table(sce))
           # u<- dim(s)[1]
           s<- s %>% filter(.,Freq==5 )
           
           
           
            o=1
           f<-list()
           for(o in 1:NROW(s)){
                 tm<- sce[o]
                 d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
                 d<- rbind(d, df)
                 ref<- d$scenario[grep(pattern = "WB_NoCC_SSP", x = d$scenario)]
                 ref<- unique(ref)
                 d<- data.frame(d,"Cat"=ifelse(d$scenario==ref,"NoCC","CC"))
                 d<-d[,-c(5:19)]
                 d<- d %>% dplyr::select(impactparameter,scenario,commodity,
                                  region,Cat,X2050) %>% filter(., scenario!=ref)%>% 
                       spread(scenario, X2050) %>% mutate(gdfl=,hgem=,ipsl=,miro=,nore=)
                 
                 #creo una variable= cambio porcentual 2020-2050
                 d$Percentage_Change<- ((d$X2050-d$X2020)/d$X2020)*100
                 d$sce<- sce[o]
                 #Elimino columnas inncesarias y de paso los organizo  
                 d<- d[,c("impactparameter","scenario", "commodity", "region", "Cat","sce","Percentage_Change")]
                 #Reshape para tener cambio porcentual por GCM
                 d<- d %>% group_by(impactparameter,commodity,region,
                                              Cat,sce) %>% summarise(mean=mean(Percentage_Change))%>% 
                       as.data.frame() %>%
                       dplyr::select(impactparameter,commodity, region,Cat,sce,mean) %>% 
                       group_by(impactparameter, commodity, region, sce) %>% 
                       spread(Cat,mean) %>% mutate(pp=CC-NoCC)
                 
                 f[[o]]<- d

           }
           
           # return 
           ### apilar resultados  
            bb<- do.call(rbind, f)
            bb<- as.data.frame(bb)
            ### exportar a csv  
            write.csv(bb, paste("./pic/Datos_",datatotal[[i]],rcp[[r]],".csv", sep = ""))

          }
     K<- list()       
      # j=4 #variables
          for(j in seq_along(dataagg)){
           
           # selecciono el cultivo
           k[[i]]<- phi[which(phi$impactparameter==dataagg[j]),]
           # filtro por numero de ref
           ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
           ref<- unique(ref)
           #filtro
           nocc<- k[[i]] %>% filter(scenario %in% ref)
           df <- nocc[!duplicated(nocc[c('impactparameter',"scenario", 
                                         "commodity","year" ,"Val")]),]
           df$year<- as.numeric(df$year)
           #Eliminando todos los NoCC
           k[[i]]<- k[[i]] %>% filter(!scenario %in% ref)
           #apilando solo un NoCC
           k[[i]]<- rbind(k[[i]], df)
           df<- df %>% spread(year,Val)
           #reordeno los datos
           rownames(k[[i]])<- 1: nrow(k[[i]])
           
           
           #reshape a lo ancho  
           k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,commodity,region)%>%
                 spread ("year","Val") #%>% dplyr::select(impactparameter,scenario,
                                        #                 commodity,region,'2050') #%>% as.data.frame()
           ### promedio de los gcms para escenario referencia
           # base<- k[[i]][grep(pattern = "RCP4.5_SSP2", k[[i]]$scenario),]
           # base<- base %>% group_by(impactparameter,commodity,
           #                          region)%>% summarise(.,prom= mean(`2050`))
           # 
           
            ##creando variable de scenario
           sce<- unique(k[[i]]$scenario)
           sce<- substring(sce, 9)
           s<- as.data.frame(table(sce))
           # u<- dim(s)[1]
           s<- s %>% filter(.,Freq==5 )
           
           o=1
           f<-list()
           for(o in 1:NROW(s)){
                 tm<- sce[o]
                 d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
                 d<- rbind(d, df)
                 ref<- d$scenario[grep(pattern = "WB_NoCC_SSP", x = d$scenario)]
                 ref<- unique(ref)
                 d<- data.frame(d,"Cat"=ifelse(d$scenario==ref,"NoCC","CC"))
                 d<- d[,-c(5:19)]
                 #creo una variable= cambio porcentual 2020-2050
                 d$Percentage_Change<- ((d$X2050-d$X2020)/d$X2020)*100
                 d$sce<- sce[o]
                 #Elimino columnas inncesarias y de paso los organizo  
                 d<- d[,c("impactparameter","scenario", "commodity", "region", "Cat","sce","Percentage_Change")]
                 #Reshape para tener cambio porcentual por GCM
                 d<- d %>% group_by(impactparameter,commodity,region,
                                    Cat,sce) %>% summarise(mean=mean(Percentage_Change))%>% 
                       as.data.frame() %>%
                       dplyr::select(impactparameter,commodity, region,Cat,sce,mean) %>% 
                       group_by(impactparameter, commodity, region, sce) %>% 
                       spread(Cat,mean) %>% mutate(pp=CC-NoCC)
                 
                 f[[o]]<- d
                 
           }
           
           # return 
           ### apilar resultados  
           bb<- do.call(rbind, f)
           bb<- as.data.frame(bb)
           ### exportar a csv  
           write.csv(bb, paste("./pic/Datos_",dataagg[[j]],rcp[[r]],".csv", sep = ""))
           
     }
   
})
# graph analysis -----------------------
ww<- list.files( path = "./pic/",pattern = g2, full.names = T)
ww<- lapply(ww, read.csv)
ww<- do.call(rbind, ww)
ww$X<- NULL
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  


n<- list()
for (i in seq_along(datatotal)){
    
      test<- ww
      test$region<- revalue(test$region,c("SSA-Ivory Coast"="Ivory Coast",
                                          "SSA-Mali"="Mali"))

      test<- test %>% gather(momento,val,5:ncol(test)) %>% 
            filter(.,impactparameter== datatotal[i]) 
      test$sce<- as.character(test$sce)
      test$commodity<- as.character(test$commodity)
      q<- unique(test$sce)
      
            write.csv(test,paste("./tables/",datatotal[[i]],"_",q[[c]],".csv",sep=""))
            
            labs3 = 'Percentage Points\n (pp)'

            e<- ggplot(test[which(test$momento=="pp"),], aes(x=region, y=commodity)) + 
                  geom_tile(aes(fill = val), colour = "white")+  
                  facet_grid(.~sce)+
                  scale_fill_gradientn(colours = hm.palette(100)) +  
                  coord_equal(40/100)+
                  labs(x="Countries",y="Cultivations")+ #,title=paste(datatotal[i],"\n",q[[c]], sep = "")
                  # scale_x_discrete(expand = c(0, 0))+
                  # scale_y_discrete(expand = c(0, 0))+
                  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                  # theme(strip.text.x = element_text(size = 11))+
                  # theme(strip.text=element_text(hjust=0,family = 'Courier'))+
                  theme(strip.text.y = element_text(angle = 0,size = 8)) +
                  theme(legend.position="bottom",legend.text=element_text(size=10))+ 
                  labs(fill=labs3) 
                  
            ggsave(file= paste("pic/",datatotal[i],"_",g2,".png",sep=""),e,width = 12,height = 8)

     
                           
                        
}  



################### TOPICS ################################################
# r=2 ###SSP
ca<- c("RCP4.5_SSP2", "RCP8.5_SSP2", "RCP6.0_SSP2","RCP2.6_SSP2")

xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP2", x = kk)]
# kk<- list(kk)
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$productiontype<- as.character(xx$productiontype)
      
# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)

k<- list()
r<- list()      
for(i in seq_along(datatotal)){
            
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==datatotal[i]),]
      # filtro por numero de ref
      ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
      ref<- unique(ref)
      #filtro
      #Eliminando todos los NoCC
      k[[i]]<- k[[i]] %>% dplyr::filter(!scenario %in% ref)
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,commodity,region)%>%
            spread ("year","Val") %>% as.data.frame()
      
      ##creando variable de scenario
      sce<- unique(k[[i]]$scenario)
      sce<- substring(sce, 9)
      s<- as.data.frame(table(sce))
      # u<- dim(s)[1]
      s<- s %>% filter(.,Freq==5 )
      
      # o=1
      f<-list()
      for(o in 1:NROW(s)){
            tm<- sce[o]
            d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
      
            #reshape a lo ancho  
            d<- d %>% dplyr::select(impactparameter,scenario,
                                                          commodity,region,'2050') %>% 
                  as.data.frame() %>%
                  group_by(impactparameter,commodity,
                           region)%>% summarise(.,prom= mean(`2050`)) %>% mutate(sce=tm)
      

                  f[[o]]<- d

      }

      ### apilar resultados  
      bb<- do.call(rbind, f)
      bb<- as.data.frame(bb)
      
      bb<- bb %>% spread(sce,prom)
      bb<- bb[c("impactparameter","commodity","region","RCP2.6_SSP2","RCP6.0_SSP2",
                "RCP8.5_SSP2", "RCP4.5_SSP2")]
      bb$"dp_2.6SSP2"<- ((bb$RCP2.6_SSP2-bb$RCP4.5_SSP2)/bb$RCP4.5_SSP2)*100
      bb$"dp_6.0SSP2"<- ((bb$RCP6.0_SSP2-bb$RCP4.5_SSP2)/bb$RCP4.5_SSP2)*100
      bb$"dp_8.5SSP2"<- ((bb$RCP8.5_SSP2-bb$RCP4.5_SSP2)/bb$RCP4.5_SSP2)*100

      bb<- bb[c("impactparameter","commodity","region", "dp_2.6SSP2" ,"dp_6.0SSP2", "dp_8.5SSP2" )]
      bb<- bb %>% tidyr::gather(var,i,4:ncol(bb))

      labs3 = 'Percentage differences by 2050 \nRef=RCP4.5SSP2'
      myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
      
      bb$region<- revalue(bb$region,c("SSA-Ivory Coast"="Ivory Coast",
                                          "SSA-Mali"="Mali"))
      options(digits=2)
      lmax<- max(bb$i)
      lmin<- min(bb$i)*-1
      
      
      if(lmax>lmin){limite<-lmax }else(limite<-lmin)
      
      e<- ggplot(bb, aes(x=region, y=commodity)) + 
            geom_tile(aes(fill = i), colour = "white")+  
            facet_grid(.~var)+
            scale_fill_gradientn(colours = myPalette(4), 
                                 limits=c((limite*-1),limite),
                                 breaks=c((limite*-1),0,limite)) +  
            coord_equal(40/100)+
            labs(x="Countries",y="Cultivations",title=datatotal[i])+
            theme(axis.text.x = element_text(angle = 0, hjust = 1))+
            theme(strip.text.y = element_text(angle = 0,size = 8)) +
            theme(legend.position="bottom",legend.text=element_text(size=10))+ 
            labs(fill=labs3) 
      
      ggsave(file= paste("test/",datatotal[i],"_Test2_1.png",sep=""),e,width = 8,height = 8)

      
      
      
}
# i=5## variables
for(i in seq_along(dataagg)){
      
      # selecciono el cultivo
      k[[i]]<- phi[which(phi$impactparameter==dataagg[i]),]
      # filtro por numero de ref
      ref<- k[[i]]$scenario[grep(pattern = "WB_NoCC_SSP", x = k[[i]]$scenario)]
      ref<- unique(ref)
      #filtro
      #Eliminando todos los NoCC
      k[[i]]<- k[[i]] %>% dplyr::filter(!scenario %in% ref)
      #reordeno los datos
      rownames(k[[i]])<- 1: nrow(k[[i]])
      #reshape a lo ancho  
      k[[i]]<- k[[i]] %>% group_by(impactparameter,scenario,commodity,region)%>%
            spread ("year","Val") %>% as.data.frame()
      
      ##creando variable de scenario
      sce<- unique(k[[i]]$scenario)
      sce<- substring(sce, 9)
      s<- as.data.frame(table(sce))
      # u<- dim(s)[1]
      s<- s %>% filter(.,Freq==5 )
      
      # o=1
      f<-list()
      for(o in 1:NROW(s)){
            tm<- sce[o]
            d<- k[[i]][grep(pattern = tm,x = k[[i]]$scenario),]
            
            #reshape a lo ancho  
            d<- d %>% dplyr::select(impactparameter,scenario,
                                    commodity,region,'2050') %>% 
                  as.data.frame() %>%
                  group_by(impactparameter,commodity,
                           region)%>% summarise(.,prom= mean(`2050`)) %>% mutate(sce=tm)
            
            
            f[[o]]<- d
            
      }
      
      ### apilar resultados  
      bb<- do.call(rbind, f)
      bb<- as.data.frame(bb)
      
      bb<- bb %>% spread(sce,prom)
      bb<- bb[c("impactparameter","commodity","region","RCP2.6_SSP2","RCP6.0_SSP2",
                "RCP8.5_SSP2", "RCP4.5_SSP2")]
      bb$"dp_2.6SSP2"<- ((bb$RCP2.6_SSP2-bb$RCP4.5_SSP2)/bb$RCP4.5_SSP2)*100
      bb$"dp_6.0SSP2"<- ((bb$RCP6.0_SSP2-bb$RCP4.5_SSP2)/bb$RCP4.5_SSP2)*100
      bb$"dp_8.5SSP2"<- ((bb$RCP8.5_SSP2-bb$RCP4.5_SSP2)/bb$RCP4.5_SSP2)*100
      
      bb<- bb[c("impactparameter","commodity","region", "dp_2.6SSP2" ,"dp_6.0SSP2", "dp_8.5SSP2" )]
      bb<- bb %>% tidyr::gather(var,i,4:ncol(bb))
      
      labs3 = 'Percentage differences by 2050 \nRef=RCP4.5SSP2'
      myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
      
      bb$region<- revalue(bb$region,c("SSA-Ivory Coast"="Ivory Coast",
                                      "SSA-Mali"="Mali"))
      bb<- na.omit(bb)
      options(digits=2)
      lmax<- max(bb$i)
      lmin<- min(bb$i)*-1
      
      
      if(lmax>lmin){limite<-lmax }else(limite<-lmin)
      
      e<- ggplot(bb, aes(x=region, y=commodity)) + 
            geom_tile(aes(fill = i), colour = "white")+  
            facet_grid(.~var)+
            scale_fill_gradientn(colours = myPalette(4), 
                                 limits=c((limite*-1),limite),
                                 breaks=c((limite*-1),0,limite)) +  
            coord_equal(40/100)+
            labs(x="Countries",y="Cultivations",title=dataagg[i])+
            theme(axis.text.x = element_text(angle = 0, hjust = 1))+
            theme(strip.text.y = element_text(angle = 0,size = 8)) +
            theme(legend.position="bottom",legend.text=element_text(size=10))+ 
            labs(fill=labs3) 
      
      ggsave(file= paste("test/",dataagg[i],"_Test2_1.png",sep=""),e,width = 8,height = 8)
      
      options(digits=2)
      # resumen<- data.frame(summary(bb$i))
      resumen<- do.call(cbind, lapply(bb, summary))
      resumen<- data.frame(resumen)
      resumen$sum<- row.names(resumen)
      row.names(resumen)<- 1:NROW(resumen)
      resumen$var<- dataagg[i]
      resumen<- resumen %>% dplyr::select(var,i,sum)
      # resumen<- data.frame(unclass(summary(bb$i)), check.names = FALSE, stringsAsFactors = FALSE)
      
      r[[i]]<- resumen
      
      
}

brief<- do.call(rbind,r)
write.csv(brief,"./tables/summary_datatotal.csv")




###################### indicator additionals ##############################################
im_share<- c("QMSHXAgg -- Import Share of Demand")
q_share<- c("QNSH1XAgg -- Net Trade Share of Production")
net_share<- c("QNSH2XAgg -- Net Trade Share of Demand")
price_consumer<- c("PCXAgg -- Consumer Prices") 

xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP2", x = kk)]
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$productiontype<- as.character(xx$productiontype)

# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)
   

# selecciono el cultivo
k<- phi %>% filter(., impactparameter %in% dataespecial)
# filtro por numero de ref
ref<- k$scenario[grep(pattern = "WB_NoCC_SSP", x = k$scenario)]
ref<- unique(ref)
#filtro
#Eliminando todos los NoCC
k<- k %>% dplyr::filter(!scenario %in% ref)
#reordeno los datos
rownames(k)<- 1: nrow(k)
#reshape a lo ancho  
k<- k %>% group_by(impactparameter,scenario,commodity,region)%>%
      spread ("year","Val") %>% as.data.frame()
      
##creando variable de scenario
sce<- unique(k$scenario)
sce<- substring(sce, 9)
s<- as.data.frame(table(sce))
# u<- dim(s)[1]
s<- s %>% filter(.,Freq==5 )
      
o=1
f<-list()
for(o in 1:NROW(s)){
      tm<- sce[o]
      d<- k[grep(pattern = tm,x = k$scenario),]
      
      #reshape a lo ancho  
      d<- d %>% dplyr::select(impactparameter,scenario,
                              commodity,region,'2050') %>% 
            as.data.frame() %>%
            group_by(impactparameter,commodity,
                     region)%>% summarise(.,prom= mean(`2050`)) %>% mutate(sce=tm)
      
      
      f[[o]]<- d
      
}

### apilar resultados  
bb<- do.call(rbind, f)
bb<- as.data.frame(bb)
      
bb<- bb %>% spread(sce,prom)
bb<- bb[c("impactparameter","commodity","region","RCP2.6_SSP2","RCP6.0_SSP2",
          "RCP8.5_SSP2", "RCP4.5_SSP2")]


## logica de los valores 
nn<-  which(bb$RCP2.6_SSP2<0 & bb$RCP6.0_SSP2<0 & bb$RCP8.5_SSP2<0 & bb$RCP4.5_SSP2<0) # net trade negativo  importador neto
pp<-  which(bb$RCP2.6_SSP2>0 & bb$RCP6.0_SSP2>0 & bb$RCP8.5_SSP2>0 & bb$RCP4.5_SSP2>0) # net trade positivo  exportador neto


# desempeño 
export<-   c(pp)
import <-  c(nn)
bb$impacto<- NA # para poner el impacto  cambio relativo

bb$"i_2.6SSP2"<- ((bb$RCP2.6_SSP2/bb$RCP4.5_SSP2))
bb$"i_6.0SSP2"<- ((bb$RCP6.0_SSP2/bb$RCP4.5_SSP2))
bb$"i_8.5SSP2"<- ((bb$RCP8.5_SSP2/bb$RCP4.5_SSP2))

bb$impacto[import] <- "Negative Net Trade"
bb$impacto[export] <- "Positive Net Trade"
bb$impacto[is.na(bb$impacto)]<- "Transition" 

bb<- bb[c("impactparameter","commodity","region", "i_2.6SSP2" ,"i_6.0SSP2", "i_8.5SSP2","impacto" )]
bb<- bb %>% tidyr::gather(var,i,4:6)

labs3 = 'Index Net trade by 2050 \nRef=RCP4.5SSP2'
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

bb$region<- revalue(bb$region,c("SSA-Ivory Coast"="Ivory Coast",
                                "SSA-Mali"="Mali"))
options(digits=2)

labs3 = 'Trends'
trade<- ggplot(data=bb[which(bb$commodity!="Sugar"),],aes(x=commodity,y=i))  +      
      geom_bar(aes(fill=impacto),stat="identity", position = "dodge" )+
      facet_grid(region~var)+ 
      theme(axis.text.x = element_text(angle =0, hjust = 1))+
      coord_flip()+
      labs(x="Index by 2050\n(Base=RCP4.5_SSP2)",y="Cultivations",title="Net Trade")+
      labs(fill=labs3)+
      theme(strip.text.x = element_text(angle = 0,size = 12, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 12, face = "bold.italic"))
      
ggsave(file= paste("test/Trade_SSP2Sce3_Sce1.png",sep=""),trade,width = 9,height = 7)
write.csv(bb, "./tables/NetTrade.csv")


################################# food autonomy ##############################

af<- c("QDXAgg -- Total Demand","QSXAgg -- Total Production" )
xx<- list.files( path = gen,pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern="_SSP2", x = kk)]
pp<- lapply(kk,read.csv) ### load data

### convirtiendo en archivo data
xx<- do.call(rbind, pp)
xx$impactparameter<- as.character(xx$impactparameter)
xx$scenario<- as.character(xx$scenario)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$productiontype<- as.character(xx$productiontype)

# transformar  factor to character
phi<- xx %>% dplyr::filter(.,!commodity %in% cropsout )

# grupos de cultivos. Este se debe ajustar por pais analizado
# Data.frame no tiene en cuenta los sistemas de producción
phi$productiontype<- NULL

crops<- unique(phi$commodity)
cultivations<-  crops[-1]
cultivationsTrade<- crops[-1]
row.names(phi)<- 1: nrow(phi)


# selecciono el cultivo
k<- phi %>% filter(., impactparameter %in% af)
# filtro por numero de ref
ref<- k$scenario[grep(pattern = "WB_NoCC_SSP", x = k$scenario)]
ref<- unique(ref)
#filtro
#Eliminando todos los NoCC
k<- k %>% dplyr::filter(!scenario %in% ref)
#reordeno los datos
rownames(k)<- 1: nrow(k)

##creando variable de scenario
sce<- unique(k$scenario)
sce<- substring(sce, 9)
sce<- unique(sce)
s<- as.data.frame(table(sce))
# u<- dim(s)[1]
s<- s %>% filter(.,Freq==5 )

# o=2
f<-list()
for(o in 1:NROW(s)){
      tm<- sce[o]
      d<- k[grep(pattern = tm,x = k$scenario),]
      d<-  as.data.frame(d)
      #reshape a lo ancho  
      d<- d %>% group_by(scenario,commodity, region, year)%>%
            spread(impactparameter, Val)
      d<-  as.data.frame(d)
      colnames(d)[5]<- "Demand"
      colnames(d)[6]<- "Supply"
      
      grouped <- group_by(d,commodity, region, year)
      brief<- summarise(grouped, ts=sum(Supply), td=sum(Demand))
      brief<- brief %>% mutate(au=ts/td)
      brief$ts[is.na(brief$ts)]<- 0
      brief<- brief %>% drop_na(td)
      brief<- brief %>% gather(var, val, 4:ncol(brief))
      brief$esce<- sce[o]
      
      
      brief$region<- revalue(brief$region,c("SSA-Ivory Coast"="Ivory Coast",
                                      "SSA-Mali"="Mali"))
      f[[o]]<- brief
      
}
### apilar resultados  
bb<- do.call(rbind, f)
bb<- as.data.frame(bb)

labs4<- "Regions"
aufood<- ggplot(bb[which(bb$var=="au"),], aes(x = commodity, y = val, fill=region))+
      geom_boxplot()+  coord_flip()+
      geom_hline(aes(yintercept=1))+
      facet_grid(region~esce) +
      labs(fill=labs4)+  
      labs(y="Proportion\n Local production/Local demand",x="Cultivations",title="Food")+
      theme(strip.text.x = element_text(angle = 0,size = 12, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 12, face = "bold.italic"))

ggsave(file= paste("test/AuFood_SSP2Sce3_Sce1.png",sep=""),aufood,width = 9,height = 7)
bb<- bb %>% spread(var,val) %>% dplyr::select(commodity, region, year, esce, au) %>% spread(esce,au)

## logica de los valores 
nn<-  which(bb$RCP2.6_SSP2<1 & bb$RCP6.0_SSP2<1 & bb$RCP8.5_SSP2<1 & bb$RCP4.5_SSP2<1) # net trade negativo  importador neto
pp<-  which(bb$RCP2.6_SSP2>1 & bb$RCP6.0_SSP2>1 & bb$RCP8.5_SSP2>1 & bb$RCP4.5_SSP2>1) # net trade positivo  exportador neto


positive<-   c(pp)
negative <-  c(nn)
bb$impacto<- NA # para poner el impacto  cambio relativo


bb$impacto[positive] <- "Surplus"
bb$impacto[negative] <- "Deficit"
write.csv(bb, "./tables/AuFood_SSP2Sce3_Sce1.csv")


####### all crops
options(digits=2)
# o=1
f<-list()
for(o in 1:NROW(s)){
      tm<- sce[o]
      d<- k[grep(pattern = tm,x = k$scenario),]
      d<-  as.data.frame(d)
      #reshape a lo ancho  
      d<- d %>% group_by(scenario,commodity, region, year)%>%
            spread(impactparameter, Val)
      d<-  as.data.frame(d)
      colnames(d)[5]<- "Demand"
      colnames(d)[6]<- "Supply"
      d$Supply[is.na(d$Supply)]<-0
      d$Demand[is.na(d$Demand)]<-0
      
      grouped <- group_by(d, region, year)
      brief<- summarise(grouped, ts=sum(Supply, rm.na=T), td=sum(Demand, rm.na=T))
      
      brief<- brief %>% mutate(au=ts/td)
      # brief$ts[is.na(brief$ts)]<- 0
      # brief<- brief %>% drop_na(td)
      brief<- brief %>% gather(var, val, 3:ncol(brief))
      brief$esce<- sce[o]
      
      
      brief$region<- revalue(brief$region,c("SSA-Ivory Coast"="Ivory Coast",
                                            "SSA-Mali"="Mali"))
      f[[o]]<- brief
      
}
### apilar resultados  
bb<- do.call(rbind, f)
bb<- as.data.frame(bb)

labs4<- "Scenarios"
aufood<- ggplot(bb[which(bb$var=="au"),], aes(x = region, y = val, fill=esce))+
      geom_boxplot()+  #coord_flip()+
      geom_hline(aes(yintercept=1))+
      labs(fill=labs4)+  
      labs(y="Proportion\n Local production/Local demand",x="Regions",title="Food")+
      theme(strip.text.x = element_text(angle = 0,size = 12, face = "bold.italic"))+
      theme(strip.text.y = element_text(angle = 0,size = 12, face = "bold.italic"))

ggsave(file= paste("test/AuFood_Total_SSP2Sce3_Sce1.png",sep=""),aufood,width = 9,height = 7)
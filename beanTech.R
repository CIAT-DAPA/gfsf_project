#Programa para analizar los datos de evaluación de frijol con y sin tecnologia-----
#bajo distintos escenarios climaticos
#Por: Jesús Rodríguez

#Cargar librerias-----
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(xtable)
library(dplyr)
library(tidyr)
library(lattice)
library(latticeExtra)

#Limitar numero de decimales-----
options(digits=3) 

#Definir directorios de trabajo y de graficos------
#Directorio de trabajo
setwd("C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/")
#Dirrectorio de graficos
grd<-"C:/Users/CEGONZALEZ/Documents/Scripts/GFSF/graph/"

#Cargar lista de nombres de los archivos----
nombres <- list.files(pattern ="*.csv" )
#Crear muna lista con los nombre de los marco de datos
nombres_md_list<-strsplit(nombres,".csv")
#nombres_md<-c(do.call("cbind",nombres_md_list))  
categoria<-gsub(pattern="Bean_",replacement="",x=nombres_md_list)

#Crear una lista vacia
pl<-NULL

#Cargar los datos dentro de la lista
for (i in 1: length(nombres_md_list)) {
    pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-read.csv(file=\'',nombres[i],'\',header=T)',sep='')))
    pl[[i]]<-eval(parse(text=paste(nombres_md_list[[i]],'<-data.frame(',nombres_md_list[i],',','"categoria"','=',
                                   "categoria[i]",')',sep='')))
    print(i)
}

#Juntar los elemntos de la lista en un marco de datos
md<-do.call(rbind, pl)

#Hacer un subconjunto que sólo contenga las variables de mi interés-----
mdsub<-subset(md,md$impactparameter=="QSXAgg -- Total Production" | 
                  md$impactparameter=="TAreaXAgg -- Total Area" |
                  md$impactparameter== "TYldXAgg -- Total Yield" | 
                  md$impactparameter== "QDXAgg -- Total Demand" |
                  md$impactparameter== "QEXAgg -- Export" | 
                  md$impactparameter== "QMXAgg -- Import" |
                  md$impactparameter== "PWXAgg -- World Prices")

mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QSXAgg -- Total Production"="Production",  
                                                        "TAreaXAgg -- Total Area"="Area",
                                                        "TYldXAgg -- Total Yield"="Yield",
                                                        "QDXAgg -- Total Demand"="Demand",
                                                        "QEXAgg -- Export"="Export",
                                                        "QMXAgg -- Import"="Import",
                                                        "PWXAgg -- World Prices"="W Prices"))


#Hacer un subconjunto que sólo contenga frijol
mdsubcrop<-droplevels(subset(mdsub,mdsub$commodity=="PUL-Beans" | mdsub$commodity=="-"))

mdsubcrop$commodity<-revalue(mdsubcrop$commodity,c("PUL-Beans"="Beans"))

#Hacer un subconjunto de md que sólo contenga los paises de LAC y SSA (Easter, Middle, Western, Southern)
lac_africa_reg<-levels(mdsubcrop$region)[c(68:96,121,127:161)]

mdsubcrop_lac_afri<-mdsubcrop[which(mdsubcrop$region %in% lac_africa_reg),]

mdsubcrop_lac_afri<-droplevels(mdsubcrop_lac_afri)

mdwide <- reshape(mdsubcrop_lac_afri, v.names = "Val", idvar = 
                      c("scenario","commodity","region","productiontype",
                        "impactparameter","categoria"),timevar = "year", direction = "wide")

#Calcular cambio porcentual entre 2020 y 2050----
mdwide<- data.frame(mdwide,"Cambio Porcentual"=( (mdwide$Val.2050-mdwide$Val.2020)/mdwide$Val.2020)*100)

mdwide<-data.frame(mdwide[,1:6],"Cambio Porcentual"=mdwide[,13])

mdwide_y<-mdwide

mdwide$categoria <- NULL

mdwide_t<-mdwide %>%
    spread("scenario","Cambio.Porcentual")

cambiop<-mdwide_t

#Comparar todos los escenarios con cambio climatico con el escenario sin cambio climatico y sin adopcion de-----
# tecnologia

for (i in 5:14) {
    cambiop[,i]<-((mdwide_t[,i]-mdwide_t[,"SSP2-NoCC-NoCC-NOTECH"])/
                      mdwide_t[,"SSP2-NoCC-NoCC-NOTECH"])*100
}

#Sacar No cambio climático
cambiop$`SSP2-NoCC-NoCC-NOTECH`<-NULL
cambiop$`SSP2-NoCC-NoCC-TECH`<-NULL

#Reformatear datos para realizar graficos

mdwidet_melt<- cambiop %>%
    gather(scenario,cp,5:14)


#forma analoga de hacer el proceso anterior usando la funcion melt
# cambiop<-data.frame(cambiop,"id"=row.names(cambiop))
# 
# cambiop_melt<-melt(cambiop,id=c("impactparameter","commodity","region","id","productiontype"),
#                                variable_name="scenario")


#Crear categorias CCnotech y CCtech
notech<-c("SSP2-GFLD-8P5-379-NOTECH","SSP2-HGEM-8P5-379-NOTECH","SSP2-IPSL-8P5-379-NOTECH",
          "SSP2-MIRO-8P5-379-NOTECH","SSP2-NORE-8P5-379-NOTECH")

tech<-c("SSP2-GFLD-8P5-379-TECH","SSP2-HGEM-8P5-379-TECH","SSP2-IPSL-8P5-379-TECH",
        "SSP2-MIRO-8P5-379-TECH","SSP2-NORE-8P5-379-TECH")

mdwidet_melt$Cat<-ifelse(mdwidet_melt$scenario %in% tech, "Tec","Notec")

#calcular la mediana de los impactos del cc por impact parameter commodity region y cat
r_melt<-aggregate(mdwidet_melt[,"cp"],by=list(mdwidet_melt$impactparameter,
                                              mdwidet_melt$region,
                                              mdwidet_melt$Cat),FUN=median)

names(r_melt)<-c("Variable","Region","Categoria","Cambio_por")

r_melt$Impacto<-cut(r_melt$Cambio_por, c(-Inf,-100,-50,-10,10,50,100,Inf),right=T,
                    labels = c("highest negative impact","highly negative","negative",
                               "neutral","positive","highly positive","highest positive impact"))

#Graficar el perfil de impacto por paises----
gg<-ggplot(r_melt, aes(x=Categoria, y=Region, fill=Impacto)) 
gg<-gg +  geom_tile(color="white", size=0.1) 
gg<-gg+labs(x=NULL, y=NULL, title=NULL)
gg<-gg+theme(axis.ticks=element_blank()) 
gg<-gg+theme(axis.text=element_text(size=7))
gg<-gg+theme(legend.title=element_blank())
gg<-gg+theme(legend.text=element_text(size=9))
gg<-gg+facet_wrap(~Variable,ncol=8)
gg<-gg+scale_fill_brewer(palette = "BrBG") 
gg<-gg + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(),legend.position="bottom")


#Guardar el archivo
tiff(filename=paste(grd,"impacto cc en variables.tiff",sep=""), 
     width = 10, height = 10, units = 'in', res = 100)

gg

dev.off()

#Sub conjunto con los rendimientos-----
mdwide_ys<-droplevels(subset(mdwide_y,mdwide_y$impactparameter=="Yield"))

mdwide_ym<-aggregate(mdwide_ys[,"Cambio.Porcentual"],by=list(mdwide_ys$impactparameter,
                                                             mdwide_ys$commodity,
                                                             mdwide_ys$region,
                                                             mdwide_ys$categoria),FUN=median)

#Estadisticas de resumen
resumen<-tapply(mdwide_ym$x, list(mdwide_ym$Group.4,mdwide_ym$Group.1), summary)
nom<-row.names(resumen)
resumen<-do.call(rbind,resumen)
row.names(resumen)<-nom
resumen_t<-tableGrob(resumen,theme=ttheme_minimal())


#Grafico con distribucion de las variables
tiff(filename=paste(grd,"Bean_var_median.tiff",sep=""), 
     width = 9, height = 6, units = 'in', res = 100)
print(ggplot(mdwide_ym, aes(x=x)) + geom_density(aes(group=Group.4, colour=Group.4)) +
          theme(legend.position="bottom",legend.title=element_blank()) +labs(x="Cambio Porcentual",
                                                                             y="Densidad") +
          facet_wrap(~Group.1))


dev.off()

#Analisis de cluster-----
ej<-droplevels(subset(r_melt,r_melt$Categoria=="Tec"))
ejtec<-ej
ejtec$Categoria<-NULL
ejtec$Impacto<-NULL
#spread datos
#Melt datos
    spread("Variable","Cambio_por")


#nomabrar filas con la regi'on
row.names(ejtec_s)<-ejtec_s$Region
ejtec_s$Region<-NULL


x  <- t(as.matrix(scale(ejtec_s)))
dd.row <- as.dendrogram(hclust(dist(x)))
row.ord <- order.dendrogram(dd.row)

dd.col <- as.dendrogram(hclust(dist(t(x))))
col.ord <- order.dendrogram(dd.col)


tiff(filename=paste(grd,"cluster_heatmaps.tiff",sep=""), 
     width = 10, height = 12, units = 'in', res = 100)
levelplot(x[row.ord, col.ord],
          aspect = "fill",
          scales = list(x = list(rot = 90)),
          colorkey = list(space = "left"),
          legend =
              list(right =
                       list(fun = dendrogramGrob,
                            args =
                                list(x = dd.col, ord = col.ord,
                                     side = "right",
                                     size = 10)),
                   top =
                       list(fun = dendrogramGrob,
                            args =
                                list(x = dd.row, 
                                     side = "top",
                                     type = "triangle"))))


dev.off()

#prueba----
r_melt$Categoria<-as.factor(r_melt$Categoria)

#Subset
ejr<-droplevels(subset(r_melt,r_melt$Variable=="Yield"))
#Borrar variable impacto
ejr$Impacto<-NULL


#Melt datos
ejr_m<-ejr %>%
    spread("Categoria","Cambio_por")

#Calcular  nuevas variables
ejr_m$Cambio_por<-((ejr_m$Tec-ejr_m$Notec)/ejr_m$Notec)*100

#Renombrar filas
row.names(ejr_m)<-paste(ejr_m$Region)

#Borrar variables no necesarias
ejr_m$Region<-NULL
ejr_m$Notec<-NULL
ejr_m$Tec<-NULL


ejd<-dist(ejr_m,method="euclidean",diag = FALSE,p=2)
ejdh<-hclust(ejd)

tiff(filename=paste(grd,"Rplot.tiff",sep=""), 
     width = 15, height = 10, units = 'in', res = 300)
plot(ejdh)
dev.off()




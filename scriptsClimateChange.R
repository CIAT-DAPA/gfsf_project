##### distribution data Bean climate Change


library(ggplot2)
library(gridExtra)
library(reshape2)
library(rgdal)
library(psych)
library(cluster)
library(fpc)
library(mclust)
library(d3heatmap)
library(RColorBrewer)


#declaracion del directorio
setwd("C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados")  
getwd()


# importar datos origen
## Read CSV file (header assumed), then put that into "csv.data" data object (any name is ok).
c <- read.csv("C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/cambios_porcentuales.csv",header=T)

# visuaizacion de los datos
## lista de las nombre de las variables
names(c)
str(c)  # estructuar de mis datos
levels(c$impactparameter)


#######
##Demanda
#Creando Subsets 
names(c)
demanda <- c[c$impactparameter == "Demand", ]

## graficas todos los GCMs de la demanda
names(demanda)

## reshape demanda
q<- melt (demanda, na.rm = FALSE, id.vars = "region", value.name = "GCM", 
  measure.vars= c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH",
                 "SSP2.NORE.8P5.379.NOTECH", "SSP2.GFLD.8P5.379.TECH",   "SSP2.HGEM.8P5.379.TECH", "SSP2.IPSL.8P5.379.TECH",
                 "SSP2.MIRO.8P5.379.TECH",  "SSP2.NORE.8P5.379.TECH", "SSP2.NoCC.NoCC.NOTECH","SSP2.NoCC.NoCC.TECH"))     
q # review datos


## creando subsets NoCCNoTEchn NoCCTEch
names(q)
# subset sin cambio climatico
s<-subset(q, variable== "SSP2.NoCC.NoCC.TECH" | variable=="SSP2.NoCC.NoCC.NOTECH" )

#  plotting box plot
q_2<-ggplot(s, aes(x=s$variable, y=s$value, fill=s$variable)) + geom_boxplot()
plot(q_2)

q_2+ theme(axis.text.x = element_text(face="bold", color=, 
                                     size=9, angle=0),
          axis.text.y = element_text(face="bold", color=, 
                                     size=9, angle=0),
          legend.position="none") +
          scale_x_discrete(breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                 labels=c("NoCCTechn","NoCCNoTech")) +
          ylab("Change Percentage Demand") +
          xlab("Scenarios") + 
  ggtitle("Demand, NoCC Technology & NoCC Without Technology")  + 
  theme(plot.title = element_text(lineheight=.1, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/DemandNoCC.png")                                                                                       


## other graph histogram
a<-ggplot(s, aes(x=s$GCM, colour=s$variable)) + geom_density(alpha=.3) +
  xlab("Scenarios") + 
  scale_colour_discrete(name="Change\npercentual",
                        breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                        labels=c("NoCCTechn","NoCCNoTech"))+
  ggtitle("Demand, NoCC Technology\n& NoCC Without Technology") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistogramDemandNoCC.png")                                                                                        #Demanda # demanda
plot(a)






##########
##Produccion
#Creando Subsets 
names(c)
pro <- c[c$impactparameter == "Production", ]

## graficas todos los GCMs de la demanda
names(pro)

## reshape demanda
pr<- melt (pro, na.rm = FALSE, id.vars = "region", value.name = "GCM", 
          measure.vars= c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH",
                          "SSP2.NORE.8P5.379.NOTECH", "SSP2.GFLD.8P5.379.TECH",   "SSP2.HGEM.8P5.379.TECH", "SSP2.IPSL.8P5.379.TECH",
                          "SSP2.MIRO.8P5.379.TECH",  "SSP2.NORE.8P5.379.TECH", "SSP2.NoCC.NoCC.NOTECH","SSP2.NoCC.NoCC.TECH"))     
pr # review datos


## creando subsets NoCCNoTEchn NoCCTEch
names(pr)
# subset sin cambio climatico
production<-subset(pr, variable== "SSP2.NoCC.NoCC.TECH" | variable=="SSP2.NoCC.NoCC.NOTECH" )

#  plotting box plot
qpro<-ggplot(production, aes(x=production$variable, y=production$value, fill=production$variable)) + geom_boxplot()
qpro<- qpro + theme(axis.text.x = element_text(face="bold", color=, 
                                      size=9, angle=0),
           axis.text.y = element_text(face="bold", color=, 
                                      size=9, angle=0),
           legend.position="none") +
  scale_x_discrete(breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                   labels=c("NoCCTechn","NoCCNoTech")) +
  ylab("Change Percentage Producion") +
  xlab("Scenarios") + 
  ggtitle("Producion, NoCC Technology & NoCC Without Technology")  + 
  theme(plot.title = element_text(lineheight=.1, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/ProducionNoCC.png")                                                                                       


## other graph histogram
b<-ggplot(production, aes(x=production$value, colour=production$variable)) + geom_density(alpha=.3) +
  xlab("Scenarios") + 
  scale_colour_discrete(name="Change\npercentual",
                        breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                        labels=c("NoCCTechn","NoCCNoTech"))+
  ggtitle("Production, NoCC Technology\n& NoCC Without Technology") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograProducionNoCC.png")                                                                                        #Produccion
plot(b)

#######  
### Rendimientos

#Creando Subsets 
names(c)
yield<- c[c$impactparameter == "Yield", ]

## graficas todos los GCMs de la demanda
names(yield)

## reshape demanda
y<- melt (yield, na.rm = FALSE, id.vars = "region", value.name = "GCM", 
           measure.vars= c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH",
                           "SSP2.NORE.8P5.379.NOTECH", "SSP2.GFLD.8P5.379.TECH",   "SSP2.HGEM.8P5.379.TECH", "SSP2.IPSL.8P5.379.TECH",
                           "SSP2.MIRO.8P5.379.TECH",  "SSP2.NORE.8P5.379.TECH", "SSP2.NoCC.NoCC.NOTECH","SSP2.NoCC.NoCC.TECH"))     
y # review datos


## creando subsets NoCCNoTEchn NoCCTEch
names(y)
# subset sin cambio climatico
rendimientos<-subset(y, variable== "SSP2.NoCC.NoCC.TECH" | variable=="SSP2.NoCC.NoCC.NOTECH" )

#  plotting box plot
r<-ggplot(rendimientos, aes(x=rendimientos$variable, y=rendimientos$value,fill=rendimientos$variable)) + geom_boxplot()
plot(r)

r + theme(axis.text.x = element_text(face="bold", color=, 
                                      size=9, angle=0),
           axis.text.y = element_text(face="bold", color=, 
                                      size=9, angle=0),
           legend.position="none") +
  scale_x_discrete(breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                   labels=c("NoCCTechn","NoCCNoTech")) +
  ylab("Change Percentage Yield") +
  xlab("Scenarios") + 
  ggtitle("Yield, NoCC Technology & NoCC Without Technology")  + 
  theme(plot.title = element_text(lineheight=.1, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/YieldNoCC.png")                                                                                       


## other graph histogram
yi<-ggplot(rendimientos, aes(x=rendimientos$value, colour=rendimientos$variable)) + geom_density(alpha=.3) +
  xlab("Scenarios") + 
  scale_colour_discrete(name="Change\npercentual",
                        breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                        labels=c("NoCCTechn","NoCCNoTech"))+
  ggtitle("Yield, NoCC Technology\n& NoCC Without Technology") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograYieldNoCC.png")                                                                                       


######
## Area
names(c)
area<- c[c$impactparameter == "Area", ]

## graficas todos los GCMs de la area
names(area)

## reshape area
ar<- melt (area, na.rm = FALSE, id.vars = "region", value.name = "GCM", 
          measure.vars= c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH",
                          "SSP2.NORE.8P5.379.NOTECH", "SSP2.GFLD.8P5.379.TECH",   "SSP2.HGEM.8P5.379.TECH", "SSP2.IPSL.8P5.379.TECH",
                          "SSP2.MIRO.8P5.379.TECH",  "SSP2.NORE.8P5.379.TECH", "SSP2.NoCC.NoCC.NOTECH","SSP2.NoCC.NoCC.TECH"))     
ar # review datos


## creando subsets NoCCNoTEchn NoCCTEch
names(ar)
# subset sin cambio climatico
are<-subset(ar, variable== "SSP2.NoCC.NoCC.TECH" | variable=="SSP2.NoCC.NoCC.NOTECH" )

#  plotting box plot
area_g<-ggplot(are, aes(x=are$variable, y=are$GCM,fill=are$variable)) + geom_boxplot()
plot(area_g)

area_g + theme(axis.text.x = element_text(face="bold", color=, 
                                     size=9, angle=0),
          axis.text.y = element_text(face="bold", color=, 
                                     size=9, angle=0),
          legend.position="none") +
  scale_x_discrete(breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                   labels=c("NoCCTechn","NoCCNoTech")) +
  ylab("Change Percentage Area") +
  xlab("Scenarios") + 
  ggtitle("Area, NoCC Technology & NoCC Without Technology")  + 
  theme(plot.title = element_text(lineheight=.1, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/AReaNoCC.png")                                                                                       

are

## other graph histogram
area_his<-ggplot(are, aes(x=are$GCM, colour=are$variable)) + geom_density(alpha=.3) +
  xlab("Scenarios") + 
  scale_colour_discrete(name="Change\npercentual",
                        breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                        labels=c("NoCCTechn","NoCCNoTech"))+
  ggtitle("Area, NoCC Technology\n& NoCC Without Technology") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 

ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograAreaNoCC.png")                                                                                       


plot(area_his)
plot(yi)



#save all graphs in one
grid.arrange(a, b, yi, area_his, ncol=2, nrow =3)
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/g2.png")                                                                                       


###################
## rendimientos con cambio climatico 

#Creando Subsets 
names(c)
yieldcc<- c[c$impactparameter == "Yield", ]

## graficas todos los GCMs de la demanda
names(yieldcc)

## reshape yield con cambio climático
ycc<- melt (yieldcc, na.rm = FALSE, id.vars = "region", value.name = "GCM", 
          measure.vars= c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH",
                          "SSP2.NORE.8P5.379.NOTECH", "SSP2.GFLD.8P5.379.TECH",   "SSP2.HGEM.8P5.379.TECH", "SSP2.IPSL.8P5.379.TECH",
                          "SSP2.MIRO.8P5.379.TECH",  "SSP2.NORE.8P5.379.TECH", "SSP2.NoCC.NoCC.NOTECH","SSP2.NoCC.NoCC.TECH"))     
ycc # review datos


## creando subsets NoTEchn TEch
names(ycc)
# subset Solo cambio climatico 
rendicc<-subset(ycc, variable!="SSP2.NoCC.NoCC.TECH" & variable!="SSP2.NoCC.NoCC.NOTECH")
levels(rendicc$variable)
View(rendicc) # visualizacion 




#  plotting box plot
rcc<-ggplot(rendicc, aes(x=rendicc$variable, y = rendicc$value,fill=rendicc$variable)) + geom_boxplot()
plot(rcc)

rcc + theme(axis.text.x = element_text(face="bold", color=, 
                                     size=9, angle=0),
          axis.text.y = element_text(face="bold", color=, 
                                     size=9, angle=0),
          legend.position="none") +
  scale_x_discrete(breaks=c("SSP2.NoCC.NoCC.TECH","SSP2.NoCC.NoCC.NOTECH"),
                   labels=c("NoCCTechn","NoCCNoTech")) +
  ylab("Change Percentage Yield") +
  xlab("Scenarios") + 
  ggtitle("Yield, With Technology &  Without Technology")  + 
  theme(plot.title = element_text(lineheight=.1, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/YieldCC.png")                                                                                       


#################
## graficos usando filtros con tecnología y sin tecnología 
levels(rendicc$variable)
rendicc_tech<-rendicc

## Creando  las variables con tecnologia y sin tecnoliogia
rendicc_tech$adoption<-as.character(rendicc_tech$variable)
  
Tg<-  as.numeric(grep(".TECH",rendicc_tech$adoption,fixed=T))
TgN<-  as.numeric(grep(".NOTECH",rendicc_tech$adoption,fixed=T))


  for(i in Tg){
    cat(i,"\n")
   rendicc_tech[i,4]<-"TECH"
    
  };rm(i)

for(i in TgN){
  cat(i,"\n")
  rendicc_tech[i,4]<-"NO_TECH"
  
};rm(i)


  
# eliminate columns by name
# rendicc_tech$inter <- NULL

## graph

yicc2<-ggplot(rendicc_tech, aes(x=value, colour=adoption)) + geom_density(alpha=.3) +
  xlab(" Change percentual") + 
  scale_colour_discrete(name="Change\npercentual")+
                      #  breaks=c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH", "SSP2.NORE.8P5.379.NOTECH","SSP2.GFLD.8P5.379.TECH","SSP2.HGEM.8P5.379.TECH","SSP2.IPSL.8P5.379.TECH","SSP2.MIRO.8P5.379.TECH","SSP2.NORE.8P5.379.TECH" ),
                       # labels=c("GDFLNoTech","HGEMNoTech","IPSLNoTech", "MIRONoTech", "NORENoTech","GDFLTech","HGEMTech","IPSLTech", "MIROTech", "NORETech" ))+
  ggtitle("Yield, with Technology\n& without Technology by GCMs") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograTechnYieldCC.png")                                                                                       
plot(yicc2)


## boxplot 

# Eliminar datos agrupados innecesarios
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="LAC",]  
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="MEN",]
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="SSA",]

# Eliminar datos que no adoptaron tecnología 
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="LAC-Cuba",]
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="LAC-Jamaica",]
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="MEN-Egypt",]
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="LAC-Other Caribbean",]
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="LAC-Haiti",]
rendicc_tech<-rendicc_tech[!rendicc_tech$region=="LAC-Dominican Republic",]

#grafico
y3<-ggplot(rendicc_tech, aes(x=adoption, y=value,fill=adoption)) + 
  geom_boxplot(outlier.colour= "red", outlier.size = 4, aes(fill=adoption)) 

y3 + theme(axis.text.x = element_text(face="bold", color=, 
                                       size=9, angle=0),
            axis.text.y = element_text(face="bold", color=, 
                                       size=9, angle=0),
            legend.position="none") +
            scale_y_continuous(name="Percentage Change %")+
            scale_x_discrete(name=" Adoption")+
         ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/boxplotTechnYieldCC.png")                                                                                       




### graph  todos los gcms y escenarios

yicc<-ggplot(rendicc, aes(x=rendicc$GCM, colour=rendicc$variable)) + geom_density(alpha=.3) +
  xlab("Scenarios") + 
  scale_colour_discrete(name="Change\npercentual",
                        breaks=c("SSP2.GFLD.8P5.379.NOTECH", "SSP2.HGEM.8P5.379.NOTECH", "SSP2.IPSL.8P5.379.NOTECH", "SSP2.MIRO.8P5.379.NOTECH", "SSP2.NORE.8P5.379.NOTECH","SSP2.GFLD.8P5.379.TECH","SSP2.HGEM.8P5.379.TECH","SSP2.IPSL.8P5.379.TECH","SSP2.MIRO.8P5.379.TECH","SSP2.NORE.8P5.379.TECH" ),
                        labels=c("GDFLNoTech","HGEMNoTech","IPSLNoTech", "MIRONoTech", "NORENoTech","GDFLTech","HGEMTech","IPSLTech", "MIROTech", "NORETech" ))+
  ggtitle("Yield, with Technology\n& without Technology by GCM's") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
ggsave(file="C:/Users/CEGONZALEZ/Documents/GFSF/datos brutos/datos procesados/HistograGCMsYieldCC.png")                                                                                       

plot(yicc)


######  
#summary datos con cambio climatico 
des<-describeBy(rendicc_tech,rendicc_tech$adoption)
des


## Creando  las variables con tecnologia y sin tecnoliogia
rendimientos$adoption<-as.character(rendimientos$variable)
Tgcc<-  as.numeric(grep(".TECH",rendimientos$adoption,fixed=T))
TgNcc<- as.numeric(grep(".NOTECH",rendimientos$adoption,fixed=T))


for(i in Tgcc){
  cat(i,"\n")
  rendimientos[i,4]<-"TECH"
  
};rm(i)

for(i in TgNcc){
  cat(i,"\n")
  rendimientos[i,4]<-"NO_TECH"
  
};rm(i)

View(rendimientos)

rendimientos # datos de rendimientos sin cambio climatico
View(rendimientos)

dNocc<-describeBy(rendimientos,rendimientos$adoption)
dNocc


### mismo analisis pero con area
are
are$adoption<-as.character(are$variable)
Tga<-  as.numeric(grep(".TECH",are$adoption,fixed=T))
TgNa<- as.numeric(grep(".NOTECH",are$adoption,fixed=T))


for(i in Tga){
  cat(i,"\n")
  are[i,4]<-"TECH"
  
};rm(i)

for(i in TgNa){
  cat(i,"\n")
  are[i,4]<-"NO_TECH"
  
};rm(i)

View(are)
darea<-describeBy(are,are$adoption)   # para visualizar 
darea    # para visualizar un objeto



#### analisis de cluster in R
clus <- dist(rendimientos, method = "euclidean") # distance matrix
fit <- hclust(clus, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# K-Means Clustering with 5 clusters
fit2 <- kmeans(rendimientos, 5)
fit2$cluster

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
clusplot(rendimientos, fit2$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions

plotcluster(rendimientos, fit2$cluster)
??plotcuster

### falta desarrollar otros clusters para incluir en el analisis.
cluster.stats(d, fit1$cluster, fit2$cluster) # validando clusters

# Model Based Clustering
# library(mclust)
# fita <- Mclust(rendimientos)
# plot(fita) # plot results 
# summary(fita) # display the best model


######################################################
## heatmap 

x<-c("data.table", "dplyr", "purrr", "tidyr", "lubridate","countrycode", "scales", "countrycode", "d3heatmap") 
install.packages(x)

# prueba utilizando rendimientos
yield <- yield[order(yield$region),] # ordenar datos
row.names(yield) <- yield$region  # preparar datos 
##r2 <- r2[,2:520]
yield_matrix <- data.matrix(yield)
yieldcc_matrix<-data.matrix(yieldcc)

### graficando 
d3heatmap(yield_matrix, scale = "column", dendrogram = "both", colors = "Blues")


### crear cambios medios/medianos con  y sin cambio climatico
c # base de datos total
View(c)
names(c)
#rendimientos
yield<- c[c$impactparameter == "Yield", ]  # para generar objeto apartir de la una base de datos con un condicional
yield$productiontype<- NULL  # eliminar columnas
yield$impactparameter<-NULL # eliminar columnas
yield$commodity<-NULL  # eliminar columnas
names(yield)   # obtener los nombres de las variables del subset.

## calcular la media
region_mean <- data.frame(Region=yield$region, Mean=apply(X=yield[,3:ncol(yield)], MARGIN=1, FUN=mean))
names(region_cc_mean)
## calculo de la dal cambio porcentual entre el cambios de uso y no de tecnologia.
region_cc_mean$cambio<- region_cc_mean$CC_Tech_mean-region_cc_mean$CC_NoTech_mean/region_cc_mean$CC_NoTech_mean



## prueba del grafico de heatmap con el perfil de impacto solo de cambio climatico
impactechn = region_cc_mean # prep impact data for plotting
rownames(impactechn) = make.names(impactechn$Region, unique = T)
impactechn2 <- data.matrix(impactechn)


### heatmap con cambio climatico y tecnologia solo de rendimientos 
d3heatmap(impactechn2, scale = "column",
          hclustfun = hclust, dendrogram = "row", colors = "Blues")





# calcular media por: CC y TECH, se utiliza para los patrones
mtch_CC_NOTECH <- grep(pattern="*8P5.379.NOTECH$", x=colnames(yield))
mtch_CC_TECH   <- grep(pattern="\\.TECH$", x=colnames(yield))

mtch_Nocc_tech <- grep(pattern = "\\.TECH$", x=colnames(yield) )
View(mtch_Nocc_tech)

region_cc_mean <- data.frame(Region=yield$region,
                          CC_Tech_mean=apply(X=yield[,mtch_CC_TECH], MARGIN=1, FUN=mean),
                          CC_NoTech_mean=apply(X=yield[,mtch_CC_NOTECH], MARGIN=1, FUN=mean))
row.names(region_cc_mean) <- region_cc_mean$region  # preparar datos 
prood_matrix<-data.matrix(region_cc_mean)




#libraries

g=gc;rm(list = ls())
# R options

options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))
suppressMessages(library(curl))
suppressMessages(library(shiny))
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(parallelsugar))
suppressMessages(library(doParallel))
suppressMessages(library(qgraph))


# Limitar numero de decimales y remove scientific notation-----
options(digits=3) 
options(scipen=999)

#directorio graficos
pic<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/Graphics")

### matrix international trade
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")
DataFiles<- list.files(pattern = "appendDataComercio",full.names = T)
DataFiles<- lapply(DataFiles, read.csv)
DataFiles<- do.call(rbind,DataFiles)
comt<- DataFiles

comt$X<- NULL
comt$Classification<- as.character(comt$Classification)
comt<- filter(comt, Classification!="No data matches your query or your query is too complex. Request JSON or XML format for more information.")


str(comt)
var<-c("Partner","Reporter","Commodity","Year","Trade.Flow", "Alt.Qty.Unit","Trade.Value..US..")
comt<- comt[,var]
cfiles<- comt
str(cfiles)
cfiles$Partner<- as.character(cfiles$Partner)
cfiles$Reporter<- as.character(cfiles$Reporter)
cfiles$Commodity<- as.character(cfiles$Commodity)
cfiles$Commodity<- as.character(cfiles$Commodity)
cfiles$Trade.Flow<- as.character(cfiles$Trade.Flow)
cfiles$Year<- as.character(cfiles$Year); cfiles$Year<- as.numeric(cfiles$Year)


cfiles<- filter(cfiles, Partner!="World")

cfiles$Reporter<- plyr::revalue(cfiles$Reporter, c("Dem. People's Rep. of Korea"="North Korea",
                                           "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                           "Côte d'Ivoire"="Ivory Coast",
                                           "Lao People's Dem. Rep."="Lao",
                                           "China, Hong Kong SAR"='Hong Kong',
                                           "China, Macao SAR"='Macao',
                                           "China, mainland"='China',
                                           "China, Taiwan Province of"='Taiwan',
                                           "Réunion"='Reunion',
                                           "Sudan (former)"="Sudan former",
                                           "Netherlands Antilles (former)"="Netherlands Antilles",
                                           "Bolivia (Plurinational State of)"="Bolivia",
                                           "Venezuela (Bolivarian Republic of)"="Venezuela",
                                           "Iran (Islamic Republic of)"="Iran",
                                           "Falkland Islands (Malvinas)"="Falkland Islands",
                                           "Curaçao"="Curacao"))

cfiles$Partner<- plyr::revalue(cfiles$Partner, c("Dem. People's Rep. of Korea"="North Korea",
                                                             "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                             "Côte d'Ivoire"="Ivory Coast",
                                                             "China, Hong Kong SAR"='Hong Kong',
                                                             "China, Macao SAR"='Macao',
                                                             "China, mainland"='China',
                                                             "China, Taiwan Province of"='Taiwan',
                                                             "Réunion"='Reunion',
                                                             "Sudan (former)"="Sudan former",
                                                             "Netherlands Antilles (former)"="Netherlands Antilles",
                                                             "Bolivia (Plurinational State of)"="Bolivia",
                                                             "Venezuela (Bolivarian Republic of)"="Venezuela",
                                                             "Iran (Islamic Republic of)"="Iran",
                                                             "Falkland Islands (Malvinas)"="Falkland Islands",
                                                             "Curaçao"="Curacao"))

##corriendo nombres columnas
colnames(cfiles)[6]<- "Qty"
colnames(cfiles)[7]<- "Val"

## countries 
pots<- table(cfiles$Partner)
####regions
caribbean<- c("Cuba", "Haiti", "Jamaica", "Dominican Rep.","N. Mariana Isds",
              "Aruba","Saint Lucia","Dominica", "Saint Vincent and the Grenadines", "Puerto Rico",
              "Anguilla", "Antigua and Barbuda", "Saint Kitts and Nevis", "Montserrat",
              "Martinique", "Barbados", "Trinidad and Tobago","Turks and Caicos Isds", 
              "Grenada", "Curacao","Cayman Isds","Neth. Antilles" )

Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico",
                     "Nicaragua","Panama","El Salvador" )

Australia_and_New_Zealand<- c( "New Zealand", "Australia")

Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")

Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar",
                   "Mayotte", "Mozambique","Malawi","Rwanda","Somalia", "Tanzania",
                   "Uganda","Zambia","Zimbabwe", "Mauritius", "Seychelles","Comoros")

Eastern_Asia<- c("China","Japan","South Korea","Mongolia","North Korea", "Taiwan", "Macao")

Eastern_Europe<- c("Bulgaria","Belarus","Czechia","Hungary","Rep. of Moldova","Other Balkans","Poland",
                   "Romania","Russian Federation","Slovakia","Ukraine", "Russian Federation", "Latvia","Lithuania",
                   "TFYR of Macedonia", "Kosovo", "Poland", "Serbia", "Romania")

Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia", "Serbia and Montenegro", "Andorra",
                    "France","Malta", "San Marino", "Gibraltar")


Poli_Mela_Micro<- c("Fiji", "Papua New Guinea","Solomon Isds","Vanuatu","New Caledonia", "Tuvalu", 
                    "Tonga", "Tokelau", "Samoa", "Marshall Isds", "Guam", "FS Micronesia", "Cook Isds", "American Samoa")

Middle_Africa<- c("Angola","Central African Rep.","Cameroon","Dem. Rep. of the Congo","Congo","Gabon","Equatorial Guinea","Chad",
                  "S�o Tom� and Pr�ncipe")

Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Fmr Sudan","Tunisia", "South Sudan")

Northern_America<-c("Canada","Greenland","USA")

Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","UK", "Estonia", 
                    "Sweden", "Faeroe")

South_America<- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guyanas","Peru",
                  "Paraguay","Uruguay","Venezuela","Suriname")

South_Eastern_Asia<- c("Indonesia","Cambodia","Laos","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "Timor-Leste","Viet Nam", "Singapore", "Brunei Darussalam")

Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")

Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Sri Lanka","Maldives", "Sri Lanka",
                  "Nepal","Other Indian Ocean","Pakistan")


Western_Africa<- c("Benin","Burkina Faso","Ivory Coast","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia",
                   "Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leone","Togo", "Saint Helena", 
                   "Sao Tome and Principe", "Cabo Verde")

Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", 
                "Palestine", "Rest of Arabia","Saudi Arabia","Iran","Bahrain","Kuwait","Oman", "Qatar","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates", "Georgia", "South Ossetia",
                "Lebanon")

Western_Europe<- c("Austria","Belgium-Luxembourg","Belgium","Switzerland","Germany","France","Netherlands","Other Atlantic")
#----------------------------------------------------------------#

placeout<- c("Areas, nes","United States Minor Outlying Islands","Wallis and Futuna Isds",
             "US Misc. Pacific Isds","So. African Customs Union",  "Sao Tome and Principe", 
             "Saint Maarten","Saint Kitts and Nevis", "Saint Barthélemy","Rest of America, nes","Pitcairn",
             "Other Europe, nes","Norfolk Isds","Other Africa, nes","Other Asia, nes", "Br. Virgin Isds","Cocos Isds",
             "Cayman Isds")
starch<- c("Manioc (cassava) starch", "Starch; manioc (cassava)")
expor_starch<- filter(cfiles, Commodity %in% starch) %>% 
      filter(., Trade.Flow=="Export") %>% filter(., !Reporter %in% placeout) %>% 
      filter(., !Partner %in% placeout)
      


# expor_starch$Year<- as.numeric(expor_starch$Year)
p<- sort(unique(cfiles$Year))
p1<- c(1997, 1998, 1999, 2000, 2001)
p2<- c(2002, 2003, 2004, 2005, 2006)
p3<- c(2007, 2008, 2009, 2010, 2011)
p4<- c(2012, 2013, 2014, 2015, 2016)

periodList <- c('1997_2006', '2002_2006','2007_2011', '2012_2016' )


# periodo1
Xstar1<- expor_starch %>% filter(., Year %in% p1) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar1<- Xstar1[c("Reporter","Partner" ,"Year","Qty")] %>% spread("Year", "Qty")

# periodo2
Xstar2<- expor_starch %>% filter(., Year %in% p2) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar2<- Xstar2[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")

# periodo3
Xstar3<- expor_starch %>% filter(., Year %in% p3) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar3<- Xstar3[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")

# periodo4
Xstar4<- expor_starch %>% filter(., Year %in% p4) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar4<- Xstar4[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")


Xstar1$mean <- rowMeans(x=Xstar1[,3:ncol(Xstar1)], na.rm=TRUE)
Xstar2$mean <- rowMeans(x=Xstar2[,3:ncol(Xstar2)], na.rm=TRUE)
Xstar3$mean <- rowMeans(x=Xstar3[,3:ncol(Xstar3)], na.rm=TRUE)
Xstar4$mean <- rowMeans(x=Xstar4[,3:ncol(Xstar4)], na.rm=TRUE)


Xstar1 <- Xstar1[,c("Reporter", "Partner", "mean" )]
Xstar2 <- Xstar2[,c("Reporter", "Partner", "mean" )]
Xstar3 <- Xstar2[,c("Reporter", "Partner", "mean" )]
Xstar4 <- Xstar4[,c("Reporter", "Partner", "mean" )]
Xstar4$mean[is.na(Xstar4$mean)]<-0

periodos<- list("1997_2006"=Xstar1, "2002_2006"=Xstar2,"2007_2011"=Xstar3, "2012_2016"=Xstar4 )



############################ circulos graficos ##################################

p=4
lapply(1: length(periodos), function(p){
      
     
      require(dplyr)
      require(tidyr)
      require(circlize)
      cat(paste(">>>>>>>>>>>>> start with ", periodList[[p]], " Done!!!!\n ", sep = ""))
      # temp<- expor_starch %>% filter(Year==p[x]) %>% select(.,Partner, Reporter,Partner,Qty)
      # temp<- as.data.frame((periodos$p1))
      temp<- as.data.frame(periodos[[p]])
#       temp$mean[which(temp$mean==0)] <- 1
#       temp$mean<- log10(temp$mean)
      rownames(temp) <- 1:nrow(temp)
      
      auxCarlos<- expand.grid(unique(temp$Reporter), unique(temp$Partner))
      colnames(auxCarlos)[1]<-"Reporter"
      colnames(auxCarlos)[2]<- "Partner"
      auxCarlos2 <- temp[,1:2]
      
      require(sqldf)
      auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
      dim(auxCarlos2)
      auxCarlos2$mean <- 0
      p1wide <- rbind(temp, auxCarlos2)
      
      p1wide <- as.data.frame(p1wide)
      p1wide$Reporter <- as.character(p1wide$Reporter)
      p1wide$Partner <- as.character(p1wide$Partner)
      p1wide$mean <- as.numeric(p1wide$mean)
      
      treal2<- p1wide %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- p1wide %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      treal4<- p1wide %>% filter(Reporter %in% Poli_Mela_Micro) %>% mutate(., zone="Melanesia")
      treal5<- p1wide %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- p1wide %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      treal7<- p1wide %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal8<- p1wide %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal9<- p1wide %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal1<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal10<- p1wide %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal11<- p1wide %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal12<- p1wide %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal13<- p1wide %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- p1wide %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal15<- p1wide %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      treal16<- p1wide %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      treal17<- p1wide %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal18<- p1wide %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      treal19<- p1wide %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")
      
      
      r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal19)
      
      rjoin<- left_join(r, p1wide, by = c("Reporter", "Partner", "mean"))
      
      #   tempcolor<- unique(rjoin$zone)
      colorstemp<- c("##f1eef6", "#d0d1e6","#a6bddb", "#74a9cf", "#3690c0","#0570b0", "#034e7b" )
      rjoin$color<- NA
      rjoin$color[rjoin$zone=="caribbean"]<-  "#40004b"
      rjoin$color[rjoin$zone=="Australia_and_New_Zealand"]<-   "#f0027f"
      rjoin$color[rjoin$zone=="Poli_Mela_Micro"]<-    "#33a02c"
      rjoin$color[rjoin$zone=="Eastern_Africa"]<- "#74a9cf"
      rjoin$color[rjoin$zone=="Middle_Africa"]<- "#3690c0"
      rjoin$color[rjoin$zone=="Northern_Africa"]<- "#ff7f00"
      rjoin$color[rjoin$zone=="Southern_Africa"]<-  "#034e7b"
      rjoin$color[rjoin$zone=="Western_Africa"]<- "#00441b"
      rjoin$color[rjoin$zone=="Eastern_Asia"]<- "#cab2d6"
      rjoin$color[rjoin$zone=="South_Eastern_Asia"]<- "#081d58"
      rjoin$color[rjoin$zone=="Southern_Asia"]<- "#543005"
      rjoin$color[rjoin$zone=="Western_Asia"]<- "#1f78b4"
      rjoin$color[rjoin$zone=="South_America"]<- "#b15928"
      rjoin$color[rjoin$zone=="Northern_America"]<- "#e31a1c"
      rjoin$color[rjoin$zone=="Central_America"]<- "#33a02c"
      rjoin$color[rjoin$zone=="Eastern_Europe"]<- "#bc80bd"
      rjoin$color[rjoin$zone=="Northern_Europe"]<- "#08306b"
      rjoin$color[rjoin$zone=="Southern_Europe"]<- "#e7298a"
      rjoin$color[rjoin$zone=="Western_Europe"]<- "#cab2d6"
      
      
      q2<- quantile(temp$mean,probs = 0.10)
      #       temp<- temp %>% filter(Value>=q2)
      
      color<- rjoin %>% filter(., mean>q2) %>% select("Reporter","Partner" , "color")  
      colorsss<- unique(rjoin$color)
      
      rjoin$color<- as.factor(rjoin$color)
      rjoin<- rjoin %>% select("Reporter","Partner" , "mean","color")
      rdata<- rjoin %>% select("Reporter","Partner" , "mean")
      
      # grafico codes
      circos.clear()
      circos.par(start.degree = 10, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      par(mar = rep(0, 4))
      
      
      png(paste(pic,"/",periodList[[p]],'_Expors_StarchCassava.png',sep = ""), width=10, height=10, units='in', res=300)
      
      chordDiagram(x = rdata[which(rdata$mean>q2),],col=color$color,  transparency = 0.25, #col=color$color #[rdata$mean>0.7,]
                   directional = 1,
                   direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                   annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            xplot = get.cell.meta.data("xplot")
            ylim = get.cell.meta.data("ylim")
            sector.name = get.cell.meta.data("sector.index")
            if(abs(xplot[2] - xplot[1]) < 20) {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                              niceFacing = TRUE, adj = c(0, 0.5))
            } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                              niceFacing = TRUE, adj = c(0.5, 0))
            }
      }, bg.border = NA)
      
      dev.off()
      
      circos.clear()
      
      dev.off()
      
      
})      

      
######################################### cassava dried ##############################################

dried<- c("Manioc (cassava), fresh or dried", "Manioc (cassava)", 
          "Vegetable roots and tubers; manioc (cassava), with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets")
expor_dried<- filter(cfiles, Commodity %in% dried) %>% 
      filter(., Trade.Flow=="Export") %>% filter(., !Reporter %in% placeout) %>% 
      filter(., !Partner %in% placeout)



# expor_starch$Year<- as.numeric(expor_starch$Year)
p<- sort(unique(cfiles$Year))
p1<- c(1997, 1998, 1999, 2000, 2001)
p2<- c(2002, 2003, 2004, 2005, 2006)
p3<- c(2007, 2008, 2009, 2010, 2011)
p4<- c(2012, 2013, 2014, 2015, 2016)

periodList <- c('1997_2006', '2002_2006','2007_2011', '2012_2016' )


# periodo1
Xstar1<- expor_dried %>% filter(., Year %in% p1) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar1<- Xstar1[c("Reporter","Partner" ,"Year","Qty")] %>% spread("Year", "Qty")

# periodo2
Xstar2<- expor_dried %>% filter(., Year %in% p2) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar2<- Xstar2[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")

# periodo3
Xstar3<- expor_dried %>% filter(., Year %in% p3) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar3<- Xstar3[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")

# periodo4
Xstar4<- expor_dried %>% filter(., Year %in% p4) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar4<- Xstar4[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")


Xstar1$mean <- rowMeans(x=Xstar1[,3:ncol(Xstar1)], na.rm=TRUE)
Xstar2$mean <- rowMeans(x=Xstar2[,3:ncol(Xstar2)], na.rm=TRUE)
Xstar3$mean <- rowMeans(x=Xstar3[,3:ncol(Xstar3)], na.rm=TRUE)
Xstar4$mean <- rowMeans(x=Xstar4[,3:ncol(Xstar4)], na.rm=TRUE)


Xstar1 <- Xstar1[,c("Reporter", "Partner", "mean" )]
Xstar1$mean[is.na(Xstar1$mean)]<-0

Xstar2 <- Xstar2[,c("Reporter", "Partner", "mean" )]
Xstar2$mean[is.na(Xstar2$mean)]<-0

Xstar3 <- Xstar3[,c("Reporter", "Partner", "mean" )]
Xstar3$mean[is.na(Xstar3$mean)]<-0

Xstar4 <- Xstar4[,c("Reporter", "Partner", "mean" )]
Xstar4$mean[is.na(Xstar4$mean)]<-0

periodos<- list("1997_2006"=Xstar1, "2002_2006"=Xstar2,"2007_2011"=Xstar3, "2012_2016"=Xstar4 )



############################ circulos graficos ##################################

p=4
lapply(1: length(periodos), function(p){
      
      
      require(dplyr)
      require(tidyr)
      require(circlize)
      cat(paste(">>>>>>>>>>>>> start with ", periodList[[p]], " Done!!!!\n ", sep = ""))
      # temp<- expor_starch %>% filter(Year==p[x]) %>% select(.,Partner, Reporter,Partner,Qty)
      # temp<- as.data.frame((periodos$p1))
      temp<- as.data.frame(periodos[[p]])
      #       temp$mean[which(temp$mean==0)] <- 1
      #       temp$mean<- log10(temp$mean)
      rownames(temp) <- 1:nrow(temp)
      
      auxCarlos<- expand.grid(unique(temp$Reporter), unique(temp$Partner))
      colnames(auxCarlos)[1]<-"Reporter"
      colnames(auxCarlos)[2]<- "Partner"
      auxCarlos2 <- temp[,1:2]
      
      require(sqldf)
      auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
      dim(auxCarlos2)
      auxCarlos2$mean <- 0
      p1wide <- rbind(temp, auxCarlos2)
      
      p1wide <- as.data.frame(p1wide)
      p1wide$Reporter <- as.character(p1wide$Reporter)
      p1wide$Partner <- as.character(p1wide$Partner)
      p1wide$mean <- as.numeric(p1wide$mean)
      
      treal2<- p1wide %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- p1wide %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      treal4<- p1wide %>% filter(Reporter %in% Poli_Mela_Micro) %>% mutate(., zone="Melanesia")
      treal5<- p1wide %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- p1wide %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      treal7<- p1wide %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal8<- p1wide %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal9<- p1wide %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal1<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal10<- p1wide %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal11<- p1wide %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal12<- p1wide %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal13<- p1wide %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- p1wide %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal15<- p1wide %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      treal16<- p1wide %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      treal17<- p1wide %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal18<- p1wide %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      treal19<- p1wide %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")
      
      
      r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal19)
      
      rjoin<- left_join(r, p1wide, by = c("Reporter", "Partner", "mean"))
      
      #   tempcolor<- unique(rjoin$zone)
      colorstemp<- c("##f1eef6", "#d0d1e6","#a6bddb", "#74a9cf", "#3690c0","#0570b0", "#034e7b" )
      rjoin$color<- NA
      rjoin$color[rjoin$zone=="caribbean"]<-  "#40004b"
      rjoin$color[rjoin$zone=="Australia_and_New_Zealand"]<-   "#f0027f"
      rjoin$color[rjoin$zone=="Poli_Mela_Micro"]<-    "#33a02c"
      rjoin$color[rjoin$zone=="Eastern_Africa"]<- "#74a9cf"
      rjoin$color[rjoin$zone=="Middle_Africa"]<- "#3690c0"
      rjoin$color[rjoin$zone=="Northern_Africa"]<- "#ff7f00"
      rjoin$color[rjoin$zone=="Southern_Africa"]<-  "#034e7b"
      rjoin$color[rjoin$zone=="Western_Africa"]<- "#00441b"
      rjoin$color[rjoin$zone=="Eastern_Asia"]<- "#cab2d6"
      rjoin$color[rjoin$zone=="South_Eastern_Asia"]<- "#081d58"
      rjoin$color[rjoin$zone=="Southern_Asia"]<- "#543005"
      rjoin$color[rjoin$zone=="Western_Asia"]<- "#1f78b4"
      rjoin$color[rjoin$zone=="South_America"]<- "#b15928"
      rjoin$color[rjoin$zone=="Northern_America"]<- "#e31a1c"
      rjoin$color[rjoin$zone=="Central_America"]<- "#33a02c"
      rjoin$color[rjoin$zone=="Eastern_Europe"]<- "#bc80bd"
      rjoin$color[rjoin$zone=="Northern_Europe"]<- "#08306b"
      rjoin$color[rjoin$zone=="Southern_Europe"]<- "#e7298a"
      rjoin$color[rjoin$zone=="Western_Europe"]<- "#cab2d6"
      
      
      q2<- quantile(temp$mean,probs = 0.10)
      #       temp<- temp %>% filter(Value>=q2)
      
      color<- rjoin %>% filter(., mean>q2) %>% select("Reporter","Partner" , "color")  
      colorsss<- unique(rjoin$color)
      
      rjoin$color<- as.factor(rjoin$color)
      rjoin<- rjoin %>% select("Reporter","Partner" , "mean","color")
      rdata<- rjoin %>% select("Reporter","Partner" , "mean")
      
      # grafico codes
      circos.clear()
      circos.par(start.degree = 10, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      par(mar = rep(0, 4))
      
      
      png(paste(pic,"/",periodList[[p]],'_Expors_DriedCassava.png',sep = ""), width=10, height=10, units='in', res=300)
      
      chordDiagram(x = rdata[which(rdata$mean>q2),],col=color$color,  transparency = 0.25, #col=color$color #[rdata$mean>0.7,]
                   directional = 1,
                   direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                   annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            xplot = get.cell.meta.data("xplot")
            ylim = get.cell.meta.data("ylim")
            sector.name = get.cell.meta.data("sector.index")
            if(abs(xplot[2] - xplot[1]) < 20) {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                              niceFacing = TRUE, adj = c(0, 0.5))
            } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                              niceFacing = TRUE, adj = c(0.5, 0))
            }
      }, bg.border = NA)
      
      dev.off()
      
      circos.clear()
      
      dev.off()
      
      
})      



################# re exports ########################################

starch<- c("Manioc (cassava) starch", "Starch; manioc (cassava)")
dried<- c("Manioc (cassava), fresh or dried", "Manioc (cassava)", 
          "Vegetable roots and tubers; manioc (cassava), with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets")
expor_dried<- filter(cfiles, Commodity %in% dried) %>% 
      filter(., Trade.Flow=="Re-Export") %>% filter(., !Reporter %in% placeout) %>% 
      filter(., !Partner %in% placeout)



# expor_starch$Year<- as.numeric(expor_starch$Year)
p<- sort(unique(cfiles$Year))
p1<- c(1997, 1998, 1999, 2000, 2001)
p2<- c(2002, 2003, 2004, 2005, 2006)
p3<- c(2007, 2008, 2009, 2010, 2011)
p4<- c(2012, 2013, 2014, 2015, 2016)

periodList <- c('1997_2006', '2002_2006','2007_2011', '2012_2016' )


# periodo1
Xstar1<- expor_dried %>% filter(., Year %in% p1) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar1<- Xstar1[c("Reporter","Partner" ,"Year","Qty")] %>% spread("Year", "Qty")

# periodo2
Xstar2<- expor_dried %>% filter(., Year %in% p2) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar2<- Xstar2[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")

# periodo3
Xstar3<- expor_dried %>% filter(., Year %in% p3) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar3<- Xstar3[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")

# periodo4
Xstar4<- expor_dried %>% filter(., Year %in% p4) %>% select(.,Partner, Reporter,Year,Qty ) 
Xstar4<- Xstar4[c("Reporter","Partner" ,"Year","Qty")]%>% spread("Year", "Qty")


Xstar1$mean <- rowMeans(x=Xstar1[,3:ncol(Xstar1)], na.rm=TRUE)
Xstar2$mean <- rowMeans(x=Xstar2[,3:ncol(Xstar2)], na.rm=TRUE)
Xstar3$mean <- rowMeans(x=Xstar3[,3:ncol(Xstar3)], na.rm=TRUE)
Xstar4$mean <- rowMeans(x=Xstar4[,3:ncol(Xstar4)], na.rm=TRUE)


Xstar1 <- Xstar1[,c("Reporter", "Partner", "mean" )]
Xstar1$mean[is.na(Xstar1$mean)]<-0

Xstar2 <- Xstar2[,c("Reporter", "Partner", "mean" )]
Xstar2$mean[is.na(Xstar2$mean)]<-0

Xstar3 <- Xstar3[,c("Reporter", "Partner", "mean" )]
Xstar3$mean[is.na(Xstar3$mean)]<-0

Xstar4 <- Xstar4[,c("Reporter", "Partner", "mean" )]
Xstar4$mean[is.na(Xstar4$mean)]<-0

periodos<- list("1997_2006"=Xstar1, "2002_2006"=Xstar2,"2007_2011"=Xstar3, "2012_2016"=Xstar4 )



############################ circulos graficos ##################################

# p=4
lapply(1: length(periodos), function(p){
      
      
      require(dplyr)
      require(tidyr)
      require(circlize)
      cat(paste(">>>>>>>>>>>>> start with ", periodList[[p]], " Done!!!!\n ", sep = ""))
      # temp<- expor_starch %>% filter(Year==p[x]) %>% select(.,Partner, Reporter,Partner,Qty)
      # temp<- as.data.frame((periodos$p1))
      temp<- as.data.frame(periodos[[p]])
      #       temp$mean[which(temp$mean==0)] <- 1
      #       temp$mean<- log10(temp$mean)
      rownames(temp) <- 1:nrow(temp)
      
      auxCarlos<- expand.grid(unique(temp$Reporter), unique(temp$Partner))
      colnames(auxCarlos)[1]<-"Reporter"
      colnames(auxCarlos)[2]<- "Partner"
      auxCarlos2 <- temp[,1:2]
      
      require(sqldf)
      auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
      dim(auxCarlos2)
      auxCarlos2$mean <- 0
      p1wide <- rbind(temp, auxCarlos2)
      
      p1wide <- as.data.frame(p1wide)
      p1wide$Reporter <- as.character(p1wide$Reporter)
      p1wide$Partner <- as.character(p1wide$Partner)
      p1wide$mean <- as.numeric(p1wide$mean)
      
      treal2<- p1wide %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- p1wide %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      treal4<- p1wide %>% filter(Reporter %in% Poli_Mela_Micro) %>% mutate(., zone="Melanesia")
      treal5<- p1wide %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- p1wide %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      treal7<- p1wide %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal8<- p1wide %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal9<- p1wide %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal1<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal10<- p1wide %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal11<- p1wide %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal12<- p1wide %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal13<- p1wide %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- p1wide %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal15<- p1wide %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      treal16<- p1wide %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      treal17<- p1wide %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal18<- p1wide %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      treal19<- p1wide %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")
      
      
      r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal19)
      
      rjoin<- left_join(r, p1wide, by = c("Reporter", "Partner", "mean"))
      
      #   tempcolor<- unique(rjoin$zone)
      colorstemp<- c("##f1eef6", "#d0d1e6","#a6bddb", "#74a9cf", "#3690c0","#0570b0", "#034e7b" )
      rjoin$color<- NA
      rjoin$color[rjoin$zone=="caribbean"]<-  "#40004b"
      rjoin$color[rjoin$zone=="Australia_and_New_Zealand"]<-   "#f0027f"
      rjoin$color[rjoin$zone=="Poli_Mela_Micro"]<-    "#33a02c"
      rjoin$color[rjoin$zone=="Eastern_Africa"]<- "#74a9cf"
      rjoin$color[rjoin$zone=="Middle_Africa"]<- "#3690c0"
      rjoin$color[rjoin$zone=="Northern_Africa"]<- "#ff7f00"
      rjoin$color[rjoin$zone=="Southern_Africa"]<-  "#034e7b"
      rjoin$color[rjoin$zone=="Western_Africa"]<- "#00441b"
      rjoin$color[rjoin$zone=="Eastern_Asia"]<- "#cab2d6"
      rjoin$color[rjoin$zone=="South_Eastern_Asia"]<- "#081d58"
      rjoin$color[rjoin$zone=="Southern_Asia"]<- "#543005"
      rjoin$color[rjoin$zone=="Western_Asia"]<- "#1f78b4"
      rjoin$color[rjoin$zone=="South_America"]<- "#b15928"
      rjoin$color[rjoin$zone=="Northern_America"]<- "#e31a1c"
      rjoin$color[rjoin$zone=="Central_America"]<- "#33a02c"
      rjoin$color[rjoin$zone=="Eastern_Europe"]<- "#bc80bd"
      rjoin$color[rjoin$zone=="Northern_Europe"]<- "#08306b"
      rjoin$color[rjoin$zone=="Southern_Europe"]<- "#e7298a"
      rjoin$color[rjoin$zone=="Western_Europe"]<- "#cab2d6"
      
      
      q2<- quantile(temp$mean,probs = 0)
      #       temp<- temp %>% filter(Value>=q2)
      
      color<- rjoin %>% filter(., mean>q2) %>% select("Reporter","Partner" , "color")  
      colorsss<- unique(rjoin$color)
      
      rjoin$color<- as.factor(rjoin$color)
      rjoin<- rjoin %>% select("Reporter","Partner" , "mean","color")
      rdata<- rjoin %>% select("Reporter","Partner" , "mean")
      
      # grafico codes
      circos.clear()
      circos.par(start.degree = 10, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      par(mar = rep(0, 4))
      
      
      png(paste(pic,"/",periodList[[p]],'_REExpors_DriedCassava.png',sep = ""), width=10, height=10, units='in', res=300)
      
      chordDiagram(x = rdata[which(rdata$mean>q2),],col=color$color,  transparency = 0.25, #col=color$color #[rdata$mean>0.7,]
                   directional = 1,
                   direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                   annotationTrack = "grids",  preAllocateTracks = list(track.height = 0.1),
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            xplot = get.cell.meta.data("xplot")
            ylim = get.cell.meta.data("ylim")
            sector.name = get.cell.meta.data("sector.index")
            if(abs(xplot[2] - xplot[1]) < 20) {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                              niceFacing = TRUE, adj = c(0, 0.5))
            } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                              niceFacing = TRUE, adj = c(0.5, 0))
            }
      }, bg.border = NA)
      
      dev.off()
      
      circos.clear()
      
      dev.off()
      
      
})      




      
     



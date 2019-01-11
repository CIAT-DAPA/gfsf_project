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

cfiles$Reporter<- plyr::revalue(cfiles$Reporter, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                           "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                           "CÃ´te d'Ivoire"="Ivory Coast",
                                           "China, Hong Kong SAR"='Hong Kong',
                                           "China, Macao SAR"='Macao',
                                           "China, mainland"='China',
                                           "China, Taiwan Province of"='Taiwan',
                                           "RÃ©union"='Reunion',
                                           "Sudan (former)"="Sudan former",
                                           "Netherlands Antilles (former)"="Netherlands Antilles",
                                           "Bolivia (Plurinational State of)"="Bolivia",
                                           "Venezuela (Bolivarian Republic of)"="Venezuela",
                                           "Iran (Islamic Republic of)"="Iran",
                                           "Falkland Islands (Malvinas)"="Falkland Islands" ))

cfiles$Partner<- plyr::revalue(cfiles$Partner, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                                             "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                                             "CÃ´te d'Ivoire"="Ivory Coast",
                                                             "China, Hong Kong SAR"='Hong Kong',
                                                             "China, Macao SAR"='Macao',
                                                             "China, mainland"='China',
                                                             "China, Taiwan Province of"='Taiwan',
                                                             "RÃ©union"='Reunion',
                                                             "Sudan (former)"="Sudan former",
                                                             "Netherlands Antilles (former)"="Netherlands Antilles",
                                                             "Bolivia (Plurinational State of)"="Bolivia",
                                                             "Venezuela (Bolivarian Republic of)"="Venezuela",
                                                             "Iran (Islamic Republic of)"="Iran",
                                                             "Falkland Islands (Malvinas)"="Falkland Islands" ))

colnames(cfiles)[6]<- "Qty"
colnames(cfiles)[7]<- "Val"

####regions
caribbean<- c("Cuba", "Dominican Republic", "Haiti", "Jamaica", "Dominican Rep.","N. Mariana Isds",
              "Aruba","Saint Lucia","Dominica", "Saint Vincent and the Grenadines", "Puerto Rico",
              "Anguilla", "Antigua and Barbuda", "Saint Kitts and Nevis", "Montserrat",
              "Martinique", "Barbados", "Trinidad and Tobago" )

Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico",
                     "Nicaragua","Panama","El Salvador" )

Australia_and_New_Zealand<- c( "New Zealand", "Australia")

Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")

Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar",
                   "Mozambique","Malawi","Rwanda","Somalia", "Tanzania",
                   "Uganda","Zambia","Zimbabwe", "Mauritius", "Seychelles")

Eastern_Asia<- c("China","Japan","South Korea","Mongolia","North Korea", "Taiwan")

Eastern_Europe<- c("Bulgaria","Belarus","Czech Republic","Hungary","Moldova","Other Balkans","Poland",
                   "Romania","Russia","Slovakia","Ukraine", "Russian Federation", "Latvia",
                   "Macedonia", "Kosovo", "Poland", "Serbia", "Romania")

Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia", "Serbia and Montenegro", "Andorra",
                    "France","Malta", "San Marino")


Melanesia<- c("Fiji", "Papua New Guinea","Solomon Islands","Vanuatu")

Middle_Africa<- c("Angola","Central African Rep.","Cameroon","DRC","Congo","Gabon","Equatorial Guinea","Chad",
                  "São Tomé and Príncipe")

Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Sudan","Tunisia", "South Sudan")

Northern_America<-c("Canada","Greenland","USA")

Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","UK", "Estonia", 
                    "Sweden")

South_America<- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guyanas","Peru",
                  "Paraguay","Uruguay","Venezuela","Suriname")

South_Eastern_Asia<- c("Indonesia","Cambodia","Laos","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "East Timor","Vietnam", "Singapore", "Brunei Darussalam")

Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")

Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Sri Lanka","Maldives", "Sri Lanka",
                  "Nepal","Other Indian Ocean","Pakistan")


Western_Africa<- c("Benin","Burkina Faso","Ivory Coast","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia",
                   "Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leon","Togo", "Saint Helena", 
                   "Sao Tome and Principe")

Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", 
                "Palestine", "Rest of Arabia","Saudi Arabia","Iran","Bahrain","Kuwait","Oman", "Qatar","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates", "Georgia", "South Ossetia",
                "Lebanon")

Western_Europe<- c("Austria","Belgium-Luxembourg","Switzerland","Germany","France","Netherlands","Other Atlantic")
#----------------------------------------------------------------#
starch<- c("Manioc (cassava) starch", "Starch; manioc (cassava)")
expor_starch<- filter(cfiles, Commodity %in% starch) %>% 
      filter(., Trade.Flow=="Export")
# expor_starch$Year<- as.numeric(expor_starch$Year)
p<- unique(cfiles$Year)
############################ circulos graficos ##################################
x=1
foreach(x=1:length(p)) %dopar%{ 
      require(dplyr)
      require(tidyr)
      require(circlize)
      #       cat(paste(">>>>>>>>>>>>> start with ", p[x], " Done!!!!\n ", sep = ""))
      temp<- expor_starch %>% filter(Year==p[x]) %>% select(.,Partner, Reporter,Partner,Qty)
      #     
      
      
      auxCarlos<- expand.grid(unique(temp$Reporter), unique(temp$Partner))
      colnames(auxCarlos)[1]<-"Reporter"
      colnames(auxCarlos)[2]<- "Partner"
      auxCarlos2 <- temp[,1:2]
      
      require(sqldf)
      auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
      dim(auxCarlos2)
      auxCarlos2$Qty <- 0
      p1wide <- rbind(temp, auxCarlos2)
      
      p1wide <- as.data.frame(p1wide)
      p1wide$Reporter <- as.character(p1wide$Reporter)
      p1wide$Partner <- as.character(p1wide$Partner)
      p1wide$Qty <- as.numeric(p1wide$Qty)
      
      
      treal1<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
       # treal2<- p1wide %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- p1wide %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      # treal4<- p1wide %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      # treal5<- p1wide %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- p1wide %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal7<- p1wide %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      # treal8<- p1wide %>% filter(Reporter %in% Melanesia) %>% mutate(., zone="Melanesia")
      # treal9<- p1wide %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      # treal10<- p1wide %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal11<- p1wide %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal12<- p1wide %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal13<- p1wide %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- p1wide %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal15<- p1wide %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal16<- p1wide %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal17<- p1wide %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal18<- p1wide %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      # treal19<- p1wide %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal20<- p1wide %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")
      

      r<- rbind(treal1,treal3,treal6,treal7,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal20)
      
      rjoin<- left_join(r, p1wide, by = c("Reporter", "Partner", "Qty"))
      
      #   tempcolor<- unique(rjoin$zone)
      colorstemp<- c("##f1eef6", "#d0d1e6","#a6bddb", "#74a9cf", "#3690c0","#0570b0", "#034e7b" )
      rjoin$color<- NA
      rjoin$color[rjoin$zone=="Eastern_Asia"]<-  "#40004b"
      rjoin$color[rjoin$zone=="Central_America"]<-   "#f0027f"
      rjoin$color[rjoin$zone=="Eastern_Europe"]<-    "#33a02c"
      rjoin$color[rjoin$zone=="Northern_America"]<- "#74a9cf"
      rjoin$color[rjoin$zone=="Northern_Europe"]<- "#3690c0"
      rjoin$color[rjoin$zone=="South_America"]<- "#ff7f00"
      rjoin$color[rjoin$zone=="South_Eastern_Asia"]<-  "#034e7b"
      rjoin$color[rjoin$zone=="Southern_Africa"]<- "#00441b"
      rjoin$color[rjoin$zone=="Southern_Asia"]<- "#cab2d6"
      rjoin$color[rjoin$zone=="Western_Africa"]<- "#fb9a99"
      rjoin$color[rjoin$zone=="Southern_Europe"]<- "#543005"
      rjoin$color[rjoin$zone=="Western_Europe"]<- "#b15928"
      
      q2<- quantile(temp$Qty,probs = 0.05)
      #       temp<- temp %>% filter(Value>=q2)
      
      color<- rjoin %>% filter(., Qty>=q2) %>% select("Reporter","Partner" , "color")  
      colorsss<- unique(rjoin$color)
      
      rjoin$color<- as.factor(rjoin$color)
      rjoin<- rjoin %>% select("Reporter","Partner" , "Qty","color")
      rdata<- rjoin %>% select("Reporter","Partner" , "Qty")
      
      # grafico codes
      circos.clear()
      circos.par(start.degree = 10, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      par(mar = rep(0, 4))
      
      
      png(paste("./pic//",p[x],'_Expors_StarchCassava.png',sep = ""), width=10, height=10, units='in', res=300)
      
      chordDiagram(x = rdata[which(rdata$Qty>=q2),],col=color$color,  transparency = 0.25, #col=color$color #[rdata$mean>0.7,]
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
      
      
      dev.off()
      circos.clear()
      
      
}

stopCluster(cl)

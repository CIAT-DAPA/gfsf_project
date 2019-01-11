### Codigo para descargar datos de COMTRADE  TEST
### Carlos Eduardo Gonzalez


# directorio general 
# setwd("/mnt/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")

# R options
options(warn = -1)
options(scipen = 999)

# Extracting codes and descriptions of reporters from the UN Comtrade API
suppressMessages(library(rjson))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(parallel))
suppressMessages(library(devtools))
suppressMessages(library(foreach))
suppressMessages(library(parallelsugar))
suppressMessages(library(doParallel))


##### function to extract information COMTRADE---------
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"


# -------------------------------------------------------------------------- #
# A user-defined function to extract data from the UN Comtrade API
# -------------------------------------------------------------------------- #

 

get.Comtrade <- function(url="http://comtrade.un.org/api/get?" 
                                                      ,maxrec=50000 
                                                      ,type="C" 
                                                     ,freq="A" 
                                                      ,px="HS" 
                                                      ,ps="now" 
                                                      ,r 
                                                      ,p 
                                                      ,rg="all" 
                                                      ,cc="TOTAL" 
                                                      ,fmt="json" 
 ) 
 {
      string<- paste(url 
                     ,"max=",maxrec,"&" #maximum no. of records returned
                     ,"type=",type,"&" #type of trade (c=commodities)
                     ,"freq=",freq,"&" #frequency 
                     ,"px=",px,"&" #classification 
                     ,"ps=",ps,"&" #time period
                     ,"r=",r,"&" #reporting area
                     ,"p=",p,"&" #partner country 
                     ,"rg=",rg,"&" #trade flow 
                     ,"cc=",cc,"&" #classification code
                     ,"fmt=",fmt        #Format 
                     ,sep = ""
                     ) 
      
      
      if(fmt == "csv") {
            raw.data<- read.csv(string,header=TRUE)
            return(list(validation=NULL, data=raw.data))
      } else {
            if(fmt == "json" ) { 
                  raw.data<- fromJSON(file=string) 
                  data<- raw.data$dataset 
                  validation<- unlist(raw.data$validation, recursive=TRUE)
                  ndata<- NULL 
                  if(length(data)> 0) { 
                        var.names<- names(data[[1]])
                        data<- as.data.frame(t( sapply(data,rbind)))
                        ndata<- NULL 
                        for(i in 1:ncol(data)){ 
                              data[sapply(data[,i],is.null),i]<- NA 
                              ndata<- cbind(ndata, unlist(data[,i])) 
                              } 
                        ndata<- as.data.frame(ndata)
                        colnames(ndata)<- var.names
                        } 
                              
                  return(list(validation=validation,data =ndata))
            }
      }
}

#### filters and creating reports and partners----------
#reporters
reporters2 <- rjson::fromJSON(file = string)
reporters2 <- as.data.frame(t(sapply(reporters2$results,rbind)))
reporters2 <- reporters2[-1,] # para eliminar la primer fila

reporters <- data.frame(Code = unlist(reporters2$V1), Country = unlist(reporters2$V2)); rm(reporters2)
reporters$Code <- as.numeric(as.character(reporters$Code))
reporters$Country <- as.character(reporters$Country)

#partners
partners2<- rjson::fromJSON(file=string)
partners2 <- as.data.frame(t(sapply(partners2$results,rbind)))
partners2 <- partners2[-1,] # para eliminar la primer fila all to all

partners <- data.frame(Code = unlist(partners2$V1), Country = unlist(partners2$V2)); rm(partners2)
partners$Code <- as.numeric(as.character(partners$Code))
partners$Country <- as.character(partners$Country)


### Complete matrix all possible combinations
all_comb <- as.data.frame(expand.grid(Reporter = reporters$Country, Partner = reporters$Country))
all <- data.frame(Reporter = unlist(all_comb$Reporter), Partner = unlist(all_comb$Partner)); rm(all_comb)
all$Reporter <- as.character(all$Reporter)
all$Partner <- as.character(all$Partner)

## lista de regiones-----------

noZonas<-c("American Samoa","Anguilla","Cayman Isds","Heard Island and McDonald Islands",
           "Holy See (Vatican City State)","Marshall Isds","Mayotte","N. Mariana Isds","Nauru","Niue" ,"Norfolk Isds" ,
           "North America and Central America, nes","Northern Africa, nes","Oceania, nes","Other Europe, nes",
           "Other Africa, nes","Ryukyu Isd", "Sabah","Saint Helena","Saint Kitts and Nevis" ,"Saint Kitts, Nevis and Anguilla",
           "Saint Maarten" , "Saint Pierre and Miquelon","Saint Vincent and the Grenadines",
           "San Marino","Sao Tome and Principe","Special Categories","US Misc. Pacific Isds","US Virgin Isds",
           "Vanuatu","Wallis and Futuna Isds","FS Micronesia","Fmr Panama, excl.Canal Zone","Fmr Panama-Canal-Zone",
           "French Polynesia" ,"FS Micronesia", "Guadeloupe","LAIA, nes", "Pitcairn", "USA (before 1981)",
           "Western Asia, nes" ,"Western Sahara","Tuvalu","Fmr Rhodesia Nyas","Fmr Pacific Isds", "Fmr Zanzibar and Pemba Isd",
           "Greenland", "Kiribati", "Mauritius", "Other Asia, nes",  "Rest of America, nes", "Areas, nes", "Antarctica",
           "Bermuda", "Bonaire", "Bouvet Island","Br. Indian Ocean Terr." , "Br. Antarctic Terr.", "Bunkers","CACM, nes", "Christmas Isds",
           "Cocos Isds", "Cook Isds", "Eastern Europe, nes", "Europe EFTA, nes", "Faeroe Isds", "Falkland Isds (Malvinas)",
           "Fr. South Antarctic Terr.", "Maldives", "Martinique", "Neutral Zone", "Saint Lucia", "Sikkim", "Solomon Isds", 
           "South Georgia and the South Sandwich Islands", "Tokelau", "Turks and Caicos Isds", "United States Minor Outlying Islands", 
           "World") #"Antigua and Barbuda","Caribbean, nes",

ZonasLAC<- c("Venezuela","Suriname","Panama","Mexico","Jamaica","Guatemala","Cuba","Chile",
             "Bolivia (Plurinational State of)","Belize","Argentina", "Aruba", "Nicaragua",
             "El Salvador","Ecuador","Colombia","Costa Rica" ,"Brazil", "Paraguay","Guyana","Haiti",
             "Peru","Honduras", "Uruguay", "Dominican Rep.", "French Guiana", "Trinidad and Tobago")



allComReg<- filter(all, !Partner %in% noZonas) %>% filter(., !Reporter %in% noZonas)
row.names(allComReg)<- 1:nrow(allComReg)


# obtener los codigos por paise que reporta
aux <- inner_join(x = allComReg, y = reporters, by = c('Reporter' = 'Country'))
allComReg$Reporter_code <- aux[,ncol(aux)]
# obtener los codigos por pais socio
aux <- inner_join(x = allComReg, y = partners, by = c('Partner' = 'Country'))
allComReg$Partner_code <- aux[,ncol(aux)]; rm(aux)


allComReg$com<- 1: nrow(allComReg)


##############% running --------------------

r1<- c(1:nrow(all))
r1<- c(1:10)
require(dplyr)
require(tidyr)
cores<- detectCores()
cl<- makeCluster(cores[1]-4)
registerDoParallel(cl)
# x=41430


vv<- foreach(x=41430:41500) %dopar% { #nrow(all)
#       cat(paste0(">>>> Procesed regions ",r1[[x]],"\n"))    
     lista<- list()
 
      try(tmp <- get.Comtrade(r = allComReg$Reporter_code[x], 
                                    p = "all", 
                                    cc = "110814", 
                                    ps = "2017", 
                                    px = "HS", 
                                    rg = "all", 
                                    fmt = "csv"))
      if(exists("tmp")){
                        s<- tmp$data
                        s$yr<- "2017"
                        s$re<- allComReg$Reporter[x]
                        s$pa<- allComReg$Partner[x]
                        df<- rbind(s)
      if(dim(df)!=is.null(df)){
             df<-df
             
       }else{
             df<-NULL
       }
      }else{
            df<-cat(paste("este hp combinacion no tienen ni mierda", r1[x], sep = ""))
                        }
       
#              df<-cat(paste("este hp combinacion no tienen ni mierda", r1[x], sep = ""))
#       }
      
      lista[[x]]<- df
      
#                   df$rr<- all$Reporter[x]
#                   df$pp<- all$Partner[x]
#                   df$com<- paste(all$com[x])
#                   return(df)
#                   df<- as.data.frame(do.call(rbind, s))
      
   
}

df<- do.call(rbind, vv)


stopCluster(cl)     



co<- unique(allComReg$Reporter_code)

###Camino dos
codes<- 1:100
L<- list()
vv<- foreach(x=1:length(codes)) %dopar% { 
      
      try(q1a <- get.Comtrade(r="1,2,3,4", 
                              p="all", 
                              ps="2017,2015,2010",
                              fmt="csv", 
                              cc="110814"))
      dq1a <- as.data.frame(do.call(rbind, q1a))
      
      L[[x]]<- dq1a
      # write.csv(dq1a,paste("./temp/",codes[x],"prueba.csv", sep=""))

}
      


try(q1a <- get.Comtrade(r="5,6,458,32", 
                        p="all", 
                        ps="2017", 
                        fmt="csv", cc="110814"))

  
dq1a <- as.data.frame(do.call(rbind, q1a))


append=do.call(rbind, list(dq1a, dq1b, dq2a, dq2b, dq3a,dq3b))


cfiles<- do.call(rbind, vv) # apilar las combinaciones 
cfiles<- cfiles[!(cfiles$Classification=="No data matches your query or your query is too complex. Request JSON or XML format for more information."),]


saveRDS(cfiles, file = paste("./", "dataComtradeRTB_Cereals.rds", sep=""))


########################################### networks analysis ####################################


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
suppressMessages(library(networkD3))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))




## Directories
ric<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/RicePaper/"

## Processing
rice<- cfiles %>% filter(., Commodity.Code==1006) %>% 
      select(Year,Trade.Flow,Reporter,Trade.Value..US..,Partner, Alt.Qty.Unit) %>%
      filter(., Trade.Flow!="Re-Export")
colnames(rice)[3]<- "Reporter.Countries"
colnames(rice)[5]<- "Partner.Countries"
colnames(rice)[2]<- "flow"
colnames(rice)[4]<- "money"
colnames(rice)[6]<- "tons"

yr<- unique(rice$Year)
yr<- sort(yr)
f<- unique(rice$flow)
rr<- rice %>% split(rice$flow)


lapply(rr, function(r){
      for(y in 1:length(yr)){
            temp<- rice %>% filter(., flow=="Import")
            temp<- temp %>% filter(., Year==2014) %>% select(Reporter.Countries,Partner.Countries,tons)
            # temp<- rrr %>% filter(Year==yr[y])
            auxCarlos<- expand.grid(unique(temp$Reporter.Countries), unique(temp$Partner.Countries))
            colnames(auxCarlos)[1]<-"Reporter.Countries"
            colnames(auxCarlos)[2]<- "Partner.Countries"
            auxCarlos2 <- temp[,1:2]
            
            require(sqldf)
            auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
            dim(auxCarlos2)
            auxCarlos2$Value <- 0
            p1wide <- rbind(yc, auxCarlos2)
      }

})
###


##############################################
#                        circulos           ##      
##############################################
auxCarlos<- expand.grid(unique(yc$Reporter.Countries), unique(yc$Partner.Countries))
colnames(auxCarlos)[1]<-"Reporter.Countries"
colnames(auxCarlos)[2]<- "Partner.Countries"
auxCarlos2 <- yc[,1:2]


require(sqldf)
auxCarlos2 <- sqldf("select * from auxCarlos except select * from auxCarlos2")
dim(auxCarlos2)
auxCarlos2$Value <- 0
p1wide <- rbind(yc, auxCarlos2)


p1wide <- as.data.frame(p1wide)
p1wide$Reporter.Countries <- as.character(p1wide$Reporter.Countries)
p1wide$Partner.Countries <- as.character(p1wide$Partner.Countries)
p1wide$Value <- as.numeric(p1wide$Value)
# p1wide$Value[which(p1wide$Value==0)] <- 1
# p1wide$mean <- log(p1wide$mean, base=exp(1))
# p1wide$mean[which(p1wide$mean==0)] <- 0.5
# 



circos.clear()
circos.par(start.degree = 90, gap.degree = 2, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


png('p1wide.png', width=8, height=8, units='in', res=300)
chordDiagram(x = p1wide[p1wide$Value!=0,], transparency = 0.25,
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
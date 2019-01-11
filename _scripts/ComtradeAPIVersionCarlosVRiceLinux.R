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

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"


# -------------------------------------------------------------------------- #
# A user-defined function to extract data from the UN Comtrade API
# -------------------------------------------------------------------------- #

get.Comtrade <- function(url = "http://comtrade.un.org/api/get?", maxrec = 50000, type = "C", freq = "A",
                         px = "HS", ps = "now", r, p, rg = "all", cc = "TOTAL", fmt = "json"){
      string<- paste(url
                     ,"max=",maxrec,"&" # maximum no. of records returned
                     ,"type=",type,"&"  # type of trade (c=commodities)
                     ,"freq=",freq,"&"  # frequency
                     ,"px=",px,"&"      # classification
                     ,"ps=",ps,"&"      # time period
                     ,"r=",r,"&"        # reporting area
                     ,"p=",p,"&"        # partner country
                     ,"rg=",rg,"&"      # trade flow (All = "all", imports = 1, exports = 2, re-export = 3)
                     ,"cc=",cc,"&"      # classification code
                     ,"fmt=",fmt        # Format
                     ,sep = ""
      )
      
      if(fmt == "csv") {
            raw.data<- read.csv(string,header=TRUE)
            return(list(validation=NULL, data=raw.data))
      } else {
            if(fmt == "json" ) {
                  raw.data<- rjson::fromJSON(file = string)
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

# crear todos dataframe todos los paises
### Complete matrix all possible combinations
all_comb <- as.data.frame(expand.grid(Reporter = reporters$Country, Partner = reporters$Country))
all <- data.frame(Reporter = unlist(all_comb$Reporter), Partner = unlist(all_comb$Partner)); rm(all_comb)
all$Reporter <- as.character(all$Reporter)
all$Partner <- as.character(all$Partner)

## lista de regiones

noZonas<-c("American Samoa","Anguilla","Antigua and Barbuda","Caribbean, nes","Cayman Isds","Heard Island and McDonald Islands",
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
           "World")

ZonasLAC<- c("Venezuela","Suriname","Panama","Mexico","Jamaica","Guatemala","Cuba","Chile",
             "Bolivia (Plurinational State of)","Belize","Argentina", "Aruba", "Nicaragua",
             "El Salvador","Ecuador","Colombia","Costa Rica" ,"Brazil", "Paraguay","Guyana","Haiti",
             "Peru","Honduras", "Uruguay", "Dominican Rep.", "French Guiana", "Trinidad and Tobago")



allComReg<- filter(all, Reporter %in% ZonasLAC)%>% filter(., !Partner %in% noZonas)
row.names(allComReg)<- 1:nrow(allComReg)

# obtener los codigos por paise que reporta
aux <- inner_join(x = allComReg, y = reporters, by = c('Reporter' = 'Country'))
allComReg$Reporter_code <- aux[,ncol(aux)]
# obtener los codigos por pais socio
aux <- inner_join(x = allComReg, y = partners, by = c('Partner' = 'Country'))
allComReg$Partner_code <- aux[,ncol(aux)]; rm(aux)


# List of years
yearList <- 1980:2016

# lista de commodities solo para arroz
commList <- c(1006)
inList <- list(combinations = allComReg,
               commodities  = commList,
               years        = yearList)
saveRDS(inList, './inputList.rds')
inList <- readRDS("./inputList.rds")



# i= 728 # uru to Brazil
# j= 1
# k= 1 #15

rbindMat <- mclapply(1:nrow(inList$combinations), function(i){
      
      rbindMat <- lapply(1:length(inList$commodities), function(j){
            
            rbindMat <- lapply(1:length(inList$years), function(k){
                  
                  tryCatch(expr={
                        s <- get.Comtrade(r = inList$combinations$Reporter_code[i],
                                          p = inList$combinations$Partner_code[i],
                                          cc = paste(inList$commodities[j]),
                                          ps = paste(inList$years[k]),
                                          px = "HS", rg = 2, fmt = "csv")
                  },
                  
                  error=function(e){
                        cat(paste0("Downloading failed in combination, Reporter:", inList$combinations$Reporter[i],
                                   ", Partner:", inList$combinations$Partner[i], ", Commodity:", inList$commodities[j],
                                   " and Year:", inList$years[k], "\n"))
                        return("Done\n")
                  })
                  
                  if(exists("s")){
                        s <- as.data.frame(do.call(bind_rows, s))
                        s$rr <- inList$combinations$Reporter[i]
                        s$pp <- inList$combinations$Partner[i]
                        s$cc <- inList$commodities[j]
                        s$yy <- inList$years[k]
                        # s <- s[,c("Year","Period","Trade.Flow","Reporter.Code","Reporter","Partner.Code", "Partner","Commodity.Code","Commodity", "Qty.Unit","Alt.Qty.Unit","Netweight..kg.", "Trade.Value..US..")]
                        
                        return(s)
                  }
                  
            })
            
            rbindMat <- do.call(bind_rows, rbindMat)
            rbindMat<- rbindMat[!(rbindMat$Classification=="No data matches your query or your query is too complex. Request JSON or XML format for more information."),]

            return(rbindMat)

            })
        
             write.csv(x = rbindMat, file = (paste("./",inList$combinations$Reporter[[i]],"_",inList$combinations$Partner[[i]], ".csv", sep="")))

      
},mc.cores=10)

# rbindMat <- do.call(bind_rows, rbindMat)
# 
# 
# readRDS("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade/inputList.rds")

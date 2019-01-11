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
# suppressMessages(library(parallelsugar))

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
           "World", "Kazakhstan", "India, excl. Sikkim")

ZonasLAC<- c("Venezuela","Suriname","Panama","Mexico","Jamaica","Guatemala","Cuba","Chile",
             "Bolivia (Plurinational State of)","Belize","Argentina", "Aruba", "Nicaragua",
             "El Salvador","Ecuador","Colombia","Costa Rica" ,"Brazil", "Paraguay","Guyana","Haiti",
             "Peru","Honduras", "Uruguay", "Dominican Rep.", "French Guiana", "Trinidad and Tobago")



# allComReg<- filter(all, Reporter %in% ZonasLAC)%>% filter(., !Partner %in% noZonas)
# row.names(allComReg)<- 1:nrow(allComReg)

allComReg<- filter(all, !Partner %in% noZonas) %>% filter(., !Reporter %in% noZonas)
row.names(allComReg)<- 1:nrow(allComReg)

# obtener los codigos por paise que reporta
aux <- inner_join(x = allComReg, y = reporters, by = c('Reporter' = 'Country'))
allComReg$Reporter_code <- aux[,ncol(aux)]
# obtener los codigos por pais socio
aux <- inner_join(x = allComReg, y = partners, by = c('Partner' = 'Country'))
allComReg$Partner_code <- aux[,ncol(aux)]; rm(aux)
allComReg$com<- 1: nrow(allComReg)


# ##################################% 1 part of commodities %###############################
# # r1<- 1:nrow(allComReg)
# ##############% parallelsugar::mclapply
# r1<- 1:44100
# # x<- 100
# system.time(lapply(1:length(r1),function(x){
#       
#             try(s<- get.Comtrade(r = allComReg$Reporter_code[x], 
#                            p = allComReg$Partner_code[x], 
#                            cc = "1006,100630,100610,100620,100640,1001,1002,1003,1004,1005,1007,100590,100110,100190,1101,1105,1106", 
#                            ps = "all", 
#                            px = "HS", 
#                            rg = "all", 
#                            fmt = "csv"))
#                           
#        if(exists("s")){
#             
#       s<- s$data
#       s$rr<- allComReg$Reporter[x]
#       s$pp<- allComReg$Partner[x]
#       s$com<- paste(allComReg$com[x])
#       write.csv(s,file=paste("./",allComReg$Reporter[x],"_",allComReg$Partner[x],"_",r1[x],"__partA.csv", sep="")) 
#       
#         }
#       return
#       cat(paste("done combination ",r1[x],"\n", sep=""))
#     
# }))      
#             
# 
# part1<- list.files(path = ,pattern = "partA",all.files = T,full.names = T) 
# part1<- lapply(part1, read.csv, header=T)
# p1<- part1                
# 
# # # using sapply
# # cfile1<- which(sapply(p1, ncol)<=4)
# # p1<- p1[-cfile1];rm(cfile1)
# 
# cfile1<- do.call(rbind,p1)
# cfile1<- cfile1[!(cfile1$Classification=="No data matches your query or your query is too complex. Request JSON or XML format for more information."),]
# cfile1$X<- NULL

##################################% 2 part of commodities %###############################
# 
# system.time(lapply(1:length(r1),function(x){
#       
#       try(s<-get.Comtrade(r = allComReg$Reporter_code[x], 
#                           p = allComReg$Partner_code[x], 
#                           cc = "1107,1109,1901,1902,0701,0708,0714,0201,0202,0203,0204,0205,0206,0901,1201,1202", 
#                           ps = "all", 
#                           px = "HS", 
#                           rg = "all", 
#                           fmt = "csv"))
#       
#       s<- s$data
#       s$rr<- allComReg$Reporter[x]
#       s$pp<- allComReg$Partner[x]
#       s$com<- paste(allComReg$com[x])
#       write.csv(s,file=paste("./",allComReg$Reporter[x],"_",allComReg$Partner[x],"__2part.csv", sep="")) 
#       
#       cat(paste("done combination ", r1[x]),"\n", sep="")
#       
# }))      
# 
# 
# part2<-list.files(path = ,pattern = "_2part",all.files = T,full.names = T) 
# part2<-lapply(part2, read.csv, header=T)
# p2<- part2                
# 
# 
# # # using sapply
# # cfile2<- which(sapply(p2, ncol)<=4)
# # p2<-p2[-cfile2];rm(cfile2)
# 
# cfile2<- do.call(rbind,p2)
# cfile2<- cfile2[!(cfile2$Classification=="No data matches your query or your query is too complex. Request JSON or XML format for more information."),]
# cfile2$X<- NULL
# 
# # deleted files 
# rm(part2, part1)
# ### unir parte1 y parte2
# 
# cdata<- rbind(cfile1,cfile2)
# names(cdata)
# cdata<- cdata[c("Classification","Year","Aggregate.Level","Trade.Flow" , "Reporter.Code","Reporter",
#               "Partner.Code","Partner","Commodity.Code","Commodity","Qty.Unit","Alt.Qty.Unit" , "Trade.Value..US..","com" )]
# goods<- unique(cdata$Commodity)    
# saveRDS(cdata,file = "./datatotal.rds")

################################## other function to extract information from comtrade ######################

r1<- 1:10

Comtrade<- lapply(r1, function(i){
      
      Sys.sleep(2)
      tmp<- get.Comtrade(r = allComReg$Reporter_code[i], 
                         p = allComReg$Partner_code[i], 
                         cc = "1006,100630,100610,100620,100640,1001,1002,1003,1004,1005,1007,100590,100110,100190,1101,1105,1106", 
                         ps = "all", 
                         px = "HS", 
                         rg = "all", 
                         fmt = "csv")
      
      df<- as.data.frame(do.call(rbind, tmp))
      return(df)


})
      
cfiles<- setNames(Comtrade,paste0("combination",r1)) # nombrar las cada objeto dentro de una lista usando prefijo "countries"
# list2env(cfiles, envir=.GlobalEnv) # crea countries1, countries2 como objetos individuales
df<- do.call(rbind, cfiles)
write.csv(df,file=paste("./partA.csv", sep="")) 





# 
# 
# 
# ########################% parallel windows %####################################33
# mclapply2 <- function(...) {
#       ## Create a cluster
#       size.of.list <- length(list(...)[[1]])
#       cl <- makeCluster( min(size.of.list, 20) ) # 20 corresponde al número de nucleos
#       
#       ## Find out the names of the loaded packages 
#       loaded.package.names <- c(
#             ## Base packages
#             sessionInfo()$basePkgs,
#             ## Additional packages
#             names( sessionInfo()$otherPkgs ))
#       
#       tryCatch( {
#             
#             ## Copy over all of the objects within scope to
#             ## all clusters. 
#             this.env <- environment()
#             while( identical( this.env, globalenv() ) == FALSE ) {
#                   clusterExport(cl,
#                                 ls(all.names=TRUE, env=this.env),
#                                 envir=this.env)
#                   this.env <- parent.env(environment())
#             }
#             clusterExport(cl,
#                           ls(all.names=TRUE, env=globalenv()),
#                           envir=globalenv())
#             
#             ## Load the libraries on all the clusters
#             ## N.B. length(cl) returns the number of clusters
#             parLapply( cl, 1:length(cl), function(xx){
#                   lapply(loaded.package.names, function(yy) {
#                         require(yy , character.only=TRUE)})
#             })
#             
#             ## Run the lapply in parallel 
#             return( parLapply( cl, ) )
#       }, finally = {        
#             ## Stop the cluster
#             stopCluster(cl)
#       })
# }
# 

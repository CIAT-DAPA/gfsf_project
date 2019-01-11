### codigo para analisis de redes arroz 

g=gc;rm(list = ls())


#### libraries------
# R options
options(warn = -1)
options(scipen = 999)


suppressMessages(library(rjson))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(circlize))
suppressMessages(library(sna))
suppressMessages(library(statnet))
# suppressMessages(library(igraph))
suppressMessages(library(network))


### directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/files")
rtbFolder<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/RicePaper/files/") 

# write.csv(append,paste(rtbFolder,"appendDataComercioCOMTRADERice_13_17.csv", sep = ""))


#### File-------------------------
xx<- list.files(pattern = "appendDataComercioCOMTRADERice", full.names = T) 
xx<- lapply(xx, read.csv)
xx<- do.call(rbind,xx)
xx<- xx %>% dplyr::select(Year,Trade.Flow,Reporter,  Partner, Commodity, Alt.Qty.Unit, Trade.Value..US..)

#### Structure-----------
cc<-xx
str(cc)
cc$Year<- as.numeric(cc$Year)
cc$Trade.Flow<- as.character(cc$Trade.Flow)
cc$Reporter<- as.character(cc$Reporter)
cc$Partner<- as.character(cc$Partner)
cc$Commodity<- as.character(cc$Commodity)

sacar<- c("Br. Indian Ocean Terr.", "World", "So. African Customs Union", 
          "Free Zones", "Fr. South Antarctic Terr.", "Fr. South Antarctic Terr.", 
          "Antarctica", "Special Categories", "US Misc. Pacific Isds", "Br. Indian Ocean Terr.", "Neutral Zone",
         "Cocos Isds", "Guam", "Tokelau", "Norfolk Isds", "Pitcairn", "Western Sahara",
         "United States Minor Outlying Islands")
cc<- cc %>% dplyr::filter(., !Partner %in% sacar) %>% dplyr::filter(., !Reporter %in% sacar) 
#       
# cc<- cc %>% dplyr::filter(., Partner!="World") %>% dplyr::filter(., Reporter!="World") %>% 
#       dplyr::filter(. , Reporter!="So. African Customs Union") %>% 
#       dplyr::filter(., Partner!="So. African Customs Union")%>% dplyr::filter(., Partner!="Free Zones") %>%
#       dplyr::filter(., Reporter!="Free Zones") %>% dplyr::filter(., Reporter!="Fr. South Antarctic Terr.") %>%
#       dplyr::filter(., Partner!="Fr. South Antarctic Terr.") %>% dplyr::filter(.,Reporter!="Antarctica") %>%
#       dplyr::filter(., Partner!="Antarctica") %>% dplyr::filter(., Reporter!="Special Categories") %>%
#       dplyr::filter(., Partner!="Special Categories")%>% dplyr::filter(., Reporter!="US Misc. Pacific Isds") %>%
#       dplyr::filter(., Partner!="US Misc. Pacific Isds")

#### Regions---------------

##AMERICA
caribbean<- c("Cuba", "Dominican Rep.", "Haiti", "Jamaica", "Dominican Rep.","N. Mariana Isds",
              "Aruba","Saint Lucia","Dominica", "Saint Vincent and the Grenadines", "Puerto Rico",
              "Anguilla", "Antigua and Barbuda", "Saint Kitts and Nevis", "Montserrat",
              "Martinique", "Barbados", "Trinidad and Tobago", "Montserrat", "Grenada", 
              "Neth. Antilles", "Bahamas", "Bermuda", "Turks and Caicos Isds", "Bunkers" , 
              "Cayman Isds", "Br. Virgin Isds", "CuraÃ§ao", "Saint Maarten", "Bonaire","Saint BarthÃ©lemy" )
Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico",
                     "Nicaragua","Panama","El Salvador" )
Northern_America<-c("Canada","Greenland","USA", "Saint Pierre and Miquelon")
South_America<- c("Argentina","Bolivia (Plurinational State of)","Brazil","Chile","Colombia","Ecuador","Guyana","Peru",
                  "Paraguay","Uruguay","Venezuela","Suriname", "Falkland Isds (Malvinas)")


### ASIA
Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")
Eastern_Asia<- c("China","Japan","Rep. of Korea","Mongolia","North Korea", "Taiwan","China, Hong Kong SAR", 
                 "China, Macao SAR","China, mainland", "Dem. People's Rep. of Korea")
South_Eastern_Asia<- c("Indonesia","Cambodia","Lao People's Dem. Rep.","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "Timor-Leste","Viet Nam", "Singapore", "Brunei Darussalam")
Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Sri Lanka","Maldives", "Sri Lanka",
                  "Nepal","Other Indian Ocean","Pakistan")
Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", "Armenia",
                "State of Palestine", "Rest of Arabia","Saudi Arabia","Iran","Bahrain","Kuwait","Oman", "Bahrain", "Qatar","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates", "Georgia", "South Ossetia",
                "Lebanon", "Lebanon")



##OCENIA
Australia_and_New_Zealand<- c( "New Zealand", "Australia", "New Caledonia")

micro_poli_melanesia<- c("Fiji", "Papua New Guinea","Solomon Isds","Vanuatu", "Tonga","Palau", "Kiribati", "French Polynesia", 
                         "FS Micronesia", "Wallis and Futuna Isds", "Tuvalu", "Samoa", "Cook Isds", "Christmas Isds",
                         "American Samoa", "Marshall Isds", "Nauru", "Niue")


## EUROPA
Eastern_Europe<- c("Bulgaria","Belarus","Czechia","Hungary","Rep. of Moldova","Other Balkans","Poland",
                   "Russia","Slovakia","Ukraine", "Russian Federation", "Latvia",
                   "TFYR of Macedonia", "Kosovo", "Poland", "Serbia", "Romania", "Lithuania")
Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia", "Serbia and Montenegro", "Andorra",
                    "Malta", "San Marino", "Montenegro", "Bosnia Herzegovina", "Gibraltar")
Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","United Kingdom", "Estonia", 
                    "Sweden", "Faeroe Isds")
Western_Europe<- c("Austria","Belgium-Luxembourg","Switzerland","Germany","France","Belgium","Netherlands","Other Atlantic", "Luxembourg")

## AFRICA

Middle_Africa<- c("Angola","Central African Rep.","Cameroon","Dem. Rep. of the Congo","Congo","Gabon","Equatorial Guinea","Chad",
                  "São Tomé and Príncipe")
Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Sudan (former)","Tunisia", "South Sudan", "Sudan", "Fmr Sudan") #"Sudan,"
Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")
Western_Africa<- c("Benin","Burkina Faso","CÃ´te d'Ivoire","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia", "Sierra Leone",
                   "Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leon","Togo", "Saint Helena", 
                   "Sao Tome and Principe", "Cabo Verde")
Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar",
                   "Mozambique","Malawi","Rwanda","Somalia", "United Rep. of Tanzania","Comoros",
                   "Uganda","Zambia","Zimbabwe", "Mauritius", "Seychelles", "Mayotte")

######################################### SNA --------------------------------
cc<- cc[!is.na(cc$Alt.Qty.Unit),]
colnames(cc)[6]<-"Q"
colnames(cc)[7]<-"Val"

nesReports<- cc[grepl(pattern =" nes",x = cc$Reporter),]
nesReports<- unique(nesReports$Reporter)
nesPartner<- cc[grepl(pattern =" nes",x = cc$Partner),] 
nesPartner<- unique(nesPartner$Partner)
nesout<- c(nesPartner, nesReports)

cc<- cc %>% dplyr::filter(., !Reporter %in% nesout) %>% dplyr::filter(.,!Partner %in% nesout)

cc$Commodity<- revalue(cc$Commodity, c("Rice in the husk (paddy/rough)" ="Rice paddy Husk",
                                       "Rice in the husk (paddy or rough)" ="Rice paddy Husk",
                                       "Cereals; rice in the husk (paddy or rough)"="Rice paddy Husk",
                                       "Semi-milled/wholly milled rice, whether or not polished/glazed" ="Rice semi-milled",
                                       "Rice, semi-milled or wholly milled" ="Rice semi-milled",
                                       "Cereals; rice, semi-milled or wholly milled, whether or not polished or glazed"="Rice semi-milled",
                                       "Semi-milled/wholly milled rice, whether/not polished/glazed"="Rice semi-milled"))

#### rescale function ------------
rescale <- function(nchar,low,high) {
      min_d <- min(nchar)
      max_d <- max(nchar)
      rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
      rscl
}

##### proceso de shocks de rendimientos-----------
## cargando datos de los shocks de rendimientos
shockYields<- read.csv("./Categories_shocksLocal&ForeighFAO.csv")
shockYields$X<- NULL
colnames(shockYields)[2]<- "nodes"
shockYields$nodes<- as.character(shockYields$nodes)

shockYields$nodes<- plyr::revalue(shockYields$nodes, c("Venezuela (Bolivarian Republic of)"="Venezuela",
                                                       "Republic of Korea"="Rep. of Korea",
                                                       "Democratic Republic of the Congo"="Dem. Rep. of the Congo",
                                                       "Sudan (former)"="Fmr Sudan",
                                                       "United Republic of Tanzania"="United Rep. of Tanzania",
                                                       "Lao Democratic Republic"="Lao Democratic Republic",
                                                       "Dominican Republic"="Dominican Rep.",
                                                       "Democratic Republic of the Congo"="Dem. Rep. of the Congo",
                                                       "The former Yugoslav Republic of Macedonia"="TFYR of Macedonia",
                                                       "Côte d'Ivoire"="CÃ´te d'Ivoire", #CÃ´te d'Ivoire
                                                       "Central African Republic"="Central African Rep",
                                                       "Solomon Islands"="Solomon Isds",
                                                       "Iran (Islamic Republic of)"="Iran",
                                                       "United States of America"="USA",
#                                                        "Fmr Sudan"="Sudan",
                                                       "Democratic People's Republic of Korea"="Dem. People's Rep. of Korea"))


# typeRice<- unique(cc$Commodity)----------------
Xrice<- cc %>% dplyr::filter(., Trade.Flow=="Export") %>% dplyr::filter(., Year!=2017)
riceTpes<- Xrice  %>% split(Xrice$Commodity) 
periododsList<- sort(unique(Xrice$Year))

netWorksPeriod<- list()
netWorksShock<- list()
networkRice<- list()
attrRed<- list()
netData<- list()
i=2 #1=husk , 2=milled
# y=9 # periods

############################################# runinggggggggggggggg ##########################
netWorksTemp<- lapply(1: length(riceTpes),function(i){
      xdata<- riceTpes[[i]]
     
      
      for(y in 1:length(periododsList)){
            ## filtramos la los datos de tipo de commoditie por yr/reporter/partner/quantity
            xdataSub<- dplyr::filter(xdata, Year==periododsList[[y]]) %>% 
                  dplyr::select(Reporter, Partner, Q) %>% 
                  dplyr::filter(., Q!=0) 
            ##Filtramos por los valores menores al 1er quantile
            q<- quantile(xdataSub$Q,probs = 0.25)
            xdataSub<- dplyr::filter(xdataSub, Q>q)
            attrValue<- xdataSub
            attrValue$QL<- log10(attrValue$Q) # logaritmo de los niveles exportados
            
            
            ###### Filtrando solo datos de reportes and partner
            rRed<- xdataSub[c("Reporter","Partner")]

            ###### crear objeto tipo red
            netWorksPeriod[[y]] <- network::network(rRed,matrix.type="edgelist")

            ###### Agregar atributo valor del edge, agregamos log(quatity export)
            network::set.edge.attribute(netWorksPeriod[[y]], "Q", c(attrValue[,4]))
            
            ###### Creando objeto con el numero y nombre de los nodos belong red/yr
            n<- netWorksPeriod[[y]]$gal$n
            Nodes<- lapply(1:n, function(v){
                  netWorksPeriod[[y]]$val[[v]]$vertex.names
                  
            })
            ##### apilamiento de los nodos
            nodesList<- as.data.frame(do.call(rbind,Nodes))
            colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
            
            
            ###### Creación de objectos regiones correspondientes a la red filtrada
            ### America
            treal1<- nodesList %>% dplyr::filter(Nodes %in% caribbean) %>% dplyr::mutate(., zone="caribbean")
            treal2<- nodesList %>% dplyr::filter(Nodes %in% Central_America) %>% dplyr::mutate(., zone="Central_America")
            treal3<- nodesList %>% dplyr::filter(Nodes %in% South_America) %>% dplyr::mutate(., zone="South_America")
            treal4<- nodesList %>% dplyr::filter(Nodes %in% Northern_America) %>% dplyr::mutate(., zone="Northern_America")
            
            ##Asia
            treal5<- nodesList %>% dplyr::filter(Nodes %in% Eastern_Asia) %>% dplyr::mutate(., zone="Eastern_Asia")
            treal6<- nodesList %>% dplyr::filter(Nodes %in% South_Eastern_Asia) %>% dplyr::mutate(., zone="South_Eastern_Asia")
            treal7<- nodesList %>% dplyr::filter(Nodes %in% Southern_Asia) %>% dplyr::mutate(., zone="Southern_Asia")
            treal8<- nodesList %>% dplyr::filter(Nodes %in% Western_Asia) %>% dplyr::mutate(., zone="Western_Asia")
            treal9<- nodesList %>% dplyr::filter(Nodes %in% Central_Asia) %>% dplyr::mutate(., zone="Central_Asia")
            
            
            ##Africa
            treal10<- nodesList %>% dplyr::filter(Nodes %in% Eastern_Africa) %>% dplyr::mutate(., zone="Eastern_Africa")
            treal11<- nodesList %>% dplyr::filter(Nodes %in% Middle_Africa) %>% dplyr::mutate(., zone="Middle_Africa")
            treal12<- nodesList %>% dplyr::filter(Nodes %in% Northern_Africa) %>% dplyr::mutate(., zone="Northern_Africa")
            treal13<- nodesList %>% dplyr::filter(Nodes %in% Southern_Africa) %>% dplyr::mutate(., zone="Southern_Africa")
            treal14<- nodesList %>% dplyr::filter(Nodes %in% Western_Africa) %>% dplyr::mutate(., zone="Western_Africa")
            
            ##Oceania
            treal15<- nodesList %>% dplyr::filter(Nodes %in% Australia_and_New_Zealand) %>% dplyr::mutate(., zone="Australia_and_New_Zealand")
            treal16<- nodesList %>% dplyr::filter(Nodes %in% micro_poli_melanesia) %>% dplyr::mutate(., zone="micro_poli_melanesia")
            
            
            ##EUROPA
            treal17<- nodesList %>% dplyr::filter(Nodes %in% Eastern_Europe) %>% dplyr::mutate(., zone="Eastern_Europe")
            treal18<- nodesList %>% dplyr::filter(Nodes %in% Northern_Europe) %>% dplyr::mutate(., zone="Northern_Europe")
            treal19<- nodesList %>% dplyr::filter(Nodes %in% Southern_Europe) %>% dplyr::mutate(., zone="Southern_Europe")
            treal20<- nodesList %>% dplyr::filter(Nodes %in% Western_Europe) %>% dplyr::mutate(., zone="Western_Europe")
            
            ##### apilan los subsets por regiones
            r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,treal15,
                      treal16,treal17,treal18,treal19, treal20)
            r$nodes[duplicated(r$nodes)] # se eliminan nodos duplicados
            
            ##### join entre la lista de nodos y dataframe de las regiones
            rjoinRegion<- dplyr::left_join(nodesList, r, by = "nodes")
            
            ##### deleted files unnecessary  
            rm(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,treal15,
               treal16,treal17,treal18,treal19, treal20)
            
            
            ###### Agregated attribute to network file, region 
            network::set.vertex.attribute(netWorksPeriod[[y]], "region", c(rjoinRegion[,2]))
            
            
            ################################ calculo de indicadores de comercio CEPAL ##############################
            #### Total exports by Reporter
            SumExports<- xdataSub %>% dplyr::filter(., Reporter %in% as.vector(nodesList[,1])) %>% 
                  dplyr::group_by(Reporter) %>% 
                  dplyr::mutate(., total=sum(Q)) %>% dplyr::select(Reporter,total) 
            SumExports<- SumExports[!duplicated(SumExports),]
            SumExports$Year<- periododsList[[y]]
            totalExports<- sum(SumExports$total) #### total exports by total year
            
            ##### Calculated of rate of exports
            SumExports$RateExpor<- (SumExports$total/totalExports)*100
            colnames(SumExports)[1]<- "nodes"
            
            ##### Exporting file to csv format
             write.csv(SumExports,paste("./data/", periododsList[[y]], 
                                       "_ExportsTotal_",unique(riceTpes[[i]]$Commodity),
                                       ".csv", sep = ""))
            
             
             #### agregar % exportaciones como atributo a cada nodo
             SumExports<- SumExports[c("nodes","RateExpor")]
             nodesList$nodes<- as.character(nodesList$nodes)
             
             #### Join local 
             rjoin1<- dplyr::left_join(nodesList, SumExports, by = "nodes")
             rjoin1$RateExpor[is.na(rjoin1$RateExpor)]<- 0
             
             #### agregar atributo de rateExports
             network::set.vertex.attribute(netWorksPeriod[[y]], "RateExpor", c(rjoin1[,2]))
            
             
            ##### agregar variable de liderazgo en importaciones
            SumExports$Top<- NA
            SumExports$Top[SumExports$RateExpor>=1]<- 1
            SumExports$Top[is.na(SumExports$Top)]<- 0
            
            SumExports<- SumExports[c("nodes","Top")]
            
            ##### join local top
            rjoinT<- dplyr::left_join(nodesList, SumExports, by = "nodes")
            rjoinT$Top[is.na(rjoinT$Top)]<- 0
            
            #### agregar atributo de Ranking of exporters
            network::set.vertex.attribute(netWorksPeriod[[y]], "Top", c(rjoinT[,2]))
             
            
            ####################################################################################################
            ##### Calculando algunas metricas
            
            deg <- sna::degree(netWorksPeriod[[y]],gmode="graph")
            cls <-  sna::closeness(netWorksPeriod[[y]],gmode="graph") # la relacion tiene direccion 
            bet <-  sna::betweenness(netWorksPeriod[[y]],gmode="graph")
            
            detach(package:igraph)
            suppressMessages(library(statnet))
            CentraBet<-centralization(netWorksPeriod[[y]],betweenness)
            CentraDeg<- centralization(netWorksPeriod[[y]],degree)
            
            
            ### data igraph y generacion de metricas basicas 
            detach(package:statnet)
            suppressMessages(library(igraph))
            suppressMessages(library(intergraph))
            iDHHS <- asIgraph(netWorksPeriod[[y]])### convert network file to igraph
            idensi<- graph.density(iDHHS)
            cw <- cluster_walktrap(iDHHS)
            clusMember<- as.vector(membership(cw))
            mod<- modularity(cw)
            gden<- graph.density(iDHHS)
            meanDeg<- mean(degree(iDHHS))
            trans<- transitivity(iDHHS)
            transLocal<- transitivity(iDHHS, type = "local")
            assor<- assortativity.degree(iDHHS, directed = T)
            
#             detach(package:statnet)
#                         suppressMessages(library(statnet))
#                         suppressMessages(library(intergraph))
            
            ############## metricas basicas stacking ##########################
            dataSUM<- data.frame(nodes= nodesList, 
                                 degree= deg,
                                 density= idensi,
                                 between= bet, 
                                 closeness=cls,
                                 centraBetw= CentraBet,
                                 centraDeg= CentraDeg,
                                 ClusWalk= clusMember,
                                 modelaridad= mod,
                                 graphdens= gden,
                                 meanDegree= meanDeg,
                                 transitiv= trans,
                                 transitivLocal=transLocal,
                                 assorta= assor, 
                                 ties= netWorksPeriod[[y]]$gal$mnext,
                                 nodesTotal= netWorksPeriod[[y]]$gal$n,
                                 Year= periododsList[[y]],
                                 riceType= unique(riceTpes[[i]]$Commodity))
            
            ###### Export to cvs format
            write.csv(dataSUM,paste("./data/", periododsList[[y]], 
                                    "_StactNetworks_",unique(riceTpes[[i]]$Commodity),
                                    ".csv", sep = ""))
            
            
            ##### guardar datos en una lista
            netData[[y]]<- dataSUM
            

            ###########################  Aggregating shocks attributes to network objetc  #####################
            
            # Filtrar 
            nodesList$nodes<- as.character(nodesList$nodes)
            n<- nodesList[,1]
            YtempHWL<- shockYields %>% dplyr::filter(., year==periododsList[[y]]) %>% 
                  dplyr:: filter(.,nodes %in% n) %>% dplyr::select(nodes, cat_localHV)
            
            YtempHWW<- shockYields %>% dplyr::filter(., year==periododsList[[y]]) %>% 
                  dplyr:: filter(.,nodes %in% n) %>% dplyr::select(nodes, cat_f_hvW)
            
            rownames(YtempHWL)<- 1:nrow(YtempHWL)
            
            #### join local shocks local yields 
            rjoin2<- dplyr::left_join(nodesList, YtempHWL, by = "nodes")
            rjoin2$cat_localHV<- as.character(rjoin2$cat_localHV)
            rjoin2$cat_localHV[is.na(rjoin2$cat_localHV)]<- "NoProducers"
            
            network::set.vertex.attribute(netWorksPeriod[[y]], "cat_localHV", c(rjoin2[,2]))
            
            #join foreing shocks
            rjoin3<- dplyr::left_join(nodesList, YtempHWW, by = "nodes")
            rjoin3$cat_f_hvW<- as.character(rjoin3$cat_f_hvW)
            rjoin3$cat_f_hvW[is.na(rjoin3$cat_f_hvW)]<- "NoProducers"
            
            network::set.vertex.attribute(netWorksPeriod[[y]], "cat_f_hvW", c(rjoin3[,2]))
            
            

            #join dummies shocks
            dummies<- shockYields
            # require(dplyr)
            dummies<- dummies %>% dplyr::select(year, nodes,cat_localHV, cat_f_hvW)
            dummies$shock<- paste(dummies$cat_localHV, "+",dummies$cat_f_hvW)
            
            YtempShock<- dummies %>% dplyr::filter(., year==periododsList[[y]]) %>% 
                  dplyr:: filter(.,nodes %in% n) %>% dplyr::select(nodes, shock)
            
            rjoin4<- dplyr::left_join(nodesList, YtempShock, by = "nodes")
            rjoin4$shock<- as.character(rjoin4$shock)
            rjoin4$shock[is.na(rjoin4$shock)]<- "NoProducers"
            
            network::set.vertex.attribute(netWorksPeriod[[y]], "Shock", c(rjoin4[,2]))
            
            
            
            #####################################################################################
            detach(package:igraph)
            suppressMessages(library(statnet))
            suppressMessages(library(intergraph))
            scatter.smooth(netWorksPeriod[[y]] %v% 'RateExpor',
                          degree(netWorksPeriod[[y]],gmode='graph'),
                           xlab='Rate Export',
                           ylab='Degree')
            
#             ### tables 
#             table_local<- mixingmatrix(netWorksPeriod[[y]],'cat_localHV')
#             table_fore<- mixingmatrix(netWorksPeriod[[y]],'cat_f_hvW')            
#             table_shock<- mixingmatrix(netWorksPeriod[[y]],'Shock')

            
           
            
            #####################################################################################
            ### Eliminando  paises sin informacion de shocks de rendimiento
            test <- get.inducedSubgraph(netWorksPeriod[[y]],
                                        which(netWorksPeriod[[y]] %v% 
                                                    "cat_localHV"!="NoProducers"))
            
            ################################# modeling #########################
            suppressMessages(library(ergm))
            #### null model
            DSmod0 <- ergm(test ~ edges,
                           control=control.ergm(seed=40))

            #### model 1 ######
            DSmod1 <- ergm(test ~ edges +
                                 nodefactor('cat_localHV')+
                                    nodefactor('Top'), 
                           control=control.ergm(seed=40))

            
            ############################################## Dyadic Predictors

            #### model 2 ########
            DSmod2 <- ergm(test ~ edges +
                                 nodefactor('Top')+
                                  nodematch('cat_localHV'),
                           control=control.ergm(seed=40))

            #### model 3a #######
            DSmod3 <- ergm(test ~ edges +
                                  nodefactor('Top')+
                                  nodematch('cat_localHV', diff=TRUE),
                            control=control.ergm(seed=40))

            
            
            ### model 3b hypothesis of differential homophily.
            DSmod3a <- ergm(test ~ edges +
                                  nodefactor('Top')+
                                 nodemix('cat_localHV'), #, base=1
                           control=control.ergm(seed=40))

            ##### Creating functions to get coeficientes and results from models
            oddsratios <- function (mem) #based on Harris, 2014
            {  
                  or <- exp( x = mem$coef )
                  ste <- sqrt( diag( mem$covar ) ) 
                  lci <- exp( mem$coef-1.96*ste ) 
                  uci <- exp( mem$coef+1.96*ste )
                  ors <- rbind( round( lci,digits = 4 ),round( or,digits = 4),round( uci,digits = 4 ) ) 
                  ors <- t(ors)
                  colnames(ors) <- c( "Lower","OR","Upper" ) 
                  return(ors)
            }
            
            gof_pt.ego <- function(x) # x = ergm.gof object 
            {
                  m <- x$summary.deg
                  selm <- m[, "obs"] == 0 & m[, "min"] == 0 & m[, "max"] == 0 
                  degcount<- x$pobs.deg[!selm] >= x$bds.deg[1,!selm] & x$pobs.deg[!selm] <= x$bds.deg[2,!selm]
                  goftab <- rbind(c(sum(degcount)), c(length(degcount)), c(sum(degcount/length(degcount))))
                  colnames(goftab) <- c("degree")
                  return(goftab)
            }
            
            #### obtener parametros
            Bcoefstab <- rbind(
                  cbind(summary(DSmod0)$coef, oddsratios(DSmod0), model="NULO", RiceT=unique(riceTpes[[i]]$Commodity)),
                  cbind(summary(DSmod1)$coef, oddsratios(DSmod1),model="1",RiceT=unique(riceTpes[[i]]$Commodity)),
                  cbind(summary(DSmod2)$coef, oddsratios(DSmod2),model="2", RiceT=unique(riceTpes[[i]]$Commodity)),
                  cbind(summary(DSmod3)$coef, oddsratios(DSmod3),model="3", RiceT=unique(riceTpes[[i]]$Commodity)),
                  cbind(summary(DSmod3a)$coef, oddsratios(DSmod3a),model="3a", RiceT=unique(riceTpes[[i]]$Commodity))

            )
            Bcoefstab$Year<- periododsList[[y]]
            
            ##### exporting results to csv files
            write.csv(Bcoefstab,paste("./data/", periododsList[[y]], 
                                    "_ResultsModelNetworks_",unique(riceTpes[[i]]$Commodity),
                                    ".csv", sep = ""))
            
            
            ##########################    Graphics              ######################################## 
            detach(package:statnet)
            suppressMessages(library(igraph))
            suppressMessages(library(intergraph))
            iDHHS <- asIgraph(netWorksPeriod[[y]])
            
            #             E(iDHHS)$Q
            #             V(iDHHS)
            #             edges(iDHHS)
            #             vertex_attr(iDHHS, name = "color")
            V(iDHHS)$color<- FALSE
#             V(iDHHS)$region
#             V(iDHHS)$color
#             #### agregando coclor a los nodos
            V(iDHHS)[(V(iDHHS)$region=="Middle_Africa")]$color <- "green"
            V(iDHHS)[V(iDHHS)$region=="caribbean"]$color<- "blue"
            V(iDHHS)[V(iDHHS)$region=="South_America"]$color<- "blue"
            V(iDHHS)[V(iDHHS)$region=="Western_Asia"]$color<- "yellow"
            V(iDHHS)[V(iDHHS)$region=="Australia_and_New_Zealand"]$color<- "grey"
            V(iDHHS)[V(iDHHS)$region=="Western_Europe"]$color<- "red"
            V(iDHHS)[V(iDHHS)$region=="Eastern_Europe"]$color<- "red"
            V(iDHHS)[V(iDHHS)$region=="Central_America"]$color<- "blue"
            V(iDHHS)[V(iDHHS)$region=="Eastern_Africa"]$color<- "green"
            V(iDHHS)[V(iDHHS)$region=="Western_Africa"]$color<- "green"
            V(iDHHS)[V(iDHHS)$region=="Northern_America"]$color<- "purple"
            V(iDHHS)[V(iDHHS)$region=="Southern_Europe"]$color<- "red"
            V(iDHHS)[V(iDHHS)$region=="micro_poli_melanesia"]$color<- "black"
            V(iDHHS)[V(iDHHS)$region=="Northern_Europe"]$color<- "red"
            V(iDHHS)[V(iDHHS)$region=="Southern_Asia"]$color<- "blue"
            V(iDHHS)[V(iDHHS)$region=="Central_Asia"]$color<- "yellow"
            V(iDHHS)[V(iDHHS)$region=="Northern_Africa"]$color<- "green"
            V(iDHHS)[V(iDHHS)$region=="South_Eastern_Asia"]$color<- "yellow"
            V(iDHHS)[V(iDHHS)$region=="Eastern_Asia"]$color<- "yellow"
            V(iDHHS)[V(iDHHS)$region=="Southern_Africa"]$color<- "green"
            
            degi<- degree(graph = iDHHS,mode = "total")
            
            png(paste("./pic/",periododsList[[y]],unique(riceTpes[[i]]$Commodity),"_Network" ,".png", sep = ""), 
                width = 700, height = 500)
            op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
            V(iDHHS)$size=(rescale(degi,1,8))*2
            E(iDHHS)$arrow.size <- .2
            plot(iDHHS,vertex.label=NA, layout=layout.fruchterman.reingold,
                 edge.color="grey", edge.width=E(iDHHS)$Q/2)
            legend(x=-1.8, y=1,c("Africa", "ALC", "Asia", "Australia", "EU", "Norte\nAmerica", "Microne&Poline"), pch=21,
                   col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
                   title="Regions", pt.bg=c("green","blue","yellow", "grey", "red","purple", "black" )) # pt.bg=unique(V(iDHHS)$color)
            
            par(op)
            dev.off()
            
              
            cat(paste("proceso bien ", periododsList[[y]]," completed done!!!\n ", sep = ""))
#             detach(package:igraph)
#             suppressMessages(library(statnet))
            }
      networkRice[[i]]<- netWorksPeriod
      
      
} )


#### procesando resultados de las metricas basicas de las redes-----------------
cfilesDes<- list.files("./data/", pattern = "_StactNetworks_", full.names = T)
cfilesDes<- lapply(cfilesDes, read.csv)
cfilesDes<- do.call(rbind, cfilesDes)
cfilesDes$X<- NULL
cfilesDes$nodes<- as.character(cfilesDes$nodes)
cfilesDes$riceType<- as.character(cfilesDes$riceType)
LAC<- c(caribbean, South_America, Central_America)
cfilesDes<- cfilesDes %>%  mutate(., zone=ifelse((nodes %in% LAC),"LAC", "WORLD"))


write.csv(cfilesDes,paste("./data/TotalindicadoresBasicos.csv", sep = ""))


### ties and nodes---------------
tino<- cfilesDes %>% dplyr::select(.,Year, riceType, ties, nodesTotal, zone)
tino<- tino[!duplicated(tino),]
row.names(tino)<- 1:nrow(tino)
write.csv(tino,paste("./data/tino.csv", sep = ""))


##### apliar los resultados de los modelos----- 
mo<- list.files("./data/", pattern = "ResultsModelNetworks", full.names = T)
mo<- lapply(mo, read.csv)
mo<- do.call(rbind, mo)
str(mo)
mo$X<- as.character(mo$X)
mo$model<-as.character(mo$model)
mo$RiceT<- as.character(mo$RiceT)
write.csv(mo, paste("./data/", "ResumenModelos.csv", sep = ""))
rice<- unique(mo$RiceT)

################### Paddy #####################
paddy<-  mo[grepl(pattern = "Rice paddy Husk" , mo$RiceT),]


mod1<- paddy[grepl(pattern = "1", paddy$model),]
mod2<- paddy[grepl(pattern = "2", paddy$model),]
mod3<- paddy[grepl(pattern = "3", paddy$model),]


sceL<- list(A=mod1,B=mod2, C=mod3)
options(warn = -1)
options(scipen = 999)

#################### ensayis
motest<- mo[!is.na(mo$p.value),]
motest<- motest %>% dplyr::filter(., model!="NULO")

tempM<- motest %>% 
            dplyr::group_by(RiceT,Year, X) %>% 
            dplyr::mutate(., sig=ifelse(p.value<=0.1, "Si", "No")) %>%
            dplyr::select(X,Estimate, model, RiceT, Year, sig) 


tempM$X<- revalue(tempM$X, c("edges1"="edges",
                                            "edges2"="edges",
                                            "edges3"="edges",
                                            "edges4"="edges",
                                            "nodefactor.Top.1"= "node.factorTop",
                                            "nodefactor.Top.11"="node.factorTop",
                                            "nodefactor.Top.12"="node.factorTop",
                                            "nodefactor.Top.13"="node.factorTop"))

colnames(tempM)[1]<- "variables"
tempM<- as.data.frame(tempM)
tempM<- tempM %>% dplyr::filter(.,variables!="edges")
tempM$variables <- factor(tempM$variables, levels = c("node.factorTop" ,
                                                      "nodematch.cat_localHV",     
                                                      "nodefactor.cat_localHV.Negative",
                                                      "nodefactor.cat_localHV.Positive",
                                                      "nodematch.cat_localHV.Middle_Range",
                                                      "nodematch.cat_localHV.Negative",
                                                      "nodematch.cat_localHV.Positive" ,
                                                      "mix.cat_localHV.Middle_Range.Middle_Range",
                                                      "mix.cat_localHV.Negative.Middle_Range",
                                                      "mix.cat_localHV.Positive.Middle_Range" ,   
                                                      "mix.cat_localHV.Middle_Range.Negative" ,
                                                      "mix.cat_localHV.Negative.Negative" ,
                                                      "mix.cat_localHV.Positive.Negative",
                                                      "mix.cat_localHV.Middle_Range.Positive", 
                                                      "mix.cat_localHV.Negative.Positive"))       

modelA<- ggplot(tempM[which(tempM$sig=="Si"),], aes(x = model, y = Estimate, fill=variables)) +
      geom_boxplot()+ facet_grid(.~RiceT)+
      theme_grey() +
      theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
            text = element_text(size = 12, family = "Tahoma"),
            axis.title = element_text(face="bold"),
            axis.text.x=element_text(size = 11)) 




# cfilesDes$nodes<- as.character(cfilesDes$nodes)
# cfilesDes$riceType<- as.character(cfilesDes$riceType)
# LAC<- c(caribbean, South_America, Central_America)
# cfilesDes<- cfilesDes %>%  mutate(., zone=ifelse((nodes %in% LAC),"LAC", "WORLD"))
# 
# 
# write.csv(cfilesDes,paste("./data/TotalindicadoresBasicos.csv", sep = ""))






require(ggplot2)

# 
# png(paste("./pic/TiesAndNodesNetwork.png", sep = ""), 
#     width =500, height = 500)

# ggplot(tino, aes(x=nodesTotal, y=ties))+  #colour=unique(tino$Year))
#       geom_point(size=2, shape=15,alpha = 0.4,size=4)+ 
#       facet_grid(~riceType, margins = T)+      #, scales = "free"
# #       coord_equal()+
#       theme()+
#       theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#       theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14))+
#       theme(axis.text.y = element_text(hjust = 1, size = 14))+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#       theme(strip.text=element_text(size=8))+
#       geom_smooth(method = "lm", se = F)+ xlab("Número nodos\nPeriodo de 1992 a 2016")+
#       ylab("Número de conexiones")+ labs(caption="COMTRADE")
#       
# 
# dev.off()

### mas graficas
tino2<- cfilesDes[c("Year", "riceType" , "density", "modelaridad","centraBetw", "centraDeg","graphdens",
                     "meanDegree" , "transitiv" , "assorta" )]
tino2<- tino2[!duplicated(tino2),]
row.names(tino2)<- 1:nrow(tino2)
tino2<- tino2 %>% gather(Sce,Val, 3:ncol(tino2))


png(paste("./pic/TiesAndNodesNetwork2.png", sep = ""), 
    width = 800, height = 500)

# ggplot(tino2, aes(x=Year, y=Val, colour=Sce))+  #colour=unique(tino$Year))
#       geom_point(size=2, shape=11,alpha = 0.4,size=4)+ 
#       facet_grid(~riceType, margins = T, scales = "free")+      
#       #       coord_equal()+
#       theme(axis.text.x = element_text(angle = 0, hjust = 1))+
#       theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11))+
#       theme(axis.text.y = element_text(hjust = 1, size = 11))+
#       theme(strip.text.x = element_text(size = 11, face = "bold.italic"))+
#       theme(strip.text=element_text(size=8))+
#       geom_smooth(method = "lm", se = TRUE)
# 
# dev.off()




# 
# 
# 
# ############# grafica
# allrice<- read.csv("./data/riceAllFAO.csv")
# allrice$X<- NULL
# demandrice<- read.csv("./data/riceDemandFAO.csv")
# demandrice$X<- NULL
# # names(allrice)
# # names(demandrice)
# 
# demandrice<- demandrice %>% dplyr::select(Area,Year,Consumption)
# rjoinFAO<- dplyr::left_join(allrice, demandrice, by=c("Area","Year"))
# colnames(rjoinFAO)[4]<- "SupplyDomestic"

      
########################################################################################################
# 
# graphics.off()
# layout(matrix(c(1,2,3),2,2)) #,3,4, byrow = T),c(2,2,2,1),c(5,4,4)
# par(mar=c(0,1,1,0))
# layout.show(19)
# plot(netWorksPeriod[[1]], vertex.cex=rescale(deg,1,6), vertex.col=my_pal[rolecat],
#      mode='fruchtermanreingold',
#      edge.col="grey80",edge.lwd=netWorksPeriod[[2]] %e% "Q")
# 
# box()
# par(mar=c(0,2,1,0))
# layout.show(19)
# plot(netWorksPeriod[[2]], vertex.cex=rescale(deg,1,6), vertex.col=my_pal[rolecat],
#      mode='fruchtermanreingold',
#      edge.col="grey80",edge.lwd=netWorksPeriod[[2]] %e% "Q")
# 
# box()
########################################################################################################

#             detach(package:igraph)
#             suppressMessages(library(statnet))

#             ### grafica de la red ponderando las conexiones y los nodos
#             my_pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#b15928", 
#                         "#33a02c","#b15928", "#cab2d6", "#40004b", "#034e7b", "#40004b", "#3690c0", "#d0d1e6", "#b2df8a",
#                         "#543005","#fb9a99" )
#             
#             
#             png(paste("./pic/",periododsList[[y]],unique(riceTpes[[i]]$Commodity),"_Network" ,".png", sep = ""), 
#                 width = 800, height = 500)
#             
#             op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
#             rolecat <- as.factor(get.vertex.attribute(netWorksPeriod[[y]],"region"))
#             legRegion<- as.vector(unique(rolecat))
#             
#             plot(netWorksPeriod[[y]],vertex.cex=rescale(deg,1,6),
#                  vertex.col=my_pal[rolecat],usearrows=TRUE,mode='fruchtermanreingold', #[rolecat]
#                  edge.col="grey80",edge.lwd=netWorksPeriod[[y]] %e% "Q", 
#                  main=paste("Networks Analysis \nYear:", periododsList[[y]],", ",unique(riceTpes[[i]]$Commodity), sep ="" ))
#             legend("left", legend=c(unique(rjoinRegion$zone)),
#                    col=my_pal,pch=19,pt.cex=1.5,bty="n",  xjust = 1, yjust = 1,text.width = strwidth("1000000"),  title.adj = 0.5,
#                    title="Regions", ncol = 1, text.font = 1)
#             
#             par(op)
#             dev.off()






# lm_eqn = function(x, y, df){
#       m <- lm(y ~ x, df);
#       eq <- substitute(italic(y) == b %.% italic(x) + a,
#                        list(a = format(coef(m)[1], digits = 2), 
#                             b = format(coef(m)[2], digits = 2)))
#       as.character(as.expression(eq));                 
# }


# 
# 
# 
# # grafica network log(tones exports)
# 
# op <- par(mar = rep(0, 4))
# gplot(netWorksPeriod[[y]],gmode="graph",edge.lwd=netWorksPeriod[[y]] %e% 'Q',
#       edge.col="grey50",vertex.col="lightblue",
#       vertex.cex=1.0,vertex.sides=20, displaylabels = FALSE)
# par(op)
# 
# # grafica LAC
# lac<- c("South_America","caribbean","Central_America","South_America" )
# LAC <- get.inducedSubgraph(netWorksPeriod[[y]], which(netWorksPeriod[[y]] %v% 
#                                                         "region" =="South_America" )) #c("caribbean","Central_America","South_America")
# op <- par(mar = rep(0, 4))
# gplot(LAC,gmode="graph",edge.lwd=LAC %e% 'Q',
#       edge.col="grey50",vertex.col="lightblue",
#       vertex.cex=1.0,vertex.sides=20, displaylabels = TRUE)
# par(op)
# 
# # grafico filter cantidad 
# as.sociomatrix(netWorksPeriod[[y]])[1:6,1:6] #matrix de exploracion de relaciones tie
# list.edge.attributes(netWorksPeriod[[y]]) #determinar los atributos por ties
# 
# d.val <- as.sociomatrix(netWorksPeriod[[y]],attrname="Q")
# d.val[d.val < 5] <- 0 # podemos usar el quintil 50% 
# d.filt <- as.network(d.val, directed=FALSE,
#                      matrix.type="a",ignore.eval=FALSE,
#                      names.eval="Q")
# summary(d.filt,print.adj=FALSE)
# # op <- par(mar = rep(0, 4))
# # gplot(d.filt,gmode="graph",displaylabels=TRUE,edge.lwd=d.filt %e% 'Q',
# #       vertex.col="lightblue",vertex.cex=1.3,
# #       label.cex=0.4,label.pos=5,
# #       displayisolates=FALSE)
# # par(op)
# 
# op <- par(mar = rep(0, 4))
# d.val <- as.sociomatrix(d.filt,attrname="Q")
# gplot(d.val,gmode="graph",thresh<5,
#       vertex.col="lightblue",vertex.cex=1.3,
#       label.cex=0.4,label.pos=5,
#       displayisolates=FALSE)
# par(op)
# 
# 
# # relaciones con direccion a sin direccion 
# # net1mat <- symmetrize(netWorksPeriod[[y]],rule="weak")
# # net1mat
# # players<- as.vector(nodesList$nodes)
# # 
# # net1symm <- network(net1mat,matrix.type="adjacency")
# # network.vertex.names(net1symm) <- players
# # summary(net1symm)
# # 
# # 
# # op <- par(mar = rep(0, 4))
# # gplot(net1symm,gmode="graph",
# #       vertex.col="lightblue",vertex.cex=1.3,
# #       label.cex=0.4,label.pos=5,
# #       displayisolates=FALSE)
# # par(op)
# 
# 
# ###design graphics
# # 
# # op <- par(mar = c(0,0,4,0),mfrow=c(1,2))
# # gplot(netWorksPeriod[[y]],gmode="graph",mode="random",
# #       vertex.cex=1.5,main="Random layout")
# # gplot(netWorksPeriod[[y]],gmode="graph",mode="fruchtermanreingold",
# #       vertex.cex=1.5,main="Fruchterman-Reingold")
# # par(op)
# # 
# # ### 
# # ## layout graphs
# # op <- par(mar=c(0,0,4,0),mfrow=c(2,3))
# # gplot(netWorksPeriod[[y]],gmode="graph",edge.col="grey75",
# #       vertex.cex=1.5,mode='circle',main="circle")
# # gplot(netWorksPeriod[[y]],gmode="graph",edge.col="grey75",
# #       vertex.cex=1.5,mode='eigen',main="eigen")
# # gplot(netWorksPeriod[[y]],gmode="graph",edge.col="grey75",
# #       vertex.cex=1.5,mode='random',main="random")
# # gplot(netWorksPeriod[[y]],gmode="graph",edge.col="grey75",
# #       vertex.cex=1.5,mode='spring',main="spring")
# # gplot(netWorksPeriod[[y]],gmode="graph",edge.col="grey75",
# #       vertex.cex=1.5,mode='fruchtermanreingold',main='fruchtermanreingold')
# # gplot(netWorksPeriod[[y]],gmode="graph",edge.col="grey75",
# #       vertex.cex=1.5,mode='kamadakawai',
# #       main='kamadakawai')
# # par(op)
# # 
# # 
# # #coord
# # 
# # mycoords1 <- gplot(netWorksPeriod[[y]],gmode="graph",
# #                    vertex.cex=1.5)
# # mycoords2 <- mycoords1
# # mycoords2[,2] <- mycoords1[,2]*1.5
# # mycoords1
# # 
# # ## almacenar color
# # op <- par(mar=c(4,3,4,3),mfrow=c(1,2))
# # gplot(netWorksPeriod[[y]],gmode="graph",coord=mycoords1,
# #       vertex.cex=1.5,suppress.axes = FALSE,
# #       ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),
# #       main="Original coordinates")
# # gplot(netWorksPeriod[[y]],gmode="graph",coord=mycoords2,
# #       vertex.cex=1.5,suppress.axes = FALSE,
# #       ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),
# #       main="Modified coordinates")
# # par(op)
# # 
# # ### ajustes a los graficos
# # col2rgb('slateblue2')
# # gplot(netWorksPeriod[[y]],vertex.col=rgb(122,103,238,
# #                                          maxColorValue=255, alpha = 50),gmode="graph")
# # gplot(netWorksPeriod[[y]],vertex.col="#7A67EE",gmode="graph")
# # 
# # ### transparencia
# # library(RColorBrewer)
# # display.brewer.pal(5, "Dark2")
# # rolelab <- get.vertex.attribute(netWorksPeriod[[y]],"region")
# # ndum <- rgraph(300,tprob=0.025,mode="graph")
# # op <- par(mar = c(0,0,2,0),mfrow=c(1,2))
# # gplot(netWorksPeriod[[y]],gmode="graph",vertex.cex=2,
# #       vertex.col=rgb(0,0,139,maxColorValue=255),
# #       edge.col="grey80",edge.lwd=netWorksPeriod[[y]] %e% 'Q',mode='fruchtermanreingold',
# #       main="Fully opaque", label=rolelab)
# # gplot(netWorksPeriod[[y]],gmode="graph",vertex.cex=2,
# #       vertex.col=rgb(0,0,139,alpha=80,
# #                      maxColorValue=255),
# #       edge.col="grey80",edge.lwd=netWorksPeriod[[y]] %e% 'Q',mode='fruchtermanreingold',
# #       main="Partly transparent", label=rolelab)
# # par(op)
# # 
# # 
# # 
# # 
# # 
# # op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
# # plot(netWorksPeriod[[y]],usearrows=T,vertex.cex=deg,main="Raw")
# # plot(netWorksPeriod[[y]],usearrows=FALSE,vertex.cex=log(deg),
# #      main="Adjusted")
# # par(op)
# # 
# # 
# # 
# 
# # ajuste nodos con relacion al betweenes y  clossennes 
# # op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
# # plot(netWorksPeriod[[y]],usearrows=T,vertex.cex=cls,main="Raw")
# # plot(netWorksPeriod[[y]],usearrows=FALSE,vertex.cex=4*cls,
# #      main="Adjusted")
# # par(op)
# # op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
# # plot(netWorksPeriod[[y]],usearrows=T,vertex.cex=bet,main="Raw")
# # plot(netWorksPeriod[[y]],usearrows=FALSE,vertex.cex=sqrt(bet+1),
# #      main="Adjusted")
# # par(op)
# # 
# 
# # plot(netWorksPeriod[[y]],vertex.cex=rescale(deg,1,6),
# #      main="Adjusted node sizes with rescale function.")
# # 
# # rolelab <- get.vertex.attribute(netWorksPeriod[[y]],"region")
# # plot(netWorksPeriod[[y]],usearrows=FALSE,label=rolelab,
# #      displaylabels=T,label.col="darkblue")
# 
# 
# # op <- par(mar = c(0,0,0,0))
# # IClevel <- netWorksPeriod[[y]] %e% "Q"
# # plot(netWorksPeriod[[y]],vertex.cex=1.5,
# #      edge.lwd=IClevel^2)
# # par(op)
# # 
# # my_pal <- brewer.pal(n=12,"Set3")
# # display.brewer.pal(11,"BrBG")
# # display.brewer.all(n=10, exact.n=FALSE)
# # display.brewer.all(n=19)
# # display.brewer.all(colorblindFriendly=TRUE)
# # brewer.pal.info["Blues",]
# # brewer.pal.info["Blues",]$maxcolors
# 
# # legend=c("Southern_Europe","Middle_Africa","South_America", "Western_Asia" , "Australia_and_New_Zealand",
# #          "Western_Europe" ,  "caribbean" ,"Eastern_Europe", "Central_America", "Eastern_Africa" ,"Northern_America",
# #          "Eastern_Asia","Northern_Europe", "Northern_Africa","micro_poli_melanesia" , "Western_Africa","Southern_Asia",
# #          "Central_Asia", "South_Eastern_Asia")
# 
# 
# my_pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#b15928", 
#             "#33a02c","#b15928", "#cab2d6", "#40004b", "#034e7b", "#40004b", "#3690c0", "#d0d1e6", "#00441b","#543005" )
# op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
# rolecat <- as.factor(get.vertex.attribute(netWorksPeriod[[y]],"region"))
# legRegion<- as.vector(unique(rolecat))
# 
# plot(netWorksPeriod[[y]],vertex.cex=rescale(deg,1,6),
#      vertex.col=my_pal[rolecat],usearrows=TRUE,mode='fruchtermanreingold',
#      edge.col="grey80",edge.lwd=netWorksPeriod[[y]] %e% "Q", 
#      main=paste("Rice Networks \nYear:", periododsList[[y]], sep ="" ))
# legend("left",legend=legRegion,
#        col=my_pal,pch=19,pt.cex=1.5,bty="n",  xjust = 1, yjust = 1,text.width = strwidth("1000000"),  title.adj = 0.5,
#        title="Regions", ncol = 1, text.font = 1)
# par(op)
# 
# 
# 
# ## grafico test ### las conexiones estan relacionadas con las candidades exportadas, y el tamaño del nodo esta relacionado con el numero de conexiones
# my_pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#b15928", 
#             "#33a02c","#b15928", "#cab2d6", "#40004b", "#034e7b", "#40004b", "#3690c0", "#d0d1e6", "#00441b","#543005" )
# 
# rolelab <- get.vertex.attribute(netWorksPeriod[[y]],"region")
# ndum <- rgraph(300,tprob=0.025,mode="graph")
# op <- par(mar = c(0,0,2,0),mfrow=c(1,2))
# gplot(netWorksPeriod[[y]],gmode="graph",vertex.cex=rescale(deg,1,6),
#       vertex.col=my_pal[rolecat],
#       edge.col="grey80",edge.lwd=netWorksPeriod[[y]] %e% 'Q',mode='fruchtermanreingold',
#       main="Fully opaque") #label=rolelab
# gplot(netWorksPeriod[[y]],gmode="graph",vertex.cex=rescale(deg,1,6),
#       vertex.col=rgb(0,0,139,alpha=80,
#                      maxColorValue=255),
#       edge.col="grey80",edge.lwd=netWorksPeriod[[y]] %e% 'Q',mode='fruchtermanreingold',
#       main="Partly transparent") # label=rolelab
# 
# par(op)
# 
# 
# 
# 
# ## graph
# gplot()
# gplot(climaRed,vertex.col = 2, displaylabels = TRUE)
# 
# 
# ########################################  SNA analysis- redes ############################################
# 
# ####################################### stock analysos

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
suppressMessages(library(qgraph))

### directorios
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles")
rtbFolder<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/NetworkAnalysis/") 

# write.csv(append,paste(rtbFolder,"appendDataComercioCOMTRADERice_13_17.csv", sep = ""))


#### File-------------------------
xx<- list.files(pattern = "appendDataComercioCOMTRADECassavaStarch", full.names = T) 
xx<- lapply(xx, read.csv)
xx<- do.call(rbind,xx)
require(dplyr)
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
          "United States Minor Outlying Islands", "Holy See (Vatican City State)")
cc<- cc %>% dplyr::filter(., !Partner %in% sacar) %>% dplyr::filter(., !Reporter %in% sacar) 

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
cc<- cc[!is.na(cc$Trade.Value..US..),]
colnames(cc)[6]<-"Q"
colnames(cc)[7]<-"Val"

nesReports<- cc[grepl(pattern =" nes",x = cc$Reporter),]
nesReports<- unique(nesReports$Reporter)
nesPartner<- cc[grepl(pattern =" nes",x = cc$Partner),] 
nesPartner<- unique(nesPartner$Partner)
nesout<- c(nesPartner, nesReports)

cc<- cc %>% dplyr::filter(., !Reporter %in% nesout) %>% dplyr::filter(.,!Partner %in% nesout)
crops<- unique(cc$Commodity)
starc<- c("Starch; manioc (cassava)",
          "Manioc (cassava) starch",
          "Manioc (cassava), fresh or dried")

# cc<- cc %>% dplyr::filter(., Commodity %in% starc)
cc$Commodity<- plyr::revalue(cc$Commodity, c("Starch; manioc (cassava)"="Starch",
                                             "Manioc (cassava) starch"="Starch",
                                             "Manioc (cassava)"="Chips",
                                             "Manioc (cassava), fresh or dried"="Chips",
                                             "Vegetable roots and tubers; manioc (cassava), with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets"="Chips"))
str(cc)
cc$Trade.Flow<- as.character(cc$Trade.Flow)
cc$Commodity<-  as.character(cc$Commodity)
######################################  suma de dolares and Cantidades del COMTRADE ####################
test<- cc %>% group_by(Year,Trade.Flow,Reporter,Commodity)%>% 
      dplyr::summarise(.,dinero=sum(Val), cantidad=sum(Q))

write.csv(test,"./totalCantidad_dinero.csv")

### filter using feature of money and Exports
testm<- test %>% dplyr::filter(.,Trade.Flow=="Export") %>% 
      select(Year, Trade.Flow, Reporter, dinero )
testm$dinero_m<- testm$dinero/1000000

testm<- testm[c("Year","Trade.Flow", "Reporter","dinero_m")]
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


# typeRice<- unique(cc$Commodity)---------
Xrice<- cc %>% dplyr::filter(., Trade.Flow=="Export") %>% dplyr::filter(., Year!=2017)
riceTpes<- Xrice  %>% split(Xrice$Commodity) 
periododsList<- sort(unique(Xrice$Year))

netWorksPeriod<- list()
netWorksShock<- list()
networkRice<- list()
attrRed<- list()
netData<- list()
i=1 #1=Chips , 2=starch
y=1 # periods

############################################# runinggggggggggggggg ##########################
netWorksTemp<- lapply(1: length(riceTpes),function(i){
      xdata<- riceTpes[[i]]
      
      
      for(y in 1:length(periododsList)){
            ## filtramos la los datos de tipo de commoditie por yr/reporter/partner/quantity
            xdataSub<- dplyr::filter(xdata, Year==periododsList[[y]]) %>% 
                  dplyr::select(Reporter, Partner, Val) %>% 
                  dplyr::filter(., Val!=0) 
            # xdataSub$Val<- log10(xdataSub$Val)
            xdataSub$Val<- xdataSub$Val/1000000
            #Filtramos por los valores menores al 1er quantile
            q<- quantile(xdataSub$Val,probs = 0.55)
            xdataSub<- dplyr::filter(xdataSub, Val>=q)
            attrValue<- xdataSub
            # attrValue$QL<- log10(attrValue$Q) # logaritmo de los niveles exportados
            
            
            ###### Filtrando solo datos de reportes and partner
            rRed<- xdataSub[c("Reporter","Partner")]
            
            ###### crear objeto tipo red
            netWorksPeriod[[y]] <- network::network(rRed,matrix.type="edgelist")
            
            ###### Creando objeto con el numero y nombre de los nodos belong red/yr
            n<- netWorksPeriod[[y]]$gal$n
            Nodes<- lapply(1:n, function(v){
                  netWorksPeriod[[y]]$val[[v]]$vertex.names
                  
            })
            ##### apilamiento de los nodos
            nodesList<- as.data.frame(do.call(rbind,Nodes))
            colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
            
            
            ###### Agregar atributo valor del edge, agregamos log(quatity export)
            network::set.edge.attribute(netWorksPeriod[[y]], "Val", c(attrValue[,3]))
            
            list.edge.attributes(netWorksPeriod[[y]]) # lista de edge attributos
            summary(netWorksPeriod[[y]] %e% "Val") # summary de un atributo especifico
            as.sociomatrix(netWorksPeriod[[y]],"Val") # ver la matrix especifico del atributo
            
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
                  dplyr::mutate(., total=sum(Val)) %>% dplyr::select(Reporter,total) 
            SumExports<- SumExports[!duplicated(SumExports),]
            SumExports$Year<- periododsList[[y]]
            totalExports<- sum(SumExports$total) #### total exports by total year
            
            ##### Calculated of rate of exports
            SumExports$RateExpor<- (SumExports$total/totalExports)*100
            colnames(SumExports)[1]<- "nodes"
            
            #             ##### Exporting file to csv format
            #             write.csv(SumExports,paste("./data/", periododsList[[y]], 
            #                                        "ExportsTotal_",unique(riceTpes[[i]]$Commodity),
            #                                        ".csv", sep = ""))
            #             
            
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
            
            ################################# sub region ###############################
#             aa<- c("South_America", "Central_America", "caribbean","South_America")      #"Northern_America"
#             z<- unique(rjoinRegion$zone)
#             ourregion <- get.inducedSubgraph(netWorksPeriod[[y]],
#                                              which(netWorksPeriod[[y]] %v% 
#                                                          "region" %in% aa))
#             plot(ourregion)
#             
#             #### eliminar isolates
#             network::delete.vertices(ourregion,isolates(ourregion))
#             
            ourregion<- netWorksPeriod[[y]]
            ####################################################################################################
            ###### Creando objeto con el numero y nombre de los nodos belong red/yr
            n<- ourregion$gal$n
            NodesLAC<- lapply(1:n, function(v){
                  ourregion$val[[v]]$vertex.names
                  
            })
            ##### apilamiento de los nodos
            nodesListLAC<- as.data.frame(do.call(rbind, NodesLAC))
            colnames(nodesListLAC)[1]<- "nodes" ## renombrando la columna 
            
            
            ##### Calculando algunas metricas
            
            deg <- sna::degree(ourregion,gmode="graph")
            cls <-  sna::closeness(ourregion,gmode="graph") # la relacion tiene direccion 
            bet <-  sna::betweenness(ourregion,gmode="graph")
            
            
            detach(package:igraph)
            suppressMessages(library(statnet))
            CentraBet<-centralization(ourregion,betweenness)
            CentraDeg<- centralization(ourregion,degree)
            
            
            ### data igraph y generacion de metricas basicas 
            detach(package:statnet)
            suppressMessages(library(igraph))
            suppressMessages(library(intergraph))
            iDHHS <- asIgraph(ourregion)### convert network file to igraph
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
            dataSUM<- data.frame(nodes= nodesListLAC, 
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
                                 ties= ourregion$gal$mnext,
                                 nodesTotal= ourregion$gal$n,
                                 Year= periododsList[[y]],
                                 riceType= unique(riceTpes[[i]]$Commodity))
            
            ###### Export to cvs format
            write.csv(dataSUM,paste("./NetworkAnalysis/", periododsList[[y]], 
                                    "_StactNetworks_",unique(riceTpes[[i]]$Commodity),
                                    ".csv", sep = ""))
            
            
            ##### guardar datos en una lista
            netData[[y]]<- dataSUM
            
            
            
            #####################################################################################
            detach(package:igraph)
            suppressMessages(library(statnet))
            suppressMessages(library(intergraph))
            scatter.smooth(ourregion %v% 'RateExpor',
                           degree(ourregion,gmode='graph'),
                           xlab='Rate Export',
                           ylab='Degree')
            
            #             ### tables 
            #             table_local<- mixingmatrix(netWorksPeriod[[y]],'cat_localHV')
            #             table_fore<- mixingmatrix(netWorksPeriod[[y]],'cat_f_hvW')            
            #             table_shock<- mixingmatrix(netWorksPeriod[[y]],'Shock')
            
            
            
            
            #####################################################################################
            ### Eliminando  paises sin informacion de shocks de rendimiento
            test <- get.inducedSubgraph(ourregion,
                                        which(ourregion %v% "cat_localHV"!="NoProducers"))
            
            
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
            write.csv(Bcoefstab,paste("./NetworkAnalysis/", periododsList[[y]], 
                                      "_ResultsModelNetworks_",unique(riceTpes[[i]]$Commodity),
                                      ".csv", sep = ""))
            
            
            ##########################  Graphics  ######################################## 
            detach(package:statnet)
            suppressMessages(library(igraph))
            suppressMessages(library(intergraph))
            iDHHS <- asIgraph(ourregion)
            
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
            
            tiff(paste("./NetworkAnalysis/pic/",periododsList[[y]],
                      unique(riceTpes[[i]]$Commodity),"World_Network" ,".tiff", sep = ""), width = 10, height = 10, units = 'in', 
                res=80)# width = 800, height = 500)
            
            op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
            V(iDHHS)$size=(rescale(degi,1,8))*1
            E(iDHHS)$arrow.size <- .2
            plot(iDHHS,vertex.label=V(iDHHS)$vertex.names, layout=layout_with_fr, #layout.fruchterman.reingold,
                 edge.width=E(iDHHS)$Val/2,edge.color="orange",
                 vertex.color="orange", vertex.frame.color="#ffffff")
            #             legend(x=-1.8, y=1,c("caribbean", "South_America", "Central_America"), pch=21,
            #                    col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
            #                    title="Regions", pt.bg=c("green","blue","yellow" )) # pt.bg=unique(V(iDHHS)$color)
            #             
            par(op)
            dev.off()
            
            ### ensayos
        gplot(ourregion,gmode="graph",mode="fruchtermanreingold",
              vertex.cex=log(deg),edge.col="grey75")

            ############ grafico 2 ##############################
            
            tiff(paste("./NetworkAnalysis/pic/",periododsList[[y]],
                       unique(riceTpes[[i]]$Commodity),"World_NetworkOtherGraph" ,".tiff", sep = ""), 
                 width = 10, height = 10, units = 'in', 
                 res=80)# width = 800, height = 500)
            op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
            V(iDHHS)$size=(rescale(degi,1,8))*2
            E(iDHHS)$arrow.size <- .2
            l<- layout_randomly(iDHHS)
            plot(iDHHS,vertex.label=NA,layout=l,
                 edge.width=E(iDHHS)$Val/2,edge.color="orange",
                 vertex.color="grey", vertex.frame.color="#555555", edge.arrow.size=.,
                 vertex.label.cex=.7,edge.curved=0)
            #             legend(x=-1.8, y=1,c("caribbean", "South_America", "Central_America"), pch=21,
            #                    col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
            #                    title="Regions", pt.bg=c("green","blue","yellow" )) # pt.bg=unique(V(iDHHS)$color)
            #             
            par(op)
            dev.off()
            
            #             #### eliminar isolates
            #             network::delete.vertices(n1F,isolates(n1F))
            #              #### modeling 
            #             m1lac<- ergm(n1F ~ edges+
            #                                nodecov('RateExpor')+
            #                                nodematch('cat_localHV', diff=TRUE),
            #                          control=control.ergm(seed=40))
            #             summary(m1lac)
            #             
            #             table(n1F %v% "cat_localHV")
            #             V(alc)$cat_localHV
            #             
            cat(paste("proceso bien ", periododsList[[y]]," completed done!!!\n ", sep = ""))
            #             detach(package:igraph)
            #             suppressMessages(library(statnet))
      }
      networkRice[[i]]<- netWorksPeriod
      
      
} )

#### procesando resultados de las metricas basicas de las redes-----------------
cfilesDes<- list.files("./NetworkAnalysis/", pattern = "_StactNetworks_", full.names = T)
cfilesDes<- lapply(cfilesDes, read.csv)
cfilesDes<- do.call(rbind, cfilesDes)
cfilesDes$X<- NULL
cfilesDes$nodes<- as.character(cfilesDes$nodes)
cfilesDes$riceType<- as.character(cfilesDes$riceType)
# LAC<- c(caribbean, South_America, Central_America)
# cfilesDes<- cfilesDes %>%  mutate(., zone=ifelse((nodes %in% LAC),"LAC", "WORLD"))
###### Creación de objectos regiones correspondientes a la red filtrada
### America
treal1<- cfilesDes %>% dplyr::filter(nodes %in% caribbean) %>% dplyr::mutate(., zone="caribbean")
treal2<- cfilesDes %>% dplyr::filter(nodes %in% Central_America) %>% dplyr::mutate(., zone="Central_America")
treal3<- cfilesDes %>% dplyr::filter(nodes %in% South_America) %>% dplyr::mutate(., zone="South_America")
treal4<- cfilesDes %>% dplyr::filter(nodes %in% Northern_America) %>% dplyr::mutate(., zone="Northern_America")

##Asia
treal5<- cfilesDes %>% dplyr::filter(nodes %in% Eastern_Asia) %>% dplyr::mutate(., zone="Eastern_Asia")
treal6<- cfilesDes %>% dplyr::filter(nodes %in% South_Eastern_Asia) %>% dplyr::mutate(., zone="South_Eastern_Asia")
treal7<- cfilesDes %>% dplyr::filter(nodes %in% Southern_Asia) %>% dplyr::mutate(., zone="Southern_Asia")
treal8<- cfilesDes %>% dplyr::filter(nodes %in% Western_Asia) %>% dplyr::mutate(., zone="Western_Asia")
treal9<- cfilesDes %>% dplyr::filter(nodes %in% Central_Asia) %>% dplyr::mutate(., zone="Central_Asia")


##Africa
treal10<- cfilesDes %>% dplyr::filter(nodes %in% Eastern_Africa) %>% dplyr::mutate(., zone="Eastern_Africa")
treal11<- cfilesDes %>% dplyr::filter(nodes %in% Middle_Africa) %>% dplyr::mutate(., zone="Middle_Africa")
treal12<- cfilesDes %>% dplyr::filter(nodes %in% Northern_Africa) %>% dplyr::mutate(., zone="Northern_Africa")
treal13<- cfilesDes %>% dplyr::filter(nodes %in% Southern_Africa) %>% dplyr::mutate(., zone="Southern_Africa")
treal14<- cfilesDes %>% dplyr::filter(nodes %in% Western_Africa) %>% dplyr::mutate(., zone="Western_Africa")

##Oceania
treal15<- cfilesDes %>% dplyr::filter(nodes %in% Australia_and_New_Zealand) %>% dplyr::mutate(., zone="Australia_and_New_Zealand")
treal16<- cfilesDes %>% dplyr::filter(nodes %in% micro_poli_melanesia) %>% dplyr::mutate(., zone="micro_poli_melanesia")


##EUROPA
treal17<- cfilesDes %>% dplyr::filter(nodes %in% Eastern_Europe) %>% dplyr::mutate(., zone="Eastern_Europe")
treal18<- cfilesDes %>% dplyr::filter(nodes %in% Northern_Europe) %>% dplyr::mutate(., zone="Northern_Europe")
treal19<- cfilesDes %>% dplyr::filter(nodes %in% Southern_Europe) %>% dplyr::mutate(., zone="Southern_Europe")
treal20<- cfilesDes %>% dplyr::filter(nodes %in% Western_Europe) %>% dplyr::mutate(., zone="Western_Europe")

##### apilan los subsets por regiones
r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,treal15,
          treal16,treal17,treal18,treal19, treal20)

write.csv(r,paste("./NetworkAnalysis/TotalindicadoresBasicos.csv", sep = ""))


### ties and nodes---------------
tino<- r %>% dplyr::select(.,Year, riceType, ties, nodesTotal, zone)
tino<- tino[!duplicated(tino),]
row.names(tino)<- 1:nrow(tino)
write.csv(tino,paste("./NetworkAnalysis/tino.csv", sep = ""))


##### apliar los resultados de los modelos----- 
mo<- list.files("./NetworkAnalysis/", pattern = "ResultsModelNetworks", full.names = T)
mo<- lapply(mo, read.csv)
mo<- do.call(rbind, mo)
str(mo)
mo$X<- as.character(mo$X)
mo$model<-as.character(mo$model)
mo$RiceT<- as.character(mo$RiceT)
write.csv(mo, paste("./NetworkAnalysis/", "ResumenModelos.csv", sep = ""))



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




########################################################################################################

#### difusion analisis
suppressMessages(library(netdiffuseR))
suppressMessages(library(readr))
s<-20000
set.seed(s)
diffnet_ran <- rdiffnet(200, 20, "random", seed.p.adopt = .1,
                        seed.graph = "small-world",
                        rgraph.args = list(undirected=FALSE, k=4, p=.5),
                        threshold.dist = function(x) 0.3)

plot(diffnet_ran)


tt<- floor(runif(76, min = 1, max = 20))
V(iDHHS)$toa<- tt
pp<- igraph_to_diffnet(graph = iDHHS,toavar = "toa", t0 = 1,t1 = 20)

plot(pp)
summary(pp)
plot_diffnet(pp)
plot_adopters(pp, add=TRUE, what="cumadopt")
plot_infectsuscep(pp, bins=15, K=3, 
                  main = "Distribution of Infectiousness and\nSusceptibility (Random)")


plot_threshold(pp)

ig_net<- barabasi.game(10)
V(ig_net)$toa<- c(1,1,1,1,3,3,3,3,6,6)
d<-igraph_to_diffnet(ig_net,toavar = "toa")
plot(d)




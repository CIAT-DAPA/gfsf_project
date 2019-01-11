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
suppressMessages(library(networkD3))
suppressMessages(library(RColorBrewer))
suppressMessages(library(RSiena))


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
netMatrix<- list()
attrRed<- list()
netData<- list()
i=1#1=Chips , 2=starch
y=20 # periods


############################################# runinggggggggggggggg ##########################
netWorksTemp<- lapply(1: length(riceTpes),function(i){
      xdata<- riceTpes[[i]]
      
      for(y in 1:length(periododsList)){
            ## filtramos la los datos de tipo de commoditie por yr/reporter/partner/quantity
            xdataSub<- dplyr::filter(xdata, Year==periododsList[[y]]) %>% 
                  dplyr::select(Reporter, Partner, Val) %>% 
                  dplyr::filter(., Val!=0) 
            xdataSub$Val<- log10(xdataSub$Val)
            # xdataSub$Val<- xdataSub$Val/1000000
            
            # dis<- quantile(xdataSub$Val)
            #Filtramos por los valores menores al 1er quantile
            q<- quantile(xdataSub$Val,probs = 0.75)
            # q[[1]]
            xdataSub<- dplyr::filter(xdataSub, Val>=q[[1]])
            attrValue<- xdataSub

            ###### Filtrando solo datos de reportes and partner
            rRed<- xdataSub[c("Reporter","Partner")]
            
            
            ###### crear objeto tipo red
            netWorksPeriod[[y]] <- network::network(rRed,matrix.type="edgelist")
            netMatrix[[y]] <- as.sociomatrix(netWorksPeriod[[y]])
      
            # # https://dnac.ssri.duke.edu/r-labs/2017/02_descriptive_statistics.php 
            # https://www.sci.unich.it/~francesc/teaching/network/components.html
            # test<- geodist(netWorksPeriod[[y]],inf.replace = 0)
            # hist(test$gdist)
            # 
            # cutpoints(netWorksPeriod[[y]], connected="recursive")
            # gplot(netWorksPeriod[[y]],vertex.col=2+cutpoints(netWorksPeriod[[y]],
            #                                                  mode="digraph",return.indicator=T))
            
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
            
            summary(netWorksPeriod[[y]] %e% "Val") # summary de un atributo especifico
            # as.sociomatrix(netWorksPeriod[[y]],"Val") # ver la matrix especifico del atributo
            
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
            
            
            ourregion<- netWorksPeriod[[y]]
            list.vertex.attributes.active(ourregion)
          
            ##### crear objeto tipo red
            
            ####################################################################################################
            ###### Creando objeto con el numero y nombre de los nodos belong red/yr
            n<- ourregion$gal$n
            NodesLAC<- lapply(1:n, function(v){
                  ourregion$val[[v]]$vertex.names
                  
            })
            ##### apilamiento de los nodos
            nodesListLAC<- as.data.frame(do.call(rbind, NodesLAC))
            colnames(nodesListLAC)[1]<- "nodes" ## renombrando la columna 
            
            
            # ##### Calculando algunas metricas
            # deg <- sna::degree(ourregion,gmode="digraph",cmode="freeman")
            # cls <-  sna::closeness(ourregion,gmode="digraph") # la relacion tiene direccion 
            # bet <-  sna::betweenness(ourregion,gmode="digraph")
            
            
            # detach(package:igraph)
            # suppressMessages(library(statnet))
            # CentraBet<- centralization(ourregion,betweenness)
            # CentraDeg<- centralization(ourregion,degree)
            
            
            ### data igraph y generacion de metricas basicas 
            # detach(package:statnet)
            suppressMessages(library(igraph))
            suppressMessages(library(intergraph))
            iDHHS <- asIgraph(ourregion)### convert network file to igraph
            
            V(iDHHS)$comp <- components(iDHHS)$membership
            ii <- induced_subgraph(iDHHS,V(iDHHS)$comp==1)
            # pp<- as.network(ii)

            nodelist<- V(ii)$vertex.names
            
            V(ii)$color<- FALSE
            ##### agregando coclor a los nodos
            V(ii)[(V(ii)$region=="Middle_Africa")]$color <- "green"
            V(ii)[V(ii)$region=="caribbean"]$color<- "blue"
            V(ii)[V(ii)$region=="South_America"]$color<- "blue"
            V(ii)[V(ii)$region=="Western_Asia"]$color<- "yellow"
            V(ii)[V(ii)$region=="Australia_and_New_Zealand"]$color<- "grey"
            V(ii)[V(ii)$region=="Western_Europe"]$color<- "red"
            V(ii)[V(ii)$region=="Eastern_Europe"]$color<- "red"
            V(ii)[V(ii)$region=="Central_America"]$color<- "blue"
            V(ii)[V(ii)$region=="Eastern_Africa"]$color<- "green"
            V(ii)[V(ii)$region=="Western_Africa"]$color<- "green"
            V(ii)[V(ii)$region=="Northern_America"]$color<- "purple"
            V(ii)[V(ii)$region=="Southern_Europe"]$color<- "red"
            V(ii)[V(ii)$region=="micro_poli_melanesia"]$color<- "black"
            V(ii)[V(ii)$region=="Northern_Europe"]$color<- "red"
            V(ii)[V(ii)$region=="Southern_Asia"]$color<- "blue"
            V(ii)[V(ii)$region=="Central_Asia"]$color<- "yellow"
            V(ii)[V(ii)$region=="Northern_Africa"]$color<- "green"
            V(ii)[V(ii)$region=="South_Eastern_Asia"]$color<- "yellow"
            V(ii)[V(ii)$region=="Eastern_Asia"]$color<- "yellow"
            V(ii)[V(ii)$region=="Southern_Africa"]$color<- "green"
            
            ##### Estadisticas estaticas
            idensi<- graph.density(ii,loops = F)
            cw <- cluster_walktrap(ii)
            clusMember<- as.vector(membership(cw))
            mod<- modularity(cw)
            gden<- graph.density(ii,loops = F)
            meanDeg<- mean(degree(ii,mode = "total",loops = F))
            trans<- transitivity(ii,type = "global")
            transLocal<- transitivity(ii, type = "local")
            assor<- assortativity.degree(ii, directed = T)
            CentraBet<- centralization.betweenness(graph = ii,directed = T)
            CentraDeg<- centralization.degree(graph = ii,mode = "total")
            deg<-degree(graph = ii,mode="all")
            cls<- closeness(graph = ii, mode = "out")
            bet<- betweenness(graph = ii, directed = T)
            eigenvector<- eigen_centrality(graph = ii,directed = T)
            eigenvector<- eigenvector$vector
            edge_bet<- cluster_edge_betweenness(graph = ii,directed = T,bridges = T,modularity = T)
            #             detach(package:statnet)
            #                         suppressMessages(library(statnet))
            #                         suppressMessages(library(intergraph))
            
            
            ############## metricas basicas stacking ##########################
            dataSUM<- data.frame(nodes= nodelist, 
                                 degree= deg,
                                 density= idensi,
                                 between= bet, 
                                 closeness=cls,
                                 centraBetw= (CentraBet$centralization),
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
            degi<- degree(graph = ii,mode = "total")
            
            tiff(paste("./NetworkAnalysis/pic/",periododsList[[y]],
                      unique(riceTpes[[i]]$Commodity),"World_Network" ,".tiff", sep = ""), width = 10, height = 10, units = 'in', 
                res=80)# width = 800, height = 500)
            
            op <- par(mar = c(0,0,2,0),mfrow=c(1,1))
            V(ii)$size=(rescale(degi,1,max(degi)))
            E(ii)$arrow.size <- .1
            E(ii)$width <- (E(ii)$Val)/2
            plot(ii,vertex.label=V(ii)$vertex.names, vertex.label.cex=.7,edge.color="grey",
                 vertex.color="orange",
                 main=paste("Cassava ",unique(riceTpes[[i]]$Commodity),"\nby ",periododsList[[y]],sep=""))
            par(op)
            dev.off()
            
            
            cat(paste("proceso bien ", periododsList[[y]]," completed done!!!\n ", sep = ""))
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


#################################### dynamic analysis  ##############################
# https://github.com/kateto/Network_Analysis_R_Examples/blob/master/R%20Scripts/Comm645-RSiena.R


# 
# ########################################################################################################
# 
# #### difusion analisis
# suppressMessages(library(netdiffuseR))
# suppressMessages(library(readr))
# s<-20000
# set.seed(s)
# diffnet_ran <- rdiffnet(200, 20, "random", seed.p.adopt = .1,
#                         seed.graph = "small-world",
#                         rgraph.args = list(undirected=FALSE, k=4, p=.5),
#                         threshold.dist = function(x) 0.3)
# 
# plot(diffnet_ran)
# 
# 
# tt<- floor(runif(76, min = 1, max = 20))
# V(iDHHS)$toa<- tt
# pp<- igraph_to_diffnet(graph = iDHHS,toavar = "toa", t0 = 1,t1 = 20)
# 
# plot(pp)
# summary(pp)
# plot_diffnet(pp)
# plot_adopters(pp, add=TRUE, what="cumadopt")
# plot_infectsuscep(pp, bins=15, K=3, 
#                   main = "Distribution of Infectiousness and\nSusceptibility (Random)")
# 
# 
# plot_threshold(pp)
# 
# ig_net<- barabasi.game(10)
# V(ig_net)$toa<- c(1,1,1,1,3,3,3,3,6,6)
# d<-igraph_to_diffnet(ig_net,toavar = "toa")
# plot(d)
# 
# 
# 

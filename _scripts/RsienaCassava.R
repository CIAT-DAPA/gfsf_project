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


#https://github.com/kateto/Network_Analysis_R_Examples/blob/master/R%20Scripts/Comm645-RSiena.R
#http://www.stats.ox.ac.uk/~snijders/siena/
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

################################################## SNA  #########################################
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


## variables dependientes del modelo dinamico de Rsiena
##################################### tipo de nettrade ##################################
netrade<- cc %>% dplyr::filter(., Year!=2017) %>% 
      select(Year,Trade.Flow,Reporter,Partner,Commodity,Q)%>%
      spread(Trade.Flow,Q) %>% 
      dplyr::group_by(Year,Reporter, Commodity) %>% 
      dplyr::mutate(., TExp=sum(Export,na.rm = T), 
                    TImp=sum(Import,na.rm = T)) %>%
      select(Year,Reporter,Commodity,TExp,TImp) %>% as.data.frame()

netrade<- netrade[!duplicated(netrade),]

pp<- which(netrade$TExp>netrade$TImp)
nn<- which(netrade$TExp<netrade$TImp)
bb<- which(netrade$TExp==netrade$TImp)
out<- which(netrade$TExp=='0' & netrade$TImp=='0')
positive<- c(pp)
negative<- c(nn)
balanced<- c(bb)

netrade$trend<- NA
netrade$trend[positive]<- "Positive_Net-trade"
netrade$trend[negative]<- "Negative_Net-trade"
netrade$trend[balanced]<- "Balanced_Net-trade"

eli<- c(out)
netrade<- netrade[-eli,]

netrade$trend<- as.factor(netrade$trend)
netrade$trend<- as.numeric(netrade$trend) # 1 Balanced_Net-trade 2 Negative_Net-trade 3 Positive_Net-trade

netrade<- netrade %>% select(Year,Reporter, Commodity,
                             trend) %>% spread(Year,trend)
netrade[is.na(netrade)]<- 0

##################### Balance relatives comparison #############################
bcr<- cc %>% dplyr::filter(., Year!=2017) %>% 
      select(Year,Trade.Flow,Reporter,Partner,Commodity,Q)%>%
      spread(Trade.Flow,Q) %>% replace(is.na(.), 0) %>%
      select(Year,Reporter,Partner, Commodity,Export,Import) %>%
      mutate(., X_plus_M= Export+Import, X_minus_M=Export-Import,
             icbr=X_minus_M/X_plus_M) %>% 
      select(Year,Reporter,Partner, Commodity,icbr) %>%
      as.data.frame()

bcr<- bcr[!is.na(bcr$icbr),]

ventaja<- which(bcr$icbr>0)
desventaja<- which(bcr$icbr<0)

ven<- c(ventaja)
des<- c(desventaja)
bcr$vcr<- NA
bcr$vcr[ven]<- "advantages"
bcr$vcr[des]<- "disadvantages"

bcr<- bcr %>% select(Year,Reporter,Partner, Commodity, vcr)
bcr<- bcr[!is.na(bcr$vcr),]
bcr$vcr<- as.factor(bcr$vcr)
bcr$vcr<- as.numeric(bcr$vcr) # 1 advantages 2 disadvantages

bcr<- bcr %>% spread(.,Year,vcr )
bcr[is.na(bcr)]<- 0
############################################# shocks de rendimientos  #####################
# z<- unique(bcr$Reporter)
# y<- unique(bcr$Partner)
# zy<- c(z,y)
# zy<- zy[!duplicated(zy)]

nodes_shock<- unique(shockYields$nodes)

shockYields<- shockYields[c("year", "nodes","cat_localHV")]
shockYields$cat_localHV<- as.factor(shockYields$cat_localHV) 
# 1 middle range 2 negative 3 positive
shockYields$cat_localHV<- as.numeric(shockYields$cat_localHV) 
shockYields<- shockYields %>% spread(year, cat_localHV)

shockYields[is.na(shockYields)]<- 0

############################################## Corvarianza ################################
### Net trade---Changing covariates 
#CHIPS
chips.net<- netrade %>% filter(., Commodity=="Chips") %>% select(.,-Commodity)
row.names(chips.net)<- chips.net$Reporter
chips.net$Reporter<- NULL
c.chips.net<- as.matrix(chips.net)
covarChips.net<- varCovar(c.chips.net)

#STARCH
starch.net<- netrade %>% filter(., Commodity=="Starch") %>% select(.,-Commodity)
row.names(starch.net)<- starch.net$Reporter
starch.net$Reporter<- NULL
c.starch.net<- as.matrix(starch.net)
covarStarch.net<- varCovar(c.starch.net)


### Shock Yields---Changing covariates 
row.names(shockYields)<- shockYields$nodes
shockYields$nodes<- NULL
c.shockYields<- as.matrix(shockYields)
covarc.shockYields<- varCovar(c.shockYields)

### VCR------------Changing covariates
#bcr How can i to include network as covariante RSIENA
vcr.chips<- bcr %>% filter(., Commodity=="Chips") %>% select(.,-Commodity)
rep<- c(vcr.chips$Reporter)
par<- c(vcr.chips$Partner)

vcr.chips<- vcr.chips %>% select(.,-Reporter,-Partner) 
m.vcr.chips<- as.matrix(vcr.chips)
Covar.vcr.Chips<- coDyadCovar(m.vcr.chips, type = c("bipartite")) 




############################################## matrices  de relacion #######################

# typeRice<- unique(cc$Commodity)---------
Xrice<- cc %>% dplyr::filter(., Trade.Flow=="Export") %>% dplyr::filter(., Year!=2017)
riceTpes<- Xrice  %>% split(Xrice$Commodity) 
periododsList<- sort(unique(Xrice$Year))


netWorksPeriod<- list()
netMatrix<- list()
attrRed<- list()
netData<- list()
i=1#1=Chips , 2=starch
y=20 # periods


############################################# runinggggggggggggggg ##########################
lapply(1:length(riceTpes),function(i){
      xdata<- riceTpes[[i]]
      
      for(y in 1:length(periododsList)){
            ## filtramos la los datos de tipo de commoditie por yr/reporter/partner/quantity
            xdataSub<- dplyr::filter(xdata, Year==periododsList[[y]]) %>% 
                  dplyr::select(Reporter, Partner, Q) %>% 
                  dplyr::filter(., Q!=0) 
            xdataSub$Q<- log10(xdataSub$Q)

            #Filtramos por los valores menores al 1er quantile
            q<- quantile(xdataSub$Val,probs = 0.75)
            xdataSub<- dplyr::filter(xdataSub, Val>=q[[1]])
            attrValue<- xdataSub
            
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
            
            
            
            
            netMatrix[[y]] <- as.sociomatrix(netWorksPeriod[[y]])
            
            
            cat(paste("proceso bien ", periododsList[[y]]," completed done!!!\n ", sep = ""))
      }

} )

###chips
# friend.t123 <- array(c(netMatrix[[1]], netMatrix[[2]], netMatrix[[3]]), dim=c(64, 64, 20))
test<- array(c(netMatrix), dim=c(64, 64, 20))
# Create a Siena network object with sienaNet()
# The type could be "oneMode" for one mode networks, 
# "bipartite" for 2-mode networks, or "behavior", 
# for behavioral dependent variables (see next section)

test.net <- sienaNet(friend.t123, type="oneMode") 
# Take a look at the SienaNet object we created:
class(test.net)
dim(test.net)
attributes(test.net)



test.net

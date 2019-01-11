## codigo para crear regiones de analisis

setwd("C:/Users/CEGONZALEZ/Documents/COMTRADE")
# R options
options(warn = -1)
options(scipen = 999)

# Extracting codes and descriptions of reporters from the UN Comtrade API
suppressMessages(library(rjson))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"


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


# Combinatios just for countries high role
# zonas<- c("Brazil","Cambodia","China", "Colombia", "Costa Rica", "Ecuador", "El Salvador","France", "India", "Indonesia", "Kenya", 
#           "Netherlands", "Nicaragua", "Nigeria", "Paraguay","Peru", "Philippines", "Thailand","Afghanistan" ,"Albania", "Algeria", 
#           "American Samoa","Andorra","Anguilla","Antigua and Barbuda","Argentina","Aruba","Austria","Bahamas" ,"Bangladesh","Belarus",
#           "Belgium-Luxembourg","Belize","Benin","Bhutan","Bolivia (Plurinational State of)","Bonaire" ,"Botswana","Bulgaria",
#           "Burkina Faso","Burundi","Cameroon","Caribbean, nes","Cayman Isds","Central African Rep.", "Chile","China, Hong Kong SAR",
#           "China, Macao SAR","Comoros","Congo","Croatia","Cuba","Curaçao","Czechia","Dem. People's Rep. of Korea",
#           "Dem. Rep. of the Congo","Denmark","Dominica","Dominican Rep." ,"East and West Pakistan","Ecuador","Egypt",
#           "El Salvador","Equatorial Guinea","Eritrea","Ethiopia","Fiji","Finland","Fmr Dem. Rep. of Germany","Fmr Dem. Yemen",
#           "Fmr Ethiopia","Fmr Fed. Rep. of Germany","Fmr Pacific Isds","Fmr Panama, excl.Canal Zone","Fmr Panama-Canal-Zone",
#           "Fmr Rep. of Vietnam", "Fmr Rhodesia Nyas","Fmr Sudan", "Fmr USSR","Fmr Zanzibar and Pemba Isd","France","French Guiana",
#           "French Polynesia","FS Micronesia","Gabon" ,"Gambia","Germany","Gibraltar","Greenland","Guadeloupe","Guatemala","Guinea",
#           "Guinea-Bissau","Guyana","Haiti","Heard Island and McDonald Islands","Holy See (Vatican City State)","Honduras","Hungary",
#           "Iceland", "India","India, excl. Sikkim","Indonesia", "Iran" ,"Iraq","Israel" ,"Italy","Jamaica" ,"Japan","Jordan" , 
#           "Kazakhstan" , "Kenya" ,"Kiribati" ,"Kuwait","Kyrgyzstan" ,"LAIA, nes" ,"Lao People's Dem. Rep.","Latvia", "Lebanon",
#           "Lesotho" ,"Liberia" ,"Libya" , "Lithuania", "Luxembourg", "Madagascar" ,"Malawi"  ,"Malaysia" ,"Maldives" , "Mali" ,
#           "Malta" ,"Marshall Isds"  ,"Martinique" ,"Mauritania" , "Mauritius" ,"Mayotte","Mexico" , "Mongolia","Montenegro" , 
#           "Montserrat", "Morocco" ,  "Mozambique" ,"Myanmar","N. Mariana Isds", "Namibia",  "Nauru","Niger" , "Nigeria",
#           "Niue"  ,  "Norfolk Isds", "North America and Central America, nes" , "Northern Africa, nes","Norway"  , 
#           "Oceania, nes","Oman"   , "Other Africa, nes" ,"Other Asia, nes"  , "Other Europe, nes","Pakistan" , "Palau" , 
#           "Panama"  ,  "Papua New Guinea"  , "Paraguay" , "Peninsula Malaysia","Peru" , "Philippines" , "Pitcairn" ,  
#           "Poland","Portugal" ,  "Qatar" ,"Rep. of Korea" ,"Rep. of Moldova","Rest of America, nes" , "Réunion" ,"Romania" , 
#           "Russian Federation", "Rwanda" , "Ryukyu Isd"  ,"Sabah"  , "Saint Helena","Saint Kitts and Nevis" , "Saint Kitts, Nevis and Anguilla" , 
#           "Saint Lucia"  ,"Saint Maarten" ,"Saint Pierre and Miquelon" , "Saint Vincent and the Grenadines", "Samoa", "San Marino"  , 
#           "Sao Tome and Principe" , "Sarawak","Saudi Arabia", "Senegal" ,"Serbia", "Serbia and Montenegro"  , "Seychelles", 
#           "Sierra Leone","Sikkim", "Singapore" ,"Slovakia","Slovenia" ,"So. African Customs Union","Solomon Isds" , "Somalia" , 
#           "South Africa" ,"South Georgia and the South Sandwich Islands","South Sudan","Spain", "Special Categories","Sri Lanka" ,
#           "State of Palestine" ,"Sudan", "Suriname" ,"Swaziland" ,"Sweden","Switzerland", "Syria","Tajikistan" , "TFYR of Macedonia",
#           "Thailand", "Timor-Leste","Togo", "Tonga", "Tunisia" ,"Turkmenistan", "Turks and Caicos Isds", "Tuvalu",
#           "US Misc. Pacific Isds" , "US Virgin Isds" ,"USA" ,"USA (before 1981)", "Uzbekistan","Vanuatu","Venezuela" , 
#           "Viet Nam","Wallis and Futuna Isds" , "Western Asia, nes" ,"Western Sahara" ,"Yemen","Zambia", "Zimbabwe")       
# 
# zonasTrue<- zonas[!duplicated(zonas)]
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

# creando grupos de paises que no se van a tener en cuenta en el analisis

# test<- as.data.frame(zonasTrue)
# test$zonasTrue<- as.character(test$zonasTrue)
# test<- filter(test, !zonasTrue %in% noZonas)
# zonasAnalysis<- unique(test$zonasTrue)
# 

allComReg<- filter(all, Reporter %in% ZonasLAC)%>% filter(., !Partner %in% noZonas)
row.names(allComReg)<- 1:nrow(allComReg)

# obtener los codigos por paise que reporta
aux <- inner_join(x = allComReg, y = reporters, by = c('Reporter' = 'Country'))
allComReg$Reporter_code <- aux[,ncol(aux)]
# obtener los codigos por pais socio
aux <- inner_join(x = allComReg, y = partners, by = c('Partner' = 'Country'))
allComReg$Partner_code <- aux[,ncol(aux)]; rm(aux)

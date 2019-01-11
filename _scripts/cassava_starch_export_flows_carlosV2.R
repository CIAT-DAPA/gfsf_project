# Cassava dried export flows - Preparing for CIAT-blog
# Author: H. Achicanoy, C. Gonzalez
# CIAT, 2016

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



# Define directory and files to read
setwd('C:/Users/CEGONZALEZ/Documents/cassava/copyData')
db_dir <- './periods_medianStar/' # Using median as summary measure
db_periods <- list.files(path = db_dir, pattern = '.csv$', full.names = T)
periodList <- c('1986-1992', '1993-1999', '2000-2006', '2007-2013')

# Load datasets correctly
db_time <- lapply(1:length(db_periods), function(i){
      z <- read.csv(db_periods[i])
      z$X <- NULL
      colnames(z) <- c('Reporter', 'Partner', 'mean') # Median
      z$Period <- periodList[i]
      return(z)
})
db_time <- do.call(rbind, db_time)

# Rename and filter countries
db_time$Reporter <- as.character(db_time$Reporter)
db_time$Partner <- as.character(db_time$Partner)

db_time$Reporter <- gsub(pattern = '* \\((.*?)\\)', replacement = '', x = db_time$Reporter)
db_time$Reporter[grep(pattern = "C么te d'Ivoire", x = db_time$Reporter, fixed = TRUE)] <- 'Ivory Coast'
# db_time$Reporter[grep(pattern = "C魌e d'Ivoire", x = db_time$Reporter, fixed = TRUE)] <- 'Ivory Coast'
db_time$Reporter[grep(pattern = "China, Hong Kong SAR", x = db_time$Reporter, fixed = TRUE)] <- 'China, Hong Kong SAR'
db_time$Reporter[grep(pattern = "China, Macao SAR", x = db_time$Reporter, fixed = TRUE)] <- 'Macao'
db_time$Reporter[grep(pattern = "China, mainland", x = db_time$Reporter, fixed = TRUE)] <- 'China'
db_time$Reporter[grep(pattern = "China, Taiwan Province of", x = db_time$Reporter, fixed = TRUE)] <- 'Taiwan'
db_time$Reporter[grep(pattern = "R茅union", x = db_time$Reporter, fixed = TRUE)] <- 'Reunion'
db_time$Reporter[grep(pattern = "United States of America", x = db_time$Reporter, fixed = TRUE)] <- 'USA'
db_time$Reporter[grep(pattern = "United Republic of Tanzania", x = db_time$Reporter, fixed = TRUE)] <- 'Tanzania'
db_time$Reporter[grep(pattern = "United Kingdom", x = db_time$Reporter, fixed = TRUE)] <- 'UK'

# db_time <- db_time[-which(db_time$Reporter=="Belgium-Luxembourg"),]

db_time$Partner <- gsub(pattern = '* \\((.*?)\\)', replacement = '', x = db_time$Partner)
db_time$Partner[grep(pattern = "C么te d'Ivoire", x = db_time$Partner, fixed = TRUE)] <- 'Ivory Coast'
# db_time$Partner[grep(pattern = "C魌e d'Ivoire", x = db_time$Partner, fixed = TRUE)] <- 'Ivory Coast'
db_time$Partner[grep(pattern = "China, Hong Kong SAR", x = db_time$Partner, fixed = TRUE)] <- 'China, Hong Kong SAR'
db_time$Partner[grep(pattern = "China, Macao SAR", x = db_time$Partner, fixed = TRUE)] <- 'Macao'
db_time$Partner[grep(pattern = "China, mainland", x = db_time$Partner, fixed = TRUE)] <- 'China'
db_time$Partner[grep(pattern = "China, Taiwan Province of", x = db_time$Partner, fixed = TRUE)] <- 'Taiwan'
db_time$Partner[grep(pattern = "R茅union", x = db_time$Partner, fixed = TRUE)] <- 'Reunion'
db_time$Partner[grep(pattern = "Democratic Republic of the Congo", x = db_time$Partner, fixed = TRUE)] <- 'DRC'
db_time$Partner[grep(pattern = "United Republic of Tanzania", x = db_time$Partner, fixed = TRUE)] <- 'Tanzania'
db_time$Partner[grep(pattern = "United States of America", x = db_time$Partner, fixed = TRUE)] <- 'USA'
db_time$Partner[grep(pattern = "Lao People's Democratic Republic", x = db_time$Partner, fixed = TRUE)] <- 'Laos'
db_time$Partner[grep(pattern = "United Kingdom", x = db_time$Partner, fixed = TRUE)] <- 'UK'


# db_time <- db_time[-which(db_time$Partner=="Belgium-Luxembourg"),]
# db_time <- db_time[-which(db_time$Partner=="Unspecified"),]

# db_time$Reporter <- paste(db_time$Reporter, ' >', sep = '')
# db_time$Partner <- paste('> ', db_time$Partner, sep = '')

# Only use countries with some amount of exports
real_exports<- db_time %>% filter(., mean>=(-0.1))
# real_exports <- db_time[db_time$mean > 0,]; rownames(real_exports) <- 1:nrow(real_exports)
# real_exports$Mean <- exp(real_exports$Mean)

countryList <- sort(unique(c(as.character(real_exports$Reporter), as.character(real_exports$Partner))))
nodes <- data.frame(name = countryList, node = (1:length(countryList)) - 1); rm(countryList)



#---------------------------------------------------------------------------#
caribbean<- c("Cuba", "Dominican Republic", "Haiti", "Jamaica", "Dominica")      
Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico","Nicaragua","Panama","El Salvador", "Trinidad and Tobago")
Australia_and_New_Zealand<- c( "New Zealand", "Australia")
Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")
Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar","Mozambique","Malawi","Rwanda","Somalia", "Tanzania",
                   "Uganda","Zambia","Zimbabwe")
Eastern_Asia<- c("China","Japan","South Korea","Mongolia","North Korea", "Taiwan") # "China, Hong Kong"
Eastern_Europe<- c("Bulgaria","Belarus","Czech Republic","Hungary","Moldova","Other Balkans","Poland",
                   "Romania","Russia","Slovakia","Ukraine", "Russian Federation")
Melanesia<- c("Fiji", "Papua New Guinea","Solomon Islands","Vanuatu")
Middle_Africa<- c("Angola","Central African Rep.","Cameroon","DRC","Congo","Gabon","Equatorial Guinea","Chad")
Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Sudan","Tunisia")
Northern_America<-c("Canada","Greenland","USA")
Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","UK")
South_America<- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guyanas","Peru","Paraguay","Uruguay","Venezuela","Suriname")
South_Eastern_Asia<- c("Indonesia","Cambodia","Laos","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "Timor LEste","Viet Nam")
Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")
Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Iran","Sri Lanka","Nepal","Other Indian Ocean","Pakistan")
Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia")
Western_Africa<- c("Benin","Burkina Faso","Ivory Coast","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leon","Togo")
Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", "Lebanon","Palestine", "Rest of Arabia","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates")
Western_Europe<- c("Austria","Belgium-Luxembourg","Switzerland","Germany","France","Netherlands","Other Atlantic")


# 
# # -------------------------------------------------------------------------- #
# # Interactive sankey graphs for each period
# # -------------------------------------------------------------------------- #
# j=1
# lapply(1:length(periodList), function(j){
#       
#       # filter data by period
#       flows <- real_exports[real_exports$Period==periodList[j],]
#       flows$Period <- NULL
#       
#       # sort by decreasing order
#       flows <- flows[order(flows$mean, decreasing = T),]; rownames(flows) <- 1:nrow(flows)
#       
#       # obtain list of countries and nodes
#       virtCountryList <- unique(c(as.character(flows$Reporter), as.character(flows$Partner))) # sort
#       virtNodes <- data.frame(name = virtCountryList, node = (1:length(virtCountryList)) - 1); rm(virtCountryList)
#       
#       # replace text strings by nodes
#       for(i in 1:nrow(virtNodes)) {
#             flows$Reporter <- gsub(pattern = paste(as.character(virtNodes$name[i]), '$', sep = ''), replacement = virtNodes$node[i], x = flows$Reporter, fixed = F)
#             flows$Partner <- gsub(pattern = paste(as.character(virtNodes$name[i]), '$', sep = ''), replacement = virtNodes$node[i], x = flows$Partner, fixed = F)
#             # flows$Reporter <- gsub(pattern = paste(as.character(nodes$name[i]), '$', sep = ''), replacement = nodes$node[i], x = flows$Reporter, fixed = F)
#             # flows$Partner <- gsub(pattern = paste(as.character(nodes$name[i]), '$', sep = ''), replacement = nodes$node[i], x = flows$Partner, fixed = F)
#       }
#       
#       flows$Reporter <- as.numeric(flows$Reporter)
#       flows$Partner <- as.numeric(flows$Partner); rownames(flows) <- 1:nrow(flows)
#       # flows <- flows[which(flows$Mean > 0.5),]; rownames(flows) <- 1:nrow(flows) # Omit countries that does not have exports
#       # flows$Mean[which(flows$Mean==0.5)] <- 0.0001
#       
#       # colour links
#       flows$gType <- sub(' .*', '', virtNodes[flows$Reporter + 1, 'name'])
#       # nodes2 <- nodes[sort(unique(c(flows$Reporter, flows$Partner)))+1,]; rownames(nodes2) <- 1:nrow(nodes2)
#       
#       sankeyList <- list(links = flows,
#                          nodes = virtNodes)
#       
# #       # make and save each plot sankey
# #       s <- sankeyNetwork(Links = sankeyList$links, Nodes = sankeyList$nodes, Source = "Reporter",
# #                          Target = "Partner", Value = "mean", NodeID = "name",
# #                          units = "Ton", LinkGroup = "gType", fontSize = 12, nodeWidth = 30)
# #       saveNetwork(s, paste("C:/Users/CEGONZALEZ/Documents/cassava/copyData/sankey_", gsub(pattern = "-", replacement = "_", periodList[j]), ".html", sep = ""), selfcontained = T)
# #       
#       
#       
#       
# })

# -------------------------------------------------------------------------- #
# Interactive d3/Steven circos of cassava dried exports
# -------------------------------------------------------------------------- #
countryList <- sort(unique(c(as.character(real_exports$Reporter), as.character(real_exports$Partner))))

# Make a transformation over Median in order to show all posible links
# y = ln(x + 10); to return to the original value make: exp(y) - 10
# real_exports$mean <- log(real_exports$mean + 10, base=exp(1))
# saveRDS(object = real_exports, file = ('C:/Users/CEGONZALEZ/Documents/cassava/copyData/Carlos_log_Starch_cassava_exports.rds'))
# real_exports <- readRDS('C:/Users/CEGONZALEZ/Documents/cassava/copyData/Starch_log_Starch_cassava_exports.rds')

# # All posible flows
# i=1
# flows2json <- lapply(1:length(periodList), function(i){
#   
#   # Subseting by period and make a square matrix of flows
#   subReal <- real_exports %>% dplyr::filter(Period == periodList[i]) %>% as.data.frame()
#   subReal <- subReal %>% tidyr::spread(Partner, mean)
#   subReal$Period <- NULL
#   rownames(subReal) <- subReal$Reporter; subReal$Reporter <- NULL
#   
#   # Identify countries that don't have exports
#   mtch_r <- setdiff(countryList, rownames(subReal))
#   mtch_c <- setdiff(countryList, colnames(subReal))
#   subReal[nrow(subReal) + 1:length(mtch_r),] <- 0
#   subReal[,ncol(subReal) + 1:length(mtch_c)] <- 0
#   rownames(subReal)[(nrow(subReal)-length(mtch_r)+1):nrow(subReal)] <- mtch_r
#   colnames(subReal)[(ncol(subReal)-length(mtch_c)+1):ncol(subReal)] <- mtch_c
#   
#   # Arrange by alphabetical order
#   subReal[is.na(subReal)] <- 0
#   subReal <- subReal[order(rownames(subReal)),]
#   subReal <- subReal[,order(colnames(subReal))]
#   
#   # Formating as Genebank distributions
#   subReal2 <- subReal
#   rownames(subReal2) <- paste(rownames(subReal2), '_rep', sep='')
#   colnames(subReal2) <- rownames(subReal2)
#   pos <- seq(1, nrow(subReal)*2, 2)
#   pos2 <- seq(2, nrow(subReal)*2, 2)
#   subRealFinal <- matrix(data=NA, nrow=nrow(subReal)*2, ncol=nrow(subReal)*2); subRealFinal <- as.data.frame(subRealFinal)
#   rownames(subRealFinal)[pos] <- colnames(subRealFinal)[pos] <- rownames(subReal)
#   rownames(subRealFinal)[pos2] <- colnames(subRealFinal)[pos2] <- rownames(subReal2)
#   subRealFinal[pos, pos] <- subReal
#   subRealFinal[pos2, pos2] <- subReal2
#   subRealFinal[pos, pos2] <- subReal
#   subRealFinal[pos2, pos] <- subReal
#   return(subRealFinal)
#   
# })
# 
# ### Mucha l韓ea
# ### csa <- mdwide_tzaCSA[,c("impactparameter", "commodity", paste("X", seq(from=2020, to=2050, by=5), sep = ""), "Cat")] %>% group_by(impactparameter, commodity, Cat) %>% summarise_each(funs(mean))
# 
# # {names} for JSON file
# mat.labels <- rownames(flows2json[[1]])
# 
# # {matrix} for JSON file
# matrices <- lapply(flows2json, as.matrix)
# names(matrices) <- c('calories','protein','fat','food_weight')
# 
# # {regions} for JSON file
# regions <- setdiff(1:length(mat.labels),grep(pattern='*_rep$', x=mat.labels)) - 1
# 
# # Making JSON file
# # Put all elements together in a list, after that apply toJSON function
# # Sublist can contain different type of information to show
# json.file <- list(names             = mat.labels,
#                   labels            = NA,
#                   metrics           = NA,
#                   regions           = regions,
#                   names_description = NA, 
#                   matrix            = matrices,
#                   help              = NA,
#                   description       = NA
# )
# 
# sink('./_data/_json_files/cassava_dried_exports.json') # redirect console output to a file
# toJSON(json.file, pretty=FALSE)
# sink()
# 

# -------------------------------------------------------------------------- #
# Static circos of cassava dried exports
# -------------------------------------------------------------------------- #
# Load data
#real_exports <- readRDS('./_data/_cassava_data/periods_median/log_dried_cassava_exports.rds')
# real_exports <- readRDS('C:/Users/CEGONZALEZ/Documents/cassava/copyData/Carlos_log_Starch_cassava_exports.rds')

# real_exports$Reporter<- sub(pattern = ">", replacement = "", x = real_exports$Reporter)
# real_exports$Partner<- sub(pattern = ">", replacement = "", x = real_exports$Partner)
# 
# real_exports$Reporter <- gsub(pattern = '* \\((.*?)\\)', replacement = '', x = real_exports$Reporter)
# real_exports$Reporter[grep(pattern = "C么te d'Ivoire", x = real_exports$Reporter, fixed = TRUE)] <- 'Ivory Coast'
# # real_exports$Reporter[grep(pattern = "C魌e d'Ivoire", x = real_exports$Reporter, fixed = TRUE)] <- 'Ivory Coast'
# real_exports$Reporter[grep(pattern = "China, Hong Kong SAR", x = real_exports$Reporter, fixed = TRUE)] <- 'Hong Kong'
# real_exports$Reporter[grep(pattern = "China, Macao SAR", x = real_exports$Reporter, fixed = TRUE)] <- 'Macao'
# real_exports$Reporter[grep(pattern = "China, mainland", x = real_exports$Reporter, fixed = TRUE)] <- 'China'
# real_exports$Reporter[grep(pattern = "China, Taiwan Province of", x = real_exports$Reporter, fixed = TRUE)] <- 'Taiwan'
# real_exports$Reporter[grep(pattern = "R茅union", x = real_exports$Reporter, fixed = TRUE)] <- 'Reunion'
# real_exports$Reporter[grep(pattern = "United States of America", x = real_exports$Reporter, fixed = TRUE)] <- 'USA'
# real_exports$Reporter[grep(pattern = "United Republic of Tanzania", x = real_exports$Reporter, fixed = TRUE)] <- 'Tanzania'
# real_exports$Reporter[grep(pattern = "United Kingdom", x = real_exports$Reporter, fixed = TRUE)] <- 'UK'
# 
# # real_exports <- real_exports[-which(real_exports$Reporter=="Belgium-Luxembourg"),]
# 
# real_exports$Partner <- gsub(pattern = '* \\((.*?)\\)', replacement = '', x = real_exports$Partner)
# real_exports$Partner[grep(pattern = "C么te d'Ivoire", x = real_exports$Partner, fixed = TRUE)] <- 'Ivory Coast'
# # real_exports$Partner[grep(pattern = "C魌e d'Ivoire", x = real_exports$Partner, fixed = TRUE)] <- 'Ivory Coast'
# real_exports$Partner[grep(pattern = "China, Hong Kong SAR", x = real_exports$Partner, fixed = TRUE)] <- 'Hong Kong'
# real_exports$Partner[grep(pattern = "China, Macao SAR", x = real_exports$Partner, fixed = TRUE)] <- 'Macao'
# real_exports$Partner[grep(pattern = "China, mainland", x = real_exports$Partner, fixed = TRUE)] <- 'China'
# real_exports$Partner[grep(pattern = "China, Taiwan Province of", x = real_exports$Partner, fixed = TRUE)] <- 'Taiwan'
# real_exports$Partner[grep(pattern = "R茅union", x = real_exports$Partner, fixed = TRUE)] <- 'Reunion'
# real_exports$Partner[grep(pattern = "Democratic Republic of the Congo", x = real_exports$Partner, fixed = TRUE)] <- 'DRC'
# real_exports$Partner[grep(pattern = "United Republic of Tanzania", x = real_exports$Partner, fixed = TRUE)] <- 'Tanzania'
# real_exports$Partner[grep(pattern = "United States of America", x = real_exports$Partner, fixed = TRUE)] <- 'USA'
# real_exports$Partner[grep(pattern = "Lao People's Democratic Republic", x = real_exports$Partner, fixed = TRUE)] <- 'Laos'
# real_exports$Partner[grep(pattern = "United Kingdom", x = real_exports$Partner, fixed = TRUE)] <- 'UK'


# i=4
# Making static plots
lapply(1:length(periodList), function(i){
      
      # Subseting by period and make a square matrix of flows
      subReal <- real_exports %>% dplyr::filter(Period == periodList[i]) %>% as.data.frame()

      treal1<- subReal %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal2<- subReal %>% filter(Reporter %in% caribbean) %>% mutate(., zone="caribbean")
      treal3<- subReal %>% filter(Reporter %in% Central_America) %>% mutate(., zone="Central_America")
      treal4<- subReal %>% filter(Reporter %in% Australia_and_New_Zealand) %>% mutate(., zone="Australia_and_New_Zealand")
      treal5<- subReal %>% filter(Reporter %in% Eastern_Africa) %>% mutate(., zone="Eastern_Africa")
      treal6<- subReal %>% filter(Reporter %in% Eastern_Asia) %>% mutate(., zone="Eastern_Asia")
      treal7<- subReal %>% filter(Reporter %in% Eastern_Europe) %>% mutate(., zone="Eastern_Europe")
      treal8<- subReal %>% filter(Reporter %in% Melanesia) %>% mutate(., zone="Melanesia")
      treal9<- subReal %>% filter(Reporter %in% Middle_Africa) %>% mutate(., zone="Middle_Africa")
      treal10<- subReal %>% filter(Reporter %in% Northern_Africa) %>% mutate(., zone="Northern_Africa")
      treal11<- subReal %>% filter(Reporter %in% Northern_America) %>% mutate(., zone="Northern_America")
      treal12<- subReal %>% filter(Reporter %in% Northern_Europe) %>% mutate(., zone="Northern_Europe")
      treal13<- subReal %>% filter(Reporter %in% South_America) %>% mutate(., zone="South_America")
      treal14<- subReal %>% filter(Reporter %in% South_Eastern_Asia) %>% mutate(., zone="South_Eastern_Asia")
      treal15<- subReal %>% filter(Reporter %in% Southern_Africa) %>% mutate(., zone="Southern_Africa")
      treal16<- subReal %>% filter(Reporter %in% Southern_Asia) %>% mutate(., zone="Southern_Asia")
      treal17<- subReal %>% filter(Reporter %in% Western_Africa) %>% mutate(., zone="Western_Africa")
      treal18<- subReal %>% filter(Reporter %in% Southern_Europe) %>% mutate(., zone="Southern_Europe")
      treal19<- subReal %>% filter(Reporter %in% Western_Asia) %>% mutate(., zone="Western_Asia")
      treal20<- subReal %>% filter(Reporter %in% Western_Europe) %>% mutate(., zone="Western_Europe")


      r<- rbind(treal1, treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,
                treal15,treal16,treal17,treal18,treal19,treal20)
      
      rjoin<- left_join(r, subReal, by = c("Reporter", "Partner", "mean", "Period"))
      
      #   tempcolor<- unique(rjoin$zone)
      colorstemp<- c("##f1eef6", "#d0d1e6","#a6bddb", "#74a9cf", "#3690c0","#0570b0", "#034e7b" )
      rjoin$color<- NA
      rjoin$color[rjoin$zone=="Western_Europe"]<-  "#40004b"
      rjoin$color[rjoin$zone=="South_America"]<-   "#f0027f"
      rjoin$color[rjoin$zone=="Eastern_Asia"]<-    "#33a02c"
      rjoin$color[rjoin$zone=="Northern_Europe"]<- "#74a9cf"
      rjoin$color[rjoin$zone=="South_Eastern_Asia"]<- "#3690c0"
      rjoin$color[rjoin$zone=="Central_America"]<- "#ff7f00"
      rjoin$color[rjoin$zone=="Southern_Europe"]<-  "#034e7b"
      rjoin$color[rjoin$zone=="Australia_and_New_Zealand"]<- "#00441b"
      rjoin$color[rjoin$zone=="Southern_Asia"]<- "#cab2d6"
      rjoin$color[rjoin$zone=="Eastern_Africa"]<- "#fb9a99"
      rjoin$color[rjoin$zone=="Northern_America"]<- "#543005"
      rjoin$color[rjoin$zone=="Eastern_Europe"]<- "#ae017e"
      rjoin$color[rjoin$zone=="Melanesia"]<- "#4d4d4d"
      rjoin$color[rjoin$zone=="Western_Africa"]<- "#b2182b"
      

      color<- rjoin %>% filter(., mean!=0.5) %>% select("Reporter","Partner" , "color") 
      colorsss<- unique(rjoin$color)
      
      rjoin$color<- as.factor(rjoin$color)
      rjoin<- rjoin %>% select("Reporter","Partner" , "mean","color")
      rdata<- rjoin %>% select("Reporter","Partner" , "mean")
      
      # Initial parameters for static circos
      circos.clear()
      circos.par(start.degree = 90, gap.degree = 2,track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      par(mar = rep(0, 4))
      
      # Save plot
      png(paste('C:/Users/CEGONZALEZ/Documents/cassava/copyData/Starch_circos3_', periodList[i],'.png', sep = ''), width = 10, height = 10, units = 'in', res = 300)
      
      chordDiagram(x = rdata[rdata$mean!=0.5,],col=color$color,  transparency = 0.25, #col=color$color #[rdata$mean>0.7,]5
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
      
})

# -------------------------------------------------------------------------- #
# Interactive d3/R chord diagram
# -------------------------------------------------------------------------- #
# Source: http://data-steve.github.io/d3-r-chord-diagram-of-white-house-petitions-data/
if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("mattflor/chorddiag")
pacman::p_load(dplyr, magrittr, ggplot2, tidyr, curl)

countryList <- sort(unique(c(as.character(real_exports$Reporter), as.character(real_exports$Partner))))

i=1
# Making interactive plots
lapply(1:length(periodList), function(i){
      
      # Subseting by period and make a square matrix of flows
      subReal <- real_exports %>% dplyr::filter(Period == periodList[i]) %>% as.data.frame()
      subReal <- subReal %>% tidyr::spread(Partner, mean)
      subReal$Period <- NULL
      rownames(subReal) <- subReal$Reporter; subReal$Reporter <- NULL
      
      # Identify countries that don't have exports
      mtch_r <- setdiff(countryList, rownames(subReal))
      mtch_c <- setdiff(countryList, colnames(subReal))
      subReal[nrow(subReal) + 1:length(mtch_r),] <- 0
      subReal[,ncol(subReal) + 1:length(mtch_c)] <- 0
      rownames(subReal)[(nrow(subReal)-length(mtch_r)+1):nrow(subReal)] <- mtch_r
      colnames(subReal)[(ncol(subReal)-length(mtch_c)+1):ncol(subReal)] <- mtch_c
      
      # Arrange by alphabetical order
      subReal[is.na(subReal)] <- 0
      subReal <- subReal[order(rownames(subReal)),]
      subReal <- subReal[,order(colnames(subReal))]
      cList <- union(x = names(which(rowSums(subReal) != 0)), y = names(which(colSums(subReal) != 0))) # Filtering by values
      subReal <- subReal[cList, cList] # Filtering by values
      ord <- order(rowSums(subReal), decreasing = T)
      subReal <- as.matrix(subReal)
      
      # set number of colors needed
      colorCount <- length(countryList)
      # makes function to create palette
      getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      
      # with the diags means there's a return
      chrd <- chorddiag::chorddiag(subReal[ord, ord], margin = 150, showTicks = FALSE # adjmat[ord, ord]
                                   , groupnameFontsize = 11  # have to shrink font for web viewing
                                   , groupnamePadding = 5
                                   , groupThickness = .05
                                   , chordedgeColor = "gray" # getPalette(colorCount)
                                   , groupColors = getPalette(subReal[ord,])
                                   , showTooltips = FALSE) # Does not show tool box with info
      saveNetwork(chrd, paste("C:/Users/CEGONZALEZ/Documents/cassava/copyData/chord_2", gsub(pattern = "-", replacement = "_", periodList[i]), ".html", sep = ""), selfcontained = T)
      return(cat('Chord diagram done! for:', periodList[i],'\n'))
      
})

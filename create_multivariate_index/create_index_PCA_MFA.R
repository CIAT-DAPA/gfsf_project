# ============================================================================================ #
# Create an index to evaluate Crop-FPU individuals                                             #
# ============================================================================================ #

# abbreviate("percentage")

# R options
options(warn = -1); options(scipen = 999)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(modelr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(broom))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))

# Load data
toMerge <- readRDS(file = "./AnalysisBID2pca.RDS")
names(toMerge) <- c("IMPACT", "Growing_Rates", "Yields", "Failures", "Outliers")

# IMPACT data treatment *
names(toMerge$IMPACT) <- c("Crop", "FPU", "System", "Parameter", "Value", "Change_prcn")
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
toMerge$IMPACT$System <- gsub(pattern = "arf", replacement = "RA", x = toMerge$IMPACT$System)
toMerge$IMPACT$System <- gsub(pattern = "air", replacement = "IRRI", x = toMerge$IMPACT$System)
toMerge$IMPACT$Parameter <- paste(firstup(toMerge$IMPACT$Parameter), "_IMPACT", sep = "")
toMerge$IMPACT <- toMerge$IMPACT %>% gather(Variable, Summary, -(Crop:Parameter)) %>%
  unite(temp, Parameter, Variable) %>%
  spread(temp, Summary)
toMerge$IMPACT$ID <- paste(toMerge$IMPACT$Crop, "-", toMerge$IMPACT$FPU, "-", toMerge$IMPACT$System, sep = "")
toMerge$IMPACT$Crop <- toMerge$IMPACT$FPU <- toMerge$IMPACT$System <- NULL

# Growing rates treatment
names(toMerge$Growing_Rates) <- c("Crop", "FPU", "System", "GCM", "RT_initial", "RT_updated", "comparison")
toMerge$Growing_Rates$comparison <- NULL
toMerge$Growing_Rates$System <- gsub(pattern = "arf", replacement = "RA", x = toMerge$Growing_Rates$System)
toMerge$Growing_Rates$System <- gsub(pattern = "air", replacement = "IRRI", x = toMerge$Growing_Rates$System)
toMerge$Growing_Rates$ID <- paste(toMerge$Growing_Rates$Crop, "-", toMerge$Growing_Rates$FPU, "-", toMerge$Growing_Rates$System, sep = "")
toMerge$Growing_Rates$Crop <- toMerge$Growing_Rates$FPU <- toMerge$Growing_Rates$System <- NULL

Growing.Rates <- toMerge$Growing_Rates %>% split(toMerge$Growing_Rates$GCM)

# Crop modelling Yields treatment
toMerge$Yields$num_pixels <- NULL
names(toMerge$Yields)[1:3] <- c("Crop", "System", "FPU")
toMerge$Yields <- toMerge$Yields %>% gather(GCM, CM_Future, bcc_csm1_1:ncc_noresm1_m)
names(toMerge$Yields)[4] <- "CM_WFD"
toMerge$Yields <- toMerge$Yields[,c("Crop", "System", "FPU", "GCM", "CM_WFD", "CM_Future")]
toMerge$Yields$ID <- paste(toMerge$Yields$Crop, "-", toMerge$Yields$FPU, "-", toMerge$Yields$System, sep = "")
toMerge$Yields$Crop <- toMerge$Yields$FPU <- toMerge$Yields$System <- NULL

Yields <- toMerge$Yields %>% split(toMerge$Yields$GCM)

# Failures treatment
toMerge$Failures$Staus <- as.character(toMerge$Failures$Staus)
toMerge$Failures$Staus <- gsub(pattern = "Fallo total (yield==0)", replacement = "Total.failures", x = toMerge$Failures$Staus, fixed = T)
toMerge$Failures$Staus <- gsub(pattern = "Highest (Higher than 99% probs)", replacement = "Total.highest", x = toMerge$Failures$Staus, fixed = T)
toMerge$Failures$Staus <- gsub(pattern = "lowest Yield (lower than 5% probs)", replacement = "Total.lowest", x = toMerge$Failures$Staus, fixed = T)
toMerge$Failures <- toMerge$Failures %>% gather(Crop, Count, Bean:Wheat) %>% spread(Staus, Count)
names(toMerge$Failures)[1:3] <- c("FPU", "System", "GCM")
toMerge$Failures <- toMerge$Failures[,c("Crop", "FPU", "System", "GCM", "Total.failures", "Total.highest", "Total.lowest")]
toMerge$Failures[is.na(toMerge$Failures)] <- 0
toMerge$Failures$ID <- paste(toMerge$Failures$Crop, "-", toMerge$Failures$FPU, "-", toMerge$Failures$System, sep = "")
toMerge$Failures$Crop <- toMerge$Failures$FPU <- toMerge$Failures$System <- NULL

Failures <- toMerge$Failures %>% split(toMerge$Failures$GCM)

# Outliers treatment
names(toMerge$Outliers)[1:4] <- c("FPU", "Crop", "System", "Variety")
toMerge$Outliers <- toMerge$Outliers %>% gather(GCM, Outliers.count, bcc_csm1_1:ncc_noresm1_m)
toMerge$Outliers <- toMerge$Outliers %>% as.data.frame() %>% group_by(Crop, FPU, System, GCM) %>% summarise(Outliers.count = sum(Outliers.count))
toMerge$Outliers$ID <- paste(toMerge$Outliers$Crop, "-", toMerge$Outliers$FPU, "-", toMerge$Outliers$System, sep = "")
toMerge$Outliers$Crop <- toMerge$Outliers$FPU <- toMerge$Outliers$System <- NULL

Outliers <- toMerge$Outliers %>% split(toMerge$Outliers$GCM)

all_data <- lapply(1:9, function(i){
  
  all_data <- left_join(x = Growing.Rates[[i]], y = Yields[[i]], by = "ID")
  all_data <- left_join(x = all_data, y = Failures[[i]], by = "ID")
  all_data <- left_join(x = all_data, y = Outliers[[i]], by = "ID")
  return(all_data)
  
})
all_data <- do.call(rbind, all_data); rm(Growing.Rates, Yields, Failures, Outliers)
all_data$GCM.y <- all_data$GCM.x.x <- all_data$GCM.y.y <- NULL; names(all_data)[1] <- "GCM"
all_data <- all_data[,c("ID", "GCM", "RT_initial", "RT_updated", "CM_WFD", "CM_Future", "Total.failures", "Total.highest", "Total.lowest", "Outliers.count")]
all_data[is.na(all_data)] <- 0

# Crop modeling: Yes; IMPACT: No
# setdiff(all_data$ID[all_data$GCM == "bcc_csm1_1"], toMerge$IMPACT$ID)
# "Rice-CRB_CRB-IRRI" (IMPACT)
# "Rice-CHC_CHL-RA" (ok)
# "Rice-CUB_CUB-RA" (IMPACT)
# "Rice-GSA_GSA-RA" (IMPACT)
# "Rice-PAR_ARG-RA" (IMPACT)
# "Rice-RIC_ARG-RA" (ok)
# "Rice-SLV_SLV-RA" (IMPACT)
# "Rice-URU_URY-RA" (IMPACT)
# "Bean-NEB_BRA-IRRI" (IMPACT)
# "Bean-SLV_SLV-IRRI" (IMPACT)
# "Bean-URU_BRA-IRRI" (IMPACT)
# "Maize-CRI_CRI-IRRI" (IMPACT)
# "Maize-CUB_CUB-IRRI" (IMPACT)
# "Maize-RVE_VEN-IRRI" (IMPACT)
# "Maize-SLV_SLV-IRRI" (IMPACT)
# "Maize-URU_URY-IRRI" (IMPACT)
# "Wheat-URU_BRA-IRRI" (IMPACT)
# "Soybean-PAR_BOL-IRRI" (IMPACT)
# "Soybean-AMA_BOL-RA" (IMPACT)
# "Soybean-AMA_COL-RA" (IMPACT)
# "Soybean-GTM_GTM-RA" (IMPACT)
# "Soybean-NWS_ECU-RA" (IMPACT)
# "Soybean-ORI_COL-RA" (IMPACT)
# "Soybean-PAR_BOL-RA" (IMPACT)
# "Soybean-YUC_MEX-RA" (IMPACT)

# IMPACT: Yes; Crop Modeling: No
# "Maize-AMA_COL-IRRI"
# "Maize-CRB_CRB-RA"
# "Maize-CUB_CUB-RA"
# "Maize-ORI_COL-IRRI"
# "Rice-CRB_CRB-RA"
# "Rice-HTI_HTI-RA"
# "Rice-NWS_COL-RA"
# "Rice-ORI_COL-RA"
# "Rice-RVE_VEN-IRRI"
# "Wheat-AMA_BRA-RA"
# "Wheat-AMA_COL-IRRI"
# "Wheat-AMA_ECU-IRRI"
# "Wheat-AMA_PER-IRRI"
# "Wheat-HND_HND-RA"
# "Wheat-MIM_MEX-RA"
# "Wheat-NWS_COL-IRRI"
# "Wheat-NWS_ECU-IRRI"
# "Wheat-ORI_COL-IRRI"
# "Wheat-ORI_VEN-RA"
# "Wheat-PAR_PRY-IRRI"
# "Wheat-PEC_PER-IRRI"
# "Bean-AMA_COL-IRRI"
# "Bean-CRB_CRB-RA"
# "Bean-CRI_CRI-IRRI"
# "Bean-DOM_DOM-IRRI"
# "Bean-HND_HND-IRRI"
# "Bean-JAM_JAM-RA"
# "Bean-NWS_COL-IRRI"
# "Bean-NWS_ECU-IRRI"
# "Bean-ORI_COL-IRRI"
# "Bean-PAN_PAN-IRRI"
# "Bean-PAR_BOL-IRRI"
# "Bean-PAR_PRY-IRRI"
# "Bean-RIC_ARG-RA"
# "Bean-RVE_VEN-IRRI"
# "Bean-RVE_VEN-RA"
# "Bean-TIE_ARG-IRRI"
# "Bean-TIE_ARG-RA"
# "Bean-URU_URY-IRRI"
# "Bean-URU_URY-RA"
# "Soybean-BLZ_BLZ-RA"
# "Soybean-GSA_GSA-RA"
# "Soybean-PAN_PAN-RA"
# "Soybean-SLV_SLV-RA"

all_data <- all_data %>% split(all_data$GCM)
all_data <- lapply(1:length(all_data), function(i){
  df <- inner_join(x = all_data[[i]], y = toMerge$IMPACT, by = "ID")
  return(df)
})
all_data <- do.call(rbind, all_data)
all_data$Crop <- sapply(strsplit(all_data$ID, "-"), `[`, 1)
all_data$ID <- paste(sapply(strsplit(all_data$ID, "-"), `[`, 2), "-", sapply(strsplit(all_data$ID, "-"), `[`, 3), "-", all_data$GCM, sep = "")
all_data$GCM <- NULL
# Until here everything is correct

ad_list <- all_data %>% split(all_data$Crop)

suppressMessages(library(corrplot))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(cluster))
suppressMessages(library(RCurl))
suppressMessages(library(ggthemes))
suppressMessages(library(tidyverse))
suppressMessages(library(tidyquant))

# Principal Component Analysis per crop
lapply(1:length(ad_list), function(i){
  
  # Filter data by crop
  df <- ad_list[[i]]
  
  # Creating directories
  wk_dir <- "D:/ToBackup/climate_and_crop_modelling/bid-cc-agricultural-sector/Multivariate_analysis"
  crop <- unique(as.character(df$Crop))
  if(!dir.exists(paths = paste(wk_dir, "/", crop, sep = ""))){ dir.create(path = paste(wk_dir, "/", crop, sep = "")) }
  
  # Deleting columns with SD = 0
  rownames(df) <- df$ID; df$ID <- df$Crop <- NULL
  df <- Filter(function(x) sd(x) != 0, df)
  
  # Correlation matrix
  if(!file.exists(paste(wk_dir, "/", crop, "/corrMatrix.png", sep = ""))){
    M <- cor(df)
    png(file = paste(wk_dir, "/", crop, "/corrMatrix.png", sep = ""), height = 8, width = 8, units = "in", res = 300)
    corrplot.mixed(M)
    dev.off()
  }
  
  # Principal Component Analysis
  res.pca <- FactoMineR::PCA(df, graph = F)
  
  if(!file.exists(paste(wk_dir, "/", crop, "/eigenValuesPCA.png", sep = ""))){
    gg <- fviz_eig(res.pca, addlabels = TRUE, hjust = -0.3) + theme_bw() # Visualize eigenvalues/variances
    ggsave(filename = paste(wk_dir, "/", crop, "/eigenValuesPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in")
  }
  
  if(!file.exists(paste(wk_dir, "/", crop, "/varQuality.png", sep = ""))){
    png(paste(wk_dir, "/", crop, "/varQuality.png", sep = ""), height = 8, width = 16, units = "in", res = 300)
    par(mfrow = c(1, 3))
    corrplot(res.pca$var$cos2[,1:2], is.corr = FALSE, title = "Representation quality", mar = c(1, 0, 1, 0))        # Representation quality of each variable
    corrplot(res.pca$var$contrib[,1:2], is.corr = FALSE, title = "Contribution", mar = c(1, 0, 1, 0))               # Contribution of each variable to dimension
    corrplot(res.pca$var$cor[,1:2], method = "ellipse", is.corr = TRUE, title = "Correlation", mar = c(1, 0, 1, 0)) # Correlation of each variable to dimension
    dev.off()
  }
  
  # Hierarchical Clustering on Principle Components
  res.hcpc <- FactoMineR::HCPC(res.pca, nb.clust = -1, graph = FALSE)
  if(!file.exists(paste(wk_dir, "/", crop, "/biplotPCA.png", sep = ""))){
    gg <- fviz_pca_biplot(res.pca, label = "var", habillage = res.hcpc$data.clust$clust, addEllipses = TRUE, ellipse.level = 0.95) + theme_bw() # Biplot of individuals and variables. Only variables are labelled (var, ind)
    ggsave(filename = paste(wk_dir, "/", crop, "/biplotPCA.png", sep = ""), plot = gg, width = 8, height = 8, units = "in")
  }
  
  # Calculate distance between observations
  if(!file.exists(paste(wk_dir, "/", crop, "/distances.png", sep = ""))){
    res.dist <- get_dist(df, stand = TRUE, method = "kendall")
    gg <- fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
    ggsave(filename = paste(wk_dir, "/", crop, "/distances.png", sep = ""), plot = gg, width = 8, height = 8, units = "in")
  }
  
  # Index based on PCA
  script <- getURL("https://raw.githubusercontent.com/haachicanoy/r_scripts/master/calculate_index_by_pca.R", ssl.verifypeer = FALSE); eval(parse(text = script)); rm(script)
  
  df_cluster <- res.hcpc$data.clust
  df_cluster$Index <- index_cal(res.pca); rm(index_cal)
  df_cluster <- data.frame(df_cluster[,colnames(df_cluster)[which(colnames(df_cluster) != "clust")]], Cluster = df_cluster[,"clust"])
  df_cluster$Combination <- rownames(df_cluster)
  df_cluster <- df_cluster %>% gather(Variable, Value, RT_initial:Index)
  
  if(!file.exists(paste(wk_dir, "/", crop, "/cluster_boxplot.png", sep = ""))){
    gg <- ggplot(df_cluster, aes(x = Cluster, y = Value, fill = Cluster)) + geom_boxplot()
    gg <- gg + facet_wrap(~ Variable, scales = 'free')
    gg <- gg + theme_bw() + geom_hline(yintercept = 0)
    ggsave(filename = paste(wk_dir, "/", crop, "/cluster_boxplot.png", sep = ""), plot = gg, width = 16, height = 8, units = "in")
  }
  
  return(cat(paste(crop, " done\n", sep = "")))
  
})

# Multiple Factor Analysis
i = 1
ad_list.MFA <- lapply(1:length(ad_list), function(i){
  
  # Filter data by crop
  df <- ad_list[[i]]
  
  # Creating directories
  # wk_dir <- "D:/ToBackup/climate_and_crop_modelling/bid-cc-agricultural-sector/Multivariate_analysis"
  crop <- unique(as.character(df$Crop))
  # if(!dir.exists(paths = paste(wk_dir, "/", crop, sep = ""))){ dir.create(path = paste(wk_dir, "/", crop, sep = "")) }
  
  # Deleting columns with SD = 0
  rownames(df) <- df$ID; df$ID <- df$Crop <- NULL
  df <- Filter(function(x) sd(x) != 0, df)
  colnames(df) <- paste(colnames(df), ".", crop, sep = "")
  
  return(df)
  
})
bind_cols(ad_list.MFA)
do.call(cbind, ad_list.MFA)

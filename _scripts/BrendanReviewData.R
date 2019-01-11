b<- read.csv("C:/Users/CEGONZALEZ/Documents/IMPACT3-Model-ver3.3/OutputFiles/Aggregation/BrendanV3.csv")

sce<- c("NoCC_SSP2", "NoCC_SSP2_8.5")

b<- b %>%dplyr::filter(scenario %in% sce)


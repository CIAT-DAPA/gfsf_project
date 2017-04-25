install.packages("FAOSTAT")

library(devtools)
install_github(repo = "mkao006/FAOSTATpackage", subdir = "FAOSTAT")
#Vignettes and demos are available and please make use of them:
      
vignette(topic = "FAOSTAT")
demo(topic = "FAOSTATdemo")
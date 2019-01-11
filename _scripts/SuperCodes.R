ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
      if (length(new.pkg))
            install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", 
              "scales", "grid", c("plyr","digest","ggplot2",
                                  "colorspace","stringr","RColorBrewer",
                                  "reshape2","zoo","proto","scales","car",
                                  "dichromat","gtable","munsell","labeling",
                                  "Hmisc","rJava","mvtnorm","bitops","rgl",
                                  "foreign","XML","lattice","e1071","gtools",
                                  "sp","gdata","Rcpp","MASS","Matrix","lmtest",
                                  "survival","caTools","multcomp","RCurl","knitr",
                                  "xtable","xts","rpart","evaluate","RODBC","tseries",
                                  "DBI","nlme","lme4","reshape","sandwich","leaps","gplots","abind","randomForest","Rcmdr","coda","maps","igraph","formatR","maptools","RSQLite","psych","KernSmooth","rgdal","RcppArmadillo","effects","sem","vcd","XLConnect","markdown","timeSeries","timeDate","RJSONIO","cluster","scatterplot3d","nnet","fBasics","forecast","quantreg","foreach","chron","plotrix","matrixcalc","aplpack","strucchange","iterators","mgcv","kernlab","SparseM","tree","robustbase","vegan","devtools","latticeExtra","modeltools","xlsx","slam","TTR","quantmod","relimp","akima","memoise"))
ipak(packages)
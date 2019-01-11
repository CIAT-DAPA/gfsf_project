## Find outliers ####
FindOutliers<-function(datost) {lowerq = quantile(datost)[2]
upperq = quantile(datost)[4]
iqr = upperq - lowerq 
#Or use IQR(data)
# we identify extreme outliers
extreme.threshold.upper = (iqr  3) + upperq
extreme.threshold.lower = lowerq - (iqr  3)
result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

# use the function to identify outliers
temp <- FindOutliers(f1$mean)
cfOut<- f1[temp,]
cfilesNEt<- f1[-temp,]
#Exports outliers 
write.csv(cfOut,paste("./Results/Outliers_",unique(f1$parametro),"_",unique(f1$sce),".csv",sep = ""))
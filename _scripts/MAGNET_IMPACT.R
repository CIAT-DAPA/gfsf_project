## Article:    Climate change effects on agriculture: Economic responses to biophysical shocks
## Authors:    Gerald C. Nelson, Hugo Valin, Ronald D. Sands, Petr Havlik et al.
## Journal:    PNAS, 2013

## Table and figures script file
## Time-stamp: <2013-11-25 22:25:25 HV>
## Contact: valin@iiasa.ac.at

suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))
suppressMessages(library(curl))
suppressMessages(library(shiny))
suppressMessages(library(TTR))
suppressMessages(library(stats))
suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(networkD3))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(reshape))
suppressMessages(library(forecast))
suppressMessages(library(plm))
suppressMessages(library(texreg))
suppressMessages(library(foreign))
suppressMessages(library(car))
suppressMessages(library(gplots))
suppressMessages(library(tseries))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(lme4))
suppressMessages(library(RColorBrewer))

## DATA
maindirc<- c("//dapadfs/workspace_cluster_6/Socioeconomia/F_and_SF/IMPACT_MAGNET/")


## Dataset imports
data.in <- read.csv("dataArticuloModels.csv") 
data.in$Year <- factor(data.in$Year)

## Variable names
data.in$Var <- levels(data.in$Var)[data.in$Var]
data.in[data.in$Var=="YILD","Var"] <- "YTOT"   # Paper acronym for the YILD variable from AgMIP
data.in[data.in$Var=="XPRP","Var"] <- "PRICE"  # Paper acronym for the XPRP variable from AgMIP
data.in$Var  <- factor(data.in$Var, levels=c("YEXO","YTOT","AREA","PROD","TRSH","CONS","PRICE"))

## Ordered sets
mod <- levels(data.in$Model)
mod1 <- c("IMPACT","MAGNET") # model solo para MAGNET e IMPACT 

i1 <- c("RIC","WHT","CGR","OSD")
scen <- c("S1","S2","S3","S4","S5","S6","S7")
reg <- levels(data.in$Region)
regsel <- c("USA","BRA","FSU","EUR","SSA","CHN","IND","SEA")
regsel.name <- c("United States","Brazil","Former Soviet Union","Europe","Sub-Saharan Africa","China","India","South-East Asia")

## Data sets preparation

data.sel <- subset(data.in, Region != "WLD")
data.sel$Model<- as.character(data.sel$Model)
data.sel.ca<- dplyr::filter(data.sel, Model %in% mod1)
data.sel.wld <- subset(data.in, Region == "WLD")
data.sel.wld.ca<- dplyr::filter(data.sel.wld, Model %in% mod1)

## With variables in column
data.sel.r <- reshape(data.sel.ca, idvar=c("Model","Modtype","Scen","GCM","CRM","Region","Prod","Year"),timevar="Var", direction="wide")
data.sel.r.wld <- reshape(data.sel.wld.ca, idvar=c("Model","Modtype","Scen","GCM","CRM","Region","Prod","Year"),timevar="Var", direction="wide")




## SETTINGS

## Export macro
outdir <- "graphs/"
dir.create(outdir)
cm <- 200 # inches
# width = 800, height =800, units = "px"
fig.create <- function(name) png(paste(outdir,name,".png",sep=""),width = 650, height =520, units = "px") #,height=h*cm,width=w*cm
tab.create <- function(tab,name) write.table(tab,paste(outdir,name,".csv",sep=""),sep=",",row.names=F,quote=F)

## Other macro
filter <- function(data,cutoff) subset(data, Val <= quantile(data$Val, 1-cutoff) & Val >= quantile(data$Val,cutoff))
sign.star <- function(v) symnum(v, corr = FALSE, na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))




## RESULTS: FIGURES & TABLES

## 1) Main text results

## Fig 2: Results distribution by variable: all models, all regions, all crops

fig.create("Fig2")
par(mar=c(4,4,1,1),mfrow=c(1,1),cex=0.5,lwd=0.7,las=2,axis.lwd=0.7)
data.sel.wb <- subset(data.sel, Var=="")
varsel <- levels(data.sel$Var)
# v=1
for (v in varsel){
      data.sel.ext <- filter(subset(data.sel, Var==v),0.05)
      data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
}
plot(NA,
     xlim=c(0,8),
     ylim=c(-60,60),
     xlab="",
     ylab="Percent change",
     axes=F)
abline(h=0,lty=3,lwd=0.8)
axis(2,lwd=0.7,at=-3:3*20)
boxplot(100*Val ~ Var,las=2,outline=T,range=5,
        data=data.sel.wb,
        axes=F,
        col="white smoke",
        add=T)
mtext("n",side=1,line=1,at=0,cex=0.4,las=1,adj=0)
mtext("Mean",side=1,line=2,at=0,cex=0.4,las=1,adj=0)
mtext("SD",side=1,line=3,at=0,cex=0.4,las=1,adj=0)
for (v in varsel){
      lines(c(match(v,varsel)-0.4,match(v,varsel)+0.4),100*rep(mean(subset(data.sel, Var==v)$Val),2),lty="21",col="red2",lwd=0.5)
      mtext(paste(length(subset(data.sel, Var==v)$Val),sep=""),side=1,line=1,at=match(v,varsel),cex=0.4,las=1)
      mtext(round(mean(subset(data.sel, Var==v)$Val),2),side=1,line=2,at=match(v,varsel),cex=0.4,las=1)
      mtext(paste("(",round(sd(subset(data.sel, Var==v)$Val),2),")",sep=""),side=1,line=3,at=match(v,varsel),cex=0.4,las=1)
}
mtext(levels(data.sel.wb$Var),side=1,line=0,at=1:7,cex=0.5,las=1)
dev.off()


## Fig 3: graphical representation of regressions on YEXO by model

fig.create("Fig3")
par(las=1,cex.main=0.9,cex.lab=0.6,cex.axis=0.6,lwd=0.7)
layout(cbind(12,rbind(matrix(1:10,2,5,byrow=T),11)),widths=c(0.5,rep(1,5)),heights=c(2,2,0.2))
par(mex=0.6,mar=c(1.5,1.8,3,1))
varsel <- c("YTOT","AREA","PROD","TRSH","CONS","PRICE")
colcode <- c("black","green4","blue","purple","red2","orange3")
ltcode <- c("21","dashed","solid","solid","solid","24")
lwcode <- c(1,1,1.5,0.8,1.5,0.8)
for (m in mod){
      if (match(m,mod)==5){
            plot(NA,axes=F,xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="")
            par(xpd=T)
            text(-0.8,1.15,"Points",cex=0.8,adj=0)
            legend(-0.8,1.1,"PROD",col=grey(0.7),pch=1,bty="n",cex=0.8,pt.cex=0.7)
            text(-0.8,0.50,"Regression lines",cex=0.8,adj=0)
            legend(-0.8,0.45,c("YEXO",varsel),col=c("black",colcode),lty=c("solid",ltcode),lwd=c(1,lwcode),bty="n",cex=0.8)
            par(xpd=F)
      }
      data.sel3 <- subset(data.sel.r, Model == m)
      if (dim(data.sel3)[1]==0) next ## no data
      if (prod(is.na(data.sel3$Val.YEXO))) next  ## no YEXO
      plot(100*data.sel3$Val.YEXO,100*data.sel3$Val.PROD,xlim=c(-75,75),ylim=c(-75,75),main=if (m=="MAGPIE") "MAgPIE" else m,col=grey(0.7),
           xlab="YEXO (percent change)",ylab="Percent change",cex=0.4,axes=F)
      box()
      axis(1,lwd=0.7)
      axis(2,lwd=0.7)
      abline(0,1,col="black",lwd=1)
      points(0,0,pch=3,cex=0.7)
      for (v in varsel){
            ncol <- match(paste("Val",v,sep="."),names(data.sel3))
            if(length(data.sel3[,ncol][!is.na(data.sel3[,ncol])])>0){
                  regress <- lm(data.sel3[,ncol] ~ Val.YEXO, data = data.sel3)
                  abline(regress$coef[1]*100,regress$coef[2], col=colcode[match(v,varsel)], lty=ltcode[match(v,varsel)],lwd=lwcode[match(v,varsel)])
            }}
      if(match(m,mod) %in% c(1,5)) mtext("Percent change",side=2,line=3,cex=0.5,las=0)
      if(match(m,mod) %in% 5:9) mtext("YEXO (percent change)",side=1,line=3,cex=0.5)
}
dev.off()


## 2) Main text regression tables

## Tab 1: analysis of variance

anova.var <- list()
varsel <- c("YEXO","YTOT","AREA","PROD","TRSH","CONS","PRICE")
v=1
for (v in varsel){
      anova.var[[v]] <- anova(lm(Val ~ Model + GCM + CRM + Prod + Region, subset(data.sel.ca, Var==v)))
      anova.var[[v]]$Var <- v
      anova.var[[v]]$Name <- row.names(anova.var[[v]])
}
anova.tab <- anova.var[["YEXO"]]
for (v in varsel[-1]) anova.tab <- rbind(anova.tab,anova.var[[v]])
anova.tab$Star <- mapply(sign.star,anova.tab["Pr(>F)"])
anova.tab$Star <- factor(anova.tab$Star)
anova.tab.r <- reshape(anova.tab,idvar=c("Name","Df"),timevar=c("Var"),drop=c("F value","Pr(>F)"),direction="wide")
tab.create(anova.tab.r,"Tab1")


## Tab 2: regression on YEXO by model

## new variable: YENDO
data.sel.r$Val.YENDO <- (1+data.sel.r$Val.YTOT)/(1+data.sel.r$Val.YEXO)-1

lm.table <- data.frame(Model=c(rep(mod1,each=2),rep("All",2)),Type=c(rep(c("Int","Slope"),3)),
                       YTOT=NA,YTOT.s=NA,YENDO=NA,YENDO.s=NA,AREA=NA,AREA.s=NA,PROD=NA,PROD.s=NA,TRSH=NA,TRSH.s=NA,CONS=NA,CONS.s=NA,PRICE=NA,PRICE.s=NA)
iv <- "YEXO"
vlist <- c("YTOT","YENDO","AREA","PROD","TRSH","CONS","PRICE")
# v=1
for (v in vlist){
      n.l <- match(paste("Val.",v,sep=""),names(data.sel.r))
      n.r <- match(paste("Val.",iv,sep=""),names(data.sel.r))
      for (m in 1:2){
            lm.dat <- subset(data.sel.r, Model==mod1[m])
            lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r])
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="Int",2*match(v,vlist)+1] <- round(lm.mod$coef[1],3)
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="Slope",2*match(v,vlist)+1] <- round(lm.mod$coef[2],3)
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="Int",2*match(v,vlist)+2] <- sign.star(summary(lm.mod)$coef[1,4])
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="Slope",2*match(v,vlist)+2] <- sign.star(summary(lm.mod)$coef[2,4])
      }
      lm.dat <- data.sel.r
      lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r])
      lm.table[lm.table$Model=="All" & lm.table$Type=="Int",2*match(v,vlist)+1] <- round(lm.mod$coef[1],3)
      lm.table[lm.table$Model=="All" & lm.table$Type=="Slope",2*match(v,vlist)+1] <- round(lm.mod$coef[2],3)
      lm.table[lm.table$Model=="All" & lm.table$Type=="Int",2*match(v,vlist)+2] <- sign.star(summary(lm.mod)$coef[1,4])
      lm.table[lm.table$Model=="All" & lm.table$Type=="Slope",2*match(v,vlist)+2] <- sign.star(summary(lm.mod)$coef[2,4])
}
tab.create(lm.table,"Tab2")



## SUPPLEMENTARY INFORMATION

## 1) Supplementary figures

## Fig S1: Fig 1 at world level only

fig.create("FigS1")
par(mar=c(6,4,2,2),mfrow=c(1,1),cex=0.6,lwd=0.7,las=2,axis.lwd=0.7)
data.sel.wb <- subset(data.sel.wld.ca, Var=="")
varsel <- levels(data.sel.wld$Var)
for (v in varsel){
      data.sel.ext <- filter(subset(data.sel.wld, Var==v),0.05)
      data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
}
plot(NA,
     xlim=c(0,8),
     ylim=c(-60,60),
     xlab="",
     ylab="Percent change",
     axes=F)
abline(h=0,lty=3,lwd=0.8)
axis(2,lwd=0.7,at=-3:3*20)
par(xpd=T)
boxplot(100*Val ~ Var,las=2,outline=T,range=5,
        data=data.sel.wb,
        axes=F,
        col="white smoke",
        add=T)
par(xpd=F)
mtext("n",side=1,line=1,at=0,cex=0.4,las=1,adj=0)
mtext("Mean",side=1,line=2,at=0,cex=0.4,las=1,adj=0)
mtext("SD",side=1,line=3,at=0,cex=0.4,las=1,adj=0)
for (v in varsel){
      lines(c(match(v,varsel)-0.4,match(v,varsel)+0.4),100*rep(mean(subset(data.sel.wld, Var==v)$Val),2),lty="21",col="red2",lwd=0.5)
      mtext(paste(length(subset(data.sel.wld, Var==v)$Val),sep=""),side=1,line=1,at=match(v,varsel),cex=0.4,las=1)
      mtext(round(mean(subset(data.sel.wld, Var==v)$Val),2),side=1,line=2,at=match(v,varsel),cex=0.4,las=1)
      mtext(paste("(",round(sd(subset(data.sel.wld, Var==v)$Val),2),")",sep=""),side=1,line=3,at=match(v,varsel),cex=0.4,las=1)
}
mtext(levels(data.sel.wb$Var),side=1,line=0,at=1:7,cex=0.5,las=1)
dev.off()


## Fig S2: Fig 1 by model

fig.create("FigS2")
par(mar=c(4,4,1,1),mfrow=c(1,2),cex=0.9,lwd=0.9,las=2,axis.lwd=0.7)
for (m in mod1){
      data.sel.wb <- subset(data.sel.ca, Var=="")
      varsel <- levels(data.sel$Var)
      for (v in varsel){
            data.sel.ext <- filter(subset(data.sel, Var==v & Model==m),0.05)
            data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
      }
      plot(NA,
           xlim=c(0,8),
           ylim=c(-75,100),
           xlab="",
           ylab="Percent change",
           axes=F)
      abline(h=0,lty=3,lwd=0.8)
      axis(2,lwd=0.8,at=-3:4*30)
      boxplot(100*Val ~ Var,las=2,outline=T,range=1E4,
              data=data.sel.wb,
              axes=F,
              col="white smoke",
              main=m,
              add=T)
      mtext(levels(data.sel.wb$Var),side=1,line=0,at=1:7,cex=0.8,las=1)
      mtext("n =",side=1,line=1,at=0,cex=0.8,las=1,adj=0)
      for (v in varsel){
            mtext(paste(length(subset(data.sel, Var==v & Model == m)$Val),sep=""),side=1,line=1,at=match(v,varsel),cex=0.8,las=1)
      }}
dev.off()


## Fig S3: Fig 1 by crop

# fig.create("FigS3")
# par(mar=c(4,4,2,2),mfrow=c(2,2),cex=0.6,lwd=0.7,las=2,axis.lwd=0.7)
# for (i in i1){
#       data.sel.wb <- subset(data.sel.ca, Var=="")
#       varsel <- levels(data.sel.ca$Var)
#       for (v in varsel){
#             data.sel.ext <- filter(subset(data.sel.ca, Var==v & Prod==i),0.05)
#             data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
#       }
#       plot(NA,
#            xlim=c(0,8),
#            ylim=c(-50,75),
#            xlab="",
#            ylab="Percent change",
#            axes=F)
#       abline(h=0,lty=3,lwd=0.8)
#       axis(2,lwd=0.7,at=-2:3*25)
#       boxplot(100*Val ~ Var,las=2,outline=T,range=1E4,
#               data=data.sel.wb,
#               axes=F,
#               col="white smoke",
#               main=i,
#               add=T)
#       mtext(levels(data.sel.wb$Var),side=1,line=0,at=1:7,cex=0.5,las=1)
#       mtext("n =",side=1,line=1,at=0,cex=0.4,las=1,adj=0)
#       for (v in varsel){
#             mtext(paste(length(subset(data.sel.ca, Var==v & Prod == i)$Val),sep=""),side=1,line=1,at=match(v,varsel),cex=0.4,las=1)
#       }}
# dev.off()
# 

## Fig S4: Fig 1 by region selection
## By region
fig.create("FigS4")
par(mar=c(4,4,2,2),mfrow=c(2,4),cex=0.6,lwd=0.7,las=2,axis.lwd=0.7)
for (r in regsel) {
      data.sel.wb <- subset(data.sel.ca, Var=="")
      varsel <- levels(data.sel.ca$Var)
      for (v in varsel){
            data.sel.ext <- filter(subset(data.sel.ca, Var==v & Region==r),0.05)
            data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
      }
      plot(NA,
           xlim=c(0,8),
           ylim=c(-50,100),
           xlab="",
           ylab="Percent change",
           axes=F)
      abline(h=0,lty=3,lwd=0.8)
      axis(2,lwd=0.7,at=-2:4*25)
      boxplot(100*Val ~ Var,las=2,outline=T,range=1E4,
              data=data.sel.wb,
              axes=F,
              col="white smoke",
              main=regsel.name[match(r,regsel)],
              add=T)
      mtext(levels(data.sel.wb$Var),side=1,line=0,at=1:7,cex=0.4,las=1)
      mtext("n =",side=1,line=1,at=0,cex=0.4,las=1,adj=0)
      for (v in varsel){
            mtext(paste(length(subset(data.sel.ca, Var==v & Region == r)$Val),sep=""),side=1,line=1,at=match(v,varsel),cex=0.4,las=1)
      }}
dev.off()


## Fig S5: Fig 1 by model at world level only

fig.create("FigS5")
par(mar=c(4,4,2,2),mfrow=c(2,2),cex=0.6,lwd=0.7,las=2,axis.lwd=0.7)
for (m in mod1){
      data.sel.wb <- subset(data.sel.ca, Var=="")
      varsel <- levels(data.sel.ca$Var)
      for (v in varsel){
            data.sel.ext <- filter(subset(data.sel.wld.ca, Var==v & Model==m),0)
            data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
      }
      plot(NA,
           xlim=c(0,8),
           ylim=c(-75,100),
           xlab="",
           ylab="Percent change",
           axes=F)
      par(xpd=F)
      abline(h=0,lty=3,lwd=0.8)
      axis(2,lwd=0.7,at=-3:4*25)
      par(xpd=T)
      boxplot(100*Val ~ Var,las=2,outline=T,range=1E4,
              data=data.sel.wb,
              axes=F,
              col="white smoke",
              main=m,
              add=T)
      mtext(c(levels(data.sel.wb$Var)),side=1,line=0,at=1:7,cex=0.5,las=1)
      mtext("n =",side=1,line=1,at=0,cex=0.4,las=1,adj=0)
      for (v in varsel){
            mtext(paste(length(subset(data.sel.wld.ca, Var==v & Model == m)$Val),sep=""),side=1,line=1,at=match(v,varsel),cex=0.4,las=1)
      }}
dev.off()


## Fig S6: scatter plot PRICE vs different variables

## additional variable: PENDO
data.sel.r$Val.PENDO <- (1+data.sel.r$Val.YENDO)*(1+data.sel.r$Val.AREA)-1

fig.create("FigS6")
par(mfrow=c(2,2),mex=0.7,cex.axis=0.7,cex.lab=0.7,cex.main=0.9,las=1)
col.xy <- as.data.frame(t(col2rgb(rainbow(9))))
plot(NA,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),xlab="PRICE (relative change)",ylab="Relative change",main="YENDO")
for (m in 1:2)
      points(Val.YENDO~Val.PRICE, subset(data.sel.r,Model==mod1[m]), col=rgb(col.xy[m,],alpha=50,maxColorValue=255),cex=0.3)
abline(lm(Val.YENDO~Val.PRICE+0, data.sel.r),lty=2)
legend(-0.5,0.5,mod1,col=rainbow(9),cex=0.5,pch=1,bty="n")
points(0,0,pch=3)
plot(NA,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),xlab="PRICE (relative change)",ylab="Relative change",main="AREA")
for (m in 1:2)
      points(Val.AREA~Val.PRICE, subset(data.sel.r,Model==mod1[m]), col=rgb(col.xy[m,],alpha=50,maxColorValue=255),cex=0.3)
abline(lm(Val.AREA~Val.PRICE+0, data.sel.r),lty=2)
legend(-0.5,0.5,mod1,col=rainbow(9),cex=0.5,pch=1,bty="n")
points(0,0,pch=3)
plot(NA,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),xlab="PRICE (relative change)",ylab="Relative change",main="PENDO")
for (m in 1:2)
      points(Val.PENDO~Val.PRICE, subset(data.sel.r,Model==mod1[m]), col=rgb(col.xy[m,],alpha=50,maxColorValue=255),cex=0.3)
abline(lm(Val.PENDO~Val.PRICE+0, data.sel.r),lty=2)
legend(-0.5,0.5,mod,col=rainbow(9),cex=0.5,pch=1,bty="n")
points(0,0,pch=3)
plot(NA,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),xlab="PRICE (relative change)",ylab="Relative change",main="CONS")
for (m in 1:2)
      points(Val.CONS~Val.PRICE, subset(data.sel.r,Model==mod1[m]), col=rgb(col.xy[m,],alpha=50,maxColorValue=255),cex=0.3)
abline(lm(Val.CONS~Val.PRICE+0, data.sel.r),lty=2)
legend(-0.5,0.5,mod,col=rainbow(9),cex=0.5,pch=1,bty="n")
points(0,0,pch=3)
dev.off()


## Macro fig S7-S16

graph.var.mod <- function(varname,gname,dataset,yscale,outliers=F){
      fig.create(gname)
      par(mfrow=c(1,3),cex=1,lwd=1,cex.axis=0.8,las=2) #mar=c(5,5,5,5),
      plot(NA,
           xlim=c(2,8*2),
           ylim=yscale,
           xlab="",
           ylab="Percent change",
           axes=F)
      data.sel.g <- droplevels(subset(dataset, Var==varname))
      data.sel.g$Scen <- factor(data.sel.g$Scen, levels=scen)
      data.sel.wb <- subset(data.sel.g, Var=="")
      for (m in mod1){
            for (s in scen){
                  data.sel.ext <- subset(data.sel.g, Scen==s & Model==m)
                  if (!outliers) data.sel.ext <- filter(subset(data.sel.g, Scen==s & Model==m), 0.05)
                  data.sel.wb <- rbind(data.sel.wb,data.sel.ext)
            }}
      scenname.map <- data.frame(Scen=c(scen,""),NScen=c("S2","S1","S4","S3","S5","S6","S7",""))
      data.sel.wb2 <- merge(data.sel.wb,scenname.map,by="Scen")
      GCM.legend <- unique(data.sel.wb2[order(data.sel.wb2$NScen),c("NScen","CRM","GCM")])
      GCM.legend$lab <- paste(GCM.legend$NScen,": ",GCM.legend$CRM," x ",GCM.legend$GCM,sep="")
      abline(h=0,lty=1,lwd=0.5)
      abline(v=1+8*1:8,lty=1,lwd=0.5,col=grey(0.8))
      par(xpd=T)
      boxplot(100*Val ~ NScen + Model,las=2,outline=T,range=1E4,
              data=data.sel.wb2 ,
              axes=F,
              col="white smoke",
              add=T)
      par(xpd=F)
      axis(2,lwd=0.8)
      yleg <- if (varname %in% c("AREA","PRICE","PENDO","YENDO")) yscale[1]*0.2 else yscale[2]
      legend(0,yleg,GCM.legend$lab,bg="white")
      mtext(levels(data.sel.wb2$NScen),side=1,line=1,at=1:8,cex=0.8,las=2,adj=0)
      mtext(levels(data.sel.wb2$Model),side=1,line=2,at=-3+8*1:8,cex=0.8,las=1)
      for (m in mod1)
            mtext(paste("(n = ",round(length(subset(data.sel.wb2, Model == m & Var == varname)$Val)/7),")",sep=""),side=1,line=3,at=(match(m,mod1)-1)*8+5,cex=1,las=1)
      dev.off()
}


## Fig S7-S16

## YENDO & PENDO in results long table
data.sel2 <- reshape(data.sel.r, idvar=c("Model","Modtype","Scen","GCM","CRM","Region","Prod","Year"),
                     varying=list(paste("Val",c(varsel,"YENDO","PENDO"),sep=".")),
                     times=c(varsel,"YENDO","PENDO"),
                     timevar="Var",v.names="Val",direction="long")


graph.var.mod("YEXO","FigS7",data.sel.ca,c(-70,70))
graph.var.mod("YTOT","FigS8",data.sel.ca,c(-100,100))
graph.var.mod("YENDO","FigS9",data.sel2,c(-100,100))
graph.var.mod("AREA","FigS10",data.sel.ca,c(-99,99))
graph.var.mod("PROD","FigS11",data.sel.ca,c(-70,70))
graph.var.mod("PENDO","FigS12",data.sel2,c(-100,100))
graph.var.mod("TRSH","FigS13",data.sel.ca,c(-70,70))
graph.var.mod("TRSH","FigS14",data.sel.ca,c(-20,20),outliers=T)
graph.var.mod("CONS","FigS15",data.sel.ca,c(-30,30))
graph.var.mod("PRICE","FigS16",data.sel.ca,c(-120,120))


## 2) Supplementary tables

## Table S3-S5: correlation matrix

varsel <- c("YEXO","YTOT","AREA","PROD","TRSH","CONS","PRICE")
varsel2 <- c(varsel[1],"YENDO",varsel[2:4],"PENDO",varsel[5:7])

corr.matrix <- matrix(NA,length(varsel2),length(varsel2))
colnames(corr.matrix) <- varsel2
rownames(corr.matrix) <- varsel2
for(v in varsel2)
      for (v2 in varsel2)
            if (match(v,varsel2)>=match(v2,varsel2))
                  corr.matrix[v,v2] <- round(cor(data.sel2[data.sel2$Var==v,'Val'],data.sel2[data.sel2$Var==v2,'Val']),3) else corr.matrix[v,v2] <- ""

corr.matrix.m <- list()
for (m in mod){
      corr.matrix.m[[m]] <- matrix(NA,length(varsel2),length(varsel2))
      colnames(corr.matrix.m[[m]]) <- varsel2
      rownames(corr.matrix.m[[m]]) <- varsel2
      for(v in varsel2)
            for (v2 in varsel2)
                  if (match(v,varsel2)>=match(v2,varsel2))
                        corr.matrix.m[[m]][v,v2] <- round(cor(data.sel2[data.sel2$Var==v & data.sel2$Model==m,'Val'],data.sel2[data.sel2$Var==v2 & data.sel2$Model==m,'Val']),3)
      else corr.matrix.m[[m]][v,v2] <- ""
}

tab.corr <- cbind(data.frame(Model="All",Var=varsel2),corr.matrix)
for (m in mod) tab.corr <- rbind(tab.corr,cbind(data.frame(Model=m,Var=varsel2),corr.matrix.m[[m]]))
tab.create(tab.corr,"TabS3_S5")


## Tab S6: regression on prices

lm.table3 <- data.frame(Model=c(mod1,"GE","PE","All"),Type=rep("Coef",5),YENDO=NA,YENDO.r2=NA,AREA=NA,AREA.r2=NA,PENDO=NA,PENDO.r2=NA,TRSH=NA,TRSH.r2=NA,CONS=NA,CONS.r2=NA)
iv <- "PRICE"
vlist <- c("YENDO","AREA","PENDO","TRSH","CONS")
for (v in vlist){
      n.l <- match(paste("Val.",v,sep=""),names(lm.dat))
      n.r <- match(paste("Val.",iv,sep=""),names(lm.dat))
      for (m in 1:2){
            lm.dat <- subset(data.sel.r, Model==mod1[m])
            lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r]+0)
            lm.table3[lm.table3$Model==mod1[m] & lm.table3$Type=="Coef",2*match(v,vlist)+1] <- round(lm.mod$coef[1],3)
            lm.table3[lm.table3$Model==mod1[m] & lm.table3$Type=="Coef",2*match(v,vlist)+2] <- round(summary(lm.mod)$adj.r.squared,3)
      }
      for (group in c("GE","PE")){
            lm.dat <- subset(data.sel.r, Modtype==group)
            lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r]+0)
            lm.table3[lm.table3$Model==group & lm.table3$Type=="Coef",2*match(v,vlist)+1] <- round(lm.mod$coef[1],3)
            lm.table3[lm.table3$Model==group & lm.table3$Type=="Coef",2*match(v,vlist)+2] <- round(summary(lm.mod)$adj.r.squared,3)
      }
      lm.dat <- data.sel.r
      lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r]+0)
      lm.table3[lm.table3$Model=="All" & lm.table3$Type=="Coef",2*match(v,vlist)+1] <- round(lm.mod$coef[1],3)
      lm.table3[lm.table3$Model=="All" & lm.table3$Type=="Coef",2*match(v,vlist)+2] <- round(summary(lm.mod)$adj.r.squared,3)
}
tab.create(lm.table3,"TabS6")


## Tab S7: summary statistics table 2

lm.table <- data.frame(Model=c(rep(mod,each=3),rep("All",3)),Type=c(rep(c("N","F-stat","Adj_R2"),10)),
                       YTOT=NA,YENDO=NA,AREA=NA,PROD=NA,TRSH=NA,CONS=NA,PRICE=NA)
iv <- "YEXO"
vlist <- c("YTOT","YENDO","AREA","PROD","TRSH","CONS","PRICE")
for (v in vlist){
      n.l <- match(paste("Val.",v,sep=""),names(data.sel.r))
      n.r <- match(paste("Val.",iv,sep=""),names(data.sel.r))
      for (m in 1:2){
            lm.dat <- subset(data.sel.r, Model==mod1[m])
            lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r])
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="N",2+match(v,vlist)] <- length(lm.mod$residuals)
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="F-stat",2+match(v,vlist)] <- round(summary(lm.mod)$fstatistic[1],3)
            lm.table[lm.table$Model==mod1[m] & lm.table$Type=="Adj_R2",2+match(v,vlist)] <- round(summary(lm.mod)$adj.r.squared,3)
      }
      lm.dat <- data.sel.r
      lm.mod <- lm(lm.dat[,n.l]~lm.dat[,n.r])
      lm.table[lm.table$Model=="All" & lm.table$Type=="N",2+match(v,vlist)] <- length(lm.mod$residuals)
      lm.table[lm.table$Model=="All" & lm.table$Type=="F-stat",2+match(v,vlist)] <- round(summary(lm.mod)$fstatistic[1],3)
      lm.table[lm.table$Model=="All" & lm.table$Type=="Adj_R2",2+match(v,vlist)] <- round(summary(lm.mod)$adj.r.squared,3)
}
tab.create(lm.table,"TabS7")


## Tab S8: Regression table 2 for all models + PE/CGE effect

varsel <- c("YTOT","YENDO","AREA","PROD","TRSH","CONS","PRICE")
reg.list <- list()
for (v in varsel){
      ncol <- match(paste("Val.",v,sep=""),names(data.sel.r))
      lmod <- lm(data.sel.r[,ncol] ~ Val.YEXO * Modtype, data.sel.r)
      reg.var <- as.data.frame(summary(lmod)$coef)
      reg.var$Var <- v
      reg.var$Name <- c("Int","Slope","PE Int","PE Slope")
      rSq<-summary(lmod)$adj.r.squared
      nRg<-length(lmod$residuals)
      Fval <- summary(lmod)$fstatistic[1]
      reg.stat <-data.frame(Estimate = as.numeric(c(nRg,Fval,rSq)),NA,Var=v,Name=c("N","F-statistic","Adjusted R-sq"))
      names(reg.stat)<-names(reg.var)[c(1,4,5,6)]
      reg.list[[v]] <- rbind(reg.var[c(1,4,5,6)],reg.stat)
}
lm.table <- data.frame(Name=reg.list[[1]]$Name,
                       YTOT=NA,YTOT.s=NA,YENDO=NA,YENDO.s=NA,AREA=NA,AREA.s=NA,PROD=NA,PROD.s=NA,TRSH=NA,TRSH.s=NA,CONS=NA,CONS.s=NA,PRICE=NA,PRICE.s=NA)
for (v in varsel){
      lm.table[,2*match(v,varsel)] <- reg.list[[v]]$Estimate
      lm.table[,2*match(v,varsel)+1] <- mapply(sign.star,reg.list[[v]][["Pr(>|t|)"]])
}
tab.create(lm.table,"TabS8")



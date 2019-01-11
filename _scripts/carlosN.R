
require(raster)
require(rgdal)
library(maptools)


dirbase <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/Future/multimodel_mean"
mask1 <- readShapePoly("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/admin_boundaries/adminFiles/10m-admin-0-countries.shp")

iDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/bc_extracts/plots"
climPath <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/Future"
shp <- readShapePoly("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/admin_boundaries/adminFiles/10m-admin-0-countries.shp")
models <- c('multimodel_mean')

# List of seasons
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
# shp <- readOGR("S:/admin_boundaries/adminFiles/continental/SHP-files-continents/sa_adm/sa0.shp", layer="sa0")
varList <- c("prec", "tmax", "tmin", "rsds")


regList <- c("Colombia", "Peru", "Uruguay", "Costa Rica", "Mexico", "Panama")
regList="Colombia"
reg="Colombia"

for (reg in regList){
      
      reg_shp <- mask1[mask1$COUNTRY == reg, ]
      
      ## Cut changes
      rsList <- list.files(dirbase, pattern="mod", full.names = TRUE)
      
      outDir <- paste(dirbase, "/by_country/", reg, sep="")
      if (!file.exists(outDir)) {dir.create(outDir, recursive = T)}
      
      
      
      for(rs in rsList){
            rsName <- basename(rs)
            
            if (!file.exists(paste0(outDir, "/", rsName, sep=""))) {
                  rsCrop <- crop(raster(rs), extent(reg_shp))
                  rsMask <- mask(rsCrop, reg_shp)
                  
                  ascWrite <- writeRaster(rsMask, paste0(outDir, "/", rsName, sep=""), overwrite=F)
                  cat(paste0(" ", rsName, " cut done\n"))
            }
            
      }
      
      ## Cut changes
      rsList <- list.files(dirbase, pattern="avg", full.names = TRUE)
      
      for(rs in rsList){
            
            rsName <- basename(rs)
            
            if (!file.exists(paste0(outDir, "/", rsName, sep=""))) {
                  rsCrop <- crop(raster(rs), extent(reg_shp))
                  rsMask <- mask(rsCrop, reg_shp)
                  
                  ascWrite <- writeRaster(rsMask, paste0(outDir, "/", rsName, sep=""), overwrite=F)
                  cat(paste0(" ", rsName, " cut done\n"))
            }
            
      }
      
      
      mt <- c()
      
      #    var<- c( "prec")
      var<- c("prec")
      for (var in varList){
            
            tiff(paste(climPath, '/multimodel_mean/by_country/clim_chg_seasonal_', var, "_", reg, ".tif", sep =""), width=1600, height=400, pointsize=8, compression='lzw',res=150)
            
            # bottom, left, top, and right
            par(mfrow = c(1,4), mar=c(0, 2, 2, 0), oma=c(0, 1, 1, 2))
            
            for (j in 1:length(models)){
                  
                  # Loop throught seasons
                  for (i in 1:4){
                        
                        rs <- raster(paste(climPath, "/multimodel_mean/by_country/", reg, "/", var, '_chg_', names(seasons[i]), '_mod.tif',sep=''))
                        
                        if (var == "prec"){
                              
                              brk <- c(-30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30)
                              colfunc <- colorRampPalette(c("red", "white", "blue"))
                              
                        } else if (var == "rsds"){
                              
                              brk <- c(-4, -3, -2, -1.5, -1, -0.5, 0, 0.5, 1, 3, 5, 8)
                              colfunc <- colorRampPalette(c("saddlebrown", "white", "orange"))
                              
                        } else {
                              
                              brk <- c(1, 1.3, 1.6, 1.9, 2.2, 2.5, 2.8, 3.1, 3.5, 3.8, 4.2)
                              colfunc <- colorRampPalette(c("yellow", "red"))
                              
                        }
                        
                        rs[rs < (brk[1])] = brk[1]
                        rs[rs > brk[length(brk)]] = brk[length(brk)]
                        
                        # rs <- mask(crop(rs, extent(shp)), shp)
                        if (i == 4) {
                              plot(rs, breaks=brk, col=colfunc(length(brk) - 1), axes=F, box=F, legend = T)
                             
                        } else {
                              plot(rs, breaks=brk, col=colfunc(length(brk) - 1), axes=F, box=F, legend = F,)
                              # legend(legend = "%",title = "Change",x = "right" )
                              }
                        
                        plot(reg_shp, add=T)
                        
                        # legend(legend = "%",title = "Change",x = "right" )
                        
                        # if (i ==1){mtext(models[[j]], WEST <-2, adj=0.5, line=1, cex=1.1)}
                        if (j==1){title(toupper(names(seasons[i])), line = 1, adj=0.5, cex.main = 1.5)}
                        
                        change <- median(values(rs), na.rm = T)
                        
                        rs <- raster(paste(climPath, "/multimodel_mean/by_country/", reg, "/", var, '_avg_', names(seasons[i]), '.tif',sep=''))
                        
                        future <- median(values(rs), na.rm = T)
                        
                        if (var == "prec"){
                              current <- future / ( abs(100 + change) / 100 )
                        } else {
                              current <- ( future - change)
                        }
                        
                        mt <- rbind(mt, cbind(var, names(seasons[i]), current, change, future))
                        
                        
                        
                  }
                  
            }
            
            dev.off()
            
      }
      
      colnames(mt) <- c("Variable", "Season", "Current", "Change", "Future")
      write.table(mt, paste(climPath, '/multimodel_mean/by_country/clim_chg_seasonal_stats_', reg, ".txt", sep =""), quote = F, row.names = F)
      
      
      
}






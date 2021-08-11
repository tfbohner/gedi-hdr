library(raster)
library(rasterVis)
library(RColorBrewer)
library(viridisLite)
devtools::source_gist('306e4b7e69c87b1826db')

path2 <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Processed drought layers/" 
loc <- c("costa_rica", "colorado", "amazon")
time <- c("3month/", "6month/", "12month/", "24month/")

plot_fun <- function(time, loc){
  drought_slopes <- stack(paste0(path2, time, loc, "_drought_slopes_-1.grd"))
  wet_slopes <- stack(paste0(path2, time, loc, "_surplus_slopes_1.grd"))
  window <- stack(paste0(path2, time, loc, "_window_metrics.grd"))
  big_drought <- stack(paste0(path2, time, loc, "_big_drought_metrics.grd"))
  
  # pdf(file=paste0(path2, time, loc, "_maps.pdf"))
  p <- levelplot(drought_slopes[[1]], margin=F, main="Drought: change in cumulative SPEI")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(drought_slopes[[2]], margin=F, main="Drought: change in minimum SPEI")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(drought_slopes[[3]], margin=F, main="Drought: change in event duration")
  print(diverge0(p, ramp=colorRampPalette(rev(brewer.pal(11, "RdBu")))))

  p <- levelplot(drought_slopes[[4]], margin=F, main="Annual change in interdrought period duration")
  print(diverge0(p, ramp="RdBu"))

  p <- levelplot(wet_slopes[[1]], margin=F, main="Surplus: change in cumulative SPEI")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(wet_slopes[[2]], margin=F, main="Surplus: change in maximum SPEI")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(wet_slopes[[3]], margin=F, main="Surplus: change in event duration")
  print(diverge0(p, ramp='RdBu'))

  p <- levelplot(window[[1]], margin=F, main="Moving window: cumulative drought SPEI")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(window[[2]], margin=F, main="Moving window: number of drought months")
  print(diverge0(p, ramp=colorRampPalette(rev(brewer.pal(11, "RdBu")))))
  p <- levelplot(window[[3]], margin=F, main="Moving window: cumulative surplus SPEI")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(window[[4]], margin=F, main="Moving window: number of surplus months")
  print(diverge0(p, ramp='RdBu'))
  p <- levelplot(window[[5]], margin=F, main="Moving window: number of interdrought months")
  print(diverge0(p, ramp='RdBu'))

  print(levelplot(big_drought[[1]], margin=F, main="Big drought: cumulative SPEI"))
  print(levelplot(big_drought[[2]], margin=F, main="Big drought: minimum SPEI"))
  print(levelplot(big_drought[[3]], margin=F, main="Big drought: duration (months)", par.settings=rasterTheme(region = rev(magma(10)))))
  print(levelplot(big_drought[[4]], margin=F, main="Big drought: time since (years)"))
  # # dev.off()
}


pdf(file=paste0(path2, time[1], loc[1], "_maps.pdf"))
plot_fun(time[1], loc[1])
dev.off()

pdf(file=paste0(path2, time[2], loc[1], "_maps.pdf"))
plot_fun(time[2], loc[1])
dev.off()

pdf(file=paste0(path2, time[3], loc[1], "_maps.pdf"))
plot_fun(time[3], loc[1])
dev.off()

pdf(file=paste0(path2, time[4], loc[1], "_maps.pdf"))
plot_fun(time[4], loc[1])
dev.off()

pdf(file=paste0(path2, time[1], loc[2], "_maps.pdf"))
plot_fun(time[1], loc[2])
dev.off()

pdf(file=paste0(path2, time[2], loc[2], "_maps.pdf"))
plot_fun(time[2], loc[2])
dev.off()

pdf(file=paste0(path2, time[3], loc[2], "_maps.pdf"))
plot_fun(time[3], loc[2])
dev.off()

pdf(file=paste0(path2, time[4], loc[2], "_maps.pdf"))
plot_fun(time[4], loc[2])
dev.off()

pdf(file=paste0(path2, time[1], loc[3], "_maps.pdf"))
plot_fun(time[1], loc[3])
dev.off()

pdf(file=paste0(path2, time[2], loc[3], "_maps.pdf"))
plot_fun(time[2], loc[3])
dev.off()

pdf(file=paste0(path2, time[3], loc[3], "_maps.pdf"))
plot_fun(time[3], loc[3])
dev.off()

pdf(file=paste0(path2, time[4], loc[3], "_maps.pdf"))
plot_fun(time[4], loc[3])
dev.off()





  # 
# drought1 <- stack(paste0(path2, "cropped_spei_intermediate/", time, loc, "_all_spei.grd"))
# NAvalue(drought1)
# 
# plot(drought[[1]])
# plot(drought[[445]])
# 
# test <- crop(drought, c(-75, -70, 0, 5))
# plot(test[[1]])
# 
# test <- stack(test)
# 
# 
# 
# test2 <- raster::calc(test, drought_slope_inter)
# plot(test2)
# hist(getValues(test2[[1]]))
# length(which(getValues(test2[[1]])==01))
# 
# plot(getValues(test2[[1]]), getValues(test2[[2]]))
# abline(0,1)
# 
# 
# test2 <- raster::calc(test, mean, na.rm=T)
# plot(test2)
# 
# 
# p <- rasterToPoints(test2[[2]], function(x) x == 1)
# ptest <- p[1,]

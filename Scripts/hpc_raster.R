library(rgdal)
library(raster)

source("scripts/calc_funs.R")

idx <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#idx <- 1

locs <- vector(length = 2)
locs[1] <- "costa_rica"
locs[2] <- "colorado"

loc <- locs[idx]

path <- "input/"
path2 <- "output/"

times <- c("3month", "6month", "12month", "24month") 

for(time in unique(times)){
    filelist <- list.files(paste0(path, loc, "/", time, "/"))
    filelist <- filelist[grep(".grd", filelist)]
    print(filelist)
    
    #for(f in filelist){
    #    temp <- stack(paste0(path, loc, "/", time, "/", f))
    #    
    #    if(f==filelist[1]) {
    #    sub_stack <- temp
    #  } else { sub_stack <- stack(sub_stack, temp)}
    #}
    
    sub_stack <- stack(paste0(path, time, "/", loc, "_all_spei.grd")) # old
    #writeRaster(sub_stack, paste0(path, time, "/", loc, "_all_spei.grd"), format="raster", overwrite=T)
    
    drought_slopes <- raster::calc(sub_stack, drought_slope_calc)
    names(drought_slopes) <- c( "drought_sum_slope", "drought_max_slope", "drought_dur_slope", "inter_slope")
    # plot(drought_slopes)
    writeRaster(drought_slopes, paste0(path2, time, "/", loc, "_drought_slopes_-1.grd"), format="raster", overwrite=T)
    print(paste0(path2, time, "/", loc, "_drought_slopes_-1.grd"))
    
    wet_slopes <- raster::calc(sub_stack, wet_slope_calc)
    names(wet_slopes) <- c( "wet_sum_slope", "wet_max_slope", "wet_dur_slope")
    # plot(wet_slopes)
    writeRaster(wet_slopes, paste0(path2, time, "/", loc, "_surplus_slopes_1.grd"), format="raster", overwrite=T)
    print( paste0(path2, time, "/", loc, "_surplus_slopes_1.grd"))
    
    window_slopes <- raster::calc(sub_stack, roll_slope_calc)
    names(window_slopes) <- c("roll_drought_sum", "roll_drought_months", "roll_wet_sum", "roll_wet_months", "roll_inter_months")
    # plot(window_slopes)
    writeRaster(window_slopes, paste0(path2, time, "/", loc, "_window_metrics.grd"), format="raster", overwrite=T)
    print(paste0(path2, time, "/", loc, "_window_metrics.grd"))
    
    big_droughts <- raster::calc(sub_stack, big_drought_calc)
    names(big_droughts) <- c( "big_drought_sum", "big_drought_min", "big_drought_dur", "time_since")
    # plot(big_droughts)
    writeRaster(big_droughts, paste0(path2, time, "/", loc, "_big_drought_metrics.grd"), format="raster", overwrite=T)
    print(paste0(path2, time, "/", loc, "_big_drought_metrics.grd"))

}

    
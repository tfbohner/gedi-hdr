library(raster)

source("calc_funs.R")

locs <- vector(length = 3)
locs[1] <- "costa_rica"
locs[2] <- "colorado"
locs[3] <- "amazon"

idx <- 3 ## choose which location to run
loc <- locs[idx]


## right now input and output dir are the same. don't need to be though
path <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Processed drought layers/" # input
path2 <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Processed drought layers/" # output

sub_stack <- stack(paste0(path, loc, "_all_spei.grd"))

drought_slopes <- raster::calc(sub_stack, drought_slope_calc)
names(drought_slopes) <- c( "drought_sum_slope", "drought_max_slope", "drought_dur_slope", "inter_slope")
# plot(drought_slopes)
writeRaster(drought_slopes, paste0(path2, loc, "_drought_slopes_-1.grd"), format="raster", overwrite=T)

wet_slopes <- raster::calc(sub_stack, wet_slope_calc)
names(wet_slopes) <- c( "wet_sum_slope", "wet_max_slope", "wet_dur_slope")
# plot(wet_slopes)
writeRaster(wet_slopes, paste0(path2, loc, "_surplus_slopes_1.grd"), format="raster", overwrite=T)

window_slopes <- raster::calc(sub_stack, roll_slope_calc)
names(window_slopes) <- c("roll_drought_sum", "roll_drought_months", "roll_wet_sum", "roll_wet_months", "roll_inter_months")
# plot(window_slopes)
writeRaster(window_slopes, paste0(path2, loc, "_window_metrics.grd"), format="raster", overwrite=T)

big_droughts <- raster::calc(sub_stack, big_drought_calc)
names(big_droughts) <- c( "big_drought_sum", "big_drought_min", "big_drought_dur", "time_since")
# plot(big_droughts)
writeRaster(big_droughts, paste0(path2, loc, "_big_drought_metrics.grd"), format="raster", overwrite=T)


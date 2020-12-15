## This script reads and plots the GEDI height data

library(raster)
library(rgdal)

## These are the moments for maximum height
path <- "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/GEDI/"
height_mean <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=1)
plot(height_mean)

height_sd <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=2)
plot(height_sd)

height_skew <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=3)
plot(height_skew)

height_kurt <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=4)
plot(height_kurt)

height_n <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=5)
plot(height_n)

## Read in the annual VPD stack
path <- "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_vpd/"
vpd_time <- stack(paste0(path, "vpd_time_conus.grd"))

slopes <- calc(vpd_time, fun)
plot(slopes)




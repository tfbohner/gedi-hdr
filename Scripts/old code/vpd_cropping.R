### This scripts crops the annual VPD layers to CONUS to match the GEDI data.

library(raster)
library(rgdal)

## GEDI layer for matching
path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/GEDI/"
height_mean <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=1)
plot(height_mean)

## CONUS shapefile
myshp <- readOGR("/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/GIS/shapefiles/cb_2016_us_state_500k/")
us2 <- spTransform(myshp,
            crs(height_mean))
us2 <- crop(us2, extent(height_mean))

height_mean2 <- mask(height_mean, us2)

path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/Global_VPD_from_ERA5_land/"
for(year in 1981:2019) {
  vpd <- raster(x= paste0(path, "VPD", year, ".nc"))
  vpd2 <- rotate(vpd)
  conus <- crop(vpd2, extent(height_mean))
  conus <- mask(conus, us2)
  names(conus)<-paste0("vpd", year)
  
  if(year==1981) {
    vpd_time <- conus
  } else { vpd_time <- stack(vpd_time, conus)}
  
}

path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/conus_vpd/"
writeRaster(vpd_time, paste0(path, "vpd_time_conus.grd"), format="raster", overwrite=T)

vpd_time <- stack(x = paste0(path, "vpd_time_conus.grd"))

time <- 1:nlayers(vpd_time)
fun <- function(x) { lm(x ~ time)$coefficients[2] }

fun <-function(x) {
  if(!is.na(x)) {
    lm(x ~ time)$coefficients[2]
  } else {
    NA
  }
}

slopes <- calc(vpd_time, fun)

plot(slopes)
writeRaster(slopes, paste0(path, "vpd_slope_conus.grd"), format="raster", overwrite=T)




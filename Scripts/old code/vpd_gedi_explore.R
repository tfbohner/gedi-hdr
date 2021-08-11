## This script reads and plots the GEDI height data

library(raster)
library(rgdal)
library(sf)

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


cover <- raster(x = paste0(path, "USA_conus_gedi02_b_cover_0.1x0.1deg.tif"), band=1)
plot(cover)

values(cover)[values(cover) < 0.1] = NA
plot(cover)

## Read in the annual VPD stack
path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/conus_vpd/"
vpd_time <- stack(paste0(path, "vpd_time_conus.grd"))

slopes <- calc(vpd_time, fun)
plot(slopes)

## pretty plot
library("rnaturalearth")
library("rnaturalearthdata")
library(rnaturalearthhires)
library(tidyverse)
library(viridis)
library(ggthemes) # theme_map()
library(rgdal)

## CONUS shapefile
myshp <- readOGR("Google Drive File Stream/My Drive/Teresa Lab Dir/GIS/shapefiles/cb_2016_us_state_500k/")
us2 <- spTransform(myshp,
                   crs(height_mean))
us2 <- crop(us2, extent(height_mean))
height_mean2 <- mask(height_mean, us2)

usa <- ne_states(country="united states of america", returnclass = "sf")
conus <- filter(usa, name!="Alaska", name!="Hawaii")

height_spdf <- as(height_mean2, "SpatialPixelsDataFrame")
height_df <- as.data.frame(height_spdf)
colnames(height_df) <- c("value", "x", "y")


## probably want to clip out great lakes

ggplot() +
  geom_tile(data=height_df, aes(x=x, y=y, fill=value), alpha=0.8) +
  scale_fill_viridis(direction=-1) +
  geom_sf(data=conus, fill=NA, color='black', size=0.3) +
  # coord_equal() +
  theme_map() + theme(legend.position="bottom") +
  ggtitle("GEDI mean height (RH 98, m)") +
  theme(legend.key.width=unit(2, "cm"), legend.justification = "center", legend.title = element_blank())
  
  




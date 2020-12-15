## This script uses the USFS forest type layer (much finer res than the GEDI and VPD layers)
## https://data.fs.usda.gov/geodata/rastergateway/forest_type/
## The only way I could work with them was to strip the attribute table out and re-write the raster
## I did not include the raw or processed layers in the Google Drive, but can easily if you want them.

library(raster)
library(rgdal)

## extents and masks
path <- "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/GEDI/"
height_mean <- raster(x = paste0(path, "USA_conus_gedi02_a_rh98_0.1x0.1deg.tif"), band=1)

myshp <- readOGR("Google Drive File Stream/My Drive/Teresa Lab Dir/GIS/shapefiles/cb_2016_us_state_500k/")
us2 <- spTransform(myshp,
                   crs(height_mean))
us2 <- crop(us2, extent(height_mean))

### Forest group


### Forest type
for_types <- raster("Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forest-type/conus_foresttype.img")
# for_types[for_types==0] <- NA

# strip the attributes
v <- values(for_types)
v[v==0] <- NA
r <- raster(nrow=nrow(for_types),ncol=ncol(for_types))
projection(r)=crs(for_types)
extent(r)=extent(for_types)
r[]<-v
plot(r)
writeRaster(r, filename = "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forest-type/foresttype.tif")

# r <- raster("Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forest-type/foresttype.tif")
for_types <- r

type_reproj <- projectRaster(for_types, 
                             crs=crs(height_mean))
writeRaster(type_reproj, filename = "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forest-type/foresttype_wgs84.tif", overwrite=T)

fortype2 <- reclassify(type_reproj, cbind(0, NA))
plot(fortype2)

for_agg <- aggregate(fortype2, fac=40, fun=modal, na.rm=T)
plot(for_agg)

### Forest group
for_group <- raster("Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forestgroup/conus_forestgroup.img")
v <- values(for_group)
v[v==0] <- NA
r <- raster(nrow=nrow(for_group),ncol=ncol(for_group))
projection(r)=crs(for_group)
extent(r)=extent(for_group)
r[]<-v
plot(r)
writeRaster(r, filename = "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forestgroup/forestgroup.tif")
for_group <- raster("Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forestgroup/forestgroup.tif")

type_reproj <- projectRaster(for_group, 
                             crs=crs(height_mean))
writeRaster(type_reproj, filename = "Google Drive File Stream/My Drive/Teresa Lab Dir/HDR big trees/conus_forestgroup/forestgroup_wgs84.tif", overwrite=T)


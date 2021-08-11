## Get shapefile for costa rica, colorado, amazon
## pull together all spei layers, crop for costa rica, colorado, amazon
#library(rnaturalearth)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(ncdf4)

idx <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

years <- seq(1981, 2019, by=1)
year <- years[idx]

path <- "/xdisk/benquist/tbohner/" # input
path2 <- "input/" # output

sub <- vector(mode='list', length=3)
sub_spei <- vector(mode='list', length=3)
loc <- vector(length = 3)

# ## Run this for Costa Rica
sub[[1]] <- readOGR(
  paste0(path2, "costa_rica_shp/costa_rica.shp"))
loc[1] <- "costa_rica"

## Run this for Colorado
sub[[2]] <- readOGR(
  paste0(path2, "colorado_shp/colorado.shp"))
loc[2] <- "colorado"

## Run this for Amazon
sub[[3]] <- readOGR(
  paste0(path2, "amapoly_ivb/amapoly_ivb.shp"))
loc[3] <- "amazon"


files <- c("SPEI_3/Global_SPEI3_", "SPEI_6/Global_SPEI6_", "SPEI_12/Global_SPEI12_", "SPEI_24/Global_SPEI24_") ## can iterate over timescales later
scale <- c("3month", "6month", "12month", "24month")

#files <- c("SPEI_24/Global_SPEI24_") ## can iterate over timescales later
#scale <- c("24month")


## Rotating the raster is the slow step. All the spei files are long -0.5 - 355.5, 
    ## all polygons are -180-180 long. There's probably a better/faster way to do this
print(year)

for(s in 1:length(scale)){

    skip_to_next <- FALSE
    
    tryCatch(spei <- raster::stack(x= paste0(path, files[s], year, ".nc")), error = function(e) {skip_to_next <<- TRUE})
    
    if(skip_to_next==TRUE) {
    print("skipped")
    next 
    } else {
        print("read")
        spei2 <- rotate(spei)
        print("rotated")
    
        for(i in 1:length(loc)){
            sub_spei[[i]] <- crop(spei2, sub[[i]]) %>%  mask(sub[[i]])
            writeRaster(sub_spei[[i]], paste0(path2,loc[i],"/", scale[s], "/", year, "_spei.grd"), format="raster", overwrite=T)
        }

        print("cropped")
        
    }

}



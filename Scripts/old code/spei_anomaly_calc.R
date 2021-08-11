## Get shapefile for costa rica, colorado
## pull together all spei layers, crop for costa rica, colorado
## mainpulate layers to get time trends for drought, interdrought, surplus
library(rnaturalearth)
library(sp)
library(sf)
library(raster)
library(tidyverse)
library(lubridate)

path <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Global SPEI layers/" # input
path2 <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Processed drought layers/" # output


## Run this for Costa Rica
sub <- ne_countries(country = 'costa rica', returnclass = "sf")
loc <- "costa_rica"

## Run this for Colorado
sub <- ne_states(country = 'united states of america', returnclass = "sf") %>% 
  filter(name=="Colorado")
loc <- "colorado"

## Run this for Amazon
sub <- st_read(
  paste0(path2, "amapoly_ivb/amapoly_ivb.shp"))
loc <- "amazon"

# ggplot() + 
#   geom_sf(data = sub, size = 3, color = "black", fill = "cyan1") + 
#   coord_sf()


files <- "SPEI_12/Global_SPEI12_"


## Rotating the raster is the slow step.
for(year in 1981:2019) {
  print(year)
  spei <- stack(x= paste0(path, files, year, ".nc"))
  print("read")
  spei2 <- rotate(spei)
  print("rotated")
  sub_spei <- crop(spei, sub) %>%  mask(sub)
  print("cropped")

  if(year==1981) {
    sub_stack <- sub_spei
  } else { sub_stack <- stack(sub_stack, sub_spei)}

}


writeRaster(sub_stack, paste0(path2, loc, "_all_spei.grd"), format="raster", overwrite=T)

sub_stack <- raster::stack(paste0(path2, loc, "_all_spei.grd"))

## sanity check
plot(sub_stack[[1]])

sub_stack2 <- crop(sub_stack, extent(c(-80, -75, -10, 0)))

## data frame manipulation to give us:
# 1. n events
# 2. time since most recent drought (start, peak, end)
# 3. drought intensity, duration

## turn raster to data frame, long format
spei_df <- rasterToPoints(sub_stack, spatial = TRUE) %>% 
  data.frame() %>% 
  pivot_longer(cols=-c(x, y, optional), names_to="time", values_to="spei") 

## We need to convert the columns Xn to year and months
dates <- data.frame(time=unique(spei_df$time),
                    date=seq.Date(as.Date("1981-01-01"), by="month", length.out = 457))

spei_dates <- left_join(spei_df, dates)

## consider droughts consectuive negative SPEI when any of the months are <= a threshold
thresh_manip <- function(threshold) {
  spei_dates %>% 
    mutate(sign=ifelse(spei<0, "neg", "pos"), ## is the spei negative
           prev=lag(sign),
           follow=lead(sign),
           start=ifelse(sign=="neg"&prev%in%c("pos", NA), as.character.Date(date), NA)) %>%  # set the start time
    # ) %>% 
    fill(start, .direction = "down") %>% # fill in na values with previous to get a grouping of events of different length
    mutate(start=ifelse(sign=="pos", NA, start)) %>%
    group_by(x, y, start) %>% 
    mutate(min_spei=min(spei),
           start=ifelse(min_spei>threshold, NA, start),
           thresh=threshold) 
}

## list of data frames for each threshold
dry_dat <- thresh_manip(-1)




## Trends through time
time_fun <- function(data){
  temp <- data %>% 
    filter(!is.na(start)) %>% 
    ungroup() %>% group_by(x,y, start) %>% 
    summarize(sum_spei=sum(spei),
              min_spei=min(spei),
              duration=length(spei)) %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(time=as.numeric(str_sub(start, 1,4))) %>% #seq_along(start)
    summarize(slope_sum = lm(sum_spei~time)$coeff[2],
              slope_min = lm(min_spei~time)$coeff[2],
              slope_duration=lm(duration~time)$coeff[2])
  
  stack(rasterFromXYZ(temp[,1:3], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 4)], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs")) 
  
}

drought_slopes <- time_fun(dry_dat) #3:52 to 3:59
writeRaster(drought_slopes, paste0(path2, loc, "_drought_slopes_-1.tif"), format="GTiff", overwrite=T)
drought_slopes <- stack(paste0(path2, loc, "_drought_slopes_-1.tif"))
plot(drought_slopes)

## Inter-drought periods
time_inter <- function(data){
  temp <- data %>% 
    ungroup() %>% 
    mutate(event_follow=lead(start),
           inter_end=ifelse(is.na(start)&!is.na(event_follow), as.character.Date(date), NA)) %>% 
    fill(inter_end, .direction = "up") %>% 
    mutate(inter_end=ifelse(!is.na(start), NA, inter_end)) %>% 
    filter(!is.na(inter_end)) %>% 
    ungroup() %>% group_by(x,y, inter_end) %>% 
    summarize(duration=length(spei)) %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(time=as.numeric(str_sub(inter_end, 1,4))) %>% #seq_along(start)
    summarize(slope_inter_duration=lm(duration~time)$coeff[2])
  
  rasterFromXYZ(temp[,1:3], crs="+proj=longlat +datum=WGS84 +no_defs")
}

inter_slopes <- time_inter(dry_dat)
plot(inter_slopes)

writeRaster(inter_slopes, paste0(path2, loc, "_interdrought_slopes_-1.tif"), format="GTiff", overwrite=T)


## Wet anomalies----
## consider droughts consectuive negative SPEI when any of the months are <= a threshold
thresh_manip_wet <- function(threshold) {
  spei_dates %>% 
    mutate(sign=ifelse(spei<0, "neg", "pos"), ## is the spei negative
           prev=lag(sign),
           follow=lead(sign),
           start=ifelse(sign=="pos"&prev%in%c("neg", NA), as.character.Date(date),NA)) %>%  # set the start time
    # ) %>% 
    fill(start, .direction = "down") %>% # fill in na values with previous to get a grouping of events of different length
    mutate(start=ifelse(sign=="neg", NA, start)) %>%
    group_by(x, y, start) %>% 
    mutate(max_spei=max(spei),
           start=ifelse(max_spei<threshold, NA, start),
           thresh=threshold) 
}

wet_dat <- thresh_manip_wet(1)

## Trends through time
time_fun2 <- function(data){
  temp <- data %>% 
    filter(!is.na(start)) %>% 
    ungroup() %>% group_by(x,y, start) %>% 
    summarize(sum_spei=sum(spei),
              max_spei=max(spei),
              duration=length(spei)) %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(time=as.numeric(str_sub(start, 1,4))) %>% #seq_along(start)
    summarize(slope_sum = lm(sum_spei~time)$coeff[2],
              slope_max = lm(max_spei~time)$coeff[2],
              slope_duration=lm(duration~time)$coeff[2])
  
  stack(rasterFromXYZ(temp[,1:3], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 4)], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs")) 
  
}

wet_slopes <- time_fun2(wet_dat)
plot(wet_slopes)

writeRaster(wet_slopes, paste0(path2, loc, "_surplus_slopes_1.tif"), format="GTiff", overwrite=T)

## Moving window----
library(mbsi)
library(zoo)

## data frame with all events
all_events <- spei_dates %>% 
  group_by(x,y) %>% 
  mutate(month=month(date),
         year=as.numeric(str_sub(date, 1, 4)),
         extremes=find_flood_drought(spei),
         floods = 1 * (extremes == "flood"),
         droughts = 1 * (extremes == "drought"),
         drought_spei=ifelse(droughts==1, spei, NA),
         flood_spei=ifelse(floods==1, spei, NA),
         interdrought=ifelse(floods==1, 1, ifelse(floods==0&droughts==0, 1, 0)),
         roll_drought_months=rollapply(droughts,60,sum,align='right',fill=NA),
         roll_wet_months=rollapply(floods,60,sum,align='right',fill=NA),
         roll_inter_months=rollapply(interdrought,60,sum,align='right',fill=NA),
         roll_drought_sum=rollapply(drought_spei,60,sum, na.rm=T, align='right',fill=NA),
         roll_wet_sum=rollapply(flood_spei,60,sum, na.rm=T, align='right',fill=NA))

# test <- all_events %>% 
#   mutate(siteID=paste0(round(x, digits = 2),",", round(y, digits=2))) %>% 
#   filter(siteID=="-85.6,11.1", month==12) %>% 
#   mutate(yearcount=seq_along(date),
#          year=as.numeric(str_sub(date, 1, 4)))
# 
# library(ggpmisc)
# ggplot(test, aes(year, roll_wet_sum)) +
#   geom_point() +
#   geom_smooth(method='lm')+
#   stat_poly_eq(formula = y~x,
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE)

all_events_slopes <- all_events %>% 
  filter(month==12) %>% 
  ungroup() %>% group_by(x,y) %>% 
  summarize(roll_drought_months = lm(roll_drought_months~year)$coeff[2],
            roll_wet_months = lm(roll_wet_months~year)$coeff[2],
            roll_inter_months = lm(roll_inter_months~year)$coeff[2],
            roll_drought_sum = lm(roll_drought_sum~year)$coeff[2],
            roll_wet_sum = lm(roll_wet_sum~year)$coeff[2])

window_raster <- stack(rasterFromXYZ(all_events_slopes[,c(1:2, 3)], crs="+proj=longlat +datum=WGS84 +no_defs"),
      rasterFromXYZ(all_events_slopes[,c(1:2, 4)], crs="+proj=longlat +datum=WGS84 +no_defs"),
      rasterFromXYZ(all_events_slopes[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs"),
      rasterFromXYZ(all_events_slopes[,c(1:2, 6)], crs="+proj=longlat +datum=WGS84 +no_defs"),
      rasterFromXYZ(all_events_slopes[,c(1:2, 7)], crs="+proj=longlat +datum=WGS84 +no_defs")) 

plot(window_raster)
writeRaster(window_raster, paste0(path2, loc, "_window_metrics.tif"), format="GTiff", overwrite=T)


## Big events----
ref_date <- as.Date("2021-01-01")

big_droughts <- dry_dat %>% 
  filter(!is.na(start)) %>% 
  ungroup() %>% group_by(x,y, start) %>% 
  summarize(end=last(date),
            sum_spei=sum(spei),
            min_spei=min(spei),
            duration=length(spei)) %>% 
  ungroup() %>% group_by(x,y) %>% 
  mutate(bigevent_sum=min(sum_spei),
         time_since=interval(end, ref_date) %>% time_length(unit="year")) %>% 
  filter(sum_spei==bigevent_sum)

big_raster <- stack(rasterFromXYZ(big_droughts[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs"),
                       rasterFromXYZ(big_droughts[,c(1:2, 6)], crs="+proj=longlat +datum=WGS84 +no_defs"),
                       rasterFromXYZ(big_droughts[,c(1:2, 7)], crs="+proj=longlat +datum=WGS84 +no_defs"),
                       rasterFromXYZ(big_droughts[,c(1:2, 9)], crs="+proj=longlat +datum=WGS84 +no_defs")) 

plot(big_raster)
writeRaster(big_raster, paste0(path2, loc, "_big_drought_metrics.tif"), format="GTiff", overwrite=T)
# big_raster <- stack(paste0(path2, loc, "_big_drought_metrics.tif"))

# big_wet <- wet_dat %>%
#   filter(!is.na(start)) %>%
#   ungroup() %>% group_by(x,y, start) %>%
#   summarize(end=last(date),
#             sum_spei=sum(spei),
#             max_spei=max(spei),
#             duration=length(spei)) %>%
#   ungroup() %>% group_by(x,y) %>%
#   mutate(bigevent_sum=max(sum_spei),
#          time_since=interval(end, ref_date) %>% time_length(unit="year")) %>%
#   filter(sum_spei==bigevent_sum)
# 
# big_raster2 <- stack(rasterFromXYZ(big_wet[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs"),
#                     rasterFromXYZ(big_wet[,c(1:2, 6)], crs="+proj=longlat +datum=WGS84 +no_defs"),
#                     rasterFromXYZ(big_wet[,c(1:2, 7)], crs="+proj=longlat +datum=WGS84 +no_defs"),
#                     rasterFromXYZ(big_wet[,c(1:2, 9)], crs="+proj=longlat +datum=WGS84 +no_defs"))
# 
# plot(big_raster2)
# writeRaster(big_raster2, paste0(path2, loc, "_big_surplus_metrics.tif"), format="GTiff", overwrite=T)




library(rasterVis)
library(RColorBrewer)
devtools::source_gist('306e4b7e69c87b1826db')

pdf(file=paste0(path2, loc, "_maps.pdf"))
p <- levelplot(drought_slopes[[1]], margin=F, main="Drought: change in cumulative SPEI")
diverge0(p, ramp='RdBu')
p <- levelplot(drought_slopes[[2]], margin=F, main="Drought: change in minimum SPEI")
diverge0(p, ramp='RdBu')
p <- levelplot(drought_slopes[[3]], margin=F, main="Drought: change in event duration")
diverge0(p, ramp=colorRampPalette(rev(brewer.pal(11, "RdBu"))))

p <- levelplot(inter_slopes, margin=F, main="Annual change in interdrought period duration")
diverge0(p, ramp="RdBu")

p <- levelplot(wet_slopes[[1]], margin=F, main="Surplus: change in cumulative SPEI")
diverge0(p, ramp='RdBu')
p <- levelplot(wet_slopes[[2]], margin=F, main="Surplus: change in maximum SPEI")
diverge0(p, ramp='RdBu')
p <- levelplot(wet_slopes[[3]], margin=F, main="Surplus: change in event duration")
diverge0(p, ramp='RdBu')
dev.off()



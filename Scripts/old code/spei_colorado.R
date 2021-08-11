## Get shapefile for costa rica, 
## pull together all spei layers, crop for costa rica
## mainpulate layers to get drought count, time since, characteristics etc. 
library(rnaturalearth)
library(sp)
library(raster)
library(tidyverse)
library(lubridate)


cr <- ne_countries(country = 'costa rica', returnclass = "sf")
cr <- ne_states(country = 'united states of america', returnclass = "sf") %>% 
  filter(name=="Colorado")

path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/Global SPEI/"
path2 <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/drought layers/"

## See which years throw errors and need new downloads
# for(year in 1981:2019) {
#   skip_to_next <- FALSE
#   tryCatch(spei <- stack(x= paste0(path, "Global_SPEI12_", year, ".nc")), 
#            error = function(e) { skip_to_next <<- TRUE})
#   
#   if(skip_to_next) { 
#     print("error") 
#     next 
#   } else{
#     print(year)
#   }
# }

## This is slow!
for(year in 1994:2019) {
  spei <- stack(x= paste0(path, "Global_SPEI12_", year, ".nc"))
  spei2 <- rotate(spei)
  cr_spei <- crop(spei2, cr) %>%  mask(cr)
  print(year)
  
  if(year==1981) {
    cr_stack <- cr_spei
  } else { cr_stack <- stack(cr_stack, cr_spei)}
  
}

## start at 1994, need to re-do costa rica now. oops

writeRaster(cr_stack, paste0(path, "colorado_all_spei.grd"), format="raster", overwrite=T)
cr_stack <- stack(paste0(path, "colorado_all_spei.grd"))


## data frame manipulation to give us:
  # 1. n events
  # 2. time since most recent drought (start, peak, end)
  # 3. drought intensity, duration

## turn raster to data frame, long format
spei_df <- rasterToPoints(cr_stack, spatial = TRUE) %>% 
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
           start=ifelse(sign=="neg"&prev=="pos", as.character.Date(date), NA)) %>%  # set the start time
    # ) %>% 
    fill(start, .direction = "down") %>% # fill in na values with previous to get a grouping of events of different length
    mutate(start=ifelse(sign=="pos", NA, start)) %>%
    group_by(x, y, start) %>% 
    mutate(min_spei=min(spei),
           start=ifelse(min_spei>threshold, NA, start),
           thresh=threshold) 
}

## list of data frames for each threshold
thresh_list <- list(-1, -1.5, -2)
manip_dfs <- lapply(thresh_list, thresh_manip)


## Following code to count events
count_fun <- function(data){
  data %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(event=as.numeric(as.factor(start))) %>% 
    summarize(max(event, na.rm=T)) %>% 
    rasterFromXYZ(crs="+proj=longlat +datum=WGS84 +no_defs")
}

drought_count <- lapply(manip_dfs, count_fun)
plot(drought_count[[3]], col=pal(10))

## write the files
writeRaster(drought_count[[1]], paste0(path2, "colorado_count_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_count[[2]], paste0(path2, "colorado_count_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_count[[3]], paste0(path2, "colorado_count_drought_-2.tif"), format="GTiff", overwrite=T)

## Following code to extract date info of most recent threshold drought
ref_date <- as.Date("2021-01-01")
date_fun <- function (data) {
  data %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(recent=max(start, na.rm=T)) %>% 
    filter(start==recent) %>% 
    summarize(start=first(start),
              end=max(date),
              peak=date[which.min(spei)],
              start=interval(start, ref_date) %>% time_length(unit="year"),
              end=interval(end, ref_date) %>% time_length(unit="year"),
              peak=interval(peak, ref_date) %>% time_length(unit="year")) %>%
    rasterFromXYZ(crs="+proj=longlat +datum=WGS84 +no_defs")
}

drought_dates <- lapply(manip_dfs, date_fun)

plot(drought_dates[[1]])

writeRaster(drought_dates[[1]], paste0(path2, "colorado_time_recent_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_dates[[2]], paste0(path2, "colorado_time_recent_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_dates[[3]], paste0(path2, "colorado_time_recent_drought_-2.tif"), format="GTiff", overwrite=T)

## Following code to extract intensity, duration info of most recent threshold drought
char_fun <- function(data){
  data %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(recent=max(start, na.rm=T)) %>% 
    filter(start==recent) %>% 
    mutate(end=max(date)) %>%
    summarize(min_spei=first(min_spei),
              avg_spei=median(spei),
              duration=interval(start, end) %>% time_length(unit="months")) %>% 
    rasterFromXYZ(crs="+proj=longlat +datum=WGS84 +no_defs")
}

drought_char <- lapply(manip_dfs, char_fun)


# ggpairs(all_char[,3:9])

plot(drought_char[[1]])

writeRaster(drought_char[[1]], paste0(path2, "colorado_char_recent_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_char[[2]], paste0(path2, "colorado_char_recent_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_char[[3]], paste0(path2, "colorado_char_recent_drought_-2.tif"), format="GTiff", overwrite=T)

## Cumulative drought
char_fun2 <- function(data){
  data %>% 
    filter(!is.na(start)) %>% 
    ungroup() %>% group_by(x,y) %>% 
    summarize(tot_spei=cumsum(spei),
              n_months=n(),
              n_months_thresh=length(which(spei<=thresh))) %>% 
    rasterFromXYZ(crs="+proj=longlat +datum=WGS84 +no_defs")
}

drought_char_all <- lapply(manip_dfs, char_fun2)
writeRaster(drought_char_all[[1]], paste0(path2, "colorado_char_all_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_char_all[[2]], paste0(path2, "colorado_char_all_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_char_all[[3]], paste0(path2, "colorado_char_all_drought_-2.tif"), format="GTiff", overwrite=T)


## Let's explore the data a bit ----
plot_dat <- manip_dfs[[3]] %>% 
  ungroup() %>% group_by(x,y) %>% 
  mutate(recent=max(start, na.rm=T),
         event=as.numeric(as.factor(start)),
         n_event=max(event, na.rm=T)) %>% 
  filter(start==recent) %>% 
  mutate(end=max(date)) %>%
  summarize(n_event=first(n_event), 
            start=first(start),
            end=max(date),
            peak=date[which.min(spei)],
            min_spei=first(min_spei),
            avg_spei=median(spei),
            duration=interval(start, end) %>% time_length(unit="months"),
            start=interval(start, ref_date) %>% time_length(unit="year"),
            end=interval(end, ref_date) %>% time_length(unit="year"),
            peak=interval(peak, ref_date) %>% time_length(unit="year")
            )

plot_dat <- plot_dat %>% 
  ungroup() %>% 
  mutate(time_since = factor(cut(peak, breaks = c(0,5,10, 25, 30,35, Inf)),
                             labels=c("0-5", "5-10", "10-25", "25-30", "30+")))



p1 <- ggplot(plot_dat, aes(min_spei)) +
  geom_histogram() +
  facet_wrap(~time_since) +
  xlab("Minimum SPEI") +
  ylab("Number of grid cells") + 
  ggtitle("Distribution of drought intensity grouped by time since drought")

p2 <- ggplot(plot_dat, aes(time_since, fill=as.factor(n_event))) +
  geom_bar(position = "dodge") +
  labs(fill="Number of droughts") +
  xlab("Time since most recent drought") +
  ylab("Number of grid cells") +
  ggtitle("Number of droughts within the time since recent drought groups")

n_years_ts <-  manip_dfs[[3]] %>% 
  left_join(dplyr::select(plot_dat, c(x,y,time_since))) %>% 
  group_by(time_since, date) %>% 
  summarize(mean_spei=mean(spei))
  
p3 <- ggplot(n_years_ts, aes(date, mean_spei, color=time_since)) +
  geom_line() +
  ylab("Mean SPEI across grid cells")+
  xlab("")+
  labs(color="Years since \n recent drought") +
  ggtitle("Average SPEI time series across time since recent drought groups")

n_event_ts <-  manip_dfs[[3]] %>% 
  left_join(dplyr::select(plot_dat, c(x,y,n_event))) %>% 
  group_by(n_event, date) %>% 
  summarize(mean_spei=mean(spei))

p4 <- ggplot(n_event_ts, aes(date, mean_spei, color=as.factor(n_event))) +
  geom_line() +
  ylab("Mean SPEI across grid cells")+
  xlab("")+
  labs(color="Total drought \n events") +
  ggtitle("Average SPEI time series grouped by total number of droughts")

## lets look at all events
## Following code to extract intensity, duration info of ALL of the threshold drought events
all_events <- manip_dfs[[3]] %>% 
  filter(!is.na(start)) %>% 
  group_by(x,y) %>% 
  mutate(event=as.numeric(as.factor(start))) %>% 
  ungroup() %>% group_by(x,y,start) %>% 
  mutate(end=max(date))%>% 
  summarize(event=first(event),
            min_spei=first(min_spei),
            avg_spei=median(spei),
            duration=interval(start, end) %>% time_length(unit="months")) 



temp <- all_events %>% 
  left_join(dplyr::select(plot_dat, c(x,y,n_event))) 

p5 <- ggplot(temp, aes(as.factor(n_event), min_spei, fill=as.factor(event)))+
  geom_boxplot()+
  xlab("Total number of drought events at a site") +
  ylab("Average minimum SPEI during event across grid cells") +
  labs(fill="Event")+
  ggtitle("Drought intensity for each event within event count groups")

temp <- data2 %>% 
  left_join(dplyr::select(plot_dat, c(x,y,time_since))) 

p6 <- ggplot(temp, aes(as.factor(time_since), min_spei, fill=as.factor(event)))+
  geom_boxplot() +
  xlab("Years since most recent drought event") +
  ylab("Average minimum SPEI during event across grid cells") +
  labs(fill="Event") +
  ggtitle("Drought intensity for each event within time since drought groups")

pdf(paste0(path, "spei_exploratory.pdf"))
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
dev.off()

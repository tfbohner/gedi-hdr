## Get shapefile for costa rica, 
## pull together all spei layers, crop for costa rica
## mainpulate layers to get drought count, time since, characteristics etc. 
library(rnaturalearth)
library(sp)
library(raster)
library(tidyverse)
library(lubridate)

## Run this for Costa Rica
sub <- ne_countries(country = 'costa rica', returnclass = "sf")
loc <- "costa_rica"

## Run this for Colorado
sub <- ne_states(country = 'united states of america', returnclass = "sf") %>% 
  filter(name=="Colorado")
loc <- "colorado"

path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/Global SPEI/"
path2 <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Processed drought layers/"

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
for(year in 1981:2019) {
  spei <- stack(x= paste0(path, "Global_SPEI12_", year, ".nc"))
  spei2 <- rotate(spei)
  sub_spei <- crop(spei2, sub) %>%  mask(sub)
  print(year)
  
  if(year==1981) {
    sub_stack <- sub_spei
  } else { sub_stack <- stack(sub_stack, sub_spei)}
  
}

# writeRaster(sub_stack, paste0(path, loc, "_all_spei.grd"), format="raster", overwrite=T)

sub_stack <- stack(paste0(path, loc, "_all_spei.grd"))

## sanity check
plot(sub_stack[[1]])

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
plot(drought_count[[2]])

## write the files
writeRaster(drought_count[[1]], paste0(path2, loc, "_count_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_count[[2]], paste0(path2, loc, "_count_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_count[[3]], paste0(path2, loc, "_count_drought_-2.tif"), format="GTiff", overwrite=T)

## Following code to extract date info of most recent threshold drought
ref_date <- as.Date("2021-01-01")
date_fun <- function (data) {
  temp <- data %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(recent=max(start, na.rm=T)) %>% 
    filter(start==recent) %>% 
    summarize(start=first(start),
              end=max(date),
              peak=date[which.min(spei)],
              start=interval(start, ref_date) %>% time_length(unit="year"),
              end=interval(end, ref_date) %>% time_length(unit="year"),
              peak=interval(peak, ref_date) %>% time_length(unit="year")) 
  
  stack(rasterFromXYZ(temp[,1:3], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 4)], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs"))
}

drought_dates <- lapply(manip_dfs, date_fun)

plot(drought_dates[[2]])

writeRaster(drought_dates[[1]], paste0(path2, loc, "_time_recent_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_dates[[2]], paste0(path2, loc, "_time_recent_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_dates[[3]], paste0(path2, loc, "_time_recent_drought_-2.tif"), format="GTiff", overwrite=T)

## Following code to extract intensity, duration info of most recent threshold drought
char_fun <- function(data){
  temp <- data %>% 
    ungroup() %>% group_by(x,y) %>% 
    mutate(recent=max(start, na.rm=T)) %>% 
    filter(start==recent) %>% 
    mutate(end=max(date)) %>%
    summarize(min_spei=first(min_spei),
              tot_spei=sum(spei),
              duration=interval(start, end) %>% time_length(unit="months")) 
  
  stack(rasterFromXYZ(temp[,1:3], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 4)], crs="+proj=longlat +datum=WGS84 +no_defs"),
        rasterFromXYZ(temp[,c(1:2, 5)], crs="+proj=longlat +datum=WGS84 +no_defs")) 
}

drought_char <- lapply(manip_dfs, char_fun)


# ggpairs(all_char[,3:9])

plot(drought_char[[2]])

writeRaster(drought_char[[1]], paste0(path2, loc, "_char_recent_drought_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_char[[2]], paste0(path2, loc, "_char_recent_drought_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_char[[3]], paste0(path2, loc, "_char_recent_drought_-2.tif"), format="GTiff", overwrite=T)

## Trends through time
time_fun2 <- function(data){
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

drought_slopes <- lapply(manip_dfs, time_fun2)
writeRaster(drought_slopes[[1]], paste0(path2, loc, "_drought_slopes_-1.tif"), format="GTiff", overwrite=T)
writeRaster(drought_slopes[[2]], paste0(path2, loc, "_drought_slopes_-1.5.tif"), format="GTiff", overwrite=T)
writeRaster(drought_slopes[[3]], paste0(path2, loc, "_drought_slopes_-2.tif"), format="GTiff", overwrite=T)


library(RColorBrewer)
library(rasterVis)
zeroCol <-"#ffffff" # (white color, same as your figure example)
reds <- rev(brewer.pal('Reds', n = 8))
reds_rev <- brewer.pal('Reds', n = 5)
blues <- brewer.pal('Blues', n = 4)
blues_rev <- rev(brewer.pal('Blues', n = 8))

myTheme <- rasterTheme(region = c(reds, zeroCol, blues))
myTheme2 <- rasterTheme(region=c(blues_rev, zeroCol, reds_rev))
# plot(drought_slopes[[1]])

# Plot
library(cowplot)
a <- levelplot(drought_slopes[[1]][[1]], par.settings=myTheme, margin=F, main="Change in cumulative SPEI")
b <- levelplot(drought_slopes[[1]][[2]], par.settings=myTheme, margin=F, main="Change in minimum SPEI")
c <- levelplot(drought_slopes[[1]][[3]], par.settings=myTheme2, margin=F, main="Change in duration (months)")

plot_grid(a, b, c, ncol=2, align="vh")


data <- manip_dfs[[1]]

temp <- data %>% 
  filter(!is.na(start)) %>% 
  ungroup() %>% group_by(x,y, start) %>% 
  summarize(sum_spei=sum(spei),
            min_spei=min(spei),
            duration=length(spei)) %>% 
  ungroup() %>% group_by(x,y) %>% 
  mutate(time=as.numeric(str_sub(start, 1,4)),
         event=seq_along(start))%>% #
  mutate(slope_sum = lm(sum_spei~time)$coeff[2],
         slope_sum2 = lm(sum_spei~event)$coeff[2],
            slope_min = lm(min_spei~time)$coeff[2],
            slope_duration=lm(duration~time)$coeff[2],
         nevent=max(event)) 

ggplot(temp, aes(slope_sum, slope_sum2, color=as.factor(nevent))) + geom_point() +
  geom_abline(intercept=0,slope=1) +
  xlab("slope over years") +
  ylab("slope over events") +
  guides(col=guide_legend("total events"))

temp2 <- filter(data, spei==5) %>% 
  group_by(x,y) %>%  summarize() %>% 
  # filter(temp, max_duration<27) %>% 
  left_join(data)

ggplot(temp, aes(time, duration, col=slope_duration)) +
  geom_point(alpha=0.1) +
  scale_color_distiller(palette = "RdBu") 

ggplot(temp, aes(event, sum_spei, col=slope_sum)) +
  geom_point(alpha=0.1) +
  scale_color_distiller(palette = "RdBu")+
  geom_smooth(aes(group=interaction(x,y)), method='lm', se=F, alpha=0.1)

ggplot(temp, aes(time, sum_spei, col=slope_sum)) +
  geom_point(alpha=0.1) +
  scale_color_distiller(palette = "RdBu")+
  geom_smooth(aes(group=interaction(x,y)), method='lm', se=F, alpha=0.1)

ggplot(temp2, aes(date, spei)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -1, linetype='dashed') +
  geom_line(aes(group=interaction(x,y)), alpha=0.1, color="gray") +
  theme_bw() +
  facet_grid(rows=vars(interaction(x,y)))
  



## Let's explore the data a bit ----
plot_dat <- manip_dfs[[1]] %>% 
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

n_event_ts_sum <-  manip_dfs[[1]] %>% 
  left_join(dplyr::select(plot_dat, c(x,y,n_event))) %>% 
  group_by(n_event, date) %>% 
  summarize(mean_spei=mean(spei))

n_event_ts <-  manip_dfs[[1]] %>% 
  left_join(dplyr::select(plot_dat, c(x,y,n_event))) %>% 
  group_by(n_event, date)

ggplot(n_event_ts, aes(date, spei)) +
  geom_line(aes(group=interaction(x, y)), color='gray', alpha=0.4) +
  geom_line(data=n_event_ts_sum, aes(date, mean_spei)) +
  facet_wrap(~n_event) +
  geom_hline(yintercept = -1, linetype="dashed") +
  ylab("SPEI")+
  xlab("")+
  labs(color="Total drought \n events") +
  ggtitle("SPEI time series grouped by total number of droughts (threshold=-1)") +
  theme_test()

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

pdf(paste0(path2, "spei_exploratory.pdf"))
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
dev.off()

test2 <- raster("/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/drought layers/costa_rica_count_drought_-2.tif")

plot(test2, col=pal(7))


time_sum <-  manip_dfs[[1]] %>% 
  group_by(date) %>% 
  summarize(mean_spei=mean(spei))

ggplot(manip_dfs[[1]], aes(date, spei)) +
  geom_line(aes(group=interaction(x, y)), color='gray', alpha=0.4) +
  geom_line(data=time_sum, aes(date, mean_spei)) +
  geom_hline(yintercept = c(-2, -1.5,-1), linetype="dashed", alpha=0.8) +
  ylab("SPEI")+
  xlab("")+
  ggtitle("SPEI time series in Costa Rica") +
  theme_test()

## for calc re-write

spei_df2 <- spei_df %>% 
  group_by(x,y) %>% 
  mutate(cell=cur_group_id())

spei_df3 <- filter(spei_df2, cell==1)

spei_vec <- spei_df3$spei

find_flood_drought <- function (spi) {
  # Convert spi to string to detect extreme events.
  spi <- (spi > -1) + (spi > 0) + (spi > 1)
  spi[is.na(spi)] <- 9
  if(length(which(spi==9))==length(spi)){spi[]<- NA} else {
    spichar <- spi %>% paste(collapse = "")
    # Detect flood and drought.
    flood_exp <- gregexpr("[2-3]*3+[2-3]*", spichar)
    drought_exp <- gregexpr("[0-1]*0+[0-1]*", spichar)
    flood <- function(n) strrep("8", n)
    drought <- function(n) strrep("7", n)
    regmatches(spichar, flood_exp) <-
      Map(flood, lapply(regmatches(spichar, flood_exp), nchar))
    regmatches(spichar, drought_exp) <-
      Map(drought, lapply(regmatches(spichar, drought_exp), nchar))
    # Convert to vector.
    spichar <- substring(spichar, 1:nchar(spichar), 1:(nchar(spichar)))
    spi <- as.numeric(spichar)
    spi[spi == 9] <- NA
    spi[!(spi %in% c(7:8, NA))] <- 0
    spi <- factor(spi, c(0, 7, 8), c("normal", "drought", "flood"))
  }

  return(spi)
}

test <- raster::calc(sub_stack, find_flood_drought)

fdvec <- find_flood_drought(spei_vec)

spi <- spei_vec
spi <- (spi > -1) + (spi > 0) + (spi > 1)
spi[is.na(spi)] <- 9
spichar <- spi %>% paste(collapse = "")
# Detect flood and drought.
flood_exp <- gregexpr("[2-3]*3+[2-3]*", spichar)
drought_exp <- gregexpr("[0-1]*0+[0-1]*", spichar)

library(zoo)



drought_start <- unlist(drought_exp)
test <- rep(NA, length=457)
test[drought_start]= seq(1, length(drought_start), by=1)
test[1] <- ifelse(is.na(test[1]), 0, 1)
test2 <- na.locf(test)

## match two vectors to associate drought event with event count
test2[fdvec!="drought"]=NA
## match drought events with spei vector to do calculations

test3 <- spei_vec

test3[fdvec!="drought"]=NA

## cumulative, intensity, duration
sum <- aggregate(test3, by=list(test2), FUN=sum)
min <- aggregate(test3, by=list(test2), FUN=min)
dur <- aggregate(test3, by=list(test2), FUN=length)

## associated date for regression
years <- as.numeric(str_sub(dates$date, 1, 4))
years[fdvec!="drought"]=NA
pred <- aggregate(years, by=list(test2), FUN=first)

slope <- coef(lm(sum$x~pred$x))[2]





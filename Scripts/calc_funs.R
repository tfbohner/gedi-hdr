library(zoo)
library(stringr)
library(lubridate)
library(dplyr)

## find flood/droughts based on mbsi function by Erick A. Chacon-Montalvan, I added the rest
## dates count backward from 12-12-2019 so should accommodate different time scale spei


# ## for testing
# library(tidyverse)
# spei_df <- rasterToPoints(test, spatial = TRUE) %>%
#   data.frame() %>%
#   pivot_longer(cols=-c(x, y, optional), names_to="time", values_to="spei")
# 
# spei_df2 <- spei_df %>%
#   group_by(x,y) %>%
#   mutate(cell=cur_group_id())
# 
# spei_df3 <- filter(spei_df2, cell==1)
# 
# spei <- spei_df3$spei

## annual slope function drought:----
drought_slope_calc <- function(raw){
  ## find floods/droughts
  spei <- raw
  spei[is.na(spei)] <- 0
  spei2 <- (spei > -1) + (spei > 0) + (spei > 1)
  spei2[is.na(spei2)] <- 1
  
  nmonths<- length(spei2)
  
  if(length(which(spei2==1))==length(spei2)){
    # spei2[]<- NA
    # events <- rep(NA, length=nmonths)
    slope1 <- NA
    slope2 <- NA
    slope3 <- NA
    slope4 <- NA} 
  else {
    spei2char <- spei2 %>% paste(collapse = "")
    # Detect flood and drought.
    flood_exp <- gregexpr("[2-3]*3+[2-3]*", spei2char)
    drought_exp <- gregexpr("[0-1]*0+[0-1]*", spei2char)
    flood <- function(n) strrep("8", n)
    drought <- function(n) strrep("7", n)
    regmatches(spei2char, flood_exp) <-
      Map(flood, lapply(regmatches(spei2char, flood_exp), nchar))
    regmatches(spei2char, drought_exp) <-
      Map(drought, lapply(regmatches(spei2char, drought_exp), nchar))
    # Convert to vector.
    spei2char <- substring(spei2char, 1:nchar(spei2char), 1:(nchar(spei2char)))
    spei2 <- as.numeric(spei2char)
    spei2[spei2 == 9] <- NA
    spei2[!(spei2 %in% c(7:8, NA))] <- 0
    spei2 <- factor(spei2, c(0, 7, 8), c("normal", "drought", "flood"))
    
    ## event count
    drought_start <- unlist(drought_exp)
    events <- rep(NA, length=nmonths)
    events[drought_start]= seq(1, length(drought_start), by=1)
    events[1] <- ifelse(is.na(events[1]), 0, 1)
    events <- na.locf(events)
    interdrought <- events 
    interdrought[spei2=="drought"]=NA
    events[spei2!="drought"]=NA
    
    ## cumulative, intensity, duration
    sum <- aggregate(spei, by=list(events), FUN=sum, na.rm=T, na.action=NULL)
    min <- aggregate(spei, by=list(events), FUN=min, na.rm=T, na.action=NULL)
    dur <- aggregate(spei, by=list(events), FUN=length)
    inter <- aggregate(spei, by=list(interdrought), FUN=length)
    
    ## associated date for regression
    dates <-rev(seq(as.Date("2019-12-12"), length.out = length(spei), by = "-1 month"))
  
    
    years <- as.numeric(str_sub(dates, 1, 4))
    pred <- aggregate(years, by=list(events), FUN=first)
    pred2 <- aggregate(years, by=list(interdrought), FUN=first)
    
    slope1 <- coef(lm(sum$x~pred$x))[2]
    slope2 <- coef(lm(min$x~pred$x))[2]
    slope3 <- coef(lm(dur$x~pred$x))[2]
    slope4 <- coef(lm(inter$x~pred2$x))[2]

  }

  out <- c(slope1, slope2, slope3, slope4)
  names(out) <- c( "drought_sum_slope", "drought_max_slope", "drought_dur_slope", "inter_slope")
  return(out)
}

## annual slope function wet:----
wet_slope_calc <- function(raw){
  ## find floods/droughts
  spei <- raw
  spei[is.na(spei)] <- 0
  spei2 <- (spei > -1) + (spei > 0) + (spei > 1)
  spei2[is.na(spei2)] <- 1
  
  nmonths<- length(spei2)
  
  if(length(which(spei2==1))==length(spei2)){
    # spei2[]<- NA
    # events <- rep(NA, length=nmonths)
    slope1 <- NA
    slope2 <- NA
    slope3 <- NA} 
  else {
    spei2char <- spei2 %>% paste(collapse = "")
    # Detect flood and drought.
    flood_exp <- gregexpr("[2-3]*3+[2-3]*", spei2char)
    drought_exp <- gregexpr("[0-1]*0+[0-1]*", spei2char)
    flood <- function(n) strrep("8", n)
    drought <- function(n) strrep("7", n)
    regmatches(spei2char, flood_exp) <-
      Map(flood, lapply(regmatches(spei2char, flood_exp), nchar))
    regmatches(spei2char, drought_exp) <-
      Map(drought, lapply(regmatches(spei2char, drought_exp), nchar))
    # Convert to vector.
    spei2char <- substring(spei2char, 1:nchar(spei2char), 1:(nchar(spei2char)))
    spei2 <- as.numeric(spei2char)
    spei2[spei2 == 9] <- NA
    spei2[!(spei2 %in% c(7:8, NA))] <- 0
    spei2 <- factor(spei2, c(0, 7, 8), c("normal", "drought", "flood"))
    
    ## event count
    flood_start <- unlist(flood_exp)
    events <- rep(NA, length=nmonths)
    events[flood_start]= seq(1, length(flood_start), by=1)
    events[1] <- ifelse(is.na(events[1]), 0, 1)
    events <- na.locf(events)
    events[spei2!="flood"]=NA
    
    ## cumulative, intensity, duration
    sum <- aggregate(spei, by=list(events), FUN=sum, na.rm=T, na.action=NULL)
    max <- aggregate(spei, by=list(events), FUN=max, na.rm=T, na.action=NULL)
    dur <- aggregate(spei, by=list(events), FUN=length)
    
    ## associated date for regression
    dates <-rev(seq(as.Date("2019-12-12"), length.out = length(spei), by = "-1 month"))
    
    years <- as.numeric(str_sub(dates, 1, 4))
    years[spei2!="flood"]=NA
    pred <- aggregate(years, by=list(events), FUN=first)
    
    slope1 <- coef(lm(sum$x~pred$x))[2]
    slope2 <- coef(lm(max$x~pred$x))[2]
    slope3 <- coef(lm(dur$x~pred$x))[2]
    
  }
  
  out <- c(slope1, slope2, slope3)
  names(out) <- c( "wet_sum_slope", "wet_max_slope", "wet_dur_slope")
  return(out)
}

## Rolling averages:----
roll_slope_calc <- function(raw){
  ## find floods/droughts
  spei <- raw
  spei[is.na(spei)] <- 0
  spei2 <- (spei > -1) + (spei > 0) + (spei > 1)
  spei2[is.na(spei2)] <- 1
  
  nmonths<- length(spei2)
  
  if(length(which(spei2==1))==length(spei2)){
    # spei2[]<- NA
    # events <- rep(NA, length=nmonths)
    slope1 <- NA
    slope2 <- NA
    slope3 <- NA
    slope4 <- NA
    slope5 <- NA} 
  else {
    spei2char <- spei2 %>% paste(collapse = "")
    # Detect flood and drought.
    flood_exp <- gregexpr("[2-3]*3+[2-3]*", spei2char)
    drought_exp <- gregexpr("[0-1]*0+[0-1]*", spei2char)
    flood <- function(n) strrep("8", n)
    drought <- function(n) strrep("7", n)
    regmatches(spei2char, flood_exp) <-
      Map(flood, lapply(regmatches(spei2char, flood_exp), nchar))
    regmatches(spei2char, drought_exp) <-
      Map(drought, lapply(regmatches(spei2char, drought_exp), nchar))
    # Convert to vector.
    spei2char <- substring(spei2char, 1:nchar(spei2char), 1:(nchar(spei2char)))
    spei2 <- as.numeric(spei2char)
    spei2[spei2 == 9] <- NA
    spei2[!(spei2 %in% c(7:8, NA))] <- 0
    spei2 <- factor(spei2, c(0, 7, 8), c("normal", "drought", "flood"))
    
    ## associated date for regression
    dates <-rev(seq(as.Date("2019-12-12"), length.out = length(spei), by = "-1 month"))
    months <- as.numeric(str_sub(dates, 6, 7))
    years <- as.numeric(str_sub(dates, 1, 4))[months==12]  ## pull out december
    
    ## Rolling averages
    win=60 # window size
    drought_spei <- spei
    drought_spei[spei2!="drought"]=0
    drought_sum <- rollapply(drought_spei,win,sum,align='right',fill=NA)[months==12]
    # drought_min <- rollapply(drought_spei,60,min,align='right',fill=NA)
    droughts <- ifelse(spei2=="drought", 1, 0)
    drought_months <- rollapply(droughts,win,sum,align='right',fill=NA)[months==12]
    
    wet_spei <- spei
    wet_spei[spei2!="flood"]=0
    wet_sum <- rollapply(wet_spei,win,sum,align='right',fill=NA)[months==12]
    # drought_min <- rollapply(drought_spei,60,min,align='right',fill=NA)
    floods <- ifelse(spei2=="flood", 1, 0)
    wet_months <- rollapply(floods,win,sum,align='right',fill=NA)[months==12]
    
    inter <- ifelse(spei2!="drought", 1, 0)
    inter_months <- rollapply(inter,win,sum,align='right',fill=NA)[months==12]
    
    slope1 <- coef(lm(drought_sum~years))[2]
    slope2 <- coef(lm(drought_months~years))[2]
    slope3 <- coef(lm(wet_sum~years))[2]
    slope4 <- coef(lm(wet_months~years))[2]
    slope5 <- coef(lm(inter_months~years))[2]
    
  }
  
  out <- c(slope1, slope2, slope3, slope4, slope5)
  names(out) <- c("roll_drought_sum", "roll_drought_months", "roll_wet_sum", "roll_wet_months", "roll_inter_months")
  return(out)
}



## Big event drought:----
big_drought_calc <- function(raw){
  ## find floods/droughts
  spei <- raw
  spei[is.na(spei)] <- 0
  spei2 <- (spei > -1) + (spei > 0) + (spei > 1)
  spei2[is.na(spei2)] <- 1
  
  nmonths<- length(spei2)
  
  if(length(which(spei2==1))==length(spei2)){
    big_drought_sum<- NA
    big_drought_min <- NA
    big_drought_dur <- NA
    time_since <- NA} 
  else {
    spei2char <- spei2 %>% paste(collapse = "")
    # Detect flood and drought.
    flood_exp <- gregexpr("[2-3]*3+[2-3]*", spei2char)
    drought_exp <- gregexpr("[0-1]*0+[0-1]*", spei2char)
    flood <- function(n) strrep("8", n)
    drought <- function(n) strrep("7", n)
    regmatches(spei2char, flood_exp) <-
      Map(flood, lapply(regmatches(spei2char, flood_exp), nchar))
    regmatches(spei2char, drought_exp) <-
      Map(drought, lapply(regmatches(spei2char, drought_exp), nchar))
    # Convert to vector.
    spei2char <- substring(spei2char, 1:nchar(spei2char), 1:(nchar(spei2char)))
    spei2 <- as.numeric(spei2char)
    spei2[spei2 == 9] <- NA
    spei2[!(spei2 %in% c(7:8, NA))] <- 0
    spei2 <- factor(spei2, c(0, 7, 8), c("normal", "drought", "flood"))
    
    ## event count
    drought_start <- unlist(drought_exp)
    events <- rep(NA, length=nmonths)
    events[drought_start]= seq(1, length(drought_start), by=1)
    events[1] <- ifelse(is.na(events[1]), 0, 1)
    events <- na.locf(events)
    events[spei2!="drought"]=NA
    
    ## cumulative for worst event selection
    sum <- aggregate(spei, by=list(events), FUN=sum, na.rm=T, na.action=NULL)
    worst <- sum$Group.1[which.min(sum$x)]
    
    big_drought_sum <- sum(spei[events==worst], na.rm=T)
    big_drought_min <- min(spei[events==worst], na.rm=T)
    big_drought_dur <- length(which(!is.na(spei[events==worst])))
    
    ref_date <- "2021-01-01"
    dates <-rev(seq(as.Date("2019-12-12"), length.out = length(spei), by = "-1 month"))[events==worst]
    end <- last(dates[which(!is.na(dates))])
    time_since <- interval(end, ref_date) %>% time_length(unit="year")
    
  }
  
  out <- c(big_drought_sum, big_drought_min, big_drought_dur, time_since)
  names(out) <- c( "big_drought_sum", "big_drought_min", "big_drought_dur", "time_since")
  return(out)
}


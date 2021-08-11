library(factoextra)
library(raster)
library(dplyr)
library(stringr)

path2 <- "/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/Processed drought layers/" 
loc <- c("costa_rica", "colorado", "amazon")
time <- c("3month/", "6month/", "12month/", "24month/")

loc_select <- loc[3]
time_select <- time[1]

drought_slopes <- stack(paste0(path2, time_select, loc_select, "_drought_slopes_-1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame() 
wet_slopes <- stack(paste0(path2, time_select, loc_select, "_surplus_slopes_1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
window <- stack(paste0(path2, time_select, loc_select, "_window_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
big_drought <- stack(paste0(path2, time_select, loc_select, "_big_drought_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()

alldrought <- left_join(drought_slopes, wet_slopes) %>% 
  left_join(window) %>% 
  left_join(big_drought)

alldrought <- left_join(window, big_drought)


res.pca <- prcomp(dplyr::select(alldrought, -c(x,y, optional)), scale = TRUE)
# fviz_eig(res.pca)

grp <- as.factor(str_split_fixed(rownames(res.pca$rotation), "_", 2)[,1])


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.circle = NA,
             repel = TRUE     # Avoid text overlapping
) + theme_bw() + ggtitle(paste(loc_select, time_select, sep="-"))

fviz_pca_var(res.pca,
             col.var = grp, 
             col.circle = NA,
             repel = TRUE     # Avoid text overlapping
) + theme_bw()+ ggtitle(loc_select)


alldrought_3 <- alldrought %>% 
  rename_at(vars(-c(x,y,optional)), funs(paste0("3_", .)))

drought_slopes <- stack(paste0(path2, time[2], loc[3], "_drought_slopes_-1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame() 
wet_slopes <- stack(paste0(path2, time[2], loc[3], "_surplus_slopes_1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
window <- stack(paste0(path2, time[2], loc[3], "_window_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
big_drought <- stack(paste0(path2, time[2], loc[3], "_big_drought_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()

alldrought_6 <- left_join(drought_slopes, wet_slopes) %>% 
  left_join(window) %>% 
  left_join(big_drought)%>% 
  rename_at(vars(-c(x,y,optional)), funs(paste0("6_", .)))

drought_slopes <- stack(paste0(path2, time[3], loc[3], "_drought_slopes_-1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame() 
wet_slopes <- stack(paste0(path2, time[3], loc[3], "_surplus_slopes_1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
window <- stack(paste0(path2, time[3], loc[3], "_window_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
big_drought <- stack(paste0(path2, time[3], loc[3], "_big_drought_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()

alldrought_12 <- left_join(drought_slopes, wet_slopes) %>% 
  left_join(window) %>% 
  left_join(big_drought)%>% 
  rename_at(vars(-c(x,y,optional)), funs(paste0("12_", .)))

drought_slopes <- stack(paste0(path2, time[4], loc[3], "_drought_slopes_-1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame() 
wet_slopes <- stack(paste0(path2, time[4], loc[3], "_surplus_slopes_1.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
window <- stack(paste0(path2, time[4], loc[3], "_window_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()
big_drought <- stack(paste0(path2, time[4], loc[3], "_big_drought_metrics.grd")) %>% 
  rasterToPoints(spatial = TRUE) %>%
  data.frame()

alldrought_24 <- left_join(drought_slopes, wet_slopes) %>% 
  left_join(window) %>% 
  left_join(big_drought)%>% 
  rename_at(vars(-c(x,y,optional)), funs(paste0("24_", .)))

alldrought <- left_join(alldrought_3, alldrought_6) %>% 
  left_join(alldrought_12) %>% 
  left_join(alldrought_24) %>% 
  na.omit()

res.pca <- prcomp(dplyr::select(alldrought, -c(x,y, optional)), scale = TRUE)
fviz_eig(res.pca)


res.pca$rotation

grp <- as.factor(str_split_fixed(rownames(res.pca$rotation), "_", 2)[,1])


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.circle = NA,
             repel = TRUE     # Avoid text overlapping
) + theme_bw()

fviz_pca_var(res.pca,
             col.var = grp, 
             col.circle = NA,
             repel = F,     # Avoid text overlapping
             labelsize=3,
) + theme_bw()


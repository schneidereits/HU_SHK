#library----
library(terra)
library(sf)
library(viridis)
library(tmap)
library(tidyverse)
library(magick)
library(rasterVis)
library(patchwork)
library(ggridges)

# data import and wrangel ----

## Landsat ----
# NUM ----
# CSO number 
 
link <- "data/1982-2021_091-319-12_HL_CSO_LNDLG/1982-2021_091-319-12_HL_CSO_LNDLG_NUM.vrt.ovr"
CSO_NUM <- rast(link)
years <- c(1982:2021)
names(CSO_NUM) <- years
# add z component
years <- seq(as.Date('1982-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_NUM) <-  years

# crop rater to speed up data exporlation and vis
#e <- ext(0.4, 0.9, 0.4, 0.9)
#CSO_NUM <- crop(CSO_NUM, e)

# test plot of map
plot(CSO_NUM[[20]], legend=F, col = viridis(n=30,option="D"), range=c(0,35))

### summarized maps ----
#  compute summarized maps
# full time series
# CSO_NUM_mean_1984_2021 <- mean(CSO_NUM[[c(2:40)]])
# # to save
# writeRaster(CSO_NUM_mean_1984_2021, "data/1982-2021_091-319-12_HL_CSO_LNDLG/CSO_NUM_mean_1984_2021.tif",
#             overwrite=TRUE,
#             datatype = "INT4S",
#             filetype='GTiff')

link <- 'data/CSO_NUM_mean_1984_2021.tif'
CSO_NUM_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_NUM_mean_1984_2021, par.settings = viridisTheme)

# AVG ----
# CSO Average

link <- "data/1982-2021_091-319-12_HL_CSO_LNDLG/1982-2021_091-319-12_HL_CSO_LNDLG_AVG.vrt.ovr"
CSO_AVG <- rast(link)
years <- c(1982:2021)
names(CSO_AVG) <- years
# add z component
years <- seq(as.Date('1982-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_AVG) <-  years

# test plot of map
plot(CSO_AVG[[20]], legend=F, col = viridis(n=30,option="D"), range=c(0,365))

# CSO_AVG_mean <- mean(CSO_AVG)
# writeRaster(CSO_AVG_mean, "data/CSO_AVG_mean.tif",
#             overwrite=TRUE,
#             datatype = "INT4S",
#             filetype='GTiff')

link <- 'data/CSO_AVG_mean.tif'
CSO_AVG_mean <- rast(link)
rasterVis::levelplot(CSO_AVG_mean, par.settings = viridisTheme)

### summarized maps ----
# create mask for areas with no observations
non_zero_mask <-  CSO_AVG_mean<191
CSO_AVG_non_zero <- mask(CSO_AVG_mean, non_zero_mask, maskvalue=0)
# save tif for avg with no observation
# writeRaster(CSO_AVG_non_zero, "data/1982-2021_091-319-12_HL_CSO_LNDLG/CSO_AVG_non_zero.tif",
#             overwrite=TRUE,
#             datatype = "INT4S",
#             filetype='GTiff')

link <- 'data/CSO_AVG_non_zero.tif'
CSO_AVG_non_zero <- rast(link)
rasterVis::levelplot(CSO_AVG_non_zero, par.settings = viridisTheme)
# rasterVis::levelplot(CSO_AVG_non_zero[[c(3:18)]], par.settings = viridisTheme)
# rasterVis::levelplot(CSO_AVG_non_zero[[c(19:35)]], par.settings = viridisTheme)
# rasterVis::levelplot(CSO_AVG_non_zero[[c(36:40)]], par.settings = viridisTheme)


# MIN ----

link <- "data/1982-2021_091-319-12_HL_CSO_LNDLG/1982-2021_091-319-12_HL_CSO_LNDLG_MIN.vrt.ovr"
CSO_MIN <- rast(link)
years <- c(1982:2021)
names(CSO_MIN) <- years
# add z component
years <- seq(as.Date('1982-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_MIN) <-  years

# crop rater to speed up data exporlation and vis
#e <- ext(0.4, 0.9, 0.4, 0.9)
#CSO_MIN <- crop(CSO_MIN, e)

# test plot of map
plot(CSO_MIN[[20]], legend=F, col = viridis(n=30,option="D"), range=c(1,364))

### summarized maps ----
#  compute summarized maps
# full time series
CSO_MIN_mean_1984_2021 <- mean(CSO_MIN[[c(2:40)]])
# to save
# writeRaster(CSO_MIN_mean_1984_2021, "data/1982-2021_091-319-12_HL_CSO_LNDLG/CSO_MIN_mean_1984_2021.tif",
#             overwrite=TRUE,
#             datatype = "INT4S",
#             filetype='GTiff')

link <- 'data/CSO_MIN_mean_1984_2021.tif'
CSO_MIN_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_MIN_mean_1984_2021, par.settings = viridisTheme)


# create mask for areas with no observations
non_zero_mask <-  CSO_MIN_mean_1984_2021<191
CSO_MIN_non_zero <- mask(CSO_MIN_mean_1984_2021, non_zero_mask, maskvalue=0)

rasterVis::levelplot(CSO_MIN_non_zero, par.settings = viridisTheme)


# MAX ----

link <- "data/1982-2021_091-319-12_HL_CSO_LNDLG/1982-2021_091-319-12_HL_CSO_LNDLG_MAX.vrt.ovr"
CSO_MAX <- rast(link)
years <- c(1982:2021)
names(CSO_MAX) <- years
# add z component
years <- seq(as.Date('1982-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_MAX) <-  years

# crop rater to speed up data exporlation and vis
#e <- ext(0.4, 0.9, 0.4, 0.9)
#CSO_MAX <- crop(CSO_MAX, e)

# test plot of map
plot(CSO_MAX[[20]], legend=F, col = viridis(n=30,option="D"), range=c(1,364))

### summarized maps ----
#  compute summarized maps
# full time series
CSO_MAX_mean_1984_2021 <- mean(CSO_MAX[[c(2:40)]])
# to save
# writeRaster(CSO_MAX_mean_1984_2021, "data/1982-2021_091-319-12_HL_CSO_LNDLG/CSO_MAX_mean_1984_2021.tif",
#             overwrite=TRUE,
#             datatype = "INT4S",
#             filetype='GTiff')

link <- 'data/CSO_MAX_mean_1984_2021.tif'
CSO_MAX_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_MAX_mean_1984_2021, par.settings = viridisTheme)


# create mask for areas with no observations
non_zero_mask <-  CSO_MAX_mean_1984_2021<191
CSO_MAX_non_zero <- mask(CSO_MAX_mean_1984_2021, non_zero_mask, maskvalue=0)

rasterVis::levelplot(CSO_MAX_non_zero, par.settings = viridisTheme)

# base visualisation ----
## landsat ----
# map plots
plot(CSO_NUM[[c(3:18)]], legend=F, col = viridis(n=30,option="D"), range=c(0,35))
plot(CSO_NUM[[c(19:35)]], legend=F, col = viridis(n=30,option="D"),  range=c(0,35))
plot(CSO_NUM[[c(36:40)]], legend=F, col = viridis(n=30,option="D"),  range=c(0,35))

# basic histograms
hist(CSO_NUM[[c(3:18)]], main = "CSO count", xlab = "n", ylab= "Frequency", col = viridis(n=30,option="D"), breaks = 30, xaxt = "n")
hist(CSO_NUM[[c(19:35)]], main = "CSO count", xlab = "n", ylab= "Frequency",
     col = viridis(n=30,option="D"),  breaks = 30, xaxt = "n")

# boxplots
boxplot(CSO_NUM[[c(3:18)]], main = "CSO count", xlab = "n", ylab= "Frequency",
        #col = viridis(n=30,option="D"),
        breaks = 30, xaxt = "n", notch=TRUE)

boxplot(CSO_NUM[[c(19:35)]], main = "CSO count", xlab = "n", ylab= "Frequency",
        #  col = viridis(n=30,option="D"),  
        breaks = 30, xaxt = "n", notch=TRUE)

# density
density(CSO_NUM[[c(3:18)]], main = "CSO count", xlab = "n", ylab= "Frequency", xaxt = "n")
density(CSO_NUM[[c(19:35)]], main = "CSO count", xlab = "n", ylab= "Frequency", xaxt = "n")

# map with geograpic density
rasterVis::levelplot(CSO_NUM[[20]], par.settings = viridisTheme)
rasterVis::levelplot(CSO_NUM[[20]], par.settings = viridisTheme, contour=TRUE)

rasterVis::histogram(CSO_NUM[[20]], maxpixels = 1e+01)

rasterVis::densityplot(CSO_NUM[[c(3:40)]], #maxpixels = 1e+03, 
                       par.settings = viridisTheme)

bwplot(CSO_NUM[[c(3:40)]],  maxpixels = 1e+01, par.settings = viridisTheme)


plot(CSO_AVG[[c(3:18)]], legend=F, col = viridis(n=30,option="D"), range=c(1,365))
plot(CSO_AVG[[c(19:35)]], legend=F, col = viridis(n=30,option="D"), range=c(1,365))
plot(CSO_AVG[[c(36:40)]], legend=F, col = viridis(n=30,option="D"), range=c(1,365))


hist(CSO_AVG[[c(3:18)]], main = "CSO count", xlab = "n", ylab= "Frequency", col = "wheat", breaks = 30, xaxt = "n")
hist(CSO_AVG[[c(19:35)]], main = "CSO count", xlab = "n", ylab= "Frequency",
     col = "wheat",  breaks = 30, xaxt = "n")





# Sentinal 2 ----
# Sentinal 2 CSO number

link <- "data/2017-2021_091-319-12_HL_CSO_SEN2/2017-2021_091-319-12_HL_CSO_SEN2L_NUM.vrt.ovr"
CSO_NUM_S2 <- rast(link) 
years <- c(2017:2021)
names(CSO_NUM_S2) <- years
# add z component
years <- seq(as.Date('2017-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_NUM_S2) <-  years

# test plot of map
plot(CSO_NUM_S2, legend=F, col = viridis(n=30,option="D"), range=c(0,70))

### summarized map ----
#  compute summarized maps
# full time series
CSO_NUM_S2_mean <- mean(CSO_NUM_S2)
# # to save
writeRaster(CSO_NUM_S2_mean, "data/CSO_NUM_S2_mean.tif",
            overwrite=TRUE,
            datatype = "INT4S",
            filetype='GTiff')

link <- 'data/2017-2021_091-319-12_HL_CSO_SEN2/CSO_NUM_S2_mean.tif'
CSO_NUM_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_NUM_mean_1984_2021, par.settings = viridisTheme)

# Sentinal 2 CSO AVG

link <- "data/2017-2021_091-319-12_HL_CSO_SEN2/2017-2021_091-319-12_HL_CSO_SEN2L_AVG.vrt.ovr"
CSO_AVG_S2 <- rast(link) 
years <- c(2017:2021)
names(CSO_AVG_S2) <- years
# add z component
years <- seq(as.Date('2017-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_AVG_S2) <-  years

# test plot of map
plot(CSO_AVG_S2, legend=F, col = viridis(n=30,option="D"), range=c(0,70))

### summarized map ----
#  compute summarized maps
# full time series
CSO_AVG_S2_mean <- mean(CSO_AVG_S2)
# # to save
writeRaster(CSO_AVG_S2_mean, "data/CSO_AVG_S2_mean.tif",
            overwrite=TRUE,
            datatype = "INT4S",
            filetype='GTiff')

link <- 'data/2017-2021_091-319-12_HL_CSO_SEN2/CSO_AVG_S2_mean.tif'
CSO_AVG_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_AVG_mean_1984_2021, par.settings = viridisTheme)

# Sentinal 2 CSO MIN ----

link <- "data/2017-2021_091-319-12_HL_CSO_SEN2/2017-2021_091-319-12_HL_CSO_SEN2L_NUM.vrt.ovr"
CSO_NUM_S2 <- rast(link) 
years <- c(2017:2021)
names(CSO_NUM_S2) <- years
# add z component
years <- seq(as.Date('2017-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_NUM_S2) <-  years

# test plot of map
plot(CSO_NUM_S2, legend=F, col = viridis(n=30,option="D"), range=c(0,70))

### summarized map ----
#  compute summarized maps
# full time series
CSO_NUM_S2_mean <- mean(CSO_NUM_S2)
# # to save
writeRaster(CSO_NUM_S2_mean, "data/CSO_NUM_S2_mean.tif",
            overwrite=TRUE,
            datatype = "INT4S",
            filetype='GTiff')

link <- 'data/2017-2021_091-319-12_HL_CSO_SEN2/CSO_NUM_S2_mean.tif'
CSO_NUM_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_NUM_mean_1984_2021, par.settings = viridisTheme)

# Sentinal 2 CSO MAX ----

link <- "data/2017-2021_091-319-12_HL_CSO_SEN2/2017-2021_091-319-12_HL_CSO_SEN2L_NUM.vrt.ovr"
CSO_NUM_S2 <- rast(link) 
years <- c(2017:2021)
names(CSO_NUM_S2) <- years
# add z component
years <- seq(as.Date('2017-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_NUM_S2) <-  years

# test plot of map
plot(CSO_NUM_S2, legend=F, col = viridis(n=30,option="D"), range=c(0,70))

### summarized map ----
#  compute summarized maps
# full time series
CSO_NUM_S2_mean <- mean(CSO_NUM_S2)
# # to save
writeRaster(CSO_NUM_S2_mean, "data/CSO_NUM_S2_mean.tif",
            overwrite=TRUE,
            datatype = "INT4S",
            filetype='GTiff')

link <- 'data/2017-2021_091-319-12_HL_CSO_SEN2/CSO_NUM_S2_mean.tif'
CSO_NUM_mean_1984_2021 <- rast(link)
rasterVis::levelplot(CSO_NUM_mean_1984_2021, par.settings = viridisTheme)





# advanced data vis ----
## landsat ----


# conversion of SpatRaster to df
#df_CSO_NUM_wide <- CSO_NUM[[c(3:40)]] %>% as.data.frame(xy = TRUE)

# random subset sample 
set.seed(42)
CSO_NUM_subset <- spatSample(CSO_NUM, size=1000, method="random", replace=FALSE, xy=T)
CSO_AVG_subset <- spatSample(CSO_AVG, size=1000, method="random", replace=FALSE, xy=T)
CSO_MIN_subset <- spatSample(CSO_MIN, size=1000, method="random", replace=FALSE, xy=T)
CSO_MAX_subset <- spatSample(CSO_MAX, size=1000, method="random", replace=FALSE, xy=T)

df_CSO_NUM_long <- CSO_NUM_subset %>% 
  select(-x,-y,-X1982, -X1983) %>% 
  pivot_longer(cols= c(X1984:X2021), names_to = "year", values_to = "NUM") %>% 
  mutate(year = str_remove(year, "X"))

df_CSO_AVG_long <- CSO_AVG_subset %>% 
  select(-x,-y,-X1982, -X1983) %>% 
  pivot_longer(cols= c(X1984:X2021), names_to = "year", values_to = "AVG") %>% 
  mutate(year = str_remove(year, "X"))

df_CSO_MIN_long <- CSO_MIN_subset %>% 
  select(-x,-y,-X1982, -X1983) %>% 
  pivot_longer(cols= c(X1984:X2021), names_to = "year", values_to = "MIN") %>% 
  mutate(year = str_remove(year, "X"))

df_CSO_MAX_long <- CSO_MAX_subset %>% 
  select(-x,-y,-X1982, -X1983) %>% 
  pivot_longer(cols= c(X1984:X2021), names_to = "year", values_to = "MAX") %>% 
  mutate(year = str_remove(year, "X"))


df_CSO_LND <- cbind(df_CSO_NUM_long, df_CSO_AVG_long[,2]) %>% 
  cbind(df_CSO_MIN_long[,2]) %>% 
  cbind(df_CSO_MAX_long[,2]) %>% 
  pivot_longer(cols = c(NUM:MAX), names_to = "variable", values_to = "value")

df_CSO_LND <- saveRDS(df_CSO_LND, file = "data/df_CSO_LND.rds")

## time series ---- 
ggplot(df_CSO_LND[1:1000,], aes(year, value, color=value)) +
    geom_point() +
    geom_jitter(width = 0.5, height = 0.5) +
    scale_colour_viridis_c(option = "D") +
    theme_minimal() +
   # scale_y_continuous(expand = c(0, 0), limits = c(-5, 50)) +
    theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~variable, scales = "free")

# make list of plots
ggList <- lapply(split(df_CSO_LND, df_CSO_LND$variable)[c(4,1,2,3)], function(i) {
  ggplot(i[1:1000,], aes(year, value, color=value)) +
    geom_point() +
    geom_jitter(width = 0.5, height = 0.5) +
    scale_colour_viridis_c(option = "D") +
    theme_minimal() +
    labs(title = paste(i$variable[1])) +
    # scale_y_continuous(expand = c(0, 0), limits = c(-5, 50)) +
    theme(axis.text.x = element_text(angle = 45))
  }
  )

# plot as grid in 1 columns
cowplot::plot_grid(plotlist = ggList, ncol = 2,
                   align = 'v', labels = levels(df_CSO_LND$variable))
  

ggplot(df_CSO_NUM_long, aes(x,y)) +
  geom_point() +
  theme_minimal()

# time series 
(ts_AVG <- ggplot(df_CSO_NUM_long, aes(year, NUM, color=NUM)) +
  geom_point() +
  geom_jitter(width = 0.5, height = 0.5) +
  scale_colour_viridis_c(option = "D") +
  theme_minimal() +
    scale_y_continuous(expand = c(0, 0), limits = c(-5, 50)) +
    theme(axis.text.x = element_text(angle = 45)))

# box
ggplot(df_CSO_NUM_long, aes(year, NUM, color=NUM)) +
  geom_boxplot(#notch = TRUE
               ) +
  #geom_jitter() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()

# violin
ggplot(df_CSO_NUM_long, aes(year, NUM, color=NUM)) +
  geom_violin() +
  #geom_jitter() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()


# density
(density_AVG <- ggplot(df_CSO_NUM_long, aes(NUM, color=year, fill=year)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom") )


# density facet
ggplot(df_CSO_NUM_long, aes(NUM, color=year, fill=year)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom") + 
  facet_wrap(~year)
  

(ridges_AVG <- ggplot(df_CSO_NUM_long, aes(x = NUM, y = year, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Number of CSO", option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40)) +
  theme_minimal() )

### AVG ----

# conversion of SpatRaster to df
#df_CSO_NUM_wide <- CSO_AVG[[c(3:40)]] %>% as.data.frame(xy = TRUE)

# random subset sample 
CSO_AVG_subset <- spatSample(CSO_AVG, size=1000, method="random", replace=FALSE, xy=T)

df_CSO_AVG_long <- CSO_AVG_subset %>% 
  select(-X1982, -X1983) %>% 
  pivot_longer(cols= c(X1984:X2021), names_to = "year", values_to = "AVG") %>% 
  mutate(year = str_remove(year, "X"))

ggplot(df_CSO_AVG_long, aes(x,y)) +
  geom_point() +
  theme_minimal()

# time series 
(ts_AVG <- ggplot(df_CSO_AVG_long, aes(year, AVG, color=AVG)) +
    geom_point() +
    geom_jitter(width = 0.5, height = 0.5) +
    scale_colour_viridis_c(option = "D") +
    theme_minimal() +
    scale_y_continuous(expand = c(0, 0), limits = c(-5, 50)) +
    theme(axis.text.x = element_text(angle = 45)))

# box
ggplot(df_CSO_AVG_long, aes(year, AVG, color=AVG)) +
  geom_boxplot(#notch = TRUE
  ) +
  #geom_jitter() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()

# violin
ggplot(df_CSO_AVG_long, aes(year, AVG, color=AVG)) +
  geom_violin() +
  #geom_jitter() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()


# density
(density_AVG <- ggplot(df_CSO_AVG_long, aes(AVG, color=year, fill=year)) +
    geom_density(alpha = 0.05) +
    #geom_jitter() +
    scale_colour_viridis_d(option = "D") +
    scale_fill_viridis_d(option = "D") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
    theme_minimal() +
    guides(col = guide_legend(nrow = 3))+
    theme(legend.position = "bottom") )


# density facet
ggplot(df_CSO_AVG_long, aes(AVG, color=year, fill=year)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom") + 
  facet_wrap(~year)


(ridges_AVG <- ggplot(df_CSO_AVG_long, aes(x = AVG, y = year, fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Avergage days between CSO", option = "D") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 40)) +
    theme_minimal() )


## sentinal 2 ----

# random subset sample 
CSO_S2_subset <- spatSample(CSO_S2, size=8000, method="random", replace=FALSE, xy=T)

df_CSO_S2_long <- CSO_S2_subset %>% 
  pivot_longer(cols= c(X2017:X2021), names_to = "year", values_to = "NUM") %>% 
  mutate(year = str_remove(year, "X"))

ggplot(df_CSO_S2_long, aes(x,y)) +
  geom_point() +
  theme_minimal()

# time series 
(ts_s2 <- ggplot(df_CSO_S2_long, aes(year, NUM, color=NUM)) +
  geom_point() +
  geom_jitter(width = 0.5, height = 0.5) +
  scale_colour_viridis_c(option = "D") +
  theme_minimal() +
    scale_y_continuous(expand = c(0, 0), limits = c(-5, 50)) +
    theme(axis.text.x = element_text(angle = 45)))

# box
ggplot(df_CSO_S2_long, aes(year, NUM, color=NUM)) +
  geom_boxplot(#notch = TRUE
  ) +
  #geom_jitter() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()

# violin
ggplot(df_CSO_S2_long, aes(year, NUM, color=NUM)) +
  geom_violin() +
  #geom_jitter() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()


# density
(density_s2 <- ggplot(df_CSO_S2_long, aes(NUM, color=year, fill=year)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom") )


# density facet
ggplot(df_CSO_S2_long, aes(NUM, color=year, fill=year)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom") + 
  facet_wrap(~year)


(ridges_s2 <- ggplot(df_CSO_S2_long, aes(x = NUM, y = year, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Number of CSO", option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40)) +
  theme_minimal() )

# result vis

(ts_AVG | ts_s2)

(density_s2 / density_AVG)

(ridges_s2/ridges_AVG)

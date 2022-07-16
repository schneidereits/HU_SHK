#library(raster) ----
library(terra)
library(sf)
library(viridis)
library(tmap)
library(tidyverse)
library(magick)   # used to create gifs (process currently outsourced to bash scripts)
library(rasterVis)
library(patchwork)

# data import and wrangel ----

# CSO number
 
link <- "/Users/shawn/Documents/jobs/Humbolt/QGIS/1982-2021_091-319-12_HL_CSO_LNDLG_NUM.vrt.ovr"
CSO_NUM <- rast(link) # read in CSO layers as a terra raster object
years <- c(1982:2021) # create a vector of observation years
names(CSO_NUM) <- years # name layers with corresponding year value
# add terra style z component 
years <- seq(as.Date('1982-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_NUM) <-  years

#writeRaster(CSO_NUM, "/Users/shawn/Documents/jobs/Humbolt/QGIS/1982-2021_091-319-12_HL_CSO_LNDLG_NUM",
#            filetype='GTiff')

# crop rater to speed up data exporlation and vis
e <- ext(0.4, 0.9, 0.4, 0.9) # currently on a 1x1 grid, due to the compressed pyramid that is created via force
CSO_NUM <- crop(CSO_NUM, e)

# test plot of map
plot(CSO_NUM[[20]], legend=F, col = viridis(n=30,option="D"), range=c(0,35))

# CSO Average

link <- "/Users/shawn/Documents/jobs/Humbolt/QGIS/1982-2021_091-319-12_HL_CSO_LNDLG_AVG.vrt.ovr"
CSO_AVG <- rast(link)
years <- c(1982:2021)
names(CSO_AVG) <- years
# add z component
years <- seq(as.Date('1982-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_AVG) <-  years

# test plot of map
plot(CSO_AVG[[20]], legend=F, col = viridis(n=30,option="D"), range=c(0,365))

# Sentinal 2 CSO number

link <- "/Users/shawn/Documents/jobs/Humbolt/QGIS/2017-2021_091-319-12_HL_CSO_SEN2L_NUM.vrt.ovr"
CSO_S2 <- rast(link) 
years <- c(2017:2021)
names(CSO_S2) <- years
# add z component
years <- seq(as.Date('2017-01-01'), as.Date('2021-01-01'), by='year')
time(CSO_S2) <-  years

# test plot of map
plot(CSO_S2, legend=F, col = viridis(n=30,option="D"), range=c(0,70))

# CSO spatial and statisitcal visualization ----

# basic visualization using base R ----

# map plots
# Note that all maps are set to idential color pallete ranges 
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


# Custom visualization ----

# map with geograpic density
rasterVis::levelplot(CSO_NUM[[20]], par.settings = viridisTheme)
# alternative map vis option with contour lines. 
rasterVis::levelplot(CSO_NUM[[20]], par.settings = viridisTheme, contour=TRUE) 

# histogram
rasterVis::histogram(CSO_NUM[[20]], maxpixels = 1e+01)

# density 
rasterVis::densityplot(CSO_NUM[[c(3:40)]], #maxpixels = 1e+03, 
                       par.settings = viridisTheme)

# Box and whiskar plot
bwplot(CSO_NUM[[c(3:40)]],  maxpixels = 1e+01, par.settings = viridisTheme)

# visualization of CSO average
plot(CSO_AVG[[c(3:18)]], legend=F, col = viridis(n=30,option="D"), range=c(1,365))
plot(CSO_AVG[[c(19:35)]], legend=F, col = viridis(n=30,option="D"), range=c(1,365))
plot(CSO_AVG[[c(36:40)]], legend=F, col = viridis(n=30,option="D"), range=c(1,365))


hist(CSO_AVG[[c(3:18)]], main = "CSO count", xlab = "n", ylab= "Frequency", col = "wheat", breaks = 30, xaxt = "n")
hist(CSO_AVG[[c(19:35)]], main = "CSO count", xlab = "n", ylab= "Frequency",
     col = "wheat",  breaks = 30, xaxt = "n")

# advanced data vis ----
# conversion of SpatRaster to df

# CSO  NUM ----

# Convert to data frame
df_CSO_NUM_wide <- CSO_NUM[[c(3:40)]] %>% as.data.frame(xy = TRUE)

# random subset sample 
## used for exploratory data vis and to ease computational costs
CSO_NUM_subset <- spatSample(CSO_NUM, size=1000, method="random", replace=FALSE, xy=T)

df_CSO_NUM_long <- CSO_NUM_subset %>% 
  # deselect years with no observations of DE
  select(-X1982, -X1983) %>%  
  # pivot into a R freindly long formate
  pivot_longer(cols= c(X1984:X2021), names_to = "year", values_to = "NUM") %>% 
  # Clean year values
  mutate(year = str_remove(year, "X"))

# basic plot of the distribution of sampled plots.
# Can be used to inform how dense the subset of CSO observations is
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
  

library(ggridges)

(ridges_AVG <- ggplot(df_CSO_NUM_long, aes(x = NUM, y = year, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Number of CSO", option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40)) +
  theme_minimal() )
  
# S2 NUM ----

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

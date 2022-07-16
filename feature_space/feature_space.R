#library----
library(terra)
#library(sf)
library(viridis)
library(tidyverse)
library(patchwork)
library(ggridges)

# data import and wrangel ----

raw_fs <- read.csv("data/feature_space/20220602_FS_LS4_LS5_LS7_LS8_LS9_1985_2022.csv")

## QA band filtering ----

### Read QA band

### What are the three most frequent values in the qa band?
raw_fs %>% group_by(QAI) %>% tally() %>% arrange(-n)

# can be used in cross reference with https://force-eo.readthedocs.io/en/latest/howto/qai.html?highlight=qai%20bits
rev(as.numeric(intToBits(8192)[1:16]))  
rev(as.numeric(intToBits(0)[1:16])) 
rev(as.numeric(intToBits(10240)[1:16]))  

# a) high confidence clouds or high confidence cloud shadows or fill values
mask_medconf <- function(x){
  bs <- intToBits(x)
  if ( ((bs[1]) | # cloud
        (bs[3]) | # shadow
        (bs[4]) | # snow
        (bs[5]) | # water
        (bs[6]) | # aerosol (only low quality)
        (bs[8]) | # subzero
        (bs[9]) | # saturation
        (bs[10])) # High sun zenith flag
       == 1){
    return("flag") } else {
      return("valid")
    }
}

# due to the nature of the intToBits function it is not possible to perform 
# this step in a mutate pipe. Therefor a vector is created via lapply, 
# joined to then joined to the fs_LND df and finally flaged pixels can be removed
mask_idex <- lapply(raw_fs$QAI, mask_medconf) %>% unlist()

fs_LND_filtered <- raw_fs %>%
  mutate(QAI = mask_idex) %>% 
  filter(QAI == "valid")

# Katja´s method to filter QAI values
raw_fs$qa_cloud <- bitwAnd(bitwShiftR(raw_fs$QAI, 1), 3)
raw_fs$qa_shadow <- bitwAnd(bitwShiftR(raw_fs$QAI, 3), 1)
raw_fs$qa_snow <- bitwAnd(bitwShiftR(raw_fs$QAI, 4), 1)
raw_fs$qa_water <- bitwAnd(bitwShiftR(raw_fs$QAI, 5), 1)
raw_fs$qa_aerosol <- bitwAnd(bitwShiftR(raw_fs$QAI, 6), 3)
raw_fs$qa_subzero <- bitwAnd(bitwShiftR(raw_fs$QAI, 8), 1)
raw_fs$qa_saturation <- bitwAnd(bitwShiftR(raw_fs$QAI, 9), 1)
raw_fs$qa_zenith <- bitwAnd(bitwShiftR(raw_fs$QAI, 10), 1)
raw_fs$qa_illumination <- bitwAnd(bitwShiftR(raw_fs$QAI, 11), 3)
raw_fs$qa_slope <- bitwAnd(bitwShiftR(raw_fs$QAI, 13), 1)
raw_fs$qa_vapor <- bitwAnd(bitwShiftR(raw_fs$QAI, 14), 1)


raw_fs$quality <- 0
raw_fs$quality[raw_fs$qa_cloud != 0 |raw_fs$qa_snow != 0 |raw_fs$qa_shadow != 0] <- 1
raw_fs <- raw_fs[raw_fs$quality == 0, ]

# reduce file size for testing
fs_LND <- fs_LND_filtered %>% #sample_n(10000) %>% 
  # filter out extreme values
  filter(across(BLUE:SWIR2, ~ . > 0),
         across(BLUE:SWIR2, ~ . < 10000)) %>% 
  mutate(date   = as.Date(str_sub(scene, 1, 9),format = "%Y%m%d"),
         month  = lubridate::month(date),
         year   = lubridate::year(date), 
         month_year = paste0(month,"_",year),
         doy    = lubridate::yday(date),
         sensor = str_sub(scene, -5, -1),
         SWIR_ratio = SWIR2/SWIR1,
         NDVI   = ((NIR-RED)/(NIR+RED)),
         NDTI   = ((RED-SWIR2)/(RED+SWIR2))) # source: https://www.mdpi.com/2072-4292/10/10/1657/htm
  


# spectra vis ----

# NDVI time series

ggplot(fs_LND, aes(doy, NDVI, color=year, group=year)) +
  geom_smooth() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()

# NDTI time series
ggplot(fs_LND, aes(doy, NDTI, color=year, group=year)) +
  geom_smooth() +
  scale_colour_viridis_c(option = "D") +
  theme_minimal()

## data wrangel into long ----

fs_LND_long <- fs_LND %>% 
  pivot_longer(cols=c("BLUE":"SWIR2"),names_to = "wavelength", values_to = "reflectance") %>% 
  as.data.frame() #%>% 
  #filter(reflectance >0, 
  #      reflectance <10000)


## boxplot ----
ggplot(fs_LND_long, aes(wavelength, reflectance, color=sensor)) +
 # geom_jitter() +
  geom_boxplot() +
  scale_colour_viridis_d(option = "D") +
  theme_minimal()

## density ----

# full feasable range
ggplot(fs_LND_long, aes(reflectance, color=sensor, fill=sensor)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom")  +
  facet_wrap(~wavelength)

# reduced range
ggplot(fs_LND_long, aes(reflectance, color=sensor, fill=sensor)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 7500)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom")  +
  facet_wrap(~wavelength)


# reduced range
ggplot(fs_LND_long, aes(reflectance, color=wavelength, color=wavelength)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 3))+
  theme(legend.position = "bottom")  +
  facet_wrap(~sensor)



## ridges ----

ggplot(fs_LND_long, aes(x = reflectance, y = sensor, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "reflectance", option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15000)) +
  theme_minimal() +
  facet_wrap(~wavelength)

ggplot(fs_LND_long, aes(x = reflectance, y = wavelength, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "reflectance", option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15000)) +
  theme_minimal() +
  facet_wrap(~sensor)
  
# feature space vis ----

# import reference data ----

reference_spectra <- read.csv("data/feature_space/sli_gen_dark_soils_0p4.csv",
                              encoding = "UTF-8") %>% 
  mutate(SWIR_ratio = SWIR2/SWIR1,
         NDVI   = ((NIR-RED)/(NIR+RED)))

ggplot(reference_spectra, aes(NDVI, SWIR_ratio, color = cover)) +
  geom_point() +
  scale_colour_viridis_d(option = "D") +
  theme_minimal() 

# No dissagregation
ggplot(fs_LND, aes(NDVI, SWIR_ratio)) +
  geom_bin2d(bins = 200) +
  geom_point(data=reference_spectra, aes(NDVI, SWIR_ratio), color ="red") +
  scale_fill_continuous(type = "viridis") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.2, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) 


# facet by sensor
ggplot(fs_LND, aes(NDVI, SWIR_ratio)) +
  geom_bin2d(bins = 200) +
  geom_point(data=reference_spectra, aes(NDVI, SWIR_ratio), color ="red") +
  scale_fill_continuous(type = "viridis") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.2, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) +
  facet_wrap(~sensor)

# facet by month
ggplot(fs_LND, aes(NDVI, SWIR_ratio)) +
  geom_bin2d(bins = 100) +
  geom_point(data=reference_spectra, aes(NDVI, SWIR_ratio), color ="red") +
  scale_fill_continuous(type = "viridis") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.2, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) +
  facet_wrap(~month)


# old computationally exorbinant method of and plotting density plots 

# # Get density of points in 2 dimensions.
# # @param x A numeric vector.
# # @param y A numeric vector.
# # @param n Create a square n by n grid to compute density.
# # @return The density within each square.
# get_density <- function(x, y, ...) {
#   dens <- MASS::kde2d(x, y, ...)
#   ix <- findInterval(x, dens$x)
#   iy <- findInterval(y, dens$y)
#   ii <- cbind(ix, iy)
#   return(dens$z[ii])
# }
# 
# fs_density <- as.data.frame(fs_LND) %>%
#   filter(SWIR_ratio != "Inf") %>% 
#   mutate(density = get_density((.)$NDVI, (.)$SWIR_ratio, n = 500)) 

# ggplot(fs_density, aes(NDVI, SWIR_ratio, color = density)) +
#   geom_point() +
#   geom_point(data=reference_spectra, aes(NDVI, SWIR_ratio), color ="red") +
#   scale_colour_viridis_c(option = "D") +
#   theme_minimal() +
#   scale_x_continuous(expand = c(0, 0), limits = c(-0.2, 1)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) +
#   facet_wrap(~sensor)

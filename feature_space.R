#library----
library(terra)
#library(sf)
library(viridis)
library(tidyverse)
library(patchwork)

# data import and wrangel ----

fs_LND <- read.csv("data/feature_space/20220602_FS_LS_1985_2020.csv")
# reduce file size for testing
fs_LND <- fs_LND %>% sample_n(100000) %>% 
  mutate(date   = as.Date(str_sub(scene, 1, 9),format = "%Y%m%d"),
         doy    = lubridate::yday(date),
         sensor = str_sub(scene, -5, -1))

# QA band filtering ----

### Read QA band

### What are the three most frequent values in the qa band?
fs_LND %>% group_by(QAI) %>% tally() %>% arrange(-n)

# can be used in cross reference with https://force-eo.readthedocs.io/en/latest/howto/qai.html?highlight=qai%20bits
rev(as.numeric(intToBits(8192)[1:16]))  
rev(as.numeric(intToBits(0)[1:16])) 
rev(as.numeric(intToBits(10240)[1:16]))  

# a) high confidence clouds or high confidence cloud shadows or fill values
mask_medconf <- function(x){
  bs <- intToBits(x)
  if ( ((bs[1]) | (bs[1])| (bs[7]) | (bs[9])) == 1){
    return("flag") } else {
      return("valid")
    }
  
}

# filtering out flaged pixels
fs_LND_filtered <- fs_LND %>% 
  #mutate(mask = mask_medconf(QAI)) %>% 
  filter(mask_medconf(QAI) == "valid")

# spectra vis ----

fs_LND_long <- fs_LND_filtered %>% 
  pivot_longer(cols=c("BLUE":"SWIR2"),names_to = "wavelength", values_to = "reflectance")

## boxplot ----
ggplot(fs_LND_long, aes(wavelength, reflectance, color=sensor)) +
  geom_boxplot() +
  theme_minimal()

## density ----

# full feasable range
ggplot(fs_LND_long, aes(reflectance, color=sensor, fill=sensor)) +
  geom_density(alpha = 0.05) +
  #geom_jitter() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15000)) +
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



## ridges ----
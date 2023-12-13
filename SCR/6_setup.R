# Installs libraries and creates global functions & objects used in other R scripts

# note that the R scripts should be run in the same R session so that these variables
# are accessible to all scripts

library(tidyverse)
library(lubridate)
library(broom)
library(sf)
library(rnaturalearth)
library(RColorBrewer)
library(viridis)
library(gridExtra)
library(raster)
library(mgcv)
library(DHARMa)
library(GGally)
library(ggeffects)
library(visreg)
library(osmdata)
library(nlme)
library(lme4)
library(multilevelTools)
library(JWileymisc)
library(weathermetrics)
library(ggridges)
library(ggpubr)
library(caret)

theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())+
            theme(strip.background =element_rect(fill="white")))

readMultiFiles <- function(directory){
  
  files <- list.files(directory, pattern='*.csv', full.names=TRUE)
  raw <- files %>% 
    map_df(~read_csv(.))
  return (raw)
  
}

biomeLookup <- tibble(BIOME_NAME = c("N/A",
                                     "Mangroves",
                                     "Temperate Conifer Forests",
                                     "Deserts & Xeric Shrublands",
                                     "Flooded Grasslands & Savannas",
                                     "Montane Grasslands & Shrublands",
                                     "Temperate Broadleaf & Mixed Forests",
                                     "Mediterranean Forests, Woodlands & Scrub",
                                     "Tropical & Subtropical Coniferous Forests",
                                     "Temperate Grasslands, Savannas & Shrublands",
                                     "Tropical & Subtropical Dry Broadleaf Forests",
                                     "Tropical & Subtropical Moist Broadleaf Forests",
                                     "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                     "Boreal Forests/Taiga",
                                     "Tundra"),
                      biome_num = c("0",
                                    "1",
                                    "2",
                                    "3",
                                    "4",
                                    "5",
                                    "6",
                                    "7",
                                    "8",
                                    "9",
                                    "10",
                                    "11",
                                    "12",
                                    "13",
                                    "14"))


world_shp <- ne_countries(scale = "medium", returnclass = "sf")

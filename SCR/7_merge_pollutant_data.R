# This script imports pre-downloaded pollutant data from EEA and EPA and joins

#### EEA data import ---------------------------------------------------

eeaRef <- read_delim('./DATA/EEA/EEA_sites.txt')%>%
  mutate(Longitude = longitude, 
         Latitude = latitude,
         AirQualityStation = site,
         AirQualityStationArea = site_area) %>%
  # Filter to mainland Europe
  filter(Longitude > -13 & Longitude < 50) %>%
  filter(Latitude > 30 & Latitude < 70) %>%
  # Filter to urban and suburban
  #filter(AirQualityStationArea %in% c('urban', 'suburban')) %>%
  dplyr::select(AirQualityStation, Latitude, Longitude, AirQualityStationArea)

nrow(eeaRef)
length(unique(eeaRef$AirQualityStation))
str(eeaRef)

names(eeaRef)
eeaRef %>% ggplot(aes(x=AirQualityStationArea)) + geom_bar() + coord_flip()

world_shp %>%
  ggplot() +
  geom_sf()+
  geom_point(data = eeaRef , aes(x=Longitude, y=Latitude, color=AirQualityStationArea)) +
  xlim(c(min(eeaRef$Longitude), max(eeaRef$Longitude)))+
  ylim(c(min(eeaRef$Latitude), max(eeaRef$Latitude)))


#### EPA data import -----------------------------------------------------------
epaRefRaw <- read_csv('./DATA/EPA/Site_properties.csv') %>%
  drop_na(Latitude, Longitude,Site_id)%>%
  # recode variable names to EEA metadata names
  mutate(AirQualityStationArea = `Location Setting`,
         AirQualityStation = Site_id) 

# Transform coordinates
epaRef_wgs <- st_as_sf(epaRefRaw %>% filter(Datum == 'WGS84'),
                       coords = c('Latitude', 'Longitude'),
                       crs = 4326)
epaRef_nad <- st_as_sf(epaRefRaw %>% filter(Datum == 'NAD83'),
                       coords = c('Latitude', 'Longitude'),
                       crs = 6783)
epaRef_nad_trans <- st_transform(epaRef_nad, st_crs(4326))

epaRefCoords <- epaRef_wgs %>%
  bind_rows(epaRef_nad_trans) %>%
  as_tibble() %>%
  mutate(Longitude = unlist(map(.$geometry,2)),
         Latitude = unlist(map(.$geometry,1))) %>%
  dplyr::select(AirQualityStation, Latitude, Longitude)%>%
  # Filter for USA mainland
  filter(Longitude > -130 & Longitude < -50) %>%
  filter(Latitude > 25)


epaRef <- epaRefRaw %>% dplyr::select(-Latitude, -Longitude) %>%
  left_join(epaRefCoords) %>%
  drop_na(Latitude) %>%
  group_by(AirQualityStation, Latitude, Longitude, AirQualityStationArea) %>%
  summarise() %>%
  mutate(AirQualityStation = as.character(AirQualityStation))
epaRef
colSums(is.na(epaRef))
nrow(epaRef)
length(unique(epaRef$AirQualityStation))

epaRef %>% ggplot(aes(x=AirQualityStationArea)) + geom_bar() + coord_flip()

world_shp %>%
  ggplot() +
  geom_sf()+
  geom_point(data = epaRef , aes(x=Longitude, y=Latitude, color=AirQualityStationArea)) +
  xlim(c(min(epaRef$Longitude), max(epaRef$Longitude)))+
  ylim(c(min(epaRef$Latitude), max(epaRef$Latitude)))

#### Merge and export to GEE where metadata will be collected ------------------------------------------

joinedRaw <- eeaRef %>% mutate(source = 'EEA') %>%
  bind_rows(epaRef %>% mutate(source = 'EPA') ) %>%
  dplyr::select(AirQualityStation, Latitude, Longitude, source)

length(joinedRaw$AirQualityStation)
length(unique(joinedRaw$AirQualityStation))

joinedRaw %>%
  write_csv('./DATA/For_GEE/airquality_stations_locations_raw.csv')

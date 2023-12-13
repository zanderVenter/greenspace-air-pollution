# Imports the metadata for stations collected in GEE, filters, and cleans pollutant 
# time series based on data with >75% coverage over given time intervals

# Import merged data
joinedRaw <- read_csv('./DATA/For_GEE/airquality_stations_locations_raw.csv')

# Import after running GEE script that collects metadata
hsl <- read_csv('./DATA/From_GEE/hsl_stations.csv') %>%
  mutate(hslCat = ifelse(mode < 2, 'Rural', 
                         ifelse(mode > 2 & mode < 3, 'Suburban', 'Urban'))) %>%
  left_join(joinedRaw, by = 'AirQualityStation') %>%
  dplyr::select(AirQualityStation, source, hslCat)

hsl %>% ggplot(aes(x=hslCat, fill=source)) + geom_bar() + coord_flip()


#### EEA data clean ---------------------------------------------------

eeaNo2 <- read_delim('./DATA/EEA/EEA_NO2_Annual_daily_summerized_microg_m3.txt')  %>%
  mutate(AirQualityStation = site,
         AirPollutant = 'NO2',
         Concentration = value,
         date = floor_date(dmy(DATE), 'month'),
         year = year(date),
         month = month(date))  %>%
  group_by(AirQualityStation, AirPollutant, date, year) %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE)) %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(eeaNo2$AirQualityStation))
hist(eeaNo2$Concentration)
eeaNo2 %>% ggplot(aes(x=date)) + geom_histogram()
eeaNo2 %>% ggplot(aes(x=nReadingsMonth)) + geom_histogram()


eeaPM10 <- read_delim('./DATA/EEA/EEA_PM10_Annual_daily_summerized_microg_m3.txt') %>%
  mutate(AirQualityStation = site,
         AirPollutant = 'PM10',
         Concentration = value,
         date = floor_date(dmy(DATE), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year)  %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE)) %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(eeaPM10$AirQualityStation))
eeaPM10 %>% ggplot(aes(x=date)) + geom_histogram()
eeaNo2 %>% ggplot(aes(x=nReadingsMonth)) + geom_histogram()

eeaPM25 <- read_delim('./DATA/EEA/EEA_PM25_Annual_daily_summerized_microg_m3.txt') %>%
  mutate(AirQualityStation = site,
         AirPollutant = 'PM25',
         Concentration = value,
         date = floor_date(dmy(DATE), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year)  %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE)) %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(eeaPM25$AirQualityStation))
eeaPM25 %>% ggplot(aes(x=date)) + geom_histogram()
eeaPM25 %>% ggplot(aes(x=nReadingsMonth)) + geom_histogram()
eeaPM25 %>% ggplot(aes(x=Concentration)) + geom_histogram() 

eeaO3 <- read_delim('./DATA/EEA/EEA_O3_Annual_daily_summerized_microg_m3.txt') %>%
  mutate(AirQualityStation = site,
         AirPollutant = 'O3',
         Concentration = value,
         date = floor_date(dmy(DATE), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year)  %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE)) %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(eeaO3$AirQualityStation))
eeaO3 %>% ggplot(aes(x=date)) + geom_histogram()
eeaO3 %>% ggplot(aes(x=nReadingsMonth)) + geom_histogram()
eeaO3 %>% ggplot(aes(x=Concentration)) + geom_histogram() 

# Create monthly filtered time series
eeaPollutantsTsMonth_raw <- eeaNo2 %>%
  bind_rows(eeaPM10)%>%
  bind_rows(eeaPM25) %>%
  bind_rows(eeaO3) 
print(paste0('Nr stations raw: ', length(unique(eeaPollutantsTsMonth_raw$AirQualityStation))))

eeaPollutantsTsMonth_f1 <- eeaPollutantsTsMonth_raw %>%
  mutate(AirQualityStation = as.character(AirQualityStation)) %>%
  # Filter for only urban stations
  left_join(hsl, by = 'AirQualityStation')  %>%
  filter(hslCat %in% c('Urban', 'Suburban')) 
print(paste0('Nr stations after urban filter: ', length(unique(eeaPollutantsTsMonth_f1$AirQualityStation))))


eeaPollutantsTsMonth_f2 <- eeaPollutantsTsMonth_f1 %>%
  # Filter for months with > 75% of days with recordings ie. > 22 days
  filter(nReadingsMonth > 22) %>%
  group_by(AirQualityStation, AirPollutant, year) %>%
  mutate(nObsYear = n()) %>%
  # Filter for > 75% of the year with recordings - ie. 9 out of 12 months
  filter(nObsYear > 9) %>%
  # Filter for unrealistic concentrations  - 94 cases
  filter(Concentration < 500 & Concentration > 0)
print(paste0('Nr stations after day and month filter: ', length(unique(eeaPollutantsTsMonth_f2$AirQualityStation))))

eeaPollutantsTsMonth <- eeaPollutantsTsMonth_f2
length(unique(eeaPollutantsTsMonth$AirQualityStation))
eeaPollutantsTsMonth %>% ggplot(aes(x=nObsYear)) + geom_histogram()
eeaPollutantsTsMonth %>% ggplot(aes(x=Concentration)) + geom_histogram()

eeaPollutantsTsMonth %>% ggplot(aes(x=date, fill=AirPollutant)) + geom_histogram()

#### EPA data clean ---------------------------------------------------

epaNo2 <- read_delim('./DATA/EPA/EPA_NO2_daily_2010_2021_Parts_per_billion.txt') %>%
  # Convert from ppb to ug/m3 using ratio 1 ppb = 1.88 µg/m3 NO2 molecular weight	46.01 g/mol
  mutate(Concentration = ArithmeticMean*1.88)   %>%
  mutate(AirQualityStation = Site_id,
         AirPollutant = 'NO2',
         date = floor_date(ymd(DateLocal), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year) %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE))  %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(epaNo2$AirQualityStation))
hist(epaNo2$Concentration)
epaNo2 %>% ggplot(aes(x=date)) + geom_histogram()

epaPM10 <- read_delim('./DATA/EPA/EPA_PM_10_daily_2010_2021_microg_m3.txt') %>%
  mutate(Concentration = ArithmeticMean)   %>%
  mutate(AirQualityStation = Site_id,
         AirPollutant = 'PM10',
         date = floor_date(ymd(DateLocal), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year) %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE))  %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(epaPM10$AirQualityStation))
epaPM10 %>% ggplot(aes(x=date)) + geom_histogram()

epaPM25 <- read_delim('./DATA/EPA/EPA_PM_25_daily_2010_2021_microg_m3.txt') %>%
  mutate(Concentration = ArithmeticMean)   %>%
  mutate(AirQualityStation = Site_id,
         AirPollutant = 'PM25',
         date = floor_date(ymd(DateLocal), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year) %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE))  %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(epaPM25$AirQualityStation))
epaPM25 %>% ggplot(aes(x=date)) + geom_histogram()


epaO3 <- read_delim('./DATA/EPA/EPA_O3_daily_2010_2022_ppm.txt') %>%
  # Convert from ppm to ug/m3 using ratio 1 ppb = 1.96 µg/m3 NO2 molecular weight	47.988 g/mol
  mutate(Concentration = ArithmeticMean*1000*1.96)   %>%
  mutate(AirQualityStation = Site_id,
         AirPollutant = 'O3',
         date = floor_date(ymd(DateLocal), 'month'),
         year = year(date),
         month = month(date)) %>%
  group_by(AirQualityStation, AirPollutant, date, year) %>%
  summarise(nReadingsMonth =  sum(!is.na(Concentration)),
            Concentration = mean(Concentration, na.rm=TRUE))  %>%
  dplyr::select(AirQualityStation:Concentration)
length(unique(epaO3$AirQualityStation))
hist(epaO3$Concentration)
epaO3 %>% ggplot(aes(x=date)) + geom_histogram()
epaO3 %>% ggplot(aes(x=Concentration)) + geom_histogram()


epaPollutantsTsMonth_raw <- epaNo2 %>%
  bind_rows(epaPM10) %>%
  bind_rows(epaPM25) %>%
  bind_rows(epaO3)  
print(paste0('Nr stations raw: ', length(unique(epaPollutantsTsMonth_raw$AirQualityStation))))

epaPollutantsTsMonth_f1 <- epaPollutantsTsMonth_raw %>%
  mutate(AirQualityStation = as.character(AirQualityStation)) %>%
  # Filter for only urban stations
  left_join(hsl, by = 'AirQualityStation')  %>%
  filter(hslCat %in% c('Urban', 'Suburban')) 
print(paste0('Nr stations after urban filter: ', length(unique(epaPollutantsTsMonth_f1$AirQualityStation))))


epaPollutantsTsMonth_f2 <- epaPollutantsTsMonth_f1 %>%
  mutate(AirQualityStation = as.character(AirQualityStation)) %>%
  # Filter for months with > 75% of days with recordings
  filter(nReadingsMonth > 22) %>%
  group_by(AirQualityStation, AirPollutant, year) %>%
  mutate(nObsYear = n()) %>%
  # Filter for > 75% of the year with recordings - ie. 9 out of 12 months
  filter(nObsYear > 9) %>%
  # Filter for unrealistic concentrations  - 94 cases
  filter(Concentration < 500 & Concentration > 0)

epaPollutantsTsMonth <- epaPollutantsTsMonth_f2

length(unique(epaPollutantsTsMonth$AirQualityStation))
epaPollutantsTsMonth %>% ggplot(aes(x=Concentration)) + geom_histogram()
epaPollutantsTsMonth %>% ggplot(aes(x=date)) + geom_histogram()

print(paste0('Nr stations after day and month filter: ', length(unique(epaPollutantsTsMonth_f2$AirQualityStation))))


#### Merge and export ---------------------------------------------------------

siteLocations <- eeaRef %>% mutate(source = 'EEA') %>%
  bind_rows(epaRef %>% mutate(source = 'EPA')) 

pollutantsTs <- eeaPollutantsTsMonth %>% mutate(source = 'EEA') %>%
  bind_rows(epaPollutantsTsMonth %>% mutate(source = 'EPA') ) %>%
  dplyr::select( -nReadingsMonth, -nObsYear)  %>%
  filter(year < 2020)

length(unique(pollutantsTs$AirQualityStation))

siteLocationsFinal <- siteLocations %>%
  filter(AirQualityStation %in% unique(pollutantsTs$AirQualityStation))

length(unique(pollutantsTs$AirQualityStation))

# Write out metadata
siteLocationsFinal %>%
  write_csv('./DATA/station_merged_metadata.csv')

siteLocationsFinal %>%
  group_by(source, AirQualityStationArea) %>%
  summarise(n=n())

# Write out for GEE
siteLocationsFinal %>%
  dplyr::select(AirQualityStation, Latitude, Longitude, source)%>%
  write_csv('./DATA/For_GEE/airquality_stations_locations.csv')

pollutantsTsFinal <- pollutantsTs %>%
  filter(AirQualityStation %in% unique(siteLocationsFinal$AirQualityStation))
length(unique(pollutantsTsFinal$AirQualityStation))

pollutantsTsFinal %>%
  write_csv('./DATA/pollutant_monthly_merged_cleaned.csv')

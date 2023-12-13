# Performs bulk of statistical analysis and produces figures in the manuscript

#### Import datasets ----------------------------------------------------------------
biomes <- read_csv('./DATA/From_GEE/biomes_stations.csv') %>% 
  mutate(biome = factor(first)) %>%
  dplyr::select(-'.geo',-'system:index',-first)

stationMetadata <- read_csv('./DATA/station_merged_metadata.csv') %>%
  left_join(read_csv('./DATA/stations_roads_buildings.csv')) %>%
  left_join(biomes)
levels(stationMetadata$biome) <- biomeLookup[biomeLookup$biome_num %in% levels(stationMetadata$biome), ]$BIOME_NAME
stationMetadata %>% ggplot(aes(y=biome)) + geom_bar()

stationMetadata <-  stationMetadata %>%
  mutate(biomeRecoded = recode_factor(biome, "Tropical & Subtropical Grasslands, Savannas & Shrublands" = 'Savanna/grassland',
                                      "Flooded Grasslands & Savannas"  = 'Savanna/grassland',
                                      "Deserts & Xeric Shrublands" = 'Mediterranean forest/shrubland',
                                      "Mediterranean Forests, Woodlands & Scrub" = 'Mediterranean forest/shrubland',
                                      "Tropical & Subtropical Coniferous Forests" = 'Forest coniferous',
                                      "Temperate Broadleaf & Mixed Forests" = 'Forest mixed',
                                      "Temperate Conifer Forests" = 'Forest coniferous',
                                      "Temperate Grasslands, Savannas & Shrublands" = 'Savanna/grassland',
                                      "Boreal Forests/Taiga" = 'Forest coniferous',
                                      "Tundra"  = 'Savanna/grassland',
                                      "Mangroves" =  'Forest mixed'))

# Pollutant time series
pollutatnTs <- read_csv('./DATA/pollutant_monthly_merged_cleaned.csv') 
length(unique(pollutatnTs$AirQualityStation))

# Create metadata data.frame with time series stats
pollutatnTsStat <- pollutatnTs %>%
  group_by(AirQualityStation, AirPollutant, year, source) %>%
  summarise(meanConc = mean(Concentration, na.rm=T)) %>%
  group_by(AirQualityStation, AirPollutant, source) %>%
  summarise(n=n(), 
            startYear = min(year), 
            endYear = max(year), 
            meanConc = mean(meanConc))
hist(pollutatnTsStat$n)

# Get station numbers for paper
getStationNumbers <- function(){
  
  for (i in seq(1,9)){
    num <- pollutatnTsStat %>%
      filter(n > i)
    print(paste0('#stations after year filter', i, '___', length(unique(num$AirQualityStation))))
    print(paste0('# timeseries  after year filter', i, '___', nrow(num)))
  }
}
getStationNumbers()


# Filter using threshold of > 6 years data (< 3 missing) ie. 75% complete data
finalStat <- pollutatnTsStat %>%
  filter(n > 6)
finalStat %>% 
  group_by(source, AirQualityStation) %>% 
  summarise() %>%
  group_by(source) %>%
  summarise(n = n())
print(paste0('#stations after year filter','___', length(unique(finalStat$AirQualityStation))))
print(paste0('# timeseries  after year filter', '___', nrow(finalStat)))

# Climate data
era5 <- read_csv('./DATA/From_GEE/era5_ts.csv') %>%
  mutate_at(vars(dewpoint_temperature_2m, temperature_2m), function(x) x - 273.15) %>%
  mutate(wind_abs = sqrt(u_component_of_wind_10m^2 + v_component_of_wind_10m^2),
         total_precipitation = total_precipitation*1000,
         relative_humidity = dewpoint.to.humidity(dp = dewpoint_temperature_2m, t = temperature_2m, temperature.metric = "celsius"),
         date = ymd(date)) %>%
  dplyr::select(AirQualityStation, date, surface_pressure,wind_abs, total_precipitation, relative_humidity, temperature_2m)  %>%
  mutate(year = year(date)) %>%
  group_by(AirQualityStation, year) %>%
  summarise_at(vars(surface_pressure:temperature_2m), mean, na.rm=T)%>% 
  ungroup() 
colSums(is.na(era5))
length(unique(era5$AirQualityStation))
era5 %>%
  gather(key, val, -year, -AirQualityStation) %>%
  ggplot(aes(x=val)) + geom_histogram() + facet_wrap(~key, scales='free')

# NDVI time series
ndvi <- readMultiFiles('./DATA/From_GEE/NDVI/')  %>%
  mutate(year = year(date)) %>%
  group_by(AirQualityStation, year, buffer) %>%
  summarise(ndvi = median(ndvi, na.rm=T))%>% 
  ungroup() 
length(unique(ndvi$AirQualityStation))
hist(ndvi$ndvi)
colSums(is.na(ndvi))
ndvi %>% ggplot(aes(x=as.factor(year))) +  geom_bar()
ndvi %>% ggplot(aes(x=as.factor(buffer))) +  geom_bar()

# Tree cover time series
treeCover <-  readMultiFiles('./DATA/From_GEE/Tree_cover/') %>%
  mutate(year = year(date)) %>%
  dplyr::select(AirQualityStation, year, buffer, treePerc)

treeCover %>% ggplot(aes(x=as.factor(year))) +  geom_bar()
treeCover %>% ggplot(aes(x=as.factor(buffer))) +  geom_bar()
treeCover %>%
  filter(buffer == 120) %>%
  ggplot(aes(x=year, y=treePerc)) +
  geom_smooth(method='lm')

# Emissions time series
emissions <- read_delim('./DATA/EDGAR/EDGARv6_Glb_anthro_NOx_yearly_sum_sectors_total_Tg_2000_2019.txt') %>%
  mutate(AirPollutant = 'NO2') %>%
  bind_rows(read_delim('./DATA/EDGAR/EDGARv6_Glb_anthro_PM10_yearly_sum_sectors_total_Tg_2000_2019.txt') %>%
              mutate(AirPollutant = 'PM10'))%>%
  bind_rows(read_delim('./DATA/EDGAR/EDGARv6_Glb_anthro_PM25_yearly_sum_sectors_total_Tg_2000_2019.txt') %>%
              mutate(AirPollutant = 'PM25')) %>%
  bind_rows(read_delim('./DATA/EDGAR/EDGARv6_Glb_anthro_NOx_yearly_sum_sectors_total_Tg_2000_2019.txt') %>%
              mutate(AirPollutant = 'O3') ) %>%
  filter(site %in% unique(stationMetadata$AirQualityStation)) %>%
  gather(year, totEmission, -site, -Origin, -AirPollutant) %>%
  mutate(AirQualityStation = site, 
         year = as.numeric(substr(year, 4, 7))) %>%
  dplyr::select(AirQualityStation, AirPollutant, year, totEmission)


#### Station distribution -------------------------------------------------------------------------
length(unique(stationMetadata$AirQualityStation))

stationMetadataToPlot <- stationMetadata %>%
  left_join(pollutatnTsStat %>%
              group_by(AirQualityStation) %>%
              summarise(n = max(n)), by = 'AirQualityStation') %>%
  # Filter for at least 6 years in time series
  filter(n > 6)

length(unique(stationMetadataToPlot$AirQualityStation))

theme_legend <- theme(legend.key.size = unit(0.45, 'cm'), #change legend key size
                      legend.key.height = unit(0.45, 'cm'), #change legend key height
                      legend.key.width = unit(0.45, 'cm'), #change legend key width
                      legend.title = element_text(size=8), #change legend title font size
                      legend.text = element_text(size=8))

dev.off()

subPlotTheme <- theme(axis.text = element_text(size=6),
                      axis.title = element_text(size=6),
                      axis.line.y.left = element_line(size=0.2),
                      axis.line.x.bottom = element_line(size=0.2),
                      panel.background = element_rect(fill = "transparent"),
                      plot.background=element_rect(fill = "transparent", color='transparent'),
                      panel.border = element_blank(),)

biomePlot <- stationMetadataToPlot %>%
  group_by(biomeRecoded) %>%
  summarise(n=n()) %>%
  ggplot(aes(y=biomeRecoded, x=n, fill=biomeRecoded)) +
  geom_bar(stat='identity')  +
  geom_text(aes(label = biomeRecoded, x=50), hjust=0, size=2.5) +
  scale_fill_manual(values = c( '#7f59b0', '#fcb424', '#63c5ea', '#ea4b8b')) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x='Number stations') +
  subPlotTheme +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

biomePlot

roadHist <- stationMetadataToPlot %>%
  ggplot(aes(x=roadProximity)) +
  labs(y='Number stations', x = 'Distance from road (m)') +
  geom_histogram()+
  subPlotTheme 
roadHist

buildingHist <- stationMetadataToPlot %>%
  # filter out 2 anomalous buffers where the UTM projection did not pull through and gave spuriously large areas
  filter(buildingFootprint < (3.14*(30^2))) %>%
  ggplot(aes(x=buildingFootprint)) +
  labs(y='Number stations', x = 'Building footprint within 30m (m2)') +
  geom_histogram()+
  subPlotTheme 
buildingHist

subPlots <- grid.arrange( biomePlot, roadHist, buildingHist,   ncol=1,
                          padding = unit(0, "line"),heights=c(1,1,1), newpage = T)

makeDistribMap <- function(){
  world_shp %>%
    ggplot() +
    geom_sf(size=0.2, fill='#f5f4f4')+
    geom_point(data = stationMetadataToPlot , 
               aes(x=Longitude, y=Latitude, color=biomeRecoded, size=source), 
               shape=21) +
    scale_color_manual(values = c( '#7f59b0', '#fcb424', '#63c5ea', '#ea4b8b')) +
    scale_size_manual(values = c(0.3, 0.6), guide = "none") +
    xlim(c(min(stationMetadataToPlot$Longitude), max(stationMetadataToPlot$Longitude)))+
    ylim(c(min(stationMetadataToPlot$Latitude), 65)) +
    theme(legend.position = '',
          legend.background = element_rect(fill="transparent", color="transparent"),
          axis.title = element_blank(),
          axis.text = element_text(size = 6)) +
    annotation_custom(
      ggplotGrob(biomePlot ),
      xmin = -127, xmax = -92, ymin = 48, ymax = 66)+
    annotation_custom(
      ggplotGrob(roadHist ),
      xmin = -55, xmax = -20, ymin = 45, ymax = 65)+
    annotation_custom(
      ggplotGrob(buildingHist ),
      xmin = -55, xmax = -20, ymin = 25, ymax = 45)
}
distribMap <-makeDistribMap()
distribMap


#### Correlation between pollutants -----------------------------------------------------


pollutatnTs %>%
  group_by(AirQualityStation, AirPollutant) %>%
  summarise() %>%
  group_by(AirQualityStation) %>%
  summarise(n=n()) %>%
  ggplot() + geom_histogram(aes(x=n))

s <- 'derp041'
outCor <- tibble()
for (s in unique(pollutatnTs$AirQualityStation)){
  print(paste0('Working correlations for: ', s))
  subdf <- pollutatnTs %>% filter(AirQualityStation == s)
  
  if (length(unique(subdf$AirPollutant)) == 1) next
  
  subdfwide <- subdf %>%
    dplyr::select(AirQualityStation, AirPollutant, Concentration, date) %>%
    pivot_wider(values_from = Concentration, names_from = AirPollutant) %>%
    drop_na()
  
  cdf <- cor(subdfwide[, unique(subdf$AirPollutant) ]) %>% as_tibble()
  
  p <- 'NO2'
  for (p in unique(subdf$AirPollutant)){
    outCor <- outCor %>%
      bind_rows(tibble(
        AirQualityStation = s,
        AirPollutant = p, 
        corrPollutant = colnames(cdf), 
        r = cdf[, p][[p]]))
  }
  
}
corrFinal <- outCor %>%
  filter(AirPollutant != corrPollutant) %>%
  mutate(corrGroup = paste0(AirPollutant, ' ~ ', corrPollutant)) %>%
  filter(!corrGroup %in% c("O3 ~ NO2", "PM10 ~ NO2", "PM25 ~ NO2",
                           "PM10 ~ O3", "PM25 ~ O3", "PM25 ~ PM10") ) %>%
  left_join(stationMetadata, by = 'AirQualityStation')

corrFinal %>%
  group_by(AirQualityStation, source) %>%
  summarise() %>%
  group_by(source) %>%
  summarise(n= n())

makeCorrelationFigure <- function(){
  corrFinal %>%
    ggplot(aes(x=r)) +
    geom_histogram() +
    facet_wrap(~corrGroup) +
    geom_vline(xintercept = 0, linetype = 2) + 
    ylab('Number of stations') + 
    xlab('Correlation coefficient')
}

ggsave("supp_corr.png", makeCorrelationFigure(), width = 20, height=12, units='cm')



#### Pollutant and NDVI trends -----------------------------------------------------------------------

randSubSet <- stationMetadata %>%
  sample_n(500)
randSubSet <- randSubSet$AirQualityStation

pollutatnTsToPlot <- pollutatnTs  %>%
  filter(AirQualityStation %in% randSubSet) %>%
  left_join(pollutatnTsStat, by = c('AirQualityStation', 'AirPollutant')) 

pollutatnTsToPlotAvg <- pollutatnTs  %>%
  left_join(pollutatnTsStat, by = c('AirQualityStation', 'AirPollutant'))  %>%
  group_by(date, AirPollutant) %>%
  summarise(Concentration = median(Concentration))


cyl_names <- c(
  'NO2' = 'NO[2]',
  'PM10' = 'PM[10]',
  'PM25' = 'PM[2.5]',
  'O3' = 'O[3]'
)

makeTSplot <- function(){
  dummy <- tibble(date = c(ymd(20100101),ymd(20100101),ymd(20100101)), Concentration = c(1,1,1),
                  colorDummy = c('Station monthly avg', 'Total monthly avg', 'Linear trend'))
  
  pollutatnTsToPlot %>%
    filter(n >= 6) %>%
    ggplot(aes(x=date, y=Concentration)) +
    geom_line(alpha=0.04, aes(group = AirQualityStation), color='grey') +
    geom_line(data = pollutatnTsToPlotAvg, size=0.4) +
    geom_smooth(data = pollutatnTsToPlotAvg,
                method='lm', linetype=2, color='#008a7c', fill='#008a7c', alpha=0.4) +
    geom_line(data = dummy, aes(color=colorDummy)) +
    facet_grid(~AirPollutant, labeller = labeller(AirPollutant  = as_labeller(cyl_names,  label_parsed)))+
    scale_color_manual(values = c('#008a7c', 'grey', 'black')) +
    scale_y_continuous(limits = c(0, 90),
                       expand = c(0, 0),
                       oob = scales::oob_squish) +
    labs(y = expression(paste("[Pollutant]", " (", mu, g, "/", m^3,")", sep=""))) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=9),
          axis.text = element_text(size=8),
          legend.position = c(0.85,0.8),
          legend.title = element_blank(),
          legend.key.size = unit(0.5, 'cm'),
          legend.text = element_text(size=7),
          legend.background = element_rect(fill = "transparent"),
          strip.text = element_text(
            size = 8, color = "black",
            margin = grid::unit(c(1,0,1,0), "mm")
          ))
}
makeTSplot()

tsPlot <- makeTSplot()

mapAndTSplot <- grid.arrange( distribMap, tsPlot, ncol=1,
                           padding = unit(0, "line"),heights=c(2,1.1), newpage = T)

mapAndTSplot

mapAndTSplot <- ggarrange( distribMap, tsPlot,
                           nrow = 2, heights = c(2, 1.2),
                           labels = c("A", "B"))+
  theme(plot.margin = margin(0.1,0.1,2,0.1, "cm")) 

ggsave("fig1.png", mapAndTSplot, width = 24, height=16, units='cm')
ggsave("fig1.tiff", mapAndTSplot, width = 24, height=16, units='cm')

pollutatnTsFiltered <- pollutatnTs  %>%
  left_join(pollutatnTsStat,  by = c('AirQualityStation', 'AirPollutant'))  %>%
  filter(n > 6)

pollutants <- c('NO2', 'PM10', 'PM25', 'O3')
trendDF <- tibble()
for (p in pollutants){
  
  model.lme.trend <- lme(log(Concentration)~ year,
                         random = ~ 1|AirQualityStation,
                         data = pollutatnTsFiltered %>%
                           left_join(stationMetadata) %>%
                           filter(AirPollutant == p),
                         method = "REML", 
                         na.action = na.omit,
                         control = lmeControl(opt = 'optim'))
  summary(model.lme.trend)
  
  #plot(model.lme.trend, resid(., type = "p") ~ fitted(.), abline = 0)
  
  coeffs <- as.data.frame(intervals(model.lme.trend,which = "fixed")$fixed)
  trendDF <- trendDF %>%
    bind_rows(tibble(AirPollutant = p,
                     est = coeffs$est.[2],
                     lowCI = coeffs$lower[2],
                     upCI = coeffs$upper[2]) %>%
                mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})) 
}

getTrendStats <- function(){
  trendDF %>% mutate(error = est - lowCI)
  
  pollutatnTsFiltered %>%
    filter(year %in% c(2010, 2019)) %>%
    group_by(AirPollutant, year) %>%
    summarise(Concentration = median(Concentration, na.rm=T)) %>%
    pivot_wider(values_from=Concentration, names_from=year) %>%
    mutate(change = `2019`-`2010`)
}
getTrendStats()

pollutants <- c('NO2', 'PM10', 'PM25', 'O3')
biomes <- unique(stationMetadata$biomeRecoded)
trendDFbiomes <- tibble()
for (p in pollutants){
  for (b in biomes){
    
    model.lme.trend <- lme(log(Concentration)~ year,
                           random = ~ 1|AirQualityStation,
                           data = pollutatnTsFiltered %>%
                             left_join(stationMetadata) %>%
                             filter(biomeRecoded == b) %>%
                             filter(AirPollutant == p),
                           method = "REML", 
                           na.action = na.omit,
                           control = lmeControl(opt = 'optim'))
    summary(model.lme.trend)
    
    #plot(model.lme.trend, resid(., type = "p") ~ fitted(.), abline = 0)
    
    coeffs <- as.data.frame(intervals(model.lme.trend,which = "fixed")$fixed)
    trendDFbiomes <- trendDFbiomes %>%
      bind_rows(tibble(AirPollutant = p,
                       biome = b,
                       est = coeffs$est.[2],
                       lowCI = coeffs$lower[2],
                       upCI = coeffs$upper[2]) %>%
                  mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})) 
  }
  
}



getTrendStatsBiomes <- function(){
  trendDFbiomes %>%
    group_by(biome) %>%
    summarise_at(vars(est:upCI), mean)
}
getTrendStatsBiomes()

joinedTrendsPollutant <- pollutatnTsFiltered %>%
  #sample_n(10000) %>%
  group_by(AirQualityStation, AirPollutant, year) %>%
  summarise_at(vars(Concentration), mean, na.rm=T) %>%
  pivot_wider(names_from = AirPollutant, values_from = Concentration) %>%
  gather(AirPollutant, val, O3:NO2)%>% 
  drop_na(val) %>%
  group_by(AirQualityStation,AirPollutant)  %>%
  nest() %>% 
  mutate(model = map(data, ~lm(log(val) ~ year, data = .x))) %>%
  mutate(tidy = map(model, tidy),
         glance = map(model, glance),
         augment = map(model, augment),
         pval = glance %>% map_dbl('p.value'),
         rsq = glance %>% map_dbl('r.squared'),
         slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))) %>%
  mutate(slope = (exp(slope) - 1) * 100 ) %>%
  dplyr::select(AirQualityStation, AirPollutant, slope, pval) %>%
  mutate(sig = ifelse(pval < 0.05, 'Sig', 'Non-sig'))

colSums(is.na(joinedTrendsPollutant))

joinedTrendsPollutant  %>%
  ggplot(aes(x=slope)) +
  geom_histogram() +
  facet_wrap(~AirPollutant)

variable <- 'PM10'
lims <-  c(-7, 7)
title <- 'test'

subPlotTheme <- theme(axis.text = element_text(size=6),
                      axis.title = element_text(size=6),
                      panel.grid.major = element_line(),
                      panel.background = element_rect(fill = "white"),
                      plot.background=element_rect(fill = "transparent", color='transparent'),
                      panel.border = element_blank(),)
makeLinearTrendMap <- function(variable, lims, insetLims, title){
  
  estPlotBiomes <- trendDFbiomes %>%
    filter(AirPollutant == variable) %>%
    ggplot(aes(x=biome, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0.25) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype=2) +
    ylim(insetLims) +
    ylab(paste0(expression(Delta)," [",variable,"] % ", expression(yr^-1))) +
    theme(axis.title.y = element_blank()) +
    subPlotTheme
  estPlotBiomes
  
  joinedTrendsPollutant %>% ungroup() %>%
    filter(AirPollutant == variable) %>%
    drop_na(slope) %>%
    drop_na(sig) %>%
    left_join(stationMetadata) %>% 
    drop_na(Longitude) %>%
    ggplot()+
    geom_sf(data = world_shp, 
            color = '#000000',
            size = 0.05)  +
    geom_point(aes(x = Longitude, y = Latitude, color = slope, size=source, shape = sig), stroke=0.5, fill=NA)+
    xlim(c(min(stationMetadata$Longitude), max(stationMetadata$Longitude)))+
    ylim(c(min(stationMetadata$Latitude), max(stationMetadata$Latitude))) +
    scale_color_gradientn(colors=rev(brewer.pal(10, 'Spectral')),
                          limits = lims, 
                          oob = scales::squish) +
    scale_shape_manual(values = c(2, 19)) +
    scale_size_manual(values = c(0.6, 1.1), guide = "none") +
    annotation_custom(
      ggplotGrob(estPlotBiomes ),
      xmin = -127, xmax = -40, ymin = 48, ymax = 66) +
    ggtitle(title) +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_blank(),
          legend.position = c(0.55, 0.5))
}

pt1 <- makeLinearTrendMap('NO2', c(-7, 7), c(-4.3, 0), expression(paste(Delta," ", NO[2]," (%/yr)", sep="")))
pt1
pt2 <- makeLinearTrendMap('PM10', c(-7, 7), c(-4.3, 0), expression(paste(Delta," ", PM10," (%/yr)", sep="")))
pt2
pt3 <- makeLinearTrendMap('PM25', c(-7, 7), c(-4.3, 0), expression(paste(Delta," ", PM2.5," (%/yr)", sep="")))
pt3
pt4 <- makeLinearTrendMap('O3', c(-7, 7), c(-1, 1.5), expression(paste(Delta," ", O[3]," (%/yr)", sep="")))
pt4
trendMaps <- grid.arrange( pt1, pt2, pt3, pt4,  ncol=1,
                           padding = unit(0, "line"),heights=c(1,1,1, 1), newpage = T)
ggsave("supp_trendMaps.png", trendMaps, width = 20, height=30, units='cm')


#### Get NDVI trends for manual-screening analysis ---------------------------------------------------

joinedTrendsNDVI <- pollutatnTsFiltered %>%
  group_by(AirQualityStation, AirPollutant, year) %>%
  summarise_at(vars(Concentration), mean, na.rm=T) %>%
  left_join(ndvi %>%
              filter(buffer == 60) , by = c('year', 'AirQualityStation')) %>%
  pivot_wider(names_from = AirPollutant, values_from = Concentration) %>%
  gather(var, val, ndvi:NO2)%>% 
  drop_na(val) %>%
  group_by(AirQualityStation,var)  %>%
  nest() %>% 
  mutate(model = map(data, ~lm(val ~ year, data = .x))) %>%
  mutate(tidy = map(model, tidy),
         glance = map(model, glance),
         augment = map(model, augment),
         rsq = glance %>% map_dbl('r.squared'),
         slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))) %>%
  dplyr::select(AirQualityStation, var, slope)  %>%
  pivot_wider(values_from=slope, names_from=var)

ndviLosses <- joinedTrendsNDVI %>%
  gather(AirPollutant, Concentration, O3:NO2) %>%
  drop_na(Concentration) %>%
  filter(ndvi < 0) %>%
  dplyr::select(AirQualityStation, ndvi, AirPollutant) %>%
  ungroup() %>%
  mutate(percCutoff = quantile(ndvi, c(0.25), na.rm=T)) %>%
  #filter(ndvi < percCutoff) %>%
  dplyr::select(-percCutoff) %>%
  top_n(-50, ndvi)
ndviLosses

ndviGains <- joinedTrendsNDVI%>%
  gather(AirPollutant, Concentration, O3:NO2) %>%
  drop_na(Concentration)%>%
  filter(ndvi > 0) %>%
  dplyr::select(AirQualityStation, ndvi, AirPollutant) %>%
  ungroup() %>%
  mutate(percCutoff = quantile(ndvi, c(0.25), na.rm=T)) %>%
  #filter(ndvi < percCutoff) %>%
  dplyr::select(-percCutoff) %>%
  top_n(50, ndvi)
ndviGains


set.seed(345)
ndviToAnalyze <- ndviLosses %>%
  bind_rows(ndviGains) %>%
  mutate(ID = sample(seq(1,nrow(.), 1))) %>%
  arrange(ID)
ndviToAnalyze
ndviToAnalyze[c(1:2),]


names <- c('zander', 'nuria','philipp','amir','erik','sourangsu')
i <- 1

ndviOutOut <- tibble()

for (i in c(1:length(names))){
  nameOut <- names[i]
  end <- i*40
  start <- end-40
  ndviOut <- ndviToAnalyze[c(start : end),]
  
  ndviOutOut <- ndviOutOut %>%
    bind_rows(ndviOut %>% mutate(name = nameOut))
  
  st_as_sf(ndviOut %>% 
             left_join(stationMetadata) %>%
             distinct(AirQualityStation, .keep_all = T), coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_set_crs(4326) %>%
    mutate(geometry = st_buffer(geometry, 60)) %>%
    mutate(name = AirQualityStation) %>%
    dplyr::select(name) %>%
    st_write(paste0('./DATA/For_samplers/ndviChanges_',nameOut,'.kml'), append=FALSE)
}

write_csv(ndviOutOut, './DATA/For_samplers/ndviOutOut.csv')

#### Modelling at manually-screened locations -------------------------------------------------------------------

# After completing the manual photointerpretation of vegetation changes at the above sites
# import here and perform statistical modelling

groundTruthRaw <- read_csv('./DATA/From_samplers/manual_landcover_change_data.csv')%>%
  mutate(changeGreen = (tree_p + grass_p) - (tree_h + grass_h),
         changeTree = tree_p - tree_h,
         changeGrass = grass_p- grass_h) %>% 
  left_join(pollutatnTsStat %>%
              group_by(AirQualityStation) %>%
              summarise(n = max(n)), by = 'AirQualityStation') %>%
  filter(changeGreen != 0) %>%
  mutate(changeDirectionAll = ifelse(changeGreen > 0, 'increase', 'decrease'),
         changeDirectionTree = ifelse(changeTree > 0, 'increase', 'decrease')) %>%
  drop_na(changeDirectionAll)


groundTruth <- groundTruthRaw

# After first round of review, we were asked to perform a cross-validation to see
# how well samplers photointerpretation compared 
# Write out 10 random locations for sampler cross-validation
set.seed(123)
crossval <- groundTruth %>%
  ungroup() %>%
  sample_n(15)

kmlCrossVal <- st_as_sf(stationMetadata %>%
           filter(AirQualityStation %in% crossval$AirQualityStation) %>%
           distinct(AirQualityStation, .keep_all = T), coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_set_crs(4326) %>%
  mutate(geometry = st_buffer(geometry, 60)) %>%
  mutate(name = AirQualityStation) %>%
  dplyr::select(name) 

kmlCrossVal%>%
  st_write(paste0('./DATA/For_samplers/cross_validation_sample.kml'), append=FALSE)

# Get summary stats for paper
getManualChangeStats <- function(){
  print(groundTruth %>%
    group_by(changeDirectionAll) %>%
    summarise(n=n(),
              avChngGreen = mean(changeGreen)))
  
  print(groundTruth %>%
    group_by(changeDirectionTree) %>%
    summarise(n=n(),
              avChngTree = mean(changeTree)))
}
getManualChangeStats()

# Join with pollutatnt time series
joined60 <- pollutatnTs  %>%
  filter(AirQualityStation %in% unique(groundTruth$AirQualityStation)) %>%
  pivot_wider(values_from=Concentration, names_from=AirPollutant) %>%
  # Filter for 60m buffer - see sensitivity analysis
  left_join(ndvi %>%
              filter(buffer == 60) %>%
              dplyr::select(-buffer), by=c('AirQualityStation','year')) %>%
  left_join(era5, by=c('AirQualityStation','year')) %>%
  group_by(AirQualityStation, year) %>%
  summarise_at(vars(NO2:temperature_2m), mean, na.rm=T) %>%
  left_join(stationMetadata) 

# Get linear trend coefficients for explanatory variables
joinedTrendsExplan <- joined60   %>%
  # gathering by air pollutant and dropping NAs so that the 
  # response variable time series match those of the pollutants 
  gather(AirPollutant, Concentration, NO2:O3) %>%
  drop_na(Concentration) %>%
  gather(var, val, ndvi:temperature_2m) %>%
  group_by(AirQualityStation,AirPollutant,var) %>% 
  drop_na(val) %>%
  nest() %>% 
  mutate(model = map(data, ~lm(val ~ year, data = .x))) %>%
  mutate(tidy = map(model, tidy),
         glance = map(model, glance),
         augment = map(model, augment),
         rsq = glance %>% map_dbl('r.squared'),
         slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))) %>%
  dplyr::select(AirQualityStation, var, slope)  %>%
  pivot_wider(values_from=slope, names_from=var)


sum(unique(joinedTrendsPollutant$AirQualityStation) %in% unique(groundTruth$AirQualityStation))

# Join manually-verified veg cahnges with pollutatnt trends
subsetDF <- groundTruth %>%
  dplyr::select(AirQualityStation, sampler, changeGreen,changeGrass, changeTree, changeDirectionAll, changeDirectionTree) %>%
  left_join(joinedTrendsExplan, by = 'AirQualityStation') %>%
  left_join(joinedTrendsPollutant %>%
              mutate(pollutantTrend = slope), by = c('AirQualityStation', 'AirPollutant')) %>%
  drop_na(changeGreen, pollutantTrend)


# Iterate over pollutants to perform mixed-effects models to estimate difference between 
# increase or decrease in veg cover
pollutants <- c('NO2', 'PM10', 'PM25', 'O3')

focusDFOut <- tibble()
p <- 'PM25'
for (p in pollutants){
  
  for (r in c('changeDirectionAll', 'changeDirectionTree')){
    subsetDatToModel <- subsetDF %>%
      filter(AirPollutant == p)%>%
      ungroup() %>%
      mutate_at(vars(total_precipitation:surface_pressure), BBmisc::normalize) 
    subsetDatToModel$changeCat <- subsetDatToModel[[r]]
    
    model.lme <- lme(pollutantTrend~ changeCat +
                       #changeTree +
                       total_precipitation +
                       relative_humidity +
                       temperature_2m +
                       surface_pressure+
                       wind_abs ,
                     random = ~ 1|sampler,
                     data = subsetDatToModel,
                     method = "REML", 
                     na.action = na.omit,
                     control = lmeControl(opt = 'optim'))
    
    
    # summary(model.lme)
    # MuMIn::r.squaredGLMM(model.lme)
    # AIC(model.lme)
    # acf(resid(model.lme), lag.max = 36, main = "ACF")
    # plot(model.lme, resid(., type = "p") ~ fitted(.), abline = 0)
    
    ano <- anova(model.lme)
    ano <- as_tibble(ano) %>%
      mutate(var = rownames(ano))
    pvals <- ano %>%
      filter(var %in% c('changeCat'))
    
    
    focusDFOut <- focusDFOut %>%
      bind_rows(ano %>%
                  dplyr::select(var, `p-value`) %>%
                  mutate(response = p,
                         changeVar = r))
  }

    
  
}

sigTable <- focusDFOut %>%
  filter(var %in% c('changeCat'))
sigTable

# Plot boxplot of change direciton and time series

cyl_names <- c(
  'NO2' = 'NO[2]',
  'PM10' = 'PM[10]',
  'PM25' = 'PM[2.5]',
  'O3'= 'O[3]'
)

makeChangeBoxplot <- function(changeCat, xlab){
  
  sigTableToPlot <- sigTable %>%
    filter(changeVar == changeCat) %>%
    mutate(changeDirection = 'Gain',
           AirPollutant = response, 
           pollutantTrend = 1)
  
  boxPlot <- subsetDF %>%
    mutate(changeDirection = .[[changeCat]]) %>%
    mutate(changeDirection = ifelse(changeDirection == 'increase', 'Gain', 'Loss')) %>%
    ggplot(aes(x=changeDirection, y = pollutantTrend)) +
    geom_hline(yintercept = 0, linetype=2,  size=0.2) +
    geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
        seed = 2, width = .1
      )
    )  + 
    geom_boxplot(
      width = .25, 
      fill= alpha('white', 0),
      outlier.shape = NA
    ) +
    #scale_y_continuous(limits = c(-3,1.1),
    #                   oob = scales::oob_squish) +
    geom_text(data = sigTableToPlot, aes(label = paste0('p = ', round(`p-value`, 2))),
              nudge_x = 0.5, size=3) +
    labs(y = expression(paste(Delta, " [pollutant] (% ", yr^-1,")", sep="")),
         x = xlab) +
    facet_grid(~AirPollutant, labeller = labeller(AirPollutant  = as_labeller(cyl_names,  label_parsed))) +
    theme(legend.position = 'none') 
      
  boxPlot
  
  return (boxPlot)
  
}
cp1 <- makeChangeBoxplot('changeDirectionAll', 'Verified change in total green space')
cp2 <- makeChangeBoxplot('changeDirectionTree', 'Verified change in tree cover')


manualChangePlot <- ggarrange( cp1, cp2,
                           nrow = 2, heights = c(1,1),
                           labels = c("A", "B"))+
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) 
manualChangePlot
ggsave("fig5.png", manualChangePlot, width = 20, height=13, units='cm')
ggsave("fig5.tiff", manualChangePlot, width = 20, height=13, units='cm')



# Write out the top stations with changes to get satellite images of change for Fig. 4
locsForFig <- st_as_sf(subsetDF %>% 
          dplyr::select(AirQualityStation, changeGreen) %>%
           left_join(stationMetadata) %>%
           distinct(AirQualityStation, .keep_all = T), coords = c("Longitude", "Latitude"), crs = 4326)%>%
  arrange(changeGreen) 

# Write these out and then take screenshots in Google Earth and save as .png files in ./DATA/
locsForFig %>%
  st_set_crs(4326) %>%
  mutate(geometry = st_buffer(geometry, 60)) %>%
  mutate(name = AirQualityStation) %>%
  dplyr::select(name) %>%
  st_write(paste0('./DATA/For_samplers/ndviChanges_setForFigures.kml'), append=FALSE)

selectedGain <- c('es1499a', 'es1424a','es1244a', 'es1697a', 'at4s407')

focusPlotTheme <- theme(
  legend.position = c(0.5, 0.9),
  legend.title = element_blank(),
  legend.margin = margin(0, 0, 0, 0),
  #legend.background = element_rect(element_rect(fill="transparent", color="transparent")),
  plot.title = element_text(size=12),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size=10),
  axis.text.y.right = element_text(color = '#219c51'),
  axis.title.y.right = element_text(color = '#219c51', size=10)
)

img1 <- png::readPNG('./DATA/refPicGain_historical.png') 
img2 <- png::readPNG('./DATA/refPicGain_present.png') 

imgPlot1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  background_image(img1) +
  #annotate("text", x = 6, y = 4.4, label = "Some text") +
  coord_equal() +
  theme(axis.title = element_blank()) +
  labs(title = 'A) 2010')
imgPlot1
imgPlot2 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  background_image(img2) +
  coord_equal() +
  theme(axis.title = element_blank())+
  labs(title = 'B) 2019')

tsPlot <- joined60 %>%
  filter(AirQualityStation == selectedGain[1]) %>% 
  gather(pol, val, NO2:PM25) %>%
  drop_na(val) %>%
  ggplot(aes(x=year, y=val)) +
  geom_point(aes(shape=pol), size=2) +
  geom_line(aes(linetype=pol)) + 
  geom_line( aes(y=(ndvi*80)-20), color='#219c51', size=1.7, alpha=0.5, linejoin='round') + # Divide by 10 to get the same range than the temperature
  guides(shape = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) + 
  scale_y_continuous(
    name = expression(paste("[Pollutant]", " (", mu, g, "/", m^3,")", sep="")),
    sec.axis = sec_axis(~(.+20)/(80), name="NDVI (green space)")
  )+
  scale_x_continuous(breaks = seq(2010,2019,2)) +
  focusPlotTheme +
  labs(title = 'C)')

focus1 <- ggarrange(imgPlot1, imgPlot2, tsPlot, widths = c(1,1,2.6),
          ncol = 3, nrow = 1)
focus1

img3 <- png::readPNG('./DATA/refPicLoss_historical.png') 
img4 <- png::readPNG('./DATA/refPicLoss_present.png') 

imgPlot3 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  background_image(img3) +
  #annotate("text", x = 6, y = 4.4, label = "Some text") +
  coord_equal() +
  theme(axis.title = element_blank()) +
  labs(title = 'D) 2010')
imgPlot3
imgPlot4 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  background_image(img4) +
  coord_equal() +
  theme(axis.title = element_blank())+
  labs(title = 'E) 2019')

selectedLoss <- c('pl0049a', 'deby053', 'it1582a', 'denw053')
#transFactors <- (ndvi*23)+7), 'deby053'

tsPlot2 <- joined60 %>%
  filter(AirQualityStation == selectedLoss[4]) %>% 
  gather(pol, val, NO2:PM25) %>%
  drop_na(val) %>%
  ggplot(aes(x=year, y=val)) +
  geom_point(aes(shape=pol), size=2) +
  geom_line(aes(linetype=pol)) + 
  geom_line( aes(y=(ndvi*70)-12), color='#219c51', size=1.7, alpha=0.5, linejoin='round') + # Divide by 10 to get the same range than the temperature
  guides(shape = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) + 
  scale_y_continuous(
    limits = c(13,32),
    name = expression(paste("[Pollutant]", " (", mu, g, "/", m^3,")", sep="")),
    sec.axis = sec_axis(~(.+12)/(70), name="NDVI (green space)")
  )+
  scale_x_continuous(breaks = seq(2010,2019,2)) +
  focusPlotTheme+
  labs(title = 'F)')
tsPlot2
dev.off()
focus2 <- ggarrange(imgPlot3, imgPlot4, tsPlot2, widths = c(1,1,2.6),
                    ncol = 3, nrow = 1)

focus2

focusAll <- ggarrange(focus1, focus2,  heights = c(1,1),
                    ncol = 1, nrow = 2)
focusAll
dev.off()

ggsave("fig4.png", focusAll, width = 25, height=12, units='cm')

ggsave("fig4.tiff", focusAll, width = 25, height=12, units='cm')



#### Cross-validation for manually-screened locations ----------------------------------------------------

# After reviewer request, we performed cross-validation of sampler photointerpretations of veg change
crossVal <-  read_csv('./DATA/From_samplers/cross_validation.csv') %>%
  mutate(green_h = (tree_h + grass_h + agriculture_h) ,
         green_p = (tree_p + grass_p + agriculture_p) ,
         changeGreen = green_p - green_h,
         changeTree = tree_p - tree_h)

# Categorize 
crossValCat <- crossVal %>%
  mutate(changeGreenCat = ifelse(changeGreen < -10, 'Loss', 
                                 ifelse(changeGreen > 10, 'Gain', 'Stable'))) %>%
  mutate(changeTreeCat = ifelse(changeTree < -10, 'Loss', 
                                ifelse(changeTree > 10, 'Gain', 'Stable')))%>%
  gather(key, val, changeGreenCat, changeTreeCat) %>%
  dplyr::select(AirQualityStation, sampler, key, val) %>%
  pivot_wider(names_from = sampler, values_from = val) %>%
  mutate_at(vars(zander:amir), factor, levels = c('Gain', 'Loss', 'Stable'))

# balanced accuracy is the average of the 
# sensitivity (True Positive Rate) and specificity (True Negative Rate)
cvOut <- tibble()
i <- 'changeGreenCat'


getAcc <- function(x, y ){
  cm <- confusionMatrix(x, y)
  cmClass <- as.data.frame(cm$byClass)
  return (cmClass$`Balanced Accuracy`)
}

for (i in unique(crossValCat$key)){
  
  c <- crossValCat %>%
    filter(key == i)
  
  cm <- confusionMatrix(c$zander, c$erik)
  cm$byClass
  cSub <- tibble(
    key = i,
    labs = c(rep("S1 ~ S2", 3),
             rep("S1 ~ S3", 3),
             rep("S1 ~ S4", 3),
             rep( "S2 ~ S3", 3),
             rep("S2 ~ S4", 3),
             rep("S3 ~ S4", 3)),
    cats = rep(c('Gain', 'Loss', 'Stable'), 6),
    corr = c(getAcc(c$zander, c$erik), 
             getAcc(c$zander, c$philipp),
             getAcc(c$zander, c$amir),
             getAcc(c$erik, c$philipp),
             getAcc(c$erik, c$amir),
             getAcc(c$philipp, c$amir))
  )
  cvOut <- cvOut %>% bind_rows(cSub)
}

cvFormatted <- cvOut %>%
  pivot_wider(values_from = corr, names_from = labs)%>% 
  replace(is.na(.), 1)

cvFormatted %>%
  write_csv('./DATA/cross_val_results.csv')

#### Modelling lme all stations iterate ------------------------------------------------------------------------------


# randSubSet <- stationMetadata %>%
#   sample_n(1000)
# randSubSet <- randSubSet$AirQualityStation

pollutantTsAnnual <- pollutatnTs %>%
  group_by(AirQualityStation, AirPollutant, year) %>%
  summarise(ConcentrationMean = mean(Concentration, na.rm=T),
            ConcentrationMax = max(Concentration, na.rm=T))

# Iterate over buffer sizes, biomes, resposne variable types, and tree/ndvi
# we also iterate over time series lengths to see if the length of a time series
# affects the results (not reported in manuscript)
bufferSizes <- unique(ndvi$buffer)
s <- 30
tsLengths <- c(6) #c(4,5 6,7,8, 9)
t <- 5
biomes <- as.character(unique(stationMetadata$biomeRecoded))
b <- 'Forest coniferous'
pollutants <- c('NO2', 'PM10', 'PM25', 'O3')
p <- 'NO2'
responseVars <- c('ConcentrationMean', 'ConcentrationMax')
r <- 'ConcentrationMax'
greenSpaceTypes <- c('treePerc', 'ndvi')

dfOut <- tibble()
sdOut <- tibble()

for (s in bufferSizes){
  
  for (r in responseVars){
    
    for (x in greenSpaceTypes){
      ndviSelect <- ndvi %>%
        mutate(green = ndvi) %>%
        filter(buffer == s) %>%
        dplyr::select(-buffer, -ndvi)
      
      treeSelect <- treeCover %>%
        mutate(green = treePerc) %>%
        filter(buffer == s) %>%
        dplyr::select(-buffer, -treePerc)
      
      if ( x == 'treePerc'){
        greenSelect <- treeSelect
      } else {
        greenSelect <- ndviSelect
      }
      
      if (x == 'treePerc' & s < 120) {
        print('skipping......')
        next
      }
        
      for (t in tsLengths){
        
        joined <- pollutantTsAnnual  %>%
          left_join(pollutatnTsStat, by = c('AirQualityStation', 'AirPollutant')) %>%
          #filter(AirQualityStation %in% randSubSet) %>%
          # Filter for time series with more than t years %>%
          filter(n > t) %>%
          left_join(greenSelect , by=c('AirQualityStation','year')) %>%
          left_join(emissions, by=c('AirQualityStation','AirPollutant', 'year')) %>%
          left_join(era5, by=c('AirQualityStation','year'))  %>%
          left_join(stationMetadata,  by=c('AirQualityStation')) 
        
        
        for (b in biomes){
          
          
          for (p in pollutants){
            print(paste0('Buffer ----- ', s))
            print(paste0('Response var ----- ', r))
            print(paste0('Green space types ----- ', x))
            print(paste0('Time series length ----- ', t))
            print(paste0('Biome ----- ', b))
            print(paste0('Pollutant ----- ', p))
            
            
            modelDF_raw <- joined  %>%
              filter(biomeRecoded == b) %>%
              filter(AirPollutant == p) %>%
              ungroup() %>%
              mutate(nyear  = year,
                     AirQualityStation = factor(AirQualityStation)) %>%
              ungroup() 
            names(modelDF_raw)
            sds <- modelDF_raw %>%
              ungroup() %>%
              summarise_at(vars(green:temperature_2m), sd, na.rm=T)
            sds
            
            modelDF <- modelDF_raw %>%
              mutate_at(vars(green:temperature_2m), BBmisc::normalize) %>%
              dplyr::select(AirQualityStation, nyear, green:temperature_2m, ConcentrationMax, ConcentrationMean)
            
            modelDF$response <- modelDF[[r]]
            
            model.lme <- lme(log(response) ~ green + 
                               nyear +
                               totEmission + 
                               #AirQualityStation +
                               total_precipitation +
                               relative_humidity +
                               temperature_2m +
                               wind_abs ,
                             random = ~ 1  | AirQualityStation,
                             data = modelDF ,
                             method = "REML", 
                             na.action = na.omit,
                             control = lmeControl(opt = 'optim'))
            
            
            coeffs <- as.data.frame(intervals(model.lme,which = "fixed")$fixed)
            coeffs$var <- rownames(coeffs)
            coeffsToPlot <- coeffs %>%
              mutate(est = est.,
                     lowCI = lower,
                     upCI = upper)  %>%
              filter(var != '(Intercept)') %>%
              filter(!str_detect(var, 'year')) %>%
              filter(!str_detect(var, 'AirQualityStation')) %>%
              dplyr::select(var, est, lowCI, upCI)
            
            sdOut <- sdOut %>% bind_rows(sds %>%
                                           mutate(tsLength = t,
                                                  buffer = s,
                                                  responseVar = r,
                                                  greenSpaceVar = x,
                                                  biome = b,
                                                  pollutant = p))
            
            dfOut <- dfOut %>% bind_rows(coeffsToPlot %>%
                                           mutate(tsLength = t,
                                                  buffer = s,
                                                  responseVar = r,
                                                  greenSpaceVar = x,
                                                  biome = b,
                                                  pollutant = p,
                                                  aic = AIC(model.lme),
                                                  r2m = MuMIn::r.squaredGLMM(model.lme)[[1]],
                                                  r2c = MuMIn::r.squaredGLMM(model.lme)[[2]]))
            
          }
          
          
        }
      }
      
      
    }
    
  }
  
  
  
}

dfOut %>%
  write_csv('./DATA/lme_modelling_results.csv')
sdOut %>%
  write_csv('./DATA/lme_modelling_stDevs.csv')


dfOut <- read_csv('./DATA/lme_modelling_results.csv')
sdOut <- read_csv('./DATA/lme_modelling_stDevs.csv')

# Set buffer categories
dfOut <- dfOut %>%
  mutate(bufferCat = ifelse(buffer <= 60, 'Street', 
                            ifelse(buffer > 60 & buffer <= 1000, 'Borough', 'City'))) %>%
  mutate(bufferCat = factor(bufferCat, levels = c('Street', 'Borough', 'City')))



coeffGreen <- dfOut %>%
  filter(var %in% c('green')) %>%
  filter(tsLength == 6) %>%
  mutate(var = greenSpaceVar)  %>%
  group_by(responseVar, pollutant, var) %>%
  summarise_at(vars(est:upCI), mean)

coeffOthers <- dfOut %>%
  filter(!var %in% c('green')) %>%
  filter(tsLength == 6)%>%
  group_by(responseVar, pollutant, var) %>%
  summarise_at(vars(est:upCI), mean)

coeffAll <- coeffGreen %>%
  bind_rows(coeffOthers)

coeffOverall <- coeffAll %>%
  group_by(responseVar, pollutant, var)%>%
  summarise_at(vars(est:upCI), mean) %>%
  mutate(biome = 'Overall')

coeffOverall_interp <- coeffOverall %>%
    mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})  %>%
    mutate_if(is.numeric, round, 3)


coeffOverall_interp %>% 
  filter(var %in% c('ndvi', 'treePerc')) %>%
  ungroup() %>%
  #group_by(var) %>%
  summarise_at(vars(est:upCI), mean) %>%
  mutate(error = est-lowCI)


getCoeffStats <- function(){
  coeffAll %>%
    mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )}) %>%
    #group_by(var) %>%
    group_by(var)  %>%
    summarise_at(vars(est:upCI), mean) %>%
    mutate(sig = ifelse((lowCI > 0 & upCI > 0) | (lowCI < 0 & upCI < 0), 'sig', 'nonSig' ))
}
getCoeffStats()

getCoeffStats_byPol <- function(){
  View(coeffAll %>%
         mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})%>%
         mutate(sig = ifelse((lowCI > 0 & upCI > 0) | (lowCI < 0 & upCI < 0), 'sig', 'nonSig' )))
}
getCoeffStats_byPol()

coeffToPlot <- coeffAll %>%
  mutate(var = factor(var, levels = c("relative_humidity", "temperature_2m", "total_precipitation", "wind_abs", "totEmission", "treePerc", "ndvi"))) %>%
  mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})

levels(coeffToPlot$var) <- c("Relative humidity", "Temperature", "Precipitation", "Wind speed", "Emissions","Tree cover", "NDVI")

cyl_names <- c(
  'NO2' = 'NO[2]',
  'PM10' = 'PM[10]',
  'PM25' = 'PM[2.5]',
  'O3' = 'O[3]'
)
makePredictorPlot <- function(response, lab){
  coeffToPlot %>%
    filter(responseVar == response) %>%
    ggplot(aes(x=var, y = est, color = var, size=abs(est))) +
    geom_point(position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0, position=position_dodge(width = 0.5), size=0.2) +
    scale_size_continuous(range = c(0.1, 2.5), limits = c(0,9), name = '') +
    geom_hline(yintercept = 0, linetype=2) +
    facet_grid(.~pollutant, labeller = labeller(pollutant  = as_labeller(cyl_names,  label_parsed))) +
    theme(axis.title = element_text(size = 10)) +
    guides(size = FALSE) +
    labs(y =  lab,
         color = '') +
    scale_color_manual(values = c('#000000', '#000000', '#000000', '#000000', '#000000', '#0ec3c7','#f9837b'))+
    coord_flip(clip = "off", ylim = c(-12, 12)) +
    theme(axis.title.y = element_blank(),
          legend.position = 'none',
          plot.title= element_text(size = 10))
}


ped1 <- makePredictorPlot('ConcentrationMean', expression('%'~Delta~' in mean [pollutant] per'~sigma~' increase in predictor')) +
  theme(legend.position = 'none')
ped1
ped2 <- makePredictorPlot('ConcentrationMax',expression('%'~Delta~' in peak [pollutant] per'~sigma~' increase in predictor')) +
  theme(legend.position = 'none')
ped2

finalPredictorPlot <- ggarrange( ped1, ped2,
                               nrow = 2, heights = c(1,1),
                               labels = c("A", "B"))+
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
finalPredictorPlot

ggsave("fig2.png", finalPredictorPlot, width = 22, height=14, units='cm')

ggsave("fig2.tiff", finalPredictorPlot, width = 22, height=14, units='cm')





response <- 'ConcentrationMean'
makeBuffZoneEst_responseVar <- function(response, lab){
  dfOut %>%
    filter(responseVar == response) %>%
    filter(var == 'green') %>%
    filter(tsLength == 6) %>%
    mutate(greenSpaceVar = ifelse(greenSpaceVar == 'ndvi', 'NDVI', 'Tree Cover')) %>%
    #mutate(bufferCat = ifelse(bufferCat == 'Borough', '           Borough', bufferCat)) %>%
    group_by(pollutant, greenSpaceVar, bufferCat)%>%
    summarise_at(vars(est:upCI), mean) %>%
    mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})%>%
    mutate(sig = ifelse((lowCI > 0 & upCI > 0) | (lowCI < 0 & upCI < 0), 'Significant', 'Non-significant' )) %>%
    ggplot(aes(x=bufferCat, y = est, color = greenSpaceVar, size=abs(est))) +
    geom_point(position=position_dodge(width = 0.5)) +
    #geom_smooth(method='lm', formula = y~poly(x,2), se=F, linetype=2) +
    geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0, position=position_dodge(width = 0.5), size=0.2) +
    #scale_x_log10()+
    scale_size_continuous(range = c(0.1, 2.5), name = '') +
    geom_hline(yintercept = 0, linetype=2) +
    facet_grid(.~pollutant, labeller = labeller(pollutant  = as_labeller(cyl_names,  label_parsed))) +
    theme(axis.title = element_text(size = 10)) +
    guides(size = FALSE) +
    labs(y =  lab,
         x = 'Spatial scale',
         color = '') +
    coord_flip()
}
makeBuffZoneEst_responseVar('ConcentrationMean', '')

makeBiomeEstPlot <- function(){
  dfOut %>%
    filter(responseVar == 'ConcentrationMean') %>%
    filter(var == 'green') %>%
    filter(tsLength == 6) %>%
    mutate(greenSpaceVar = ifelse(greenSpaceVar == 'ndvi', 'NDVI', 'Tree Cover')) %>%
    group_by(pollutant, greenSpaceVar, biome)%>%
    summarise_at(vars(est:upCI), mean, na.rm=T) %>%
    mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})%>%
    mutate(biome = str_wrap(biome, width = 15)) %>%
    ggplot(aes(x=biome, y = est, color = greenSpaceVar, size=abs(est))) +
    geom_point(position=position_dodge(width = 0.5)) +
    #geom_smooth(method='lm', formula = y~poly(x,2), se=F, linetype=2) +
    geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0, position=position_dodge(width = 0.5), size=0.2) +
    scale_size_continuous(range = c(0.1, 2.5), limits = c(0,8), name = '') +
    geom_hline(yintercept = 0, linetype=2) +
    facet_grid(.~pollutant, labeller = labeller(pollutant  = as_labeller(cyl_names,  label_parsed))) +
    theme(axis.title = element_text(size = 10)) +
    guides(size = FALSE) +
    labs(y =  expression('%'~Delta~' in mean [pollutant] per'~sigma~' increase in predictor'),
         x = 'Biome',
         color = '') +
    coord_flip(clip = "off", ylim = c(-10, 10)) +
    theme(legend.position = 'top',
          plot.title= element_text(size = 10))
}

bze1 <- makeBuffZoneEst_responseVar('ConcentrationMean', expression('%'~Delta~' in mean [pollutant] per'~sigma~' increase in predictor')) +
  theme(legend.position = 'top')
bze1
bze2 <- makeBiomeEstPlot() +
  theme(legend.position = 'none')
bze2

scale_biome_effects <- ggarrange( bze1, bze2,
                               nrow = 2, heights = c(1.2,1),
                               labels = c("A", "B"))+
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
scale_biome_effects
ggsave("fig3.png", scale_biome_effects, width = 22, height=14, units='cm')
ggsave("fig3.tiff", scale_biome_effects, width = 22, height=14, units='cm')







# Second check to see the effect of buffer size on NDVI coeffifient

makeBufferEstPlot <- function(response, lab){
  dfOut %>%
    filter(responseVar == response) %>%
    filter(var == 'green') %>%
    filter(tsLength == 6) %>%
    mutate(greenSpaceVar = ifelse(greenSpaceVar == 'ndvi', 'NDVI', 'Tree Cover')) %>%
    group_by(pollutant, greenSpaceVar, buffer)%>%
    summarise_at(vars(est:upCI), mean) %>%
    mutate_at(vars(est:upCI), function(x){return((exp(x) - 1) * 100 )})%>%
    mutate(sig = ifelse((lowCI > 0 & upCI > 0) | (lowCI < 0 & upCI < 0), 'Significant', 'Non-significant' )) %>%
    ggplot(aes(x=buffer, y = est, color = greenSpaceVar, size=abs(est))) +
    geom_point(position=position_dodge(width = 0.5)) +
    #geom_smooth(method='lm', formula = y~poly(x,2), se=F, linetype=2) +
    geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0, position=position_dodge(width = 0.5), size=0.2) +
    scale_x_log10()+
    scale_size_continuous(range = c(0.1, 2.5), name = '') +
    geom_hline(yintercept = 0, linetype=2) +
    facet_grid(.~pollutant, labeller = labeller(pollutant  = as_labeller(cyl_names,  label_parsed))) +
    theme(axis.title = element_text(size = 10)) +
    guides(size = FALSE) +
    labs(y =  lab,
         x = 'Buffer zone radius (m)',
         color = '') +
    coord_flip()
}


be1 <- makeBufferEstPlot('ConcentrationMean', expression('%'~Delta~' in mean [pollutant] per'~sigma~' increase in predictor')) +
  theme(legend.position = 'top')
be1
be2 <- makeBufferEstPlot('ConcentrationMax',expression('%'~Delta~' in peak [pollutant] per'~sigma~' increase in predictor')) +
  theme(legend.position = 'none')
be2

finalBuffEstPlot <- ggarrange( be1, be2,
                             nrow = 2, heights = c(1.1,1),
                             labels = c("A", "B"))+
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) 
finalBuffEstPlot
ggsave("supp_bufferEsts.png", finalBuffEstPlot, width = 25, height=16, units='cm')


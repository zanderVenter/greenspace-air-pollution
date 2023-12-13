# Collects additional station metadata from OpenStreetMap for filtered and cleaned stations


stationMetadata <- read_csv('./DATA/station_merged_metadata.csv')

# Iterate over stations and perform OSM query
i <- 10
osmOutput <- tibble()

stations_sf <- st_as_sf(stationMetadata, coords=c('Longitude', 'Latitude'))%>%
  st_set_crs(4326)

for (i in seq(97,length(stationMetadata$AirQualityStation), 1)){
  
  print(paste0('Iteration number...', i, '  -  ', stationMetadata$AirQualityStation[i]))
  
  select <- stations_sf %>%
    filter(AirQualityStation == stationMetadata$AirQualityStation[i])
  
  zone <- (floor((select$geometry[[1]][1]+ 180)/6) %% 60) + 1
  utmCrs <-  st_crs(paste0("+proj=utm +zone=",zone," +ellps=WGS84"))
  selectUTM <- st_transform(select %>%
                              st_set_crs(4326), utmCrs)
  selectUTM_buff <- selectUTM %>% st_buffer(250)
  
  aoi <-  select %>% st_buffer(250) %>% st_bbox()
  
  x <- opq(bbox = aoi) %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_sf () 
  
  if(length(x$osm_lines) != 0){
    roads <- st_as_sf(x$osm_lines) %>%
      st_intersection(select %>% st_buffer(250))%>%
      dplyr::select(highway) %>%
      filter(!highway %in% c('cycleway', 'footway', 'bridleway', 'elevator',
                             "emergency_access_point", "emergency_bay","escape",
                             'path', 'pedestrian', 'platform','rest_area',
                             'service', 'services','trailhead', 'track'
      )) 
    
    if(nrow(roads)){
      roadDists <- roads %>%
        st_transform(utmCrs) %>%
        mutate(distance = as.numeric(st_distance(geometry, selectUTM ))) 
      
      roadProximity <- roadDists %>%
        summarise(roadProxim = min(distance ))
      roadProximity <- roadProximity$roadProxim
      roadType <- roadDists[roadDists$distance == roadProximity, ]$highway
    } else {
      roadProximity <- 250
      roadType <- 'unclassified'
    }
  } else {
    print('no roads.....')
    roadProximity <- 250
    roadType <- 'unclassified'
  }
  
  
  
  y <- opq(bbox = aoi) %>%
    add_osm_feature(key = 'building') %>%
    osmdata_sf() 
  
  if (length(y$osm_polygons) != 0) {
    buildings <- st_as_sf(y$osm_polygons)
    
    buildingFootprint <- buildings %>%
      st_transform(utmCrs) %>%
      st_intersection(selectUTM %>% st_buffer(30)) 
    
    if (nrow(buildingFootprint)) {
      buildingFootprint <- buildingFootprint %>%
        mutate(area = as.numeric(st_area(geometry))) %>%
        summarise(area = sum(area))
      buildingFootprint <- buildingFootprint$area
    } else {
      buildingFootprint <- 0
    }
  } else {
    print('no buildings....')
    buildingFootprint <- 0
  }
  
  osmInner <- tibble(AirQualityStation = stationMetadata$AirQualityStation[i],
                     roadProximity = roadProximity,
                     roadType = roadType,
                     buildingFootprint = buildingFootprint)
  osmOutput <- osmOutput %>% bind_rows(osmInner)


}

osmOutput 

osmOutput %>%
  write_csv('./DATA/stations_roads_buildings.csv')


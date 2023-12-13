//// Extract ERA 5 climate reanalysis time series for air quality stations

var stations = ee.FeatureCollection('users/zandersamuel/NINA/Vector/Global_airquality_stations_locations');
Map.addLayer(stations, {},'stations stations'); 
print(stations.size())

stations = stations.select(['AirQualityStation'])

/// Climate data -------------------------------------------------------------------------------
var era5 = ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY")
  .filterDate('2010-01-01', '2020-01-01')
  .select(['temperature_2m',  'dewpoint_temperature_2m', 'u_component_of_wind_10m', 'v_component_of_wind_10m',
    'surface_pressure', 'total_precipitation']);
era5 = era5.map(function(i){
  return i.unmask(i.focal_mean(10000,'square', 'meters')).reproject(era5.first().projection())
})

Map.addLayer(era5.first().randomVisualizer(), {}, 'era5', 0)
var table = era5.map(function(img){
  var feats = stations.map(function(ft){return ft.set('date', img.date())})
  return img.reduceRegions(feats, ee.Reducer.mean(), 1000)
}).flatten();
table = table.filter(ee.Filter.notNull(['temperature_2m']))
table = table.map(function(ft){
  return ft.setGeometry(null)
});
print(table.limit(10))
Export.table.toDrive({
  collection: table,
  description: 'era5_ts',
  fileFormat: 'CSV'
});




//// Extract biome and human settlement layer for each air quality station

var extent = ee.Geometry.Polygon(
        [[[-128.52305068007934, 72.1102199149282],
          [-128.52305068007934, 19.753758265433287],
          [53.93788681992065, 19.753758265433287],
          [53.93788681992065, 72.1102199149282]]], null, false);
          
var stations = ee.FeatureCollection('users/zandersamuel/NINA/Vector/Global_airquality_stations_locations_raw');


Map.addLayer(stations, {},'stations stations'); 
print(stations.size())

stations = stations.select(['AirQualityStation'])

var proj = ee.ImageCollection("ECMWF/ERA5_LAND/MONTHLY").first().projection()

// Human settlement -------------------------------------------------------------------------
// For codes see page 20 here: https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2019.pdf
var hsl = ee.ImageCollection("JRC/GHSL/P2023A/GHS_SMOD")
  .filterDate('2010-01-01', '2020-01-01')
  .max().divide(10);
Map.addLayer(hsl, {min:0, max:3}, 'hsl', 0)

var hslStations = hsl.reduceRegions(stations, ee.Reducer.mode(), 1000);
hslStations = hslStations.map(function(ft){
  return ft.setGeometry(null)
});
print(hslStations.limit(10), 'hslStations')
Export.table.toDrive({
  collection: hslStations,
  description: 'hsl_stations',
  fileFormat: 'CSV'
});

//// Biomes -------------------------------------------------------------------------
var ecoRegions = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017")
  .select(['BIOME_NAME', 'ECO_NAME']);
Map.addLayer(ecoRegions, {}, 'ecoRegions',0)


var biomeImg = featColToImageToMap(ecoRegions, 'BIOME_NAME');
biomeImg = biomeImg
  .setDefaultProjection(proj.atScale(500))
  .reduceResolution(ee.Reducer.mode(), true, 256)
  .reproject(proj);
biomeImg = biomeImg.unmask(biomeImg.focal_mode(10000,'square', 'meters')).reproject(proj)
Map.addLayer(biomeImg.randomVisualizer(),{},'biomes',0);

var biomesStations = biomeImg.rename('biome_num').reduceRegions(stations, ee.Reducer.first(), 500);
biomesStations = biomesStations.map(function(ft){
  return ft.setGeometry(null)
});
print(biomesStations.limit(10), 'biomesStations')
Export.table.toDrive({
  collection: biomesStations,
  description: 'biomes_stations',
  fileFormat: 'CSV'
});

Export.image.toDrive({
  image: biomeImg,
  description: 'biomes_img',
  scale: 5000,
  maxPixels: 1e10,
  region: extent
})


// Function for visualizing Biome vector as a raster
function featColToImageToMap (featCol, attribute) {
  
  var lookup = featCol.distinct([attribute]);
  var lookupList = lookup.toList(1000);
  
  var lookupNum = lookup.map(function(ft){
    return ft.set('DESIGindex', lookupList.indexOf(ft));
  });
  
  var keyList = lookupNum.reduceColumns(ee.Reducer.toList(), [attribute]);
  var valueList = lookupNum.reduceColumns(ee.Reducer.toList(), ['DESIGindex']);
  print(keyList)
  print(valueList)
  featCol = featCol.map(function(ft){
    return ft.set('DESIGindex', ft.get(attribute));
  });
  featCol = featCol.remap(keyList.get('list'), valueList.get('list'), 'DESIGindex');
  
  featCol = featCol.map(function(ft){
    return ft.set('DESIGindex', ee.Number(ft.get('DESIGindex')));
  });
  
  var featImg = featCol.reduceToImage(['DESIGindex'], ee.Reducer.max());
  featImg = featImg.rename(attribute);
  
  return featImg
}



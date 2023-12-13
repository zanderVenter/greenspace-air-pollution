
//// Extract Landsat NDVI time series for air quality stations

var stations = ee.FeatureCollection("users/zandersamuel/NINA/Vector/Global_airquality_stations_locations");
stations = stations.select(['AirQualityStation'])
Map.addLayer(stations, {},'stations stations'); 
print(stations.size())


// Landsat parameters

var startYear  = 2010;  
var endYear    = 2019; 
var startDay   = '01-01';
var endDay     = '12-01';

//------ L8 to L7 HARMONIZATION FUNCTION -----
// slope and intercept citation: Roy, D.P., Kovalskyy, V., Zhang, H.K., Vermote, E.F., Yan, L., Kumar, S.S, Egorov, A., 2016, Characterization of Landsat-7 to Landsat-8 reflective wavelength and normalized difference vegetation index continuity, Remote Sensing of Environment, 185, 57-70.(http://dx.doi.org/10.1016/j.rse.2015.12.024); Table 2 - reduced major axis (RMA) regression coefficients
var harmonizationRoy = function(oli) {
  var slopes = ee.Image.constant([0.9785, 0.9542, 0.9825, 1.0073, 1.0171, 0.9949]);  
  var itcp = ee.Image.constant([-0.0095, -0.0016, -0.0022, -0.0021, -0.0030, 0.0029]);  
  var y = oli.select(['B2','B3','B4','B5','B6','B7'],['B1', 'B2', 'B3', 'B4', 'B5', 'B7']) 
             .resample('bicubic')   
             .subtract(itcp.multiply(10000)).divide(slopes)  
             .set('system:time_start', oli.get('system:time_start'));    
  return y.toShort();
};

//------ All: RETRIEVE A SENSOR SR COLLECTION FUNCTION -----
var getSRcollectionAll = function(startYear, endYear, startDay, endDay, sensor, aoi) {
  
  
  // get a landsat collection for given year, day range, and sensor
  var srCollection = ee.ImageCollection('LANDSAT/'+ sensor + '/C01/T1_SR') 
                       .filterBounds(aoi)
                       .filterDate(startYear+'-'+startDay, endYear+'-'+endDay);  
  
  // apply the harmonization function to LC08 (if LC08), subset bands, unmask, and resample           
  srCollection = srCollection.map(function(img) {
    var dat = ee.Image(
      ee.Algorithms.If(
        sensor == 'LC08',
        harmonizationRoy(img.unmask()),      
        img.select(['B1', 'B2', 'B3', 'B4', 'B5', 'B7'])
            .unmask()  
            .resample('bicubic')     
            .set('system:time_start', img.get('system:time_start'))  
      )
    );
    
    
    // make a cloud, cloud shadow, and snow mask from fmask band
    var qa = img.select('pixel_qa');                                       // select out the fmask band
    var mask = qa.bitwiseAnd(8).eq(0).and(                                 // include shadow
               qa.bitwiseAnd(16).eq(0)).and(                               // include snow
               qa.bitwiseAnd(32).eq(0));                                   // include clouds
    
    // apply the mask to the image and return it
    return dat.mask(mask); //apply the mask - 0's in mask will be excluded from computation and set to opacity=0 in display
  });

  return srCollection; // return the prepared collection
};

//------ All: FUNCTION TO COMBINE LT05, LE07, & LC08 COLLECTIONS -----
var getCombinedSRcollectionAll = function(startYear, endYear, startDay, endDay, aoi) {
    var lt5 = getSRcollectionAll(startYear, endYear,  startDay, endDay, 'LT05', aoi);
    var le7 = getSRcollectionAll(startYear, endYear, startDay, endDay, 'LE07', aoi);
    var lc8 = getSRcollectionAll(startYear, endYear,  startDay, endDay, 'LC08', aoi);
    var mergedCollection = ee.ImageCollection(lt5.merge(le7).merge(lc8));
    return mergedCollection;         
};

var addIndices = function(image) {
  var ndvi = image.normalizedDifference(['nir', 'red']).rename('ndvi')
  return image.addBands(ndvi)
};

var L457_BANDS = ['B1', 'B2', 'B3', 'B4',  'B5',  'B7']; // Landsat TM/ETM+ bands
var LTS_NAMES = ['blue', 'green', 'red', 'nir', 'swir1', 'swir2']; // Common names


var landsatCombo = getCombinedSRcollectionAll(startYear, endYear, startDay, endDay, stations)
    .select(L457_BANDS, LTS_NAMES)
    .map(addIndices)
    .select('ndvi');

var years = [2010,2011,2012,2013,2014,2015,2016,2017,2018,2019]
var buffs = [15, 30, 60]
var buffs = [ 120, 250, 500, 1000, 2000, 4000, 8000, 16000]
 
for (var i= 0; i<10; i++){ // (var i= 0; i<10; i++)
  for (var x = 0; x<8; x++){ // (var x = 0; x<4; x++)
    var landsatComboSelect = landsatCombo.filter(ee.Filter.calendarRange(years[i], years[i], 'year'))
  
    var table = landsatComboSelect.map(function(img){
      var stationsSelect = stations.filterBounds(img.geometry());
      //stationsSelect = getBufferedStations(stations);
      stationsSelect = stationsSelect.map(function(ft){return ft.buffer(buffs[x],2).set('buffer', buffs[x])})
      var subTable = img.reduceRegions({
        collection: stationsSelect,
        reducer: ee.Reducer.mean(),
        scale: 100,
        tileScale : 2
      });
      subTable = subTable.filter(ee.Filter.notNull(['mean']))
      return subTable.map(function(ft){return ft.setGeometry(null).set('date', img.date())})
    }).flatten();
    table = table.select(['AirQualityStation', 'date','buffer',  'mean'], ['AirQualityStation','date','buffer','ndvi'])
    
    Export.table.toDrive({
      collection: table,
      description: 'ndvi_landsat_ts_' + years[i] + '_' +  String(buffs[x]),
      folder: 'ndvi_'+ String(buffs[x]),
      fileFormat: 'CSV'
    })
  }
  
}



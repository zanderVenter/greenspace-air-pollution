//// Extract tree cover time series

var stations = ee.FeatureCollection('users/zandersamuel/NINA/Vector/Global_airquality_stations_locations');
stations = stations.select(['AirQualityStation']);
Map.addLayer(stations, {},'stations stations'); 

// Tree cover change MODIS

var modisTree = ee.ImageCollection("MODIS/006/MOD44B").select('Percent_Tree_Cover');
Map.addLayer(modisTree, {min:0, max:100}, 'modisTree', 0)

var years = [2010,2011,2012,2013,2014,2015,2016,2017,2018,2019]
var buffs = [120, 250, 500, 1000, 2000, 4000, 8000, 16000]

for (var i= 0; i<10; i++){ // (var i= 0; i<10; i++)
  for (var x = 0; x<8; x++){ // (var x = 0; x<4; x++)
    var modisTreeSelect = modisTree.filter(ee.Filter.calendarRange(years[i], years[i], 'year'))
  
    var table = modisTreeSelect.map(function(img){
      var stationsSelect = stations;
      //stationsSelect = getBufferedStations(stations);
      stationsSelect = stationsSelect.map(function(ft){return ft.buffer(buffs[x],2).set('buffer', buffs[x])})
      var subTable = img.reduceRegions({
        collection: stationsSelect,
        reducer: ee.Reducer.mean(),
        scale: 250
      });
      subTable = subTable.filter(ee.Filter.notNull(['mean']))
      return subTable.map(function(ft){return ft.setGeometry(null).set('date', img.date())})
    }).flatten();
    table = table.select(['AirQualityStation', 'date','buffer',  'mean'], ['AirQualityStation','date','buffer','treePerc'])
    //print(table.limit(10))
    Export.table.toDrive({
      collection: table,
      description: 'treePerc_modis_ts_' + years[i] + '_' +  String(buffs[x]),
      folder: 'treePerc',
      fileFormat: 'CSV'
    })
  }
  
}





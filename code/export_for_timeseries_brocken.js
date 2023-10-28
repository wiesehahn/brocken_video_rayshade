
// Google Earth Engine Script to export image composites for the Brocken region for Gif creation

var geometry = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[10.562804796780107, 51.82958486918908],
          [10.562804796780107, 51.763762790422845],
          [10.670436480129716, 51.763762790422845],
          [10.670436480129716, 51.82958486918908]]], null, false);
          
          
//++++ FUNCTIONS ++++//

// Function to get S2-Surface Reflectance Collection and join Cloud Mask
var getS2_SR_CLOUD_PROBABILITY = function () {
  var innerJoined = ee.Join.inner().apply({
    primary: ee.ImageCollection("COPERNICUS/S2_SR"),
    secondary: ee.ImageCollection("COPERNICUS/S2_CLOUD_PROBABILITY"),
    condition: ee.Filter.equals({
      leftField: 'system:index',
      rightField: 'system:index'
    })
  });
  var mergeImageBands = function (joinResult) {
    return ee.Image(joinResult.get('primary'))
          .addBands(joinResult.get('secondary'));
  };
  var newCollection = innerJoined.map(mergeImageBands);
  return ee.ImageCollection(newCollection)
  .select('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B9', 'B11', 'B12', 'SCL', 'probability');
};


// Function to mask clouds using Sentinel Hub cloud mask and mask cloud shadows using SCL band.
var maskClouds = function(image) {
  var cloudProbabilityThreshold = 40;
  var scl = image.select('SCL'); 
  var shadow = scl.eq(3); // 3 = cloud shadow
  var cloudMask = image.select('probability').lt(cloudProbabilityThreshold).and((shadow).neq(1));
  return image.updateMask(cloudMask);
};

// function to add indices
var addIndices = function(image) {
    var ndvi = image.normalizedDifference(['B8', 'B4']).rename("NDVI");
  return(image.addBands(ndvi).float());
};



//create composite
var reduceYearly = function(col, year, monthstart, monthend){
  return col
  .filter(ee.Filter.calendarRange(year, year, 'year'))
  .filter(ee.Filter.calendarRange(monthstart, monthend, 'month'))
  .reduce(ee.Reducer.median());
};

// vizualisation
var l8viz = {bands: ['B6_median', 'B5_median', 'B4_median'], min: 170, max: 4770};

var s2viz = {bands: ['B11_median', 'B8_median', 'B4_median'], min: 170, max: 4770};


//++++ END OF FUNCTIONS ++++//

// Load Sentinel-2 surface reflectance data 
var s2 = getS2_SR_CLOUD_PROBABILITY()
  .filterBounds(geometry)
  .map(maskClouds)
  .map(addIndices);




// L8

function maskL8sr(image) {
  // Bits 3 and 5 are cloud shadow and cloud, respectively.
  var cloudShadowBitMask = (1 << 3);
  var cloudsBitMask = (1 << 5);
  // Get the pixel QA band.
  var qa = image.select('pixel_qa');
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
                 .and(qa.bitwiseAnd(cloudsBitMask).eq(0));
  return image.updateMask(mask);
}

var l8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
                  .filterBounds(geometry)
                  .map(maskL8sr);


//Export L8
// (fill gaps with values from previous year)
Export.image.toDrive({
  image: reduceYearly(l8, 2013, 6, 9).visualize(l8viz),
  description: 'brocken2013',
  dimensions: "500x500",
  region: geometry
});
Export.image.toDrive({
  image: ee.ImageCollection([reduceYearly(l8, 2013, 6, 9), reduceYearly(l8, 2014, 6, 9)]).mosaic().visualize(l8viz),
  description: 'brocken2014',
  dimensions: "500x500",
  region: geometry
});
Export.image.toDrive({
  image: ee.ImageCollection([reduceYearly(l8, 2014, 6, 9), reduceYearly(l8, 2015, 6, 9)]).mosaic().visualize(l8viz),
  description: 'brocken2015',
  dimensions: "500x500",
  region: geometry
});
Export.image.toDrive({
  image: ee.ImageCollection([reduceYearly(l8, 2015, 6, 9), reduceYearly(l8, 2016, 6, 9)]).mosaic().visualize(l8viz),
  description: 'brocken2016',
  dimensions: "500x500",
  region: geometry
});


// Export S2
Export.image.toDrive({
  image: reduceYearly(s2, 2017, 6, 9).visualize(s2viz),
  description: 'brocken2017',
  dimensions: "500x500",
  region: geometry
});

Export.image.toDrive({
  image: reduceYearly(s2, 2018, 6, 9).visualize(s2viz),
  description: 'brocken2018',
  dimensions: "500x500",
  region: geometry
});

Export.image.toDrive({
  image: reduceYearly(s2, 2019, 6, 9).visualize(s2viz),
  description: 'brocken2019',
  dimensions: "500x500",
  region: geometry
});

Export.image.toDrive({
  image: reduceYearly(s2, 2020, 6, 9).visualize(s2viz),
  description: 'brocken2020',
  dimensions: "500x500",
  region: geometry
});




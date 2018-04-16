
for (i in unique(mobilier$insee_com)){
  
  coords <- as.matrix(mobilier[insee_com == i,.(as.numeric(lon),as.numeric(lat))])
  points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  mobilier_GEOJSON <- SpatialPointsDataFrame(coords = coords,data = mobilier[insee_com == i,.(type,lib,insee_com,code_iris)],proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  mobilier_GEOJSON <- geojsonio::geojson_json(mobilier_GEOJSON)
  geojsonio::geojson_write(mobilier_GEOJSON, file = paste0("json/mobilier_",i,".geojson"))
}


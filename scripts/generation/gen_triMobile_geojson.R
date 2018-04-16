coords <- as.matrix(triMobile[,.(as.numeric(lon),as.numeric(lat))])
points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
triMobile_GEOJSON <- SpatialPointsDataFrame(coords = coords,data = triMobile,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

triMobile_GEOJSON <- geojsonio::geojson_json(triMobile_GEOJSON)
geojsonio::geojson_write(triMobile_GEOJSON, file = "json/triMobile.geojson")

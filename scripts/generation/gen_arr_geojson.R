
arrData <- copy(iris@data[,lapply(.SD,sum),.SDcols=varBase,by=.(insee_com,nom_com)])

tonnageArr <- tonnage[, .(tonnageJaunes=sum(tonnagejaunes),tonnageVerre=sum(tonnageverre),tonnageVerts=sum(tonnageverts)),by=insee_com]
arrData <- merge(arrData,tonnageArr)
arrData <- merge(arrData,villeIdeale)


varBase <- names(arrData)[!(names(arrData) %in% c("code_iris","insee_com","nom_com","iris","nom_iris","typ_iris","id"))]

arrData <- computeIndicators(arrData)

varLoop <-  names(arrData)[!(names(arrData) %in% c("code_iris","insee_com","nom_com","iris","nom_iris","typ_iris","id"))]
for (i in varLoop){
  arrData[, c(paste0('index_',i)) := get(i) - min(get(i),na.rm=T)]
  arrData[, c(paste0('index_',i)) := get(paste0('index_',i))/max(get(paste0('index_',i)),na.rm=T)]
}

arr <- merge(arr,arrData)

arr@data <- data.table(arr@data)

arr_GEOJSON <- geojsonio::geojson_json(arr)
geojsonio::geojson_write(arr_GEOJSON, file = "json/arr.geojson")

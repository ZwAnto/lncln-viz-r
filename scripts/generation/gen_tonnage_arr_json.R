


tonnageArrMonth <- copy(tonnage)
tonnageArrMonth <- tonnageArrMonth[order(date)]

tonnageArrMonth <- tonnageArrMonth[,y := tonnagejaunes + tonnageverre + tonnageverts]
tonnageArrMonth[, y2 := y / head(y,1),by=insee_com]

tonnageArrMonth[, x := format(as.numeric(as.POSIXct.Date(date))*1000,scientific = F)]
tonnageArrMonth[, date := NULL]

tonnageArrMonth <- tonnageArrMonth[,.(insee_com,x,y,y2)]

refArr <- arr@data[,.(insee_com,nom_com)]
refArr[, insee_com := paste(insee_com)]

tonnageArrMonth <- data.table(merge(refArr,tonnageArrMonth))

tonnageArrMonth[,':=' (
  insee_com = NULL,
  name = gsub('Paris ','',gsub('Arrondissement','Arr.',nom_com)),
  nom_com = NULL
)]


json <- list()
for (i in unique(tonnageArrMonth$name)){
  temp <- tonnageArrMonth[name==i,]
  temp <- temp[order(x)]
  json[[length(json)+1]] <- paste0('{"name":"',i,'","data": [',temp[,paste0('{"x":',x,',"y":',y,'}',collapse = ',')],']}')
}

tonnageArrMonth_JSON <- paste0('[',paste0(json,collapse = ','),']')

write.table(paste(tonnageArrMonth_JSON),file = 'json/tonnage_arr.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')


json <- list()
for (i in unique(tonnageArrMonth$name)){
  temp <- tonnageArrMonth[name==i,]
  temp <- temp[order(x)]
  json[[length(json)+1]] <- paste0('{"name":"',i,'","data": [',temp[,paste0('{"x":',x,',"y":',y2,'}',collapse = ',')],']}')
}

tonnageArrMonth_JSON <- paste0('[',paste0(json,collapse = ','),']')

write.table(paste(tonnageArrMonth_JSON),file = 'json/tonnageIndex_arr.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

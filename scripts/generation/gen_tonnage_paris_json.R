tonnageParis <- tonnage[,lapply(.SD,sum), by = date, .SDcols=c('tonnagejaunes','tonnageverre','tonnageverts')]

tonnageParis_JSON <- tonnageParis

tonnageParis_JSON[,date2 := format(as.numeric(as.POSIXct.Date(date))*1000,scientific = F)]
tonnageParis_JSON[, date := NULL]
setnames(tonnageParis_JSON,"date2",'date')

tonnageverre <- tonnageParis_JSON[, paste0('{"x": ',date,', "y" : ',tonnageverre,'}',collapse = ',')]
tonnagejaunes <- tonnageParis_JSON[, paste0('{"x": ',date,', "y" : ',tonnagejaunes,'}',collapse = ',')]
tonnageverts <- tonnageParis_JSON[, paste0('{"x": ',date,', "y" : ',tonnageverts,'}',collapse = ',')]

tonnageverre <- paste0('{"name": "Jaunes", "color": "#cfe0c3", "data" : [',tonnageverre,']}')
tonnagejaunes <- paste0('{"name": "Verres", "color": "#70a9a1", "data" : [',tonnagejaunes,']}')
tonnageverts <- paste0('{"name": "Vertes", "color": "#9ec1a3", "data" : [',tonnageverts,']}')

tonnageParis_JSON <- paste0('[',paste0(tonnageverre,',',tonnagejaunes,',',tonnageverts),']')

write.table(tonnageParis_JSON,file = 'json/tonnage_paris.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

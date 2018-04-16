tonnageHabArr <- copy(arr@data[,c("nom_com","tonnage_hab")])

tonnageHabArr[, arr := gsub('Paris ','',gsub('Arrondissement','Arr.',nom_com))]
tonnageHabArr[, name := gsub('Paris ','',gsub('Arrondissement','',nom_com))]
tonnageHabArr[, y := tonnage_hab]

tonnageHabArr <- tonnageHabArr[, .(y,name,arr)]

tonnageHabArr_JSON <- paste0('[{"data": [',tonnageHabArr[,paste0('{"y":',y,',"name":"',name,'","arr":"',arr,'"}',collapse = ',')],']}]')
write.table(tonnageHabArr_JSON,file = 'json/tonnageHab_arr.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

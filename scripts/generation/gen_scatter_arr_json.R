scatterArr <- copy(arr@data[,c("nom_com","POU","p13_pop","tonnage")])


scatterArr[, arr := gsub('Paris ','',gsub('Arrondissement','Arr.',nom_com))]
scatterArr[, name := gsub('Paris ','',gsub('Arrondissement','',nom_com))]
scatterArr[, x := p13_pop]
scatterArr[, y := POU]
scatterArr[, z := tonnage]

scatterArr <- scatterArr[, .(x,y,z,name,arr)]

scatterArr_JSON <- paste0('[{"data": [',scatterArr[,paste0('{"x":',x,',"y":',y,',"z":',z,',"name":"',name,'","arr":"',arr,'"}',collapse = ',')],']}]')

write.table(scatterArr_JSON,file = 'json/scatter_arr.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')


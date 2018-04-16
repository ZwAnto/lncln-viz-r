parisData <- copy(arr@data[,lapply(.SD,sum),.SDcols=varBase])

parisData <- computeIndicators(parisData)

paris_JSON <- paste0('{',paste0(paste0('"',names(parisData),'": '),parisData,collapse = ','),'}')
write.table(paris_JSON,file = 'json/paris.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

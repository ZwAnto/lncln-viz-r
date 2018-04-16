

dansMaRueParis <- dansMaRue[, .(dansMaRueN = .N), by=.(type,soustype,date)]
dansMaRueParis[, date := as.Date(paste(year(date),formatC(month(date),width=2,flag='0'),'01',sep = '-'))]
dansMaRueParis <- dansMaRueParis[, .(dansMaRueN = sum(dansMaRueN,na.rm = T)), by=.(type,date)]

dansMaRueParis <- dansMaRueParis[order(date),]

dansMaRueParis[,date2 := as.numeric(as.POSIXct.Date(date))*1000]
dansMaRueParis[, date := NULL]
setnames(dansMaRueParis,"date2",'date')

dansMaRueParis_JSON <- list()

for (i in unique(dansMaRueParis$type)){
  temp <- dansMaRueParis[type==i,]
  temp[,data := paste0('{"x": ',date,', "y": ',dansMaRueN,', "drilldown": "',type,'"}')]
  
  temp <- temp[, .(data = paste0(data,collapse = ',')),by=type]
  temp[, data := paste0('[',data,']')]
  setnames(temp,'type','name')
  dansMaRueParis_JSON[[i]] <- temp
}

dansMaRueParis_JSON <- rbindlist(dansMaRueParis_JSON)

dansMaRueParis_JSON[, color := colors_r[1:.N]]

dansMaRueParis_JSON[,json := paste0('{"name": "',name,'", "data": ',data,', "color": "',color,'"}')]
dansMaRueParis_JSON <- dansMaRueParis_JSON[, .(json = paste0(json,collapse = ','))]
dansMaRueParis_JSON[, json := paste0('[',json,']')]

write.table(dansMaRueParis_JSON,file = 'json/dansMaRue_paris.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

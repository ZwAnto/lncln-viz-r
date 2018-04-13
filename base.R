
######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Programme principal                                       #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

# Working directory -------------------------------------------------------

setwd('E:/Challenge OpenDataParis/')

# Fun ---------------------------------------------------------------------

lapply(list.files('fun',pattern = '[.](r|R)$',full.names = T),source)

# Packages ----------------------------------------------------------------

# install.packages(c('rgdal','ggplot2','sp','data.table','doParallel','foreach','rgeos','leaflet'))

library(rgdal)
library(sp)
library(ggplot2)
library(data.table)
library(rgeos)
library(leaflet)
library(reshape2)
library(geojsonio)
library(jsonlite)

# Liens -------------------------------------------------------------------

# https://opendata.paris.fr/explore/dataset/zones-touristiques-internationales/

# Base --------------------------------------------------------------------

# Iris
load('data/iris.RData')

# Tri mobile
source('scripts/triMobile.R')

# Mobilier
source('scripts/mobilierparis2011.R')

# dans ma rue
source('scripts/dans-ma-rue.R',encoding = 'UTF-8')

source('scripts/tonnage.R',encoding = 'UTF-8')

colors <- c('#1F363D','#40798C','#70A9A1','#9EC1A3','#CFE0C3')
colors_r <- rev(colors)
# Affecting iris to tri Mobile --------------------------------------------

coords <- as.matrix(triMobile[,.(as.numeric(lon),as.numeric(lat))])
points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

check <- gContains(iris,points,byid = T)
check <- apply(check,1,which)

check <- sapply(check,function(x){
  if (!length(x)){
    return(NA)
  } 
  return(x)
})

triMobile$code_iris <- iris$code_iris[check]

# Affecting iris to mobilier --------------------------------------

coords <- as.matrix(mobilier[,.(as.numeric(lon),as.numeric(lat))])
points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

check <- gContains(iris,points,byid = T)
check <- apply(check,1,which)

check <- sapply(check,function(x){
  if (!length(x)){
    return(NA)
  } 
  return(x)
})

mobilier$code_iris <- iris$code_iris[check]

#' Si on veut rajouter les poubelle hors des limites de paris
#' Affectation à l'IRIS la plus proche

# coords <- as.matrix(mobilier[is.na(CODE_IRIS),.(as.numeric(lon),as.numeric(lat))])
# points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# check <- gDistance(iris,points,byid = T)
# check <- apply(check,1,which.min)
# 
# mobilier[is.na(CODE_IRIS), CODE_IRIS := iris$CODE_IRIS[check]] 

mobilier <- mobilier[!is.na(code_iris),]

# Affecting iris to dans ma rue -----------------------------------

coords <- as.matrix(dansMaRue[,.(as.numeric(lon),as.numeric(lat))])
points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

check <- gContains(iris,points,byid = T)
check <- apply(check,1,which)

check <- sapply(check,function(x){
  if (!length(x)){
    return(NA)
  } 
  return(x)
})

dansMaRue$code_iris <- iris$code_iris[check]

dansMaRue <- dansMaRue[!is.na(code_iris),]

# Arr ---------------------------------------------------------------------

triMobile[, insee_com := substr(code_iris,1,5)]
mobilier[, insee_com := substr(code_iris,1,5)]
dansMaRue[, insee_com := substr(code_iris,1,5)]

triMobileArr <- triMobile[, .(triMobileN = sum(jours_n), triMobileNGeo = .N), by=insee_com]

mobilierArr <- mobilier[, .(mobilierN = .N), by=.(type,lib,insee_com)]
mobilierArr <- dcast(mobilierArr,insee_com ~ type)

dansMaRueArr <- dansMaRue[, .(dansMaRueN = .N), by=.(type,soustype,insee_com)]
dansMaRueArr[grepl('rue', soustype), type := 'debordrue']
dansMaRueArr[grepl('verre', soustype), type := 'debordverre']
dansMaRueArr <- dcast(dansMaRueArr,insee_com ~ type)

tonnageArr <- tonnage[, .(tonnageJaunes=sum(tonnagejaunes),tonnageVerre=sum(tonnageverre),tonnageVerts=sum(tonnageverts)),by=insee_com]

arr <- merge(arr,triMobileArr)
arr <- merge(arr,mobilierArr)
arr <- merge(arr,dansMaRueArr)
arr <- merge(arr,tonnageArr)

arr@data[is.na(arr@data)] <- 0

# Iris --------------------------------------------------------------------


triMobileIris <- triMobile[, .(triMobileN = sum(jours_n), triMobileNGeo = .N), by=code_iris]

mobilierIris <- mobilier[, .(mobilierN = .N), by=.(type,lib,code_iris)]
mobilierIris <- dcast(mobilierIris,code_iris ~ type)

dansMaRueIris <- dansMaRue[, .(dansMaRueN = .N), by=.(type,soustype,code_iris)]
dansMaRueIris[grepl('rue', soustype), type := 'debordrue']
dansMaRueIris[grepl('verre', soustype), type := 'debordverre']
dansMaRueIris <- dcast(dansMaRueIris,code_iris ~ type)

iris <- merge(iris,triMobileIris)
iris <- merge(iris,mobilierIris)
iris <- merge(iris,dansMaRueIris)

iris@data[is.na(iris@data)] <- 0

# Paris -------------------------------------------------------------------

paris <- data.table(arr@data)
paris <- paris[, lapply(.SD,sum), .SDcols=c('p13_pop','area','triMobileN','triMobileNGeo','POU','POUP','PRE','VER','debordrue','debordverre','tonnageJaunes','tonnageVerre','tonnageVerts')]

paris$p13_pop_m = paris$p13_pop / 1000000
paris$tonnage = paris$tonnageJaunes + paris$tonnageVerre + paris$tonnageVerts
paris$tonnage_hab = paris$tonnage / paris$p13_pop * 1000
paris$pou_hab = paris$p13_pop / paris$POU
paris$pou_m = 1 / sqrt(paris$POU / (paris$area))

paris_JSON <- paste0('{',paste0(paste0('"',names(paris),'": '),paris,collapse = ','),'}')
write.table(paris_JSON,file = 'paris.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')


dansMaRueParis <- dansMaRue[, .(dansMaRueN = .N), by=.(type,soustype,date)]
# dansMaRueParis[grepl('rue', soustype), type := 'debordrue']
# dansMaRueParis[grepl('verre', soustype), type := 'debordverre']
dansMaRueParis[, date := as.Date(paste(year(date),formatC(month(date),width=2,flag='0'),'01',sep = '-'))]
dansMaRueParis <- dansMaRueParis[, .(dansMaRueN = sum(dansMaRueN,na.rm = T)), by=.(type,soustype,date)]

# Dans ma rue paris soustype JSON -----------------------------------------
# 
# dansMaRueParis <- dansMaRueParis[order(date),]
# dansMaRueParis[,date2 := as.numeric(as.POSIXct.Date(date))*1000]
# dansMaRueParis[, date := NULL]
# setnames(dansMaRueParis,"date2",'date')
# 
# 
# dansMaRueParis_JSON <- list()
# for (i in unique(dansMaRueParis$type)){
# 
#   for (j in unique(dansMaRueParis[type==i,soustype])){
#     temp <- dansMaRueParis[type==i & soustype == j,]
#     temp[,data := paste0('{"x": ',date,', "y": ',dansMaRueN,'}')]
#     
#     temp <- temp[, .(data = paste0(data,collapse = ',')),by=.(type,soustype)]
#     temp[, data := paste0('[',data,']')]
#     setnames(temp,'type','name')
#     dansMaRueParis_JSON[[j]] <- temp
#     break
#   }
# }
# 
# dansMaRueParis_JSON <- rbindlist(dansMaRueParis_JSON)
# 
# dansMaRueParis_JSON[,json := paste0('{"id": "',name,'", "name": "',soustype,'", "data": ',data,'}')]
# dansMaRueParis_JSON <- dansMaRueParis_JSON[, .(json = paste0(json,collapse = ','))]
# dansMaRueParis_JSON[, json := paste0('[',json,']')]
# 
# write.table(dansMaRueParis_JSON,file = 'dansMaRue_paris_soustype.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

# Dans ma rue paris type JSON ---------------------------------------------

dansMaRueParisType <- dansMaRueParis[, .(dansMaRueN = sum(dansMaRueN,na.rm = T)), by=.(type,date)]
dansMaRueParisType <- dansMaRueParisType[order(date),]
dansMaRueParisType[,date2 := as.numeric(as.POSIXct.Date(date))*1000]
dansMaRueParisType[, date := NULL]
setnames(dansMaRueParisType,"date2",'date')

dansMaRueParisType_JSON <- list()

for (i in unique(dansMaRueParisType$type)){
  temp <- dansMaRueParisType[type==i,]
  temp[,data := paste0('{"x": ',date,', "y": ',dansMaRueN,', "drilldown": "',type,'"}')]
  
  temp <- temp[, .(data = paste0(data,collapse = ',')),by=type]
  temp[, data := paste0('[',data,']')]
  setnames(temp,'type','name')
  dansMaRueParisType_JSON[[i]] <- temp
}

dansMaRueParisType_JSON <- rbindlist(dansMaRueParisType_JSON)

dansMaRueParisType_JSON[, color := colors_r[1:.N]]

dansMaRueParisType_JSON[,json := paste0('{"name": "',name,'", "data": ',data,', "color": "',color,'"}')]
dansMaRueParisType_JSON <- dansMaRueParisType_JSON[, .(json = paste0(json,collapse = ','))]
dansMaRueParisType_JSON[, json := paste0('[',json,']')]

write.table(dansMaRueParisType_JSON,file = 'dansMaRue_paris_type.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')




# tonnage paris JSON ------------------------------------------------------

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

write.table(tonnageParis_JSON,file = 'tonnage_paris.json',quote = F,row.names = F,col.names = F,fileEncoding = 'UTF-8')

# Plot --------------------------------------------------------------------

m <- leaflet() %>%
  addPolygons(data= iris, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorNumeric("RdPu",as.numeric(INSEE_COM))(as.numeric(INSEE_COM)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),popup = ~CODE_IRIS) %>% 
  addPolygons(data=arr)

addCircles(map= m ,data = dansMaRue,lng = ~lon, lat = ~lat, weight = 1,
           radius = 2
)

m

a <- merge(mobilier,iris@data,by='CODE_IRIS')
a <- a[,.(.N,sum(P13_POP)),by=.(INSEE_COM,CODE_IRIS)]

a[, inner := F]
a[grepl('0',INSEE_COM), inner := T]

ggplot(a) +
  geom_point(aes(x=N,y=V2,color=inner))


# toJSON ------------------------------------------------------------------

arr_GEOJSON <- geojsonio::geojson_json(arr)
geojsonio::geojson_write(arr_GEOJSON, file = "arr.geojson")


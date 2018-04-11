
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

triMobileArr <- triMobile[, .(triMobileN = sum(jours_n)), by=insee_com]

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


triMobileIris <- triMobile[, .(triMobileN = sum(jours_n)), by=code_iris]

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
paris <- paris[, lapply(.SD,sum), .SDcols=c('p13_pop','area','triMobileN','POU','POUP','PRE','VER','debordrue','debordverre','tonnageJaunes','tonnageVerre','tonnageVerts')]

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

paris_JSON <- jsonlite::toJSON(paris)
jsonlite::write_json(paris_JSON,'paris.json')

arr_GEOJSON <- geojsonio::geojson_json(arr)
geojsonio::geojson_write(arr_GEOJSON, file = "arr.geojson")


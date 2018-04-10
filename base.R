
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

triMobile$CODE_IRIS <- iris$CODE_IRIS[check]

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

mobilier$CODE_IRIS <- iris$CODE_IRIS[check]

#' Si on veut rajouter les poubelle hors des limites de paris
#' Affectation à l'IRIS la plus proche

# coords <- as.matrix(mobilier[is.na(CODE_IRIS),.(as.numeric(lon),as.numeric(lat))])
# points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# check <- gDistance(iris,points,byid = T)
# check <- apply(check,1,which.min)
# 
# mobilier[is.na(CODE_IRIS), CODE_IRIS := iris$CODE_IRIS[check]] 

mobilier <- mobilier[!is.na(CODE_IRIS),]

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

dansMaRue$CODE_IRIS <- iris$CODE_IRIS[check]

dansMaRue <- dansMaRue[!is.na(CODE_IRIS),]

# Arr ---------------------------------------------------------------------

triMobile[, INSEE_COM := substr(CODE_IRIS,1,5)]
mobilier[, INSEE_COM := substr(CODE_IRIS,1,5)]
dansMaRue[, INSEE_COM := substr(CODE_IRIS,1,5)]

triMobileArr <- triMobile[, .(triMobileN = sum(jours_n)), by=INSEE_COM]

mobilierArr <- mobilier[, .(mobilierN = .N), by=.(type,lib,INSEE_COM)]
mobilierArr <- dcast(mobilierArr,INSEE_COM ~ type)

dansMaRueArr <- dansMaRue[, .(dansMaRueN = .N), by=.(TYPE,SOUSTYPE,INSEE_COM)]
dansMaRueArr[grepl('rue', SOUSTYPE), TYPE := 'DEBORDRUE']
dansMaRueArr[grepl('verre', SOUSTYPE), TYPE := 'DEBORDVERRE']
dansMaRueArr <- dcast(dansMaRueArr,INSEE_COM ~ TYPE)

tonnageArr <- tonnage[, .(tonnageJaunes=sum(tonnageJaunes),tonnageVerre=sum(tonnageVerre),tonnageVerts=sum(tonnageVerts)),by=INSEE_COM]

arr <- merge(arr,triMobileArr)
arr <- merge(arr,mobilierArr)
arr <- merge(arr,dansMaRueArr)
arr <- merge(arr,tonnageArr)

arr@data[is.na(arr@data)] <- 0

# Iris --------------------------------------------------------------------


triMobileIris <- triMobile[, .(triMobileN = sum(jours_n)), by=CODE_IRIS]

mobilierIris <- mobilier[, .(mobilierN = .N), by=.(type,lib,CODE_IRIS)]
mobilierIris <- dcast(mobilierIris,CODE_IRIS ~ type)

dansMaRueIris <- dansMaRue[, .(dansMaRueN = .N), by=.(TYPE,SOUSTYPE,CODE_IRIS)]
dansMaRueIris[grepl('rue', SOUSTYPE), TYPE := 'DEBORDRUE']
dansMaRueIris[grepl('verre', SOUSTYPE), TYPE := 'DEBORDVERRE']
dansMaRueIris <- dcast(dansMaRueIris,CODE_IRIS ~ TYPE)

iris <- merge(iris,triMobileIris)
iris <- merge(iris,mobilierIris)
iris <- merge(iris,dansMaRueIris)

iris@data[is.na(iris@data)] <- 0


# Paris -------------------------------------------------------------------

paris <- data.table(arr@data)
paris <- paris[, lapply(.SD,sum), .SDcols=c('P13_POP','AREA','triMobileN','POU','POUP','PRE','VER','DEBORDRUE','DEBORDVERRE','tonnageJaunes','tonnageVerre','tonnageVerts')]

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

# a <- geojsonio::geojson_json(iris)
# 
# geojsonio::geojson_write(a, file = "F:/Challenge OpenDataParis/iris.geojson")


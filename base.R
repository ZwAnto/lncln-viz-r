
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
source('scripts/importation/triMobile.R')

# Mobilier
source('scripts/importation/mobilierparis2011.R')

# dans ma rue
source('scripts/importation/dans-ma-rue.R',encoding = 'UTF-8')

# Tonnage
source('scripts/importation/tonnage.R',encoding = 'UTF-8')

# Color vector
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
#' Affectation ? l'IRIS la plus proche

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

# Affecting iris to trilib ------------------------------------------------

coords <- as.matrix(trilib[,.(as.numeric(lon),as.numeric(lat))])
points <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

check <- gContains(iris,points,byid = T)
check <- apply(check,1,which)

check <- sapply(check,function(x){
  if (!length(x)){
    return(NA)
  } 
  return(x)
})

trilib$code_iris <- iris$code_iris[check]

# Creating insee_com ------------------------------------------------------

triMobile[, insee_com := substr(code_iris,1,5)]
mobilier[, insee_com := substr(code_iris,1,5)]
dansMaRue[, insee_com := substr(code_iris,1,5)]

# JSON Generation ---------------------------------------------------------

source('scripts/generation/gen_iris_geojson.R',encoding = 'UTF-8')

source('scripts/generation/gen_arr_geojson.R',encoding = 'UTF-8')

source('scripts/generation/gen_paris_json.R',encoding = 'UTF-8')

source('scripts/generation/gen_dansMaRue_paris_json.R', encoding = 'UTF-8')

source('scripts/generation/gen_tonnage_paris_json.R', encoding = 'UTF-8')

source('scripts/generation/gen_mobilier_geojson.R', encoding = 'UTF-8')

source('scripts/generation/gen_triMobile_geojson.R', encoding = 'UTF-8')

source('scripts/generation/gen_scatter_arr_json.R', encoding = 'UTF-8')

source('scripts/generation/gen_tonnageHab_arr_json.R', encoding = 'UTF-8')

source('scripts/generation/gen_tonnage_arr_json.R', encoding = 'UTF-8')

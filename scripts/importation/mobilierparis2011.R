
######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Importation mobilier                                      #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

source('scripts/importation/mobilierenvironnementparis2011.R')
source('scripts/importation/mobilierpropreteparis2011.R')
source('scripts/importation/triLib.R')

mobilier <- rbindlist(list(mobilierEnvironnement,mobilierProprete,trilib),use.names = T)

mobilier[, ':='(
  lon = as.numeric(lon),
  lat = as.numeric(lat)
)]

rm(mobilierEnvironnement,mobilierProprete,trilib)

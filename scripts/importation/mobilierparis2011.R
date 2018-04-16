
######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Importation mobilier                                      #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

source('scripts/mobilierenvironnementparis2011.R')
source('scripts/mobilierpropreteparis2011.R')

mobilier <- rbindlist(list(mobilierEnvironnement,mobilierProprete),use.names = T)

mobilier[, ':='(
  lon = as.numeric(lon),
  lat = as.numeric(lat)
)]

rm(mobilierEnvironnement,mobilierProprete)


######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Importation dans ma rue                                   #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

dansMaRue <- fread('data/dans-ma-rue.csv',encoding = 'UTF-8')

dansMaRue[, unique(TYPE)]
dansMaRue[, unique(paste(TYPE,'|',SOUSTYPE))]

# dansMaRue <- dansMaRue[TYPE == 'Propreté' & grepl('(rue|verre) débord', SOUSTYPE)]

dansMaRue <- dansMaRue[TYPE %in% c('Propreté',"Graffitis, tags, affiches et autocollants","Objets abandonnés")]


dansMaRue[, lat := scan(geo_point_2d,',',1)]
dansMaRue[, lon := scan(geo_point_2d,',',2)]

dansMaRue <- dansMaRue[, .(TYPE, SOUSTYPE, lon, lat, DATEDECL)]

dansMaRue[, ':='(
  lat = as.numeric(lat),
  lon = as.numeric(lon)
)]

dansMaRue[, DATE := substr(DATEDECL,1,10)]
dansMaRue[, DATE := as.Date(DATE)]

dansMaRue[,DATEDECL := NULL]

names(dansMaRue) <- tolower(names(dansMaRue))

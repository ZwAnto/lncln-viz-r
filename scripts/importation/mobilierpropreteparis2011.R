
######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Importation mobilier proprete                             #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

mobilierProprete <- fread('data/mobilierpropreteparis2011.csv',encoding = 'UTF-8')

names(mobilierProprete) <- make.names(names(mobilierProprete))

names(mobilierProprete)

plot(factor(mobilierProprete$Etat))
plot(factor(mobilierProprete$Pose))
plot(factor(mobilierProprete$Frequence))
plot(factor(mobilierProprete$DateDepose))
plot(factor(mobilierProprete$Ad_Nvo))
plot(factor(mobilierProprete$Ad_Clvo))

table(factor(mobilierProprete$Etat),factor(mobilierProprete$Pose))
table(factor(mobilierProprete$Etat),factor(mobilierProprete$DateDepose))

mobilierProprete <- mobilierProprete[Etat == 'Actif', .(geom_x_y)]

mobilierProprete[, lat := scan(geom_x_y,',',1)]
mobilierProprete[, lon := scan(geom_x_y,',',2)]

mobilierProprete[, ':=' (
  type = 'VER',
  lib = 'Colonne de verre'
)]

mobilierProprete[,geom_x_y := NULL]

names(mobilierProprete) <- tolower(names(mobilierProprete))

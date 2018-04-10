
######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Importation mobilier environnement                        #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

mobilierEnvironnement <- fread('data/mobilierenvironnementparis2011.csv',encoding = 'UTF-8')

names(mobilierEnvironnement) <- make.names(names(mobilierEnvironnement))

names(mobilierEnvironnement)

mobilierEnvironnement[, unique(paste(Type.Mobilier,Libelle.Mobilier))]

mobilierEnvironnement <- mobilierEnvironnement[Type.Mobilier %in% c('PRE','POU','POUP')]
mobilierEnvironnement[, geom := NULL]

mobilierEnvironnement[, lat := scan(geom_x_y,',',1)]
mobilierEnvironnement[, lon := scan(geom_x_y,',',2)]

plot(factor(mobilierEnvironnement$Type.Mobilier))

setnames(mobilierEnvironnement,'Type.Mobilier','type')
setnames(mobilierEnvironnement,'Libelle.Mobilier','lib')

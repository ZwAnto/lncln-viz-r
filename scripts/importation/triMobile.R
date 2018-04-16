
######################################################################### #
#                                                                         #
#   Projet    :                                                           #
#   Programme :                                                           #
#   Auteur    :                                                           #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

triMobile <- fread('data/tri-mobile0.csv',encoding = 'UTF-8')

names(triMobile) <- make.names(names(triMobile))

triMobile[, jours_split := lapply(jours.de.tenue,function(x){
  strsplit(x,' ')[[1]]
})]

triMobile[, jours_n := sapply(jours_split,function(x) {
  length(x = grep(pattern = 'et', x = x)) +1
  })][, jours_split := NULL]

triMobile[, lat := as.numeric(scan(XY,',',1))]
triMobile[, lon := as.numeric(scan(XY,',',2))]

triMobile[,c('XY','ville','Pays','code.postal','Complément.d.adresse') := NULL]

names(triMobile) <- tolower(names(triMobile))

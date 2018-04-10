
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

triMobile[, lat := scan(XY,',',1)]
triMobile[, lon := scan(XY,',',2)]


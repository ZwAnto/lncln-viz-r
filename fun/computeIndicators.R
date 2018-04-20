computeIndicators <- function(data){
  
  data <- copy(data)
  data <-  as.data.table(data)
  
  data[, p13_pop_m := data$p13_pop / 1000000]
  data[, mobilier := POU + POUP + PRE+ VER]
  
  data[, mobilier_hab := 0]
  data[, pou_hab := 0]
  data[, pou_m := 0]
  data[mobilier > 0, mobilier_hab := p13_pop / mobilier] 
  data[POU > 0, pou_hab := p13_pop / POU] 
  
  data[POU > 0, pou_m := 1 / sqrt(POU/area)] 
  
  try(data[, tonnage := tonnageJaunes + tonnageVerre + tonnageVerts], silent = T)
  try(data[, tonnage_hab := tonnage / p13_pop * 1000], silent = T)
  
  data[, dansMaRue := abandonnes + graffitis + proprete]
  
  data[is.na(data)] <- 0
  
  return(data)
}








a <- jsonlite::read_json('./Data/ville_ideale_sal.geojson')

library(data.table)


out <- list()
for (i in 1:20){
  out[[i]] <- as.data.table(a$features[[i]][[2]])
}

villeIdeale <- rbindlist(out)

villeIdeale <- villeIdeale[, .(insee,n_coms_sale,n_coms,prop,is_crotte,is_poubelle,is_sale)]

villeIdeale[,insee_com := paste(insee) ]
villeIdeale[, insee := NULL]
villeIdeale[, prop := prop * 100]

villeIdeale[, is_crotte := is_crotte / n_coms *100]
villeIdeale[, is_poubelle := is_poubelle / n_coms *100]
villeIdeale[, is_sale := is_sale / n_coms *100]

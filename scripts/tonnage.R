
######################################################################### #
#                                                                         #
#   Projet    :                                                           #
#   Programme :                                                           #
#   Auteur    :                                                           #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

tonnageVerre <- fread('data/tonnages_de_la_collecte_du_verre.csv',encoding = 'UTF-8')
tonnageJaune <- fread('data/tonnages_des_dechets_bacs_jaunes.csv',encoding = 'UTF-8')
tonnageVert <- fread('data/tonnages_des_dechets_bacs_verts.csv',encoding = 'UTF-8')

for (i in c('Verre','Verts','Jaunes')){

  df <- fread(list.files('.',pattern = paste0(tolower(i),'.*?[.]csv$'),recursive = T),encoding = 'UTF-8')
  names(df) <- make.names(names(df))
  
  df[, Total.arrondissement.2011 := NULL]
  df <- melt(df,id.vars = "Granularité",variable.name = 'date')
  
  df <- df[!is.na(Granularité)]
  
  df[, value := as.numeric(gsub(',','.',value))]
  setnames(df,'value',paste0('tonnage',i))
  
  df[, INSEE_COM := gsub('750','751',Granularité)]
  df[, Granularité := NULL]
  
  levels(df$date) <- c('2011-01-01','2011-02-01','2011-03-01','2011-04-01','2011-05-01','2011-06-01','2011-07-01','2011-08-01','2011-09-01','2011-10-01','2011-11-01','2011-12-01')
  df[,date := as.Date(date)]
  
  
  assign(paste0('tonnage',i),df)
  
  }

tonnage <- merge(merge(tonnageJaunes,tonnageVerre),tonnageVerts)

names(tonnage) <- tolower(names(tonnage))

rm(tonnageJaunes,tonnageVerts,tonnageVerre)


######################################################################### #
#                                                                         #
#   Projet    : CHALLENGE DATAVIS Lincoln                                 #
#   Programme : Iris + INSEE                                              #
#   Auteur    : AHM                                                       #
#   Date      :                                                           #
#                                                                         #
######################################################################### #

# Contour des iris
iris <- readOGR("data/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2016","CONTOURS-IRIS",drop_unsupported_fields = T)

# Base de population
pop <- data.table::fread('data/base-ic-evol-struct-pop-2013/base-ic-evol-struct-pop-2013.csv')

# Joining pop and iris ----------------------------------------------------

pop[,IRIS := paste(IRIS)]
iris <- iris[iris$CODE_IRIS %in% pop$IRIS,]

data <- merge(iris@data,pop[,.(IRIS,P13_POP)],by.x='CODE_IRIS',by.y='IRIS')

rownames(data) <- data$CODE_IRIS
iris@data <- data[paste0(iris$CODE_IRIS),]

iris@data$id <- iris@data$CODE_IRIS

iris@data$AREA <- gArea(iris,T)

iris <- spTransform(iris, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

rm(pop)

# Arrondissements ---------------------------------------------------------

outList <- list()
for (i in 1:20){
  outList[[i]] <- polygons(gUnaryUnion(iris[iris$INSEE_COM==75100+i,]))
  slot(outList[[i]]@polygons[[1]],'ID') <- paste(i)
}
arr <- SpatialPolygons(lapply(outList,function(x){
  x@polygons[[1]]
}))

arr <- SpatialPolygonsDataFrame(arr,data.frame('INSEE_COM'=75101:75120))

df <- data.table(iris@data)
df <- df[,.(P13_POP = sum(P13_POP),AREA = sum(AREA), NOM_COM = unique(NOM_COM)), by=INSEE_COM]

arr <- merge(arr,df,by='INSEE_COM')

rm(df)
names(arr@data) <- tolower(names(arr@data))
names(iris@data) <- tolower(names(iris@data))

save(iris,arr,file = 'data/iris.RData')

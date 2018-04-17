
trilib <- fread('data/trilib.csv',encoding = 'UTF-8')


unique(trilib$wastetype_designation)

trilib[, lat := scan(geo,',',1)]
trilib[, lon := scan(geo,',',2)]


trilib[,.N, by = localisationfo_number ]

trilib[, type := 'TRI']
trilib[, lib := wastetype_designation]
trilib <- trilib[, .(lat,lon,type,lib)]

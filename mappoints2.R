library(dismo)

mymap <- gmap("New York State", type = "terrain", filename = "NewYork.gmap", zoom= 7, lonlat = T)
## lonlat = T is CRS instead of Mercator
plot(mymap)
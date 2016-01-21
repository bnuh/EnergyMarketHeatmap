library(dismo) 
laurus <- gbif

library(googleVis)

locs <- subset(laurus, select = c("country", "lat", "lon"))
head(locs)  # a simple data frame with coordinates
locs <- subset(locs, locs$lat < 90)
gbmap <- gmap(locs.gb, type = "terrain")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)

locs.gb <- subset(locs, locs$country == "Spain")  # select only locs in UK

points(locs.gb.merc, pch = 6, col = "red")



points.gb <- as.data.frame(locs.gb)
points.gb$latlon <- paste(points.gb$lat, points.gb$lon, sep=":")
map.gb <- gvisMap(points.gb, locationvar="latlon", tipvar="lat", 
                  options = list(showTip=T, showLine=F, enableScrollWheel=TRUE, icons=
                                 useMapTypeControl=T, width=1400,height=800))
plot(map.gb)

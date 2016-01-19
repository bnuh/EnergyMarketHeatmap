library(dismo)  # check also the nice 'rgbif' package!
laurus <- gbif("Laurus", "nobilis")

# get data frame with spatial coordinates (points)
locs <- subset(laurus, select = c("country", "lat", "lon"))
head(locs)  # a simple data frame with coordinates


# Discard data with errors in coordinates:
locs <- subset(locs, locs$lat < 90)


coordinates(locs) <- c("lon", "lat")  # set spatial coordinates
##plot(locs)

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)

## plot(locs, pch = 20, col = "steelblue")
library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
##plot(coastsCoarse, add = T)

locs.gb <-
  subset(locs, locs$country == "Spain")  # select only locs in UK
## plot(locs.gb, pch = 20, cex = 2, col = "steelblue")
##plot(countriesLow, add = T)

gbmap <- gmap(locs.gb, type = "terrain")
locs.gb.merc <-
  Mercator(locs.gb)  # Google Maps are in Mercator projection.
# This function projects the points to that projection to enable mapping
##plot(gbmap)

points(locs.gb.merc, pch = 20, col = "red")

require(RgoogleMaps)

locs.gb.coords <-
  as.data.frame(coordinates(locs.gb))  # retrieves coordinates
# (1st column for longitude, 2nd column for latitude)
PlotOnStaticMap(
  lat = locs.gb.coords$lat, lon = locs.gb.coords$lon, zoom = 5,
  cex = 1.4, pch = 19, col = "red", FUN = points, add = F
)

map.lim <-
  qbbox(locs.gb.coords$lat, locs.gb.coords$lon, TYPE = "all")  # define region
# of interest (bounding box)
mymap <-
  GetMap.bbox(map.lim$lonR, map.lim$latR, destfile = "gmap.png", maptype = "terrain")

iconSel = paste0(
  "{","'default': {'normal': 'https://helpdesk.lastpass.com/wp-content/uploads/sites1.png',\n",
  "'selected': 'http://webking.com.hk/web-form/images/shared_icons/new_menu_icons/14.gif'",
  "}}"
)

require(googleVis)

mymap <-
  GetMap.bbox(map.lim$lonR, map.lim$latR, destfile = "gmap.png", maptype = "hybrid")

PlotOnStaticMap(
  mymap, lat = locs.gb.coords$lat, lon = locs.gb.coords$lon, zoom = NULL,
  cex = 1.3, pch = 19, col = "red", FUN = points, add = F
)


points.gb <- as.data.frame(locs.gb)
points.gb$price <- paste(paste("GENERATOR NAME", "", sep="<br>"), "LBMP: ", paste(points.gb$lat + points.gb$lon, "", sep = "<br>"), "Latitude: ", 
                         paste(points.gb$lat,  "", sep = "<br>"), "Longitude: ", 
                         paste(points.gb$lon,  "", sep = "<br>"), sep = "")

points.gb$latlon <- paste(points.gb$lat, points.gb$lon, sep=":")
map.gb <- gvisMap(
  points.gb, locationvar = "latlon", tipvar = "price",
  options = list(
    showTip = T, icons = iconSel, showLine = F, enableScrollWheel = TRUE,
    useMapTypeControl = T, mapType = 'terrain', labels = F, width = 1400,height = 950
  )
)
plot(map.gb)
library(dismo)  # check also the nice 'rgbif' package!

test = read.csv("Test Data.csv", skip = 1, fill = T, header = T, na.strings = "null")
test2 = read.csv("Test Data2.csv", skip = 1, fill = T, header = T, na.strings = "null")
test <- test[-c(1,2)]
test <- test[-c(4,6,7)]
test2 <- test2[-c(1,2,4,5)]
test$LBMP <- test2

locs = read.csv("Test.csv", header = T, col.names = c("country", "lat", "lon"), as.is = T, na.strings = 0)
lbmp = read.csv("Test2.csv", header = T, col.names = c("price", "gen"), as.is = T)

coordinates(locs) <- c("lon", "lat")  # set spatial coordinates

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)

nymap <- gmap(locs, type = "terrain")
locs.merc <-
  Mercator(locs)  # Google Maps are in Mercator projection.
# This function projects the points to that projection to enable mapping

require(RgoogleMaps)

locs.coords <- as.data.frame(coordinates(locs))

iconSel = paste0("{","'default': {'normal': 'https://helpdesk.lastpass.com/wp-content/uploads/sites1.png',\n",
  "'selected': 'http://webking.com.hk/web-form/images/shared_icons/new_menu_icons/14.gif'",
  "}}"
)

require(googleVis)

points.ny <- as.data.frame(locs)
points.ny$tip <- paste(paste("<b>",paste(lbmp$gen, "", sep = "<br>"),"</b>", sep=""), "LBMP: ", paste(lbmp$price, "", sep = "<br>"), sep = "")

points.ny$latlon <- paste(points.ny$lat, points.ny$lon, sep=":")
map.ny <- gvisMap(
  points.ny, locationvar = "latlon", tipvar = "tip",
  options = list(
    showTip = T, icons = iconSel, showLine = F, enableScrollWheel = TRUE,
    useMapTypeControl = T, mapType = 'terrain', labels = F, width = 1400,height = 950
  )
)
plot(map.ny)

htmlOut = print(map.ny)
write(htmlOut, file = "")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

tmin <- getData("worldclim", var = "tmin", res = 10)  # this will download 
# global data on minimum temperature at 10' resolution

tmin1 <- raster(paste(getwd(), "/wc10/tmin2.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)

tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 

plot(tmin1)

##locs2ras <- rasterize(xy, field = rep(1, nrow(xy)))
##locs2ras
##plot(locs2ras)

library(gtools)
file.remove(paste(getwd(), "/wc10/", "tmin_10m_bil.zip", sep = ""))

list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, 
                                 pattern = ".bil"))

tmin.all <- stack(list.ras)

tmin.brick <- brick(tmin.all)  # creates rasterbrick

library(lattice) 
require(stats)

projection(tmin1) <- crs.geo
locs$tmin1 <- extract(tmin1, locs)  # raster values

# rasterToPoints
tminvals <- rasterToPoints(tmin1)

# click(tmin1)  # click n times in the map to get values

locs2ras <- rasterize(locs, tmin1, field = rep(1, nrow(locs)))
plot(locs2ras, legend = F, axes = F, box = F)

tmin1.lowres <- aggregate(tmin1, fact = 2, fun = mean)

xy <- data.frame(locs$lon, locs$lat) 

vals <- lbmp$price

library(fields)
spline <- Tps(locs.coords, vals)  # thin plate spline
intras <- interpolate(tmin1, spline)
plot(intras)
intras <- mask(intras, tmin1)  # mask to land areas only
plot(intras)

k <- kde2d(locs$lat, locs$lon, n=500)
image(k, axes = F)

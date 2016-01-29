library(dismo)

locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 1, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

coordinates(locs) <- c("lon", "lat")  # set spatial coordinates

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

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
points.ny$tip <- paste(paste("<b>",paste(locs$name, "", sep = "<br>"),"</b>", sep=""), "LBMP: ", paste(locs$lbmp, "", sep = "<br>"), sep = "")

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

tmin1 <- raster(paste(getwd(), "/wc10/tmin5.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)

tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 
library(dismo)

locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 1, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

coordinates(locs) <- c("lon", "lat")  # set spatial coordinates

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

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
points.ny$tip <- paste(paste("<b>",paste(locs$name, "", sep = "<br>"),"</b>", sep=""), "LBMP: ", paste(locs$lbmp, "", sep = "<br>"), sep = "")

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

tmin1 <- raster(paste(getwd(), "/wc10/tmin5.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)

tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 

plot(tmin1)

library(gtools)
file.remove(paste(getwd(), "/wc10/", "tmin_10m_bil.zip", sep = ""))

list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, 
                                 pattern = ".bil"))

tmin.all <- stack(list.ras)

tmin.brick <- brick(tmin.all)  # creates rasterbrick

plot(locs)

library(lattice) 
require(stats)

projection(tmin1) <- crs.geo
locs$tmin1 <- extract(tmin1, locs)  # raster values

# rasterToPoints
tminvals <- rasterToPoints(tmin1)

# click(tmin1)  # click n times in the map to get values

locs2ras <- rasterize(locs, tmin1, field = rep(1, nrow(locs)))
plot(locs2ras, xlim = c(-78, -73), ylim = c(40, 46), legend = F, box = F, axes = F)
data(wrld_simpl)
plot(wrld_simpl, add = T)

tmin1.lowres <- aggregate(tmin1, fact = 2, fun = mean)

xy <- data.frame(x,y) 

vals <- z

library(fields)
spline <- Tps(xy, vals)  # thin plate spline
intras <- interpolate(locs2ras, spline)
plot(intras)
intras <- mask(intras, tmin1)  # mask to land areas only
plot(intras)

k <- kde2d(locs$lat, locs$lon, n=500)
image(k, axes = F)

###############

locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 1, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

X       <- locs$lon
Y       <- locs$lat
Z       <- locs$lbmp

x       <- as.vector(X)
y       <- as.vector(Y)
z       <- as.vector(Z)

s=interp(x,y,z, duplicate = "mean")
mesh(s$x,s$y,s$z)

dfx = data.frame(ev1=1:10, ev2=sample(10:99, 10), ev3=10:1)

scatter3D(x, y, z, theta= 0, phi = 90, box = F, alpha = 1, pch = 19, cex = (z/12), colkey = FALSE)

#####################

plot(tmin1)

library(gtools)
file.remove(paste(getwd(), "/wc10/", "tmin_10m_bil.zip", sep = ""))

list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, 
                                 pattern = ".bil"))

tmin.all <- stack(list.ras)

tmin.brick <- brick(tmin.all)  # creates rasterbrick

plot(locs)

library(lattice) 
require(stats)

projection(tmin1) <- crs.geo
locs$tmin1 <- extract(tmin1, locs)  # raster values

# rasterToPoints
tminvals <- rasterToPoints(tmin1)

# click(tmin1)  # click n times in the map to get values

locs2ras <- rasterize(locs, tmin1, field = rep(1, nrow(locs)))
plot(locs2ras, xlim = c(-78, -73), ylim = c(40, 46), legend = F, box = F, axes = F)
data(wrld_simpl)
plot(wrld_simpl, add = T)

tmin1.lowres <- aggregate(tmin1, fact = 2, fun = mean)

xy <- data.frame(x,y) 

vals <- z

library(fields)
spline <- Tps(xy, vals)  # thin plate spline
intras <- interpolate(locs2ras, spline)
plot(intras)
intras <- mask(intras, tmin1)  # mask to land areas only
plot(intras)

k <- kde2d(locs$lat, locs$lon, n=500)
image(k, axes = F)

###############

locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 2, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

X       <- locs$lon
Y       <- locs$lat
Z       <- locs$lbmp

x       <- as.vector(X)
y       <- as.vector(Y)
z       <- as.vector(Z)

s=interp(x,y,z, duplicate = "mean")
mesh(s$x,s$y,s$z)

dfx = data.frame(ev1=1:10, ev2=sample(10:99, 10), ev3=10:1)

scatter3D(x, y, z, theta= 0, phi = 90, box = F, alpha = 0.5, pch = 19, cex = z/17, colkey = FALSE)

#####################

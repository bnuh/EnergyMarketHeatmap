library(dismo)  # check also the nice 'rgbif' package!
laurus <- gbif("Laurus", "nobilis")

# get data frame with spatial coordinates (points)
locs <- subset(laurus, select = c("country", "lat", "lon"))

# Discard data with errors in coordinates:
locs <- subset(locs, locs$lat < 90)


coordinates(locs) <- c("lon", "lat")  # set spatial coordinates
##plot(locs)

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)

locs.gb <-
  subset(locs, locs$country == "United Kingdom")  # select only locs in UK
## plot(locs.gb, pch = 20, cex = 2, col = "steelblue")
##plot(countriesLow, add = T)

gbmap <- gmap(locs.gb, type = "terrain")
locs.gb.merc <- Mercator(locs.gb)  # Google Maps are in Mercator projection.
# This function projects the points to that projection to enable mapping
##plot(gbmap)

require(RgoogleMaps)

locs.gb.coords <-
  as.data.frame(coordinates(locs.gb))  # retrieves coordinates
# (1st column for longitude, 2nd column for latitude)

iconSel = paste0(
  "{","'default': {'normal': 'https://helpdesk.lastpass.com/wp-content/uploads/sites1.png',\n",
  "'selected': 'http://webking.com.hk/web-form/images/shared_icons/new_menu_icons/14.gif'",
  "}}"
)

require(googleVis)

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

htmlOut = print(map.gb)
write(htmlOut, file = "")

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

tmin <- getData("worldclim", var = "tmin", res = 10)  # this will download 
# global data on minimum temperature at 10' resolution

tmin1 <- raster(paste(getwd(), "/wc10/tmin2.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)

tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 

us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
nestates <- "New York"

ne = us[match(toupper(nestates),toupper(us$NAME_1)),]

# plot it with the boundaries we want to clip against:
plot(ne)
plot(tmin1, add=TRUE)
newext <- extent(ne)
tmin1.c <- crop(tmin1, newext)
plot(tmin1.c)

# now use the mask function
rr <- mask(tmin1.c, ne)

# plot, and overlay:
plot(rr, box = F, legend = F, axes = F);plot(ne, box = F, legend = F, axes = F, add=TRUE)

library(gtools)
file.remove(paste(getwd(), "/wc10/", "tmin_10m_bil.zip", sep = ""))

list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, 
                                 pattern = ".bil"))

tmin.all <- stack(list.ras)

tmin.brick <- brick(tmin.all)  # creates rasterbrick

newext <- drawExtent()  # click twice on the map to select the region of interest
tmin1.c <- crop(tmin1, newext)
plot(tmin1.c)

tmin.all.c <- crop(tmin.all, newext)

projection(tmin1.c) <- crs.geo
projection(tmin.all.c) <- crs.geo

library(lattice) 
require(stats)

plot(tmin1.c)

projection(tmin1) <- crs.geo
locs$tmin1 <- extract(tmin1, locs)  # raster values 

plot(tmin1.c)
reg.clim <- extract(tmin1.c, drawExtent())  # click twice to 
# draw extent of the region of interest
summary(reg.clim)

# rasterToPoints
tminvals <- rasterToPoints(tmin1.c)

plot(tmin1.c)
click(tmin1.c)  # click n times in the map to get values

locs2ras <- rasterize(locs.gb, tmin1, field = rep(1, nrow(locs.gb)))
plot(locs2ras, xlim = c(-5, 5), ylim = c(45, 60), legend = F, axes = F, box = F)

tmin1.lowres <- aggregate(tmin1.c, fact = 2, fun = mean)

xy <- data.frame(xyFromCell(tmin1.lowres, 1:ncell(tmin1.lowres)))  # get raster cell coordinates

vals <- getValues(tmin1.lowres)

library(fields)
spline <- Tps(xy, vals)  # thin plate spline
intras <- interpolate(tmin1.c, spline)
plot(intras)
intras <- mask(intras, tmin1.c)  # mask to land areas only
plot(intras)

require(spatial.tools)

elevation <- getData("alt", country = "GBR")

x <- terrain(elevation, opt = c("slope"), unit = "degrees")
plot(x, box = F, axes = F, legend = F)

slope <- terrain(elevation, opt = "slope")
aspect <- terrain(elevation, opt = "aspect")
plot(elevation, col = rainbow(30, alpha = 0.60), useRaster = TRUE, axes = FALSE, box = FALSE, legend = 'FALSE')

writeRaster(tmin1.c, filename = "tmin1.c.grd")

## class       : RasterLayer 
## dimensions  : 120, 120, 14400  (nrow, ncol, ncell)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : C:\Users\FRS\Dropbox\R.scripts\my.Rcode\R-GIS tutorial\tmin1.c.grd 
## names       : tmin1 
## values      : -12.3, 10.3  (min, max)


# writeRaster(tmin.all.c, filename = "tmin.all.grd", overwrite=TRUE)

## class       : RasterBrick 
## dimensions  : 120, 120, 14400, 12  (nrow, ncol, ncell, nlayers)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : C:\Users\FRS\Dropbox\R.scripts\my.Rcode\R-GIS tutorial\tmin.all.grd 
## names       : tmin1, tmin2, tmin3, tmin4, tmin5, tmin6, tmin7, tmin8, tmin9, tmin10, tmin11, tmin12 
## min values  : -12.3, -12.5, -10.8,  -8.6,  -4.2,  -0.8,   1.8,   1.6,  -0.1,   -3.3,   -8.1,  -10.8 
## max values  :  10.3,  10.8,  12.5,  14.5,  19.7,  24.7,  27.6,  26.7,  22.9,   16.9,   13.7,   11.3


##Exporting to KML (Google Earth)
##tmin1.c <- raster(tmin.all.c, 1)
##KML(tmin1.c, file = "tmin1.kml")
##KML(tmin.all.c)  # can export multiple layers
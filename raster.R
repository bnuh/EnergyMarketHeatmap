library(dismo)  # check also the nice 'rgbif' package!

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

tmin <- getData("worldclim", var = "tmin", res = 0.2)  # this will download 
# global data on minimum temperature at 10' resolution

tmin1 <- raster(paste(getwd(), "/wc10/tmin2.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)

tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 
tmin1  # look at the info

plot(tmin1, interpolate=T)

library(gtools)
## file.remove(paste(getwd(), "/wc10/", "tmin_10m_bil.zip", sep = ""))

list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, 
                                 pattern = ".bil"))
list.ras  # I have just collected a list of the files containing monthly temperature values

tmin.all <- stack(list.ras)
tmin.all

tmin.brick <- brick(tmin.all)  # creates rasterbrick

plot(tmin1)
newext <- drawExtent()  # click twice on the map to select the region of interest
tmin1.c <- crop(tmin1, newext)
plot(tmin1.c)

projection(tmin1.c) <- crs.geo
projection(tmin.all.c) <- crs.geo

tmin1.proj <- projectRaster(tmin1.c, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")  # can also use a template raster, see ?projectRaster
tmin1.proj  # notice info at coord.ref.


library(lattice) 
require(stats)

plot(tmin1.proj)

plot(tmin1.c)

Extract values from raster 

Use extract function:
  head(locs)  # we'll obtain tmin values for our points
##   country
## 1   Spain
## 2   Spain
## 3   Spain
## 4   Spain
## 5   Spain
## 6   Spain
projection(tmin1) <- crs.geo
locs$tmin1 <- extract(tmin1, locs)  # raster values 
# are incorporated to the dataframe
head(locs)
##   country tmin1
## 1   Spain   6.7
## 2   Spain   2.1
## 3   Spain   6.7
## 4   Spain   4.2
## 5   Spain   6.2
## 6   Spain   6.7

You can also extract values for a given region instead of the whole raster:
  plot(tmin1.c)
reg.clim <- extract(tmin1.c, drawExtent())  # click twice to 
# draw extent of the region of interest
summary(reg.clim)

Using rasterToPoints:
  # rasterToPoints
  tminvals <- rasterToPoints(tmin1.c)
head(tminvals)
##            x     y tmin1
## [1,] -6.4167 49.92   4.2
## [2,] -6.2500 49.92   4.2
## [3,] -5.2500 49.92   2.4
## [4,]  0.5833 49.92   1.0
## [5,]  0.7500 49.92   1.0
## [6,]  0.9167 49.92   1.0

And also, the click function will get values from particular locations in the map
plot(tmin1.c)
click(tmin1.c, n = 3)  # click n times in the map to get values




Rasterize points, lines or polygons 
locs2ras <- rasterize(locs.gb, tmin1, field = rep(1, nrow(locs.gb)))
locs2ras
## class       : RasterLayer 
## dimensions  : 900, 2160, 1944000  (nrow, ncol, ncell)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -180, 180, -60, 90  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : layer 
## values      : 1, 1  (min, max)
plot(locs2ras, xlim = c(-10, 10), ylim = c(45, 60), legend = F)
data(wrld_simpl)
plot(wrld_simpl, add = T)

plot of chunk unnamed-chunk-50 




Changing raster resolution 

Use aggregate function:
  tmin1.lowres <- aggregate(tmin1.c, fact = 2, fun = mean)
tmin1.lowres
## class       : RasterLayer 
## dimensions  : 60, 60, 3600  (nrow, ncol, ncell)
## resolution  : 0.3333, 0.3333  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : tmin1 
## values      : -10.57, 10.1  (min, max)
tmin1.c  # compare
## class       : RasterLayer 
## dimensions  : 120, 120, 14400  (nrow, ncol, ncell)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : tmin1 
## values      : -12.3, 10.3  (min, max)
par(mfcol = c(1, 2))
plot(tmin1.c, main = "original")
plot(tmin1.lowres, main = "low resolution")

plot of chunk unnamed-chunk-51 

Spline interpolation 
xy <- data.frame(xyFromCell(tmin1.lowres, 1:ncell(tmin1.lowres)))  # get raster cell coordinates
head(xy)
##        x     y
## 1 -9.833 49.83
## 2 -9.500 49.83
## 3 -9.167 49.83
## 4 -8.833 49.83
## 5 -8.500 49.83
## 6 -8.167 49.83
vals <- getValues(tmin1.lowres)
library(fields)
spline <- Tps(xy, vals)  # thin plate spline
intras <- interpolate(tmin1.c, spline)
intras  # note new resolution
## class       : RasterLayer 
## dimensions  : 120, 120, 14400  (nrow, ncol, ncell)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : layer 
## values      : -10.43, 13.16  (min, max)
plot(intras)

plot of chunk unnamed-chunk-52 
intras <- mask(intras, tmin1.c)  # mask to land areas only
plot(intras)
title("Interpolated raster")

plot of chunk unnamed-chunk-52 

Setting all rasters to the same extent, projection and resolution all in one 

See spatial_sync_raster function from spatial.tools package.




Elevations, slope, aspect, etc 


Download elevation data from internet:
  elevation <- getData("alt", country = "ESP")

Some quick maps:
  x <- terrain(elevation, opt = c("slope", "aspect"), unit = "degrees")
plot(x)

plot of chunk unnamed-chunk-54 

slope <- terrain(elevation, opt = "slope")
aspect <- terrain(elevation, opt = "aspect")
hill <- hillShade(slope, aspect, 40, 270)
plot(hill, col = grey(0:100/100), legend = FALSE, main = "Spain")
plot(elevation, col = rainbow(25, alpha = 0.35), add = TRUE)

plot of chunk unnamed-chunk-54 

Saving and exporting raster data 

Saving raster to file:
  writeRaster(tmin1.c, filename = "tmin1.c.grd")
## class       : RasterLayer 
## dimensions  : 120, 120, 14400  (nrow, ncol, ncell)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : C:\Users\FRS\Dropbox\R.scripts\my.Rcode\R-GIS tutorial\tmin1.c.grd 
## names       : tmin1 
## values      : -12.3, 10.3  (min, max)
writeRaster(tmin.all.c, filename = "tmin.all.grd")
## class       : RasterBrick 
## dimensions  : 120, 120, 14400, 12  (nrow, ncol, ncell, nlayers)
## resolution  : 0.1667, 0.1667  (x, y)
## extent      : -10, 10, 30, 50  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0 
## data source : C:\Users\FRS\Dropbox\R.scripts\my.Rcode\R-GIS tutorial\tmin.all.grd 
## names       : tmin1, tmin2, tmin3, tmin4, tmin5, tmin6, tmin7, tmin8, tmin9, tmin10, tmin11, tmin12 
## min values  : -12.3, -12.5, -10.8,  -8.6,  -4.2,  -0.8,   1.8,   1.6,  -0.1,   -3.3,   -8.1,  -10.8 
## max values  :  10.3,  10.8,  12.5,  14.5,  19.7,  24.7,  27.6,  26.7,  22.9,   16.9,   13.7,   11.3

writeRaster can export to many different file types, see help.




##Exporting to KML (Google Earth)
##tmin1.c <- raster(tmin.all.c, 1)
##KML(tmin1.c, file = "tmin1.kml")
##KML(tmin.all.c)  # can export multiple layers


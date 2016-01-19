library(dismo)  # check also the nice 'rgbif' package!

crs.geo <-
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data

tmin <- getData("worldclim", var = "tmin", res = 0.2)  # this will download 
# global data on minimum temperature at 10' resolution

tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)

tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 
tmin1  # look at the info

plot(tmin1)

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


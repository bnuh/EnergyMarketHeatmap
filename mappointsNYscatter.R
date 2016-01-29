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

library(akima)
locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 1, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

x       <- locs$lon
y       <- locs$lat
z       <- locs$lbmp

contour(interp(x, y, z, duplicate = 'mean', nx = 1000, ny = 1000))
image(interp(x, y, z, duplicate = 'mean', nx = 1000, ny = 1000), color.pallette = heat.colors,maxpixels = 10000, useRaster = T)
filled.contour(interp(x, y, z, duplicate = 'mean', nx = 1000, ny = 1000), nlevels = 500, axes = F, color.pallette = cm.colors)


library(raster)
rasterFromXYZ(locs)

g <- mesh(X,Y)

x       <- as.vector(X)
y       <- as.vector(Y)
z       <- as.vector(Z)

s=interp(x,y,z, duplicate = "mean")
w = mesh(s$x,s$y,s$z)

dfx = data.frame(ev1=1:10, ev2=sample(10:99, 10), ev3=10:1)

scatter3D(g$x, g$y, g$y, theta= 0, phi = 90, box = F, alpha = 1, pch = 19, cex = (z/12), colkey = FALSE)

persp3D(w)

#####################

require(lattice)

# begin generating my 3D shape
b <- seq(from=0, to=20,by=0.5)
s <- seq(from=0, to=20,by=0.5)
payoff <- expand.grid(b=b,s=s)
payoff$payoff <- payoff$b - payoff$s
payoff$payoff[payoff$payoff < -1] <- -1
# end generating my 3D shape

wireframe(payoff ~ s * b, payoff, shade = TRUE, aspect = c(1, 1),
          light.source = c(10,10,10), 
          scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
          screen = list(z = 40, x = -75, y = 0))



###################################
v_x <- seq(-80,-71.7,388)
v_y = seq(40,45.2,388)


##################################

library("locfit")

env <- environmental
env$ozone <- env$ozone^(1/3)
env$Radiation <- equal.count(env$radiation, 4)

fm1.env <- lm(ozone ~ radiation * temperature * wind, env)
fm2.env <-
  loess(ozone ~ wind * temperature * radiation, env,
        span = 0.75, degree = 1)
fm3.env <-
  loess(ozone ~ wind * temperature * radiation, env,
        parametric = c("radiation", "wind"),
        span = 0.75, degree = 2)

fm4.env <- locfit(ozone ~ wind * temperature * radiation, env)
w.mesh <- with(env, do.breaks(range(wind), 50))
t.mesh <- with(env, do.breaks(range(temperature), 50))
r.mesh <- with(env, do.breaks(range(radiation), 3))
grid <-
  expand.grid(wind = w.mesh,
              temperature = t.mesh,
              radiation = r.mesh)
grid[["fit.linear"]] <- predict(fm1.env, newdata = grid)
grid[["fit.loess.1"]] <- as.vector(predict(fm2.env, newdata = grid))
grid[["fit.loess.2"]] <- as.vector(predict(fm3.env, newdata = grid))
grid[["fit.locfit"]] <- predict(fm4.env, newdata = grid)

useOuterStrips(levelplot(fit.linear + fit.loess.1 + fit.loess.2 + 
                           fit.locfit ~ wind * temperature | radiation, data = grid))
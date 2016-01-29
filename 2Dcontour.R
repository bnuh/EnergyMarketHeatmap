require(akima)
require(rgl)
require(plot3D)
require(fields)



s=interp(x,y,z, duplicate = "mean")
mesh(s$x,s$y,s$z)

surf3D (x, y, z, theta = 90)

scatter3D(x, y, z, theta= 0, phi = 90, pch = "", cex = 3, colkey = FALSE)


image2D(s$x,s$y,s$z)

library(plot3D)
image2D(z,x,y)
library(zoo)


Volcano <- volcano[seq(1, nrow(volcano), by = 1), seq(1, ncol(volcano), by = 1)]

image2D(s, axes = F, resfac = 10, alpha = 1, rasterImage = TRUE, colkey = F, contour = F)


locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 2, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

x = as.vector(locs$lon)
y = as.vector(locs$lat)
z = as.vector(locs$lbmp)

##kriged <- kriging(x, y, z, polygons=p, pixels=500)

resolution <- 0.01 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
a <- interp(x=x, y=y, z=z, duplicate="mean") 
filled.contour(a, axes = F, color.palette=heat.colors, nlevels = 500)

#####################

##########
library(akima)
library(maps)
library(mapdata)

GRID.RES <- 500

## original values
x <- seq(100,170,le=10) + runif(10,-5,5)
y <- seq(-50,0,le=10) + runif(10,-5,5)
z <- x*y + rnorm(length(x),2)

## interpolation using akima
x0 <- seq(100,170,le=GRID.RES)
y0 <- seq(-50,0,le=GRID.RES)
z.hat <- interp(x,y, z, xo=x0, yo=y0, linear = FALSE, duplicate="mean", extrap=TRUE)

## find loc in australia, set others to NA
myGrid <- expand.grid(x0, y0)
temp <- map.where(x=myGrid[,1], y=myGrid[,2])
toKeep <- grep("australia",temp, ignore.case=TRUE)
toRemove <- setdiff(1:length(z.hat$z), toKeep)
z.hat$z[toRemove] <- NA

## plot
image(z.hat)
map(add=TRUE, lwd=3)
contour(z.hat,add=TRUE)
############

#####################

# Krige random data for a specified area using a list of polygons
library(maps)
usa <- map("usa", "main", plot = FALSE)
p <- list(data.frame(usa$x, usa$y))
# Create some random data
x <- runif(50, min(p[[1]][,1]), max(p[[1]][,1]))
y <- runif(50, min(p[[1]][,2]), max(p[[1]][,2]))
z <- rnorm(50)
# Krige and create the map
kriged <- kriging(x, y, z, polygons=p, pixels=300)
image(kriged, xlim = extendrange(x), ylim = extendrange(y))

filled.contour(volcano, color = terrain.colors, asp = 1) # simple

x <- 10*1:nrow(volcano)
f <- volcano
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano, color = terrain.colors,
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               plot.axes = { axis(1, seq(100, 800, by = 100))
                 axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "Height\n(meters)"),
               key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
mtext(paste("filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = .66)

xy <- data.frame(test$Longitude, test$Latitude)
library(akima)
vals <- as.vector(test$LBMP)
vals <- as.vector(vals$LBMP)

spline <- Tps(xy, vals)

intras <- interp(x,y, vals, duplicate = "mean")
plot(intras)

locs2ras <- rasterize(xy, field = rep(1, nrow(xy)))
locs2ras
plot(locs2ras)


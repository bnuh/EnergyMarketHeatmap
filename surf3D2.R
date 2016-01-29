
## =======================================================================
## A three-dimensional shape 
## (ala http://docs.enthought.com/mayavi/mayavi/mlab.html)
## =======================================================================

locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 1, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

# create grid matrices
X       <- seq(0, pi, length.out = 50)
Y       <- seq(0, 2*pi, length.out = 50)
Z       <- seq(0, 2*pi, length.out = 50)
M       <- mesh(X, Y)
phi     <- M$x
theta   <- M$y

# x, y and z grids
r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
x <- r * sin(phi) * cos(theta)
y <- r * cos(phi)
z <- r * sin(phi) * sin(theta)

# create grid matrices
X       <- locs$lon
Y       <- locs$lat
Z       <- locs$lbmp

x       <- as.vector(X)
y       <- as.vector(Y)
z       <- as.vector(Z)

vx = seq(0,(length(x)-1)/100,0.01)
vy = seq(0,(length(y)-1)/100,0.01)
vz = seq(0,(length(z)-1)/100,0.01)

s=interp(x,y,z, duplicate = "mean")
mesh(s$x,s$y,s$z)

surf3D (x, y, z, theta = 90)

scatter3D(x, y, z, theta= 0, phi = 0, pch = "+", cex = 3, colkey = FALSE)
image2D(s$x,s$y,s$z)
library(plot3D)
image2D(z,x,y)
library(zoo)

insertCol(x, 2, y)
insertCol(x, 4, z)

x       <- as.matrix(X)
y       <- as.matrix(Y)
z       <- as.matrix(Z)

Volcano <- volcano[seq(1, nrow(volcano), by = 1), seq(1, ncol(volcano), by = 1)]

image2D(s, axes = F, resfac = 10, alpha = 1, rasterImage = TRUE, colkey = F, contour = F)

x <- insertCol( x, c = 1, v = 0, cName = "" )
y <- insertCol( y, c = 1, v = 0, cName = "" )
z <- insertCol( zz, c = 1, v = 0, cName = "" )


# x, y and z grids
r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
x <- r * sin(phi) * cos(theta)
y <- r * cos(phi)
z <- r * sin(phi) * sin(theta)

# full colored image
surf3D(x, y, z, colvar = z, resfac = 10, colkey = FALSE, box = FALSE, 
       theta = 0, phi = 75, facets = T, inttype = 3, full = T, alpha = 1, scale = 1, xlim = range(x)*1, 
       ylim = range(y)*1, zlim = range(z)*1)


slice3D  (x, y, z, theta = 0, phi = 0, xs = vx, ys = 0, zs = vz, colvar = z, resfac = 10, colkey = T, box = T, 
          facets = T, inttype = 2, alpha = 1, xlim = range(x)*0.8, 
          ylim = range(y)*0.8, zlim = range(z)*0.8)

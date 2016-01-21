
## =======================================================================
## A three-dimensional shape 
## (ala http://docs.enthought.com/mayavi/mayavi/mlab.html)
## =======================================================================

# create grid matrices
X       <- seq(0, pi, length.out = 50)
Y       <- seq(0, 2*pi, length.out = 50)
M       <- mesh(X, Y)
phi     <- M$x
theta   <- M$y

# x, y and z grids
r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
x <- r * sin(phi) * cos(theta)
y <- r * cos(phi)
z <- r * sin(phi) * sin(theta)

# full colored image
surf3D(x, y, z, colvar = y, resfac = 10, colkey = FALSE, box = FALSE, 
       theta = 0, phi = 0, facets = T, inttype = 2, alpha = 1, xlim = range(x)*0.8, 
       ylim = range(y)*0.8, zlim = range(z)*0.8)



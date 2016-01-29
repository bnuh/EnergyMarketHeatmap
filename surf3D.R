
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
surf3D(s, colvar = z, resfac = 10, colkey = FALSE, box = FALSE, 
       theta = 0, phi = 75, facets = T, inttype = 3, full = T, alpha = 1, scale = 1, xlim = range(x)*1, 
       ylim = range(y)*1, zlim = range(z)*1)





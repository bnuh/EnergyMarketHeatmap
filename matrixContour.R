

library(raster)
library(colorRamps) # for some crispy colors
library(vegan) # will be used for PCNM

# empty matrix and spatial coordinates of its cells
side=30
my.mat <- matrix(NA, nrow=side, ncol=side)
x.coord <- rep(1:side, each=side)
y.coord <- rep(1:side, times=side)
xy <- data.frame(x.coord, y.coord)

# all paiwise euclidean distances between the cells
xy.dist <- dist(xy)

# PCNM axes of the dist. matrix (from 'vegan' package)
pcnm.axes <- pcnm(xy.dist)$vectors

# using 8th PCNM axis as my atificial z variable
z.value <- pcnm.axes[,8]*200 + rnorm(side*side, 0, 1)

# plotting the artificial spatial data
my.mat[] <- z.value
r <- raster(my.mat)
filledContour(r, axes=F, col=matlab.like(19))

library(ncf)
ncf.cor <- correlog(x.coord, y.coord, z.value,
                    increment=2, resamp=500)

install.packages("pgirmess")
library(pgirmess)
pgi.cor <- correlog(coords=xy, z=z.value, method="Moran", nbclass=21)

# 'nb' - neighbourhood of each cell
r.nb <- dnearneigh(as.matrix(xy), d1=0.5, d2=1.5)
# 'nb' - an alternative way to specify the neighbourhood
# r.nb <- cell2nb(nrow=side, ncol=side, type="queen")
sp.cor <- sp.correlogram(r.nb, z.value, order=15,
                         method="I", 
                         
                         
s100 <- matrix(c(locs$lon, locs$lat, locs$lbmp),  ncol=3,  byrow=F)
colnames(s100) <- c('X', 'Y', 'Z')

library(raster)
# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(s100[,1:2])

r <- raster(e, ncol=388, nrow=388)
# or r <- raster(xmn=, xmx=,  ...

# you need to provide a function 'fun' for when there are multiple points per cell
x <- rasterize(s100[, 1:2], r, s100[,3], fun = mean)
plot(x)

library(plotly)
set.seed(100)
plot_ly(locs, x = locs$lon, y = locs$lat, z = locs$lbmp, type = "heatmap")


Here's the code for the Denny Yard data:

## Working with remote sensing data from GEM Gradiometer

# First get data from machine onto a computer. Here we've got the 
# data on a google sheet...

# get data from google drive... connect to google sheet
require(RCurl) # if you get an error message here that says something like 'there is no package called 'RCurl''
# then you need to install the package by typing this into the console (and then hitting enter): install.packages("RCurl")
# wait for the package to download and install, then run line 3 again before proceeding.
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
# if the file is in google drive, it needs to be opened in google sheets first then
# in google spreadsheet, go to file-> publish to web -> get link to publish to web -> get csv file
# this is the URL for "15-18"
goog <- "https://docs.google.com/spreadsheet/pub?key=0AiSgZmtbWmWmdGdKQUVCYlB5MnZlaVJMNHZ6OExzR1E&single=true&gid=0&output=csv"
# assign data from google sheet to R object - this may take some time
data <- s100
# inspect to confirm that data has imported OK...
head(data)
# subset just X, Y and mag (easting, northing and mag data)
data1 <- data
# convert all to numeric for computation           
data1 <- na.omit(data.frame(apply(data1, 2, function(x) as.numeric(as.character(x)))))
# find max and min values for later steps
xmn=min(data1[,1]); xmx=max(data1[,1])
ymn=min(data1[,2]); ymx=max(data1[,2])


# Plot points over google maps
# just a sample as it's rather slow!
# n <- 100
# data2 <- data1[sample(nrow(data1), n), ]
data2 <- data1

# convert UTM to latlong
require(sp); require(rgdal)
a<-data.frame(cbind(data2$V2, data2$V3))
coordinates(a) = ~X1 + X2
proj4string(a) <-CRS("+proj=utm +zone=10 +datum=WGS84")
# use spTransform now
a1 <- spTransform(a,CRS("+proj=longlat"))
# inspect output
head(coordinates(a1)) 
# insert lat-longs back into data
data2$long <-  a1$X1
data2$lat <-   a1$X2

# plot as raster
library(raster) # if you get an error, type install.packages("raster") at the console
r <- raster(nrows=100, ncols=100, 
            xmn=xmn, xmx=xmx, 
            ymn=ymn, ymx=ymx )
ras <- rasterize(data1[,1:2], r, field = data1[,3])
# make a plot of the data
plot(ras)

## This is ok, but we want to fill in the gaps in the plot
## to get a nice continuous surface of data. So we need to 
## interpolate. Let's do that...

# plot as interpolated raster
library(akima)
akima.li <- interp(data1[,1], data1[,2], data1[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
# plot interpolated raster over the top
image(akima.li, maxpixels=100000, useRaster = T, col = rainbow(100, alpha = 1))
# plot interpolated contour over the top


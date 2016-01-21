require(akima)
require(rgl)

s=interp(x,y,z, duplicate = "mean")
mesh(s$x,s$y,s$z)

scatter3D(x, y, z, pch = "+", cex = 3, colkey = FALSE)
image2D(s$x,s$y,s$z)
library(plot3D)
image2D(z,x,y)

Volcano <- volcano[seq(1, nrow(volcano), by = 1), seq(1, ncol(volcano), by = 1)]

image2D(s, axes = F, resfac = 10, alpha = 1, rasterImage = TRUE, colkey = F, contour = F)

x = as.vector(test$Latitude)
y = as.vector(test$Longitude)
z = as.vector(test$Price)
z = z[-c(2)]
z = as.vector(z$X42.71)

test = read.csv("Test Data.csv", skip = 1, fill = T, header = T, na.strings = "null")
test2 = read.csv("Test Data2.csv", skip = 1, fill = T, header = T, na.strings = "null")
test <- test[-c(1,2)]
test
head(test)
test <- test[-c(4,6,7)]
test2 <- test2[-c(1,2,4,5)]
head(test2)
test$LBMP <- test2
head(test)


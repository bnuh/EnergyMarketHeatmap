library(maptools)

kpacks <- c('sp','rgdal', 'gstat', 'raster')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

data(wrld_simpl)

p.utm33n <- CRS("+init=epsg:32633") # UTM 33N Landsat Images

ago <- wrld_simpl[wrld_simpl@data$NAME == 'Angola',]

ago <- spTransform(ago, p.utm33n)

ago_p <- spsample(ago, type="random", n=25)    
plot(ago, col = 'grey' , axes = T)
plot(ago_p, add = T)

tdata <- data.frame(x=rep(coordinates(ago_p)[,1], 3), 
                    y=rep(coordinates(ago_p)[,2], 3),
                    temp=runif(75, 12,35),
                    day = rep(1:3, each = 25))

coordinates(tdata) <- ~x+y 

proj4string(tdata) <- CRS(proj4string(ago))

rago <- raster(extent(ago))
res(rago) <- c(10000,10000)
rago[] <- 1
proj4string(rago) <- CRS(proj4string(ago))
r_ago <- mask(rago, ago)
#plot(r_ago)
grid_ago <- as(r_ago, 'SpatialPointsDataFrame')
grid_ago <- grid_ago[!is.na(grid_ago@data$layer), ]
gridded(grid_ago) <- TRUE

idw_ago <- idw(temp ~ 1, tdata[tdata$day == 1, ], grid_ago, idp = 2.5)

image(idw_ago, "var1.pred", useRaster = T, colorkey = F, fill = NA)
plot(idw_ago)

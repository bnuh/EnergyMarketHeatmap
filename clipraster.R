library(raster)
# use state bounds from gadm website:
# us = shapefile("USA_adm1.shp")
us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
nestates <- "New York"

ne = us[match(toupper(nestates),toupper(us$NAME_1)),]


# create a random raster over the space:        
r = raster(xmn=-85,xmx=-65,ymn=36,ymx=48,nrow=100,ncol=100)
r[]=runif(100*100)

# plot it with the boundaries we want to clip against:
plot(r)
plot(ne,add=TRUE)

# now use the mask function
rr <- mask(r, ne)

# plot, and overlay:
plot(rr);plot(ne,add=TRUE)

##############################3

library(rgdal)
library(raster)

state <- readOGR(dsn = ".", layer = "cty036")
projection(state) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Subset US shapefile by desired states
nestates <- c("Maine", "Vermont", "Massachusetts", "New Hampshire" ,"Connecticut",
              "Rhode Island","New York","Pennsylvania", "New Jersey",
              "Maryland", "Delaware", "Virginia", "West Virginia")

state.sub <- state[as.character(state@data$STATE_NAME) %in% nestates, ]

elevation <- raster("wc10/tmin1.bil")

# Crop elevation data by extent of state subset
elevation.sub <- crop(elevation, extent(state.sub))

elevation.sub <- mask(elevation.sub, state.sub)

plot(elevation.sub)
plot(state.sub, add = TRUE)
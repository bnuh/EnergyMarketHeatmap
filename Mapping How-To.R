rm(list = ls())

install.packages("maptools")
install.packages("mapdata")
install.packages("spdep")
install.packages("ggplot2")

library(maptools)
library(mapdata)
library(spdep)
library(ggplot2)

# Set working directory for loading and saving:

#############
# World Map #
#############

w2007 <- readShapePoly("countries07")

# get coordinates
coords <- coordinates(w2007)
dst.mat <- as.matrix(spDists(coords, longlat=T))
xy <- cmdscale(dst.mat, k=3)
r <- rank(xy[ ,1]) / dim(dst.mat)[1]
g <- rank(xy[ ,2]) / dim(dst.mat)[1]
b <- rank(xy[ ,3]) / dim(dst.mat)[1]

w2007$mapcolor <- rgb(1-r, 1-g, 1-b, .75)
# can play with this to get different representations
# close countries have similar colors.

pdf(file = "amap.pdf")
plot(w2007, col = w2007$mapcolor)
dev.off()

##############################
# Similarly, for Afghanistan #
##############################

afg <- readShapePoly("AFG_adm1")
plot(afg)

################################
# USA Electoral Choropleth Map #
################################

# County Shapefile included in package mapdata:
Counties <- map_data("county")
head(Counties)

# Vote data from CQ, which I've edited to merge better:
Votes <- read.csv("Election Returns by County 2008.csv")
head(Votes)

# Getting identical variables in both data frames:
Counties$CountyState <- paste(Counties$subregion, Counties$region)
Votes$CountyState <- paste(Votes$ShapeCounty, Votes$ShapeState)

# Variable of interest:
Votes$RepPCT <- Votes$Republican / Votes$Total.Vote

# Getting variable of interest aligned with county shapes:
Counties$RepPCT <- by(Votes$RepPCT, Votes$CountyState, mean)[Counties$CountyState]

# Making a map with ggplot2:
Map <- qplot(long, lat, data = Counties,
 geom = "polygon",
 group = group,
 fill = RepPCT,
 xlab = NULL,
 ylab = NULL)
print(Map)

# Remove legend:
Map <- Map + opts(legend.position = "none")

# On projections: http://www.progonos.com/furuti/MapProj/Normal/TOC/cartTOC.html
# Also: http://cran.r-project.org/web/packages/mapproj/mapproj.pdf
Map <- Map + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)

# Remove a lot of extra stuff:
Map <- Map + scale_x_continuous(breaks = NA) +
    scale_y_continuous(breaks = NA) +
    opts(
      panel.grid.major = theme_blank(),
      panel.grid.minor = theme_blank(),
      panel.background = theme_blank(),
      panel.border = theme_blank(),
      axis.ticks = theme_blank())

print(Map)

### Custom Color Palettes ###
# Colour Brewer through ggplot2: http://had.co.nz/ggplot2/scale_brewer.html

CustomPalette <- colorRampPalette(c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#E0E0E0", "#BABABA", "#878787", "#4D4D4D"))
SequentialPalette <- colorRampPalette(c("#ECE2F0", "#A6BDDB", "#1C9099"))

Map <- Map + scale_fill_gradientn(colour = CustomPalette(100))
print(Map)

### Save ###
ggsave(plot = Map, "2008 Choropleth.pdf", h = 9, w = 16)

######################################################
# If we have extra time, an example coefficient plot #
######################################################

# Don't use this as a model of scientific inquiry!
Model <- lm(RepPCT ~ log(Total.Vote) + State, data = Votes)
summary(Model)

ModelEstimates <- data.frame(summary(Model)$coef)
ModelEstimates$IV <- rownames(ModelEstimates)

Multiplier <- qnorm(1 - 0.05 / 2)

CoefPlot <- qplot(IV, Estimate,  data = ModelEstimates,
 ymin = Estimate - Multiplier * Std..Error,
 ymax = Estimate + Multiplier * Std..Error,
 geom = "pointrange",
 ylab = "Independent Variable", xlab = "Estimate",
 main = "Regression Estimates and 95% CIs")
CoefPlot <- CoefPlot + coord_flip()
CoefPlot <- CoefPlot + theme_bw()
CoefPlot <- CoefPlot + geom_hline(yintercept = 0, colour = I("RED"), alpha = I(1/2))
print(CoefPlot)

# Reorder IVs according to point estimate:
ModelEstimates$IV <- factor(ModelEstimates$IV, levels = ModelEstimates$IV[order(ModelEstimates$Estimate)])

CoefPlot <- qplot(IV, Estimate,  data = ModelEstimates,
 ymin = Estimate - Multiplier * Std..Error,
 ymax = Estimate + Multiplier * Std..Error,
 geom = "pointrange",
 ylab = "Independent Variable", xlab = "Estimate",
 main = "Regression Estimates and 95% CIs")
CoefPlot <- CoefPlot + coord_flip()
CoefPlot <- CoefPlot + theme_bw()
CoefPlot <- CoefPlot + geom_hline(yintercept = 0, colour = I("RED"), alpha = I(1/2))
print(CoefPlot)
library(rworldmap)

newmap <- getMap(resolution = "high")  # different resolutions available
plot(newmap)

mapCountryData(mapRegion = "africa")

mapGriddedData(mapRegion = "africa", colourPalette = 'heat', borderCol = 'black', addLegend = T, addBorders = T, plotData = T, lwd = 5)
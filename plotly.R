library(dismo)

locs = read.csv("Test Data.csv", skip = 2, fill = T, col.names = c("ID", "name", "lon", "lat"), header = T, na.strings = c("null", 0))
lbmp = read.csv("Test Data2.csv", skip = 1, fill = T, col.names = c("ID", "time", "LBMP", "loss", "congestion"), header = T, na.strings = c("null", 0))

locs <- locs[-c(1)]
lbmp <- lbmp[-c(1,2,4,5)]
locs$lbmp <- lbmp$LBMP

locs = na.omit(locs)
locs <- locs[!is.na(locs$lbmp),]

library(plotly)

# change default color scale title
m <- list(colorbar = list(title = "LBMP", tickprefix = '$'))

# geo styling
g <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 20,
  projection = list(
    type = 'conic conformal',
    rotation = list(
      lon = -100
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-80, -70),
    dtick = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(40, 46),
    dtick = 0.5
  )
)

plot_ly(locs, size = locs$lbmp, lat = locs$lat, lon = locs$lon, text = paste("LBMP: $", locs$lbmp), color = locs$lbmp,
        type = 'scattergeo', marker = m)
  layout(title = '', geo = g)
  
## plotly_POST(p, filename = "NYISO_Pricing", world_readable = TRUE)


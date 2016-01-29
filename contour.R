require(dismo)
require(akima)

locs = read.csv("Test.csv", header = T, col.names = c("country", "lat", "lon"), as.is = T, na.strings = 0)
lbmp = read.csv("Test2.csv", header = T, col.names = c("price", "gen"), as.is = T)

data <- data.frame(x=locs$lat,
                   y=locs$lon,
                   price=lbmp$price)

resolution <- 0.01 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
a <- interp(x=data$x, y=data$y, z=data$price, 
            xo=seq(min(data$x),max(data$x),by=resolution), 
            yo=seq(min(data$y),max(data$y),by=resolution), linear = T, duplicate="mean")

filled.contour(a, color.palette=heat.colors, axes = F, extra)

p = ggplot(df, aes(x,y))
p + stat_binhex(bins=25) + scale_fill_gradientn(colours=r)
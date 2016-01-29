library("treemap")
library("viridisLite")
library("highcharter")

data(GNI2010)

tm <- treemap(GNI2010, index = c("continent", "iso3"),
              vSize = "population", vColor = "GNI",
              type = "value", palette = viridis(7, alpha = 0.5))

hc_tm <- highchart(height = 800) %>% 
  hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                        layoutAlgorithm = "squarified",
                        dataLabels = list(
                          enabled = TRUE,
                          format = "{point.label}"),
                          name = "tmdata")
  hc_title(text = "Gross National Income World Data") %>% 
  hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
             Pop: {point.value:,.0f}<br>
             GNI: {point.valuecolor:,.0f}")

hc_tm
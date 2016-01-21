crimeplot.wrapper <- function(
  point.of.interest = "London Eye",  ## user-defined location
  period = c("2013-01","2013-02"),  ## period of time in YYYY-MM
  type.map = "roadmap",  ## roadmap, terrain, satellite or hybrid
  type.facet = NA,  ## options: NA, month, category or type
  type.print = NA,  ## options: NA, panel or window
  output.plot = TRUE,  ## print it to a png file?
  output.filename = "temp.png",  ## provide a filename
  output.size = c(700,700)) ## width and height setting  )
  
## Define the period
ex1.period <- format(seq(as.Date("2013-01-01???),length=4,by="months"),"%Y-%m")


## Use the wrapper
ex1.plot <- crimeplot.wrapper(point.of.interest = "London Eye",
                              period = ex1.period,
                              type.map = "roadmap",
                              output.filename = "ex1.png",
                              output.size = c(700,700))
## Define the period
ex2.period <- format(seq(as.Date("2013-01-01"),length=4,by="months"),"%Y-%m")

## Use the wrapper
ex2.plot <- crimeplot.wrapper(point.of.interest = "London Eye",
                              period = ex2.period,
                              type.map = "roadmap",
                              type.facet = "type",
                              output.filename = "ex2.png",
                              output.size = c(1400,700))

## Define the period
ex3.period <- format(seq(as.Date("2012-01-01"),length=12,by="months"),"%Y-%m")

## Use the wrapper
ex3.plot <- crimeplot.wrapper(point.of.interest = "Manchester",
                              period = ex3.period,
                              type.map = "satellite",
                              type.facet = "month",
                              output.filename = "ex3.png",
                              output.size = c(1400,1400))

## Define the period
ex4.period <- format(seq(as.Date("2013-01-01"),length=4,by="months"),"%Y-%m")

## Use the wrapper
ex4.plot <- crimeplot.wrapper(point.of.interest = "Liverpool",
                              period = ex4.period,
                              type.map = "hybrid",
                              type.facet = "category",
                              output.filename = "ex4.png",
                              output.size = c(1400,1400))

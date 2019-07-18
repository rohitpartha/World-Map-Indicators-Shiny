library(leaflet)
library(geojsonio)
library(data.table)
library(dplyr)
library(ggplot2)
library(sp)
library(wbstats)


Countries <- 
  geojson_read( 
    "./countries.geojson"
    , what = "sp"
  )

pop_total <- wb(indicator = "SP.POP.TOTL", startdate = 2013,
                   enddate = 2013)


mergedcountries <- sp::merge(Countries, pop_total, by.x = "ISO_A2", by.y = "iso2c", all.x = FALSE)
dataframedisplay <- mergedcountries@data %>% select(.,country,ISO_A2,
                                                    ISO_A3,
                                                    ECONOMY,
                                                    INCOME_GRP,
                                                    CONTINENT,
                                                    REGION_WB )

#compute bins
min = min(mergedcountries@data$value, na.rm = TRUE)
max = max(mergedcountries@data$value, na.rm = TRUE)
diff = max - min
std = sd(mergedcountries@data$value, na.rm = TRUE)

equal.interval = seq(min, max, by = diff/6)
quantile.interval = quantile(mergedcountries@data$value, probs=seq(0, 1, by = 1/6), na.rm = TRUE)
std.interval = c(seq(min, max, by=std), max)

mergedcountries@data$population.equal = cut(mergedcountries@data$value, breaks = equal.interval, include.lowest = TRUE)
mergedcountries@data$population.quantile = cut(mergedcountries@data$value, breaks=quantile.interval, include.lowest = TRUE)
mergedcountries@data$population.std = cut(mergedcountries@data$value, breaks=std.interval, include.lowest = TRUE)



bins <- quantile.interval
pal <- colorBin("YlOrRd", domain = mergedcountries$value, bins = bins)

leaflet(mergedcountries) %>% setView(-96, 37.8, 4) %>% addTiles() %>% addPolygons(
  fillColor = ~pal(value),
  weight = 2,
  opacity = 1, 
  color = "white",
  dashArray = "3",
  fillOpacity = .7) %>% addLegend(position = "bottomleft", pal = pal, values = bins)

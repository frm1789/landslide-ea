library(leaflet)
library(dplyr)

url_land <- 'https://raw.githubusercontent.com/frm1789/landslide-ea/master/landslides.csv'
df_land <- read_csv(url(url_land))
df <- df_land

b <- leaflet(df) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude, clusterOptions = markerClusterOptions())
b

library(readr)
library(dplyr)
library(viridis)
library(viridisLite)
library(leaflet)
library(geojsonio)
library(countrycode)

# get data for map
json_api <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/countries.geojson"
mundo <- geojson_read(json_api, what = "sp")

# get info for landslides 
url_land <- 'https://raw.githubusercontent.com/frm1789/landslide-ea/master/landslides.csv'
df_land <- read_csv(url(url_land))
df_cou <- dplyr::count(df_land, country_name, id_two = df_land$country_code)

## In order to match both dataset, I am adding three letters code
url_code <- 'https://raw.githubusercontent.com/frm1789/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv'
df_code <- read_csv(url(url_code))
df_cou <- mutate(df_cou, id = (df_cou$id = countrycode(df_cou$country_name, 'country.name', 'iso3c')))
## End Adding three letters code

##Complete mundo@data w/info about landslides
mundo@data <- left_join(mundo@data, df_cou)
mundo@data$name <- as.character(mundo@data$name)

##Adding colors
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("Purples", domain = mundo@data$n, bins = bins)

casecountpopup <- paste0("<strong>", mundo@data$name, "</strong>", "<br>", "Number of Landslides: ", mundo@data$n)


m <- leaflet(data = mundo) %>%
  addPolygons(fillColor = ~pal(n), 
              fillOpacity = 0.9, 
              color = "white", 
              weight = 1,
              popup = casecountpopup) %>%
  addLegend(position = "bottomleft",pal = pal, values = ~n, title = "<strong>Landslide</strong><br>2007-2016") %>%
  setView(lat = 38.0110306, lng = -110.4080342, zoom = 3)
m %>% addProviderTiles(providers$CartoDB.Positron)





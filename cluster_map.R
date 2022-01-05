library(leaflet)
library(rgdal)
library(tmap)
library(dplyr)
library(tidyverse)

accidents <- read.csv("data/AccidentesLatLong.csv")


leaflet(accidents) %>% addTiles() %>% addMarkers(
  lat = ~coordenada_x_utm, lng= ~coordenada_y_utm,
  clusterOptions = markerClusterOptions()
)

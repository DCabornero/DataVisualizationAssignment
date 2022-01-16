library(leaflet)
library(rgdal)
library(tmap)
library(dplyr)
library(tidyverse)
library(streamgraph)
library(ggplot2)
library(ggstream)

df = read.csv('data/AccidentesLatLong.csv')

df$fecha <- as.Date(df$fecha,'%d/%m/%y')

counts = df %>%
  group_by(fecha, estado_meteorológico) %>%
  tally()

ggplot(counts, aes(x = fecha, y = n, fill = estado_meteorológico)) +
  geom_stream()

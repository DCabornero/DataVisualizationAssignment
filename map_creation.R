library(rgdal)
library(tmap)

districts <- readOGR( 
  dsn= paste0(getwd(),"/data/districts"),
  layer= "Distritos_20210712",
  verbose=FALSE
)

districts$density <- runif(21, min=0, max=100)
districts$density

tm_shape(districts) + 
  tm_polygons(col='density', title = "Accidents", palette = "Spectral") + 
  tm_scale_bar(position = c("right", "top")) +
  tm_text("NOMBRE", size = 0.5)

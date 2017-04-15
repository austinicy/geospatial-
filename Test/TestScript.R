# import SingPost locations from Dropbox folder as spatial points data frame
SingPost <- read_csv("C:/Users/Joanne Tan/Dropbox/Year 4/IS415 Geospatial Analysis for Business Intelligence/FinalProject/www/default/Singpost_Locations.csv",
                     col_types = cols(POSTAL = col_character()), col_names = TRUE)
coordinates(SingPost) <- c("X-COORDINATES","Y-COORDINATES")
proj4string(SingPost) <- CRS("+init=epsg:3414")
longlat.df <- as.data.frame(spTransform(SingPost,CRS("+proj=longlat")))
SingPost$LONGITUDE <- longlat.df$X.COORDINATES
SingPost$LATITUDE <- longlat.df$Y.COORDINATES
SingPost.df <- as.data.frame(SingPost)

leaflet(SingPost.df) %>% 
  addTiles() %>%
  setView(103.8509, 1.2800, zoom = 10) %>%
  addMarkers()
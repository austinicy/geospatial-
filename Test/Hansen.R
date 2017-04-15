library(geosphere)
library(data.table)
library(shiny)
library(leaflet)
library(rgdal)
library(readr)
library(spatstat)
library(maptools)
library(raster)
library(REAT)
library(dplyr)

originPt <- read_csv("www/default/Office Locations.csv")
destPt <- read_csv("www/default/Singpost_Locations.csv")

# check for empty values
originPt[originPt==""] <- NA
originPt <- na.omit(originPt)
destPt[destPt==""] <- NA
destPt <- na.omit(destPt)

# sets XY coordinates & projection 
coordinates(originPt) <- c("X-COORDINATES","Y-COORDINATES")
coordinates(destPt) <- c("X-COORDINATES","Y-COORDINATES")
proj4string(originPt) <- CRS("+init=epsg:3414")
proj4string(destPt) <- CRS("+init=epsg:3414")

# converts XY coordinates to LongLat coordinates
originlonglat.df <- as.data.frame(spTransform(originPt,CRS("+proj=longlat")))
destlonglat.df <- as.data.frame(spTransform(destPt,CRS("+proj=longlat")))
originPt$LONGITUDE <- originlonglat.df$X.COORDINATES
originPt$LATITUDE <- originlonglat.df$Y.COORDINATES
destPt$LONGITUDE <- destlonglat.df$X.COORDINATES
destPt$LATITUDE <- destlonglat.df$Y.COORDINATES

originPt_df <- as.data.frame(originPt)
destPt_df <- as.data.frame(destPt)
distmat <- dist.mat(originPt_df, "POSTAL", "LATITUDE", "LONGITUDE", destPt_df, "POSTAL", "LATITUDE", "LONGITUDE", unit = "km")


#odmatrix <- bind_cols(originPt_df,destPt_df)
#colnames(odmatrix) <- c("orig_id", "long_orig", "lat_orig", "dest_id", "long_dest", "lat_dest")
#setDT(odmatrix)
#odmatrix[ , dist_km2 := distGeo(matrix(c(long_orig, lat_orig), ncol = 2), 
#                                matrix(c(long_dest, lat_dest), ncol = 2))]

hansen_output <- hansen(distmat,"from","to",attrac=1,"distance",gamma=1, lambda=-2,dtype="exp",accnorm="TRUE")
originPt_df <- left_join(originPt_df,hansen_output,by=c("POSTAL"="from"))
x <- originPt_df$accessibility
hist(x,breaks=6)

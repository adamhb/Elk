#LOAD
library(tidyverse)
library(magrittr)
library(adehabitatHR)
library(foreign)
library(rgdal)
library(raster)
library(ggmap)
library(rgeos)

setwd("C:/Users/ahanb/OneDrive/Documents/Berkeley/AHB Research/elk/elk_R/Elk_R_project/data")

elk_locs <- foreign::read.dbf("elk_data.dbf")

elk_locs_raw <- foreign::read.dbf("elk_data.dbf")

#Creating a polygon of the northern study area

coords = matrix(c(-123.607, 41.74352,
         -123.577838, 41.430753,
         -123.2873,  41.430753,
         -123.32927, 41.727123,
         -123.607, 41.74352), 
       ncol = 2, byrow = TRUE)


#function for creating a polygon shapefile of the northern area
P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "study_area_1")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(Ps1, axes = TRUE)

#writing to a shapefile
writeOGR(obj = as(Ps1, "SpatialPolygonsDataFrame"), dsn = "study_area_1", layer = "study_area_1", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#Creating a polygon of the southern study area
#plot(locs_clean, axes = TRUE)
coords2 <- matrix(data = c(-123.3059, 40.98755,#bottom left
                           -123.3059, 41.20548,
                           -122.9226, 41.20548,
                           -122.9226, 40.98755,
                           -123.3059, 40.98755
                           ), ncol = 2, byrow = TRUE)
P2 <- Polygon(coords2)
#proj4string(P2) <- WGS1984
Ps2 <- SpatialPolygons(list(Polygons(srl = list(P2), ID = "study_area_2")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(Ps2, axes = TRUE)
#points(locs_clean)

#writing to a shapefile
writeOGR(obj = as(Ps2, "SpatialPolygonsDataFrame"), dsn = "study_area_2", layer = "study_area_2", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#reading in a clipped vector file of burn severity and year of burn for the northern study area (study area 1)
burn_sev <- shapefile("burn_sev_c")

#reading in a clippe vector file of burn severity and year of burn for the southern study area (study area 2)

burn_sev_south <- shapefile("burn_sev_area_2")


#reading in the shapefile of the fire perimeters
fire_perims <- shapefile("fire_perims")

#setting CRS variables
WGS1984 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
UTM10 <- CRS("+proj=utm +zone=10 +datum=WGS84")












#CANOPY EDGE DATA


#loading canopy edge data for the northern study area (Indep. Creek and Patterson) 
#150 m buffer on all canopy edges
canopy_edge <- raster("canopy_edge.tif")
canopy_edge_north_60_m_2010 <- raster("buffered_edge_north_2010_60_m.tif")


plot(canopy_edge)


#This is an alternative canopy edge dataset for the northern study area (Indep. Creek and Patterson) with a smaller buffer (not sure how small though)
canopy_edge2 <- raster("buffered_edge_2.tif")
plot(canopy_edge2)



#loading canopy edge data for the southern study area (Summerville / Cecilville) 
#150 m buffer on all canopy edges
canopy_edge_south <- raster("buffered_edge_south_2010.tif")
canopy_edge_south_60m <- raster("buffered_edge_south_2010_60_m_buffer.tif")
canopy_edge_south_60m_2005 <- raster("buffered_edge_south_2005_60_m_buffer.tif")


plot(canopy_edge_south_60m)


#open areas data
open_areas_north <- raster("open_areas_north.tif")
reclass.xxx <- matrix(data = c(1.01,256,1), ncol = 3, byrow = T)

open_areas_north <- reclassify(open_areas_north, reclass.xxx)


open_areas_south <- raster("open_areas_south_2005_utm.tif")

open_areas_south <- reclassify(open_areas_south, reclass.xxx)






#saving the canopy edge 2 data set as as kml
proextent <- projectExtent(object = canopy_edge2, crs = WGS1984)
canopy_edge2_WGS <- projectRaster(from = canopy_edge2, to = proextent)
KML(x = canopy_edge2_WGS, filename = "canopy_edge.kml")





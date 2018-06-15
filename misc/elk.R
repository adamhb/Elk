#this script shows data cleaning and manipulation of my tracking data from the GPX files to excel

library('spocc')
library('plotKML')
library(tidyverse)
library(magrittr)
library(plotKML)

#importing GPX of my waypoints for first two trips
elk_points <- readGPX("all_elk_wpts_and_tracks_7_17_2017.gpx")
elk_points <- elk_points$waypoints
elk_points <- elk_points %>%
  select(-sym, -type, -extensions)
elk_points <- elk_points[-c(1:5,14:18),]

#creating a csv for google fusion tables
#write.csv(elk_points, "elk_points_trip1_and_2.csv")

#8/5/2017
#changing timezone of the waypoints
elk_points$time <- gsub("T", " ", elk_points$time)
elk_points$time <- gsub("Z", " ", elk_points$time)
elk_date_time <- as.POSIXct(elk_points$time, tz="GMT") 
attributes(elk_date_time)$tzone <- "America/Los_Angeles" 
elk_points$time <- elk_date_time

#creating a shapefile of thoese observations
WGScoor <-  elk_points
coordinates(WGScoor)=~lon+lat
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
raster::shapefile(WGScoor, "tracking_data.shp")



#doing the same as above for 3rd summer trip and winter data.

dec2017_wpts <- readGPX("Dec_2017_tracking_data.gpx")
d17_wpts <- dec2017_wpts$waypoints
d17_wpts <- d17_wpts %>%
  select(lon, lat, name)
d17_wpts <- d17_wpts[-9,]
write.csv(d17_wpts, "d17_wpts.csv")













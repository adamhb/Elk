library(maps)
library(foreign)
library(adehabitatHS)
library(magrittr)
library(tidyverse)
library(sp)


#LOAD

elk_locs <- read.dbf("elk_data.dbf")

#aggregating at each individual elk
elk_locs %>%
  select(ANIMAL_ID) %>%
  unique(.) %>%
  length(.$ANIMAL_ID)

#max and min dates
min(elk_locs$OBSDATE)
t <- elk_locs$OBSDATETIM
max(elk_locs$OBSTIME)


#coverting time to POSIXct, making column "dt"
date_time <- ISOdatetime(year = elk_locs$YEAR_, month = elk_locs$MONTH_, day = elk_locs$DAY_, hour = elk_locs$HOUR_, min = elk_locs$MINUTE_, sec = elk_locs$SECOND_, tz = "")

elk_locs$dt <- date_time

date_time <- as.POSIXct(as.character(date_time)) 

#creating new animal names

old_names <- unique(elk_locs$ANIMAL_ID)
new_names <- paste0("E",1:length(old_names))

elk_locs$AID <- ''
for(i in 1:length(old_names)){
  elk_locs[elk_locs$ANIMAL_ID == old_names[i],]$AID <- new_names[i]
}

#creating individual trajectories and spatial points dataframes for the first 8 animals (which don't have duplicate issues)

traj_names <- paste0("traj",new_names)
new_names <- new_names[1:8]

for(i in 1:length(new_names)){
  traj_names[i] <- as.ltraj(
    xy = elk_locs[elk_locs$AID == new_names[i],c("UTM_EAST", "UTM_NORTH")], 
    date = as.POSIXct(elk_locs[elk_locs$AID == new_names[i],]$dt, tz = "PST"), 
    id = as.character(elk_locs[elk_locs$AID == new_names[i],]$AID), 
    burst = as.character(elk_locs[elk_locs$AID == new_names[i],]$AID), 
    typeII = TRUE, 
    infolocs = data.frame(pkey = elk_locs[elk_locs$AID == new_names[i],]$OBJECTID, alt = elk_locs[elk_locs$AID == new_names[i],]$ALTITUDE, temp = elk_locs[elk_locs$AID == new_names[i],]$TEMP, sex = elk_locs[elk_locs$AID == new_names[i],]$SEX),
    proj4string = CRS("+init=epsg:32610"))
}


#turning the above into spatial dataframes

coordinates(trajs[[1]]) = ~x+y
proj4string(trajs[[1]])<- CRS("+proj=utm +zone=10 +datum=WGS84")

for(i in 2:16){
  coordinates(trajs[[i]])=~x+y
  proj4string(trajs[[i]])<- CRS("+proj=utm +zone=10 +datum=WGS84")
}

i <- 1

#converting to a geographic coordinate system
trajs_sp <- list()
for(i in 1:16){
  trajs_sp[i] <- spTransform(trajs[[i]], CRS("+proj=longlat +datum=WGS84"))
}

trajs_sp[[4]]

#importing a shapefile with the study area
study_area <- readOGR("study_area.shp")


b_study_area <- bbox(study_area)

#downloading a basemap based off of that extent
base_study_area <- ggmap(get_map(location = b_study_area))
points(trajs_sp[[4]])

basetest <- base_study_area + geom_point(data = as.data.frame(coordinates(trajs_sp[[16]])), aes(x,y))






#creating a clean dataset for one individual

locs_E160128F_12 <- elk_locs %>%
  filter(ANIMAL_ID == "E160128F_12")

#creating a first trajectory for the first one I tried
traj_E12_first <- as.ltraj(xy = locs_E160128F_12[,c("UTM_EAST", "UTM_NORTH")], date = as.POSIXct(locs_E160128F_12$dt, tz = "PST"), id = as.character(locs_E160128F_12$ANIMAL_ID), burst = as.character(locs_E160128F_12$ANIMAL_ID), typeII = TRUE, infolocs = data.frame(pkey = locs_E160128F_12$OBJECTID, alt = locs_E160128F_12$ALTITUDE, temp = locs_E160128F_12$TEMP, sex = locs_E160128F_12$SEX), proj4string = CRS("+init=epsg:32610"))

summary.ltraj(traj_E12)


#getting a base map for the E12 trajectory 
library(ggmap)

#converting the trajectory object to a spatial points dataframe
E12_sp <- traj_E12[[1]]
coordinates(E12_sp)=~x+y
proj4string(E12_sp)<- CRS("+proj=utm +zone=10 +datum=WGS84")

#converting to geographic coordinate system
E12_sp <- spTransform(E12_sp,CRS("+proj=longlat +datum=WGS84"))
plot(E12_sp)

E000 <- E12_sp


















#creating an extent based off of the points of the one animal
b <- bbox(E12_sp)

#downloading a basemap based off of that extent
base <- ggmap(get_map(location = b))

base
points(E12_sp)

base <- base + geom_point(data = as.data.frame(coordinates(E12_sp)), aes(x,y))
base

#creating a raster where this one elk has gone (first I need the rasters for habitat selection)

E12_raster <- rasterize.ltraj()




##creating the trajectory object for all elk (note: can't do this yet because need to reconcile the duplicate time issue)
elk_traj <- as.ltraj(xy = elk_locs[,c("UTM_EAST", "UTM_NORTH")], date = as.POSIXct(elk_locs$dt), id = as.character(elk_locs$ANIMAL_ID), burst = as.character(elk_locs$ANIMAL_ID), typeII = TRUE, infolocs = data.frame(pkey = elk_locs$OBJECTID, alt = elk_locs$ALTITUDE, temp = elk_locs$TEMP, sex = elk_locs$SEX), proj4string = CRS("+init=epsg:32610"))

unique(elk_locs$ANIMAL_ID)

elk_06 <- elk_locs %>%
  filter(ANIMAL_ID == "E160327F_06")

duplicated(elk_06$dt)

dupes <- duplicated(elk_locs$dt)


elk_locs$ANIMAL_ID





ltraj.elk






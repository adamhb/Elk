library(tidyverse)
library(magrittr)
library(adehabitatHR)
library(foreign)
library(rgdal)
library(raster)
library(ggmap)
library(rgeos)
library(leaflet)
library(suncalc)
library(lubridate)
library(sf)
library(velox)
library(lme4)


####################LOADING ALL DATA#######################
setwd("C:/Users/ahanb/OneDrive/Documents/Berkeley/AHB Research/elk/elk_R/Elk_R_project/funcs")
source("FUNCS_elk.R")


WGS84 <- CRS("+init=epsg:4326")


setwd("C:/Users/ahanb/OneDrive/Documents/Berkeley/AHB Research/elk/elk_R/Elk_R_project/data")
elk_locs <- foreign::read.dbf("elk_data.dbf") #LOADING THE RELOCATION DATA
open_areas_north <- raster("open_areas_north.tif")
burn_sev_last_burn_2011_north_test <- raster("burn_sev_north.tif")



###########CLEANING RELOCATION DATA###################


#formatting the date and time
date_time <- paste0(elk_locs$YEAR_,".", elk_locs$MONTH_,".", elk_locs$DAY_,".", elk_locs$HOUR_,".", elk_locs$MINUTE_,".",elk_locs$SECOND_)


#setting the time zone
date_time <- as.POSIXct(strptime(date_time,format="%Y.%m.%d.%H.%M.%S"), tz = "America/Los_Angeles")
#attr(date_time, "tzone") <- "America/Los_Angeles"
elk_locs$date_time <- date_time

#taking just the variables I care about
elk_c1 <- elk_locs %>% select(OBJECTID, ANIMAL_ID, date_time, UTM_EAST, UTM_NORTH, ALTITUDE, TEMP_, ACTIVITY, SEX)
names(elk_c1)[7] <- "TEMP"

#adding in Adam HB's animal ID names
old_new_names <- read.csv("old_new_names.csv")
elk_c1$AID <- plyr::mapvalues(x = elk_c1$ANIMAL_ID, from = as.character(old_new_names$old), to = as.character(old_new_names$new))


#adding lat/long to the data
lat_long <- spTransform(x = SpatialPoints(coords = elk_c1[,c(4,5)], proj4string = UTM10), CRSobj = WGS84) %>% as.data.frame(.)
names(lat_long) <- c("lon", "lat")
elk_c1 <- cbind(elk_c1, lat_long)

#getting rid of spurious relocations 
elk_c1 <- elk_c1[elk_c1$UTM_EAST > 10,]
elk_c1 <- elk_c1[!is.na(elk_c1$date_time),]
#plot(x = elk_c1$UTM_EAST, y = elk_c1$UTM_NORTH)














#########################CONVERTING THE RELOCATION DATA TO A TRAJECTORY OBJECT TO CALCULATE MORE MOVEMENT VARIABLES ############

#preparing the data for a trajectory object
xy.elk <- elk_c1[c("UTM_EAST", "UTM_NORTH")]
xy.sp.elk <- SpatialPoints(na.omit(xy.elk), proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))
xy.df.elk <- data.frame(xy.sp.elk)

#creating a dataframe with the elk IDs and the dates
move.df.elk <- data.frame(ID = elk_c1$AID, Burst = elk_c1$date_time)
move.df.elk$ID <- factor(move.df.elk$ID, levels = unique(move.df.elk$ID))
#adding coordinates to the move dataframe
coordinates(move.df.elk) <- xy.df.elk

#creating a trajectory object with the above move dataframe
ltraj.elk <- as.ltraj(coordinates(move.df.elk), move.df.elk$Burst, id = move.df.elk$ID)









#########CONVERTING THE RELOCATION DATA BACK TO A DATAFRAME AND DOING MORE CLEANING###################

#converting the trajectory object back to dataframe mode
ltraj.elk.df <- ld(ltraj.elk)

#exploring the time lags between points
ggplot(data = ltraj.elk.df) +
  geom_histogram(mapping = aes(x = dt/3600), binwidth = 0.25) +
  coord_cartesian(ylim = c(0, 10000), xlim = c(0,15)) 

#joining the ltraj df with the original to preserve all data
elk_c2 <- cbind(ltraj.elk.df, elk_c1) %>% select(OBJECTID, id, date_time, date, x, y, lon, lat, dx, dy, dist, dt, R2n, abs.angle, rel.angle, ALTITUDE, TEMP, ACTIVITY, SEX)

# getting rid of relocations where the time lag was more than 24 hours
elk_c2 <- elk_c2[elk_c2$dt/3600 < 24,]





#############ADDING THE DIEL CYCLE TO THE RELOCATION DATA###################

#Adding the diel cycle variable to the data
#preparing a date object for suncalc, such that the date is never rounded up...
date_time4suncalc <- as.Date(substr(as.character(elk_c2$date_time), start = 1, stop = 10))

#calculating sunrise and sunset for nor cal
sunrise_sunset <- getSunlightTimes(date = date_time4suncalc, lat = as.data.frame(elk_c2$lat)[1,1], lon = as.data.frame(elk_c2$lon)[1,1], keep = c("sunrise", "sunset"), tz = "America/Los_Angeles")

#defining the start and end time of sunrise and sunset according to prior research (find this paper)
sunrise_sunset <- sunrise_sunset %>% mutate(sunrise_st = sunrise - 3600) %>% mutate(sunrise_stp = sunrise + 7200) %>% mutate(sunset_st = sunset - (3600*3)) %>% mutate(sunset_stp = sunset + (2.5*3600))

names(sunrise_sunset)[1] <- "date_suncalc"

#joining df
elk_c3 <- cbind(elk_c2, sunrise_sunset[,-c(2,3)])

#getting rid of NA times
elk_c3 <- elk_c3[is.na(elk_c3$date_time) == F,]

#adding the diet cycle to the data set
diel_cycle <- c()
for(i in 1:length(elk_c3$OBJECTID)){
  
  if(elk_c3[i,"date_time"] >= elk_c3[i,"sunrise_st"] & elk_c3[i,"date_time"] < elk_c3[i,"sunrise_stp"]){
    diel_cycle[i] <- "sunrise"
    }
  
  if(elk_c3[i,"date_time"] >= elk_c3[i,"sunset_st"] & elk_c3[i,"date_time"] < elk_c3[i,"sunset_stp"]){
    diel_cycle[i] <- "sunset"
  }
  
  if(elk_c3[i,"date_time"] >= elk_c3[i,"sunset_stp"] | elk_c3[i,"date_time"] < elk_c3[i,"sunrise_st"]){
    diel_cycle[i] <- "night"
  } 
  
  
  if(elk_c3[i,"date_time"] < elk_c3[i,"sunset_st"] & elk_c3[i,"date_time"] >= elk_c3[i,"sunrise_stp"]){
    diel_cycle[i] <- "day"
  }

}


elk_c3$diel_cycle <- diel_cycle
elk_c3$diel_cycle <- as.factor(elk_c3$diel_cycle)





#####MORE CLEANING OF THE RELOCATION DATA ####################
#adding the CDFW id back into the df
elk_c3$CDFW_id <- plyr::mapvalues(x = elk_c3$id, from = as.character(old_new_names$new), to = as.character(old_new_names$old))


#saving a trajectory object of the cleaned relocation data
ltraj.elk.c1 <- dl(elk_c3)

###CLEANED RELOCATION DATA#### = elk_c3
#END DATA CLEANING FOR ALL ELK IN THIS DATA SET###
#BELOW IS A SPECIAL CASE FOR E12#












#####################Generating a Study Area and Sampling Units for E12's Summer Range#############

E12 <- elk_c3 %>% filter(id == "E12") 

#specifying the summer and winter ranges 
#adding a range variable

#NEED TO DEFINE THIS MANUALLY
range <- rep(0, length(E12$OBJECTID))
range[E12$x <= 462673] <- "winter_rng"
range[E12$x > 462673] <- "summer_rng"
E12$range <- range


#checking the relocation times make sense for E12 using temperature as a check
E12 %>% filter(range == "summer_rng") %>% ggplot(mapping = aes(x = diel_cycle, y = TEMP)) + geom_boxplot()
#look at activity by diel cycle
E12 %>% filter(range == "winter_rng") %>% ggplot(mapping = aes(x = diel_cycle, y = ACTIVITY)) + geom_boxplot()


#SPDF: creating spatial points dataframe object of E12's relocations
E12_sp <- SpatialPoints(E12[c("x","y")], proj4string = UTM10)
E12_SPDF <- SpatialPointsDataFrame(E12_sp, data.frame(OBJECTID=E12$OBJECTID), match.ID = T)
E12_SPDF <- merge(E12_SPDF, E12)





##########DEFINING E12'S SUMMER RANGE####
#creating E12s homerange
E12_hr <- HRmaker(ltraj.elk.c1, AnimalID = "E12")
plot(E12_hr)


#defining a bounding box for E12's summer range
#NEED TO DEFINE THIS MANUALLY#


coords = matrix(c(462673, 4599931,
                  471732, 4599931,
                  471732, 4588267.4,
                  462673, 4588267.4,
                  462673, 4599931), 
                ncol = 2, byrow = TRUE)
#creating a polygon of summer range from the bounding box
P1 = Polygon(coords)
#creating a spatial polygons object from the bounding box
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=UTM10)

#intersecting the summer bounding box and the home range to create a summer range polygon 
E12_summer <- gIntersection(E12_hr, Ps1)
plot(E12_summer)







########### CREATING SAMPLING UNITS FOR E12 ######################


#creating random sample of circular sample units in E12s summer range
smp_mid <- spsample(E12_summer, n = 250, type = "random")
smp <- (st_buffer(st_as_sf(smp_mid), dist = 200))

#converting to spatial polygons dataframe
sample_units_sp <- sf::as_Spatial(smp$geometry)
sample_units <- SpatialPolygonsDataFrame(Sr = sample_units_sp, data = data.frame(s_unit = 1:length(smp_mid)), match.ID = F)
crs(sample_units) <- UTM10


#########Creating Output Variables in each sampling unit (N_relocs_per_sampling unit) ######################
#adding the number of relocations per sampling unit to the sample unit dataframe
res <- over(E12_SPDF, sample_units)
response_var <- res %>% na.omit(.) %>% .$s_unit %>% table() %>% as.data.frame()
names(response_var) <- c("s_unit","N_relocs")

sample_units <- merge(sample_units, response_var, by = "s_unit")
sample_units@data$N_relocs <- plyr::mapvalues(sample_units@data$N_relocs, from = NA, to = 0)
sample_units@data$N_relocs_normlzd <- sample_units@data$N_relocs / length(E12_SPDF)



#getting response variables for each diel cycle 
sunrise_response <- over(E12_SPDF[E12_SPDF@data$diel_cycle == "sunrise",], sample_units) %>%
  .$s_unit %>% table() %>% as.data.frame()
names(sunrise_response) <- c("s_unit","N_relocs")
sunrise_response <-  merge(data.frame(s_unit = 1:length(sample_units)), sunrise_response, by = "s_unit" ,all = T)
sunrise_response$N_relocs <- plyr::mapvalues(x = sunrise_response$N_relocs, from = NA, to = 0)


sunset_response <- over(E12_SPDF[E12_SPDF@data$diel_cycle == "sunset",], sample_units) %>%
  .$s_unit %>% table() %>% as.data.frame()
names(sunset_response) <- c("s_unit","N_relocs")
sunset_response <-  merge(data.frame(s_unit = 1:length(sample_units)), sunset_response, by = "s_unit" ,all = T)
sunset_response$N_relocs <- plyr::mapvalues(x = sunset_response$N_relocs, from = NA, to = 0)


day_response <- over(E12_SPDF[E12_SPDF@data$diel_cycle == "day",], sample_units) %>%
  .$s_unit %>% table() %>% as.data.frame()
names(day_response) <- c("s_unit","N_relocs")
day_response <-  merge(data.frame(s_unit = 1:length(sample_units)), day_response, by = "s_unit" ,all = T)
day_response$N_relocs <- plyr::mapvalues(x = day_response$N_relocs, from = NA, to = 0)

night_response <- over(E12_SPDF[E12_SPDF@data$diel_cycle == "night",], sample_units) %>%
  .$s_unit %>% table() %>% as.data.frame()
names(night_response) <- c("s_unit","N_relocs")
night_response <-  merge(data.frame(s_unit = 1:length(sample_units)), night_response, by = "s_unit" ,all = T)
night_response$N_relocs <- plyr::mapvalues(x = night_response$N_relocs, from = NA, to = 0)


#making sure each diel cycle response adds to the total
all_responses <- data.frame(s_unit = 1:length(sample_units), total = sample_units@data$N_relocs, sunrise = sunrise_response$N_relocs, day = day_response$N_relocs, sunset = sunset_response$N_relocs, night = night_response$N_relocs)


#normalizing each diel cycle response by the total number of relocations in that diel cycle
sunrise_total_relocs <- nrow(E12_SPDF[E12_SPDF@data$diel_cycle == "sunrise",])
day_total_relocs <- nrow(E12_SPDF[E12_SPDF@data$diel_cycle == "day",])
sunset_total_relocs <- nrow(E12_SPDF[E12_SPDF@data$diel_cycle == "sunset",])
night_total_relocs <- nrow(E12_SPDF[E12_SPDF@data$diel_cycle == "night",])

names(all_responses)[-1] <- paste0(names(all_responses)[-1],"_", "response")


all_responses$sunrise_response_norm <- all_responses$sunrise_response / sunrise_total_relocs
all_responses$day_response_norm <- all_responses$day_response / day_total_relocs
all_responses$sunset_response_norm <- all_responses$sunset_response / sunset_total_relocs
all_responses$night_response_norm <- all_responses$night_response / night_total_relocs

sample_units <- merge(sample_units, all_responses[-2], by = "s_unit")


#mapping in leaflet

sample_units_WGS84 <- spTransform(sample_units_c1, CRSobj = WGS84)
E12_SPDF %>% spTransform(., CRSobj =  WGS84)


popup_content <- paste("<b>Unit ID</b>: ", sample_units_WGS84$s_unit, "<br>",
                       "<b>N_relocs:</b>", sample_units_WGS84$N_relocs,"<br>", 
                       "<b>N_relocs_normlzd:</b> ", sample_units_WGS84$N_relocs_normlzd, "<br>",
                       "<b>Elevation:</b>", sample_units_WGS84$ele)


leaflet() %>%
addTiles() %>%
  addPolygons(data=sample_units_WGS84, popup = popup_content) %>%
    addRasterImage(srtm_crop) %>%
    addMarkers(data = E12_SPDF %>% spTransform(., CRSobj =  WGS84), lat = E12_SPDF$lat, lng = E12_SPDF$lon)
  
  
  




#####################generating explanatory variables for each sampling unit (i.e. circle)#######################


#elevation
#bringing in elevation data
srtm_big <- getData('SRTM', lon = -123, lat = 41)

#downscaling while still projected in lat long
srtm_smaller <- raster::crop(x = srtm_big, y = study_area)
#converting the srtm data to utm zone 10
utm10_srtm_extent <- projectExtent(object = srtm_smaller, crs = UTM10)
srtm_smaller_utm <- projectRaster(from = srtm_smaller, to = utm10_srtm_extent)
plot(srtm_smaller_utm)

#cropping to the study area (for now this is E12's summer range)
srtm_crop <- crop(srtm_smaller_utm, E12_summer)

plot(srtm_crop)
plot(E12_summer, add = T)

#creating slope and aspect variables
E12_summer_terrain <- raster::terrain(x = srtm_crop, opt = c("slope", "aspect", "TPI", "TRI", "roughness"), unit = "degrees")


ele <- unlist(lapply(raster::extract(x = srtm_crop, y = sample_units), mean))
terrain_vars <- lapply(raster::extract(x = E12_summer_terrain, y = sample_units), colMeans)
terrain_vars <- cbind(ele, as.data.frame(do.call(rbind, terrain_vars)))


#adding burn severity data
#the object "burn_sev_last_burn_2011_north" was created in the script "fire.R"
plot(burn_sev_last_burn_2011_north)
plot(E12_summer, add = T)

burn_sev_E12_summer <- raster::extract(burn_sev_last_burn_2011_north, y = sample_units, fun = mean)
burn_sev_E12_summer[is.na(burn_sev_E12_summer[,1]),1] <- 1

#adding the tree canopy data ("open"=<25% tree cover)
open_cropped <- crop(open_areas_north, E12_summer)
tree_cover_e12_summer <- raster::extract(open_cropped, y = sample_units, fun = mean)
#tree_cover_e12_summer <- round(tree_cover_e12_summer)


#combining all all explanatory variables into one data frame
E12_vars <- cbind(terrain_vars, burn_sev_E12_summer[,1], tree_cover_e12_summer[,1])
names(E12_vars)[7] <- "burn_sev"
E12_vars$s_unit <- 1:length(sample_units) 


##############merging explanatory vars with sample units and dependent variables################
sample_units_c1 <- merge(sample_units, E12_vars, by = "s_unit")
names(sample_units_c1)[19] <- "pct_open_canopy"
summary(sample_units_c1) #this is the finalized SPDF object for E12

#########converting the SPDF to a regular dataframe
E12_HSA_data <- sample_units_c1@data
str(E12_HSA_data)
  
  



  
#######################Statistical Analysis##########################

hist(E12_HSA_data$N_relocs) #checking distribution of the number of relocations in each sampling unit


#fitting a negative binomial model with all relocations
mod_all <- glm.nb(data = E12_HSA_data, formula = N_relocs_normlzd ~ slope + I(slope^2) + aspect + pct_open_canopy)


mod_sunrise <- glm.nb(data = E12_HSA_data, formula = sunrise_response ~ slope + I(slope^2) + aspect + pct_open_canopy)


mod_sunset <- glm.nb(data = E12_HSA_data, formula = sunset_response ~ slope + I(slope^2) + aspect + pct_open_canopy)


mod_night <- glm.nb(data = E12_HSA_data, formula = night_response ~ slope + I(slope^2) + aspect + pct_open_canopy)


mod_day <- glm.nb(data = E12_HSA_data, formula = day_response ~ slope + I(slope^2) + aspect + pct_open_canopy)



summary(mod_all)
summary(mod_sunrise)
summary(mod_sunset)
summary(mod_night)
summary(mod_day)



mod_generalized <- glm.nb(data = E12_HSA_data, formula = log(day_response_norm) ~ slope + I(slope^2) + aspect + pct_open_canopy)
summary(mod_generalized)


E12_HSA_data$N_relocs_normlzd

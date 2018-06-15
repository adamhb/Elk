
##Study Area North (Patterson and Ti Creek)
#creating four fire layers: 1) Time since the last fire 2) burn severity of the last burn 3) #fires since records began 4) # fires since 2000.

#creating datafram of all cleaned elk relocations
locs_clean <- na.omit(ltraj.elk.df[,c(1,2)])
coordinates(locs_clean) <- ~x+y
proj4string(locs_clean) <- UTM10
locs_clean <- spTransform(locs_clean, WGS1984)
plot(locs_clean)

#defining the southern study area (might not need this though)
#plot(locs_clean)
#study_area_poly_south <- drawPoly()
#proj4string(study_area_poly_south) <- WGS1984
#plot(study_area_poly_south)


#transforming the burn severity spatial polygons object to WGS1984 with only the best assessment. SA1 = study area north, which "burn sev" was initially clipped to in ArcGIS.
Bsev_best_assess_SA1 <- burn_sev[burn_sev@data$BEST_ASSES == "YES" ,]
Bsev_WGS1984_SA1 <- spTransform(Bsev_best_assess_SA1, WGS1984)

#disaggregating the polygon to make a rasterstack of the burn severity for every fire in study area1. 
fireNames <- Bsev_WGS1984_SA1@data$VB_ID
fireNames <- sort(unique(fireNames) ,decreasing = TRUE)
fire_stack <- onefire(data = Bsev_WGS1984_SA1, fireName = fireNames[1])
reclass <- matrix(data = c(1,NA), ncol = 2, byrow = TRUE)
fire_stack <- reclassify(fire_stack, reclass)
for(i in 2:length(fireNames)){
  frNm <- fireNames[i]
  fire_rast <- onefire(data = Bsev_WGS1984_SA1, fireName = frNm, field = "BURNSEV")
  print(paste0("finished ",frNm))
  fire_rast <- reclassify(x = fire_rast, rcl = reclass)
  fire_stack <- stack(fire_stack, fire_rast)
}
names(fire_stack) <- fireNames

#merging the layers to create a "burn severity of the last burn layer"
#this part can be tweaked for each elk you are working on
#OBJECT BELOW IS WHAT I SHOULD USE FOR EXTRACTING POINTS FOR ANALYSIS
burn_sev_last_burn <- merge(fire_stack)

#for 2012 as an example
burn_sev_last_burn_2011 <- merge(fire_stack[[3:16]])
filledContour(burn_sev_last_burn_2011)


#for just the 2008 fires
burn_sev_2008_fires <- merge(fire_stack[[c(3,4)]])

burn_sev_1987_thru_2007 <- merge(fire_stack[[5:16]])

#creating samples of points of the 2011 burn severity map that can be exported to ArcGIS online and then interpolated for a contour map
rast2points <- function(rasterName, output_name, NA_vals = 1, size.denom = 20){
  size.x <- length(getValues(rasterName))/size.denom
  points <- sampleRegular(x = rasterName, extent = extent(rasterName), size = size.x, xy = TRUE)
  points <- as.data.frame(points)
  points[is.na(points$layer),]$layer <- NA_vals
  write.csv(x = points, file = paste0(output_name,".csv"))
  coordinates(points) <- ~x+y
  proj4string(points) <- CRS("+init=epsg:4326")
  writeOGR(points,driver='ESRI Shapefile', dsn = output_name, layer = output_name, overwrite_layer = TRUE)
  return(points)
}

#this function created the burn severity mosaic that I have imported to ArcGIS online as a csv file and then run interpolation on to graph the burn severity. 
rast2points(rasterName = burn_sev_last_burn_2011, output_name = "2011_burn_sev_mosaic_small", NA_vals = 1, size.denom = 100)

rast2points(rasterName = burn_sev_2008_fires, output_name = "Burn_Severity_2008", NA_vals = 1, size.denom = 100)

rast2points(rasterName = burn_sev_1987_thru_2007, output_name = "burn_sev_1987_thru_2007", NA_vals = 1, size.denom = 100)



#creating a "time-since-last-burn" stack for each
year.x <- 2016
reclass1 <- matrix(data = c(2, year.x,
                            3, year.x,
                            4, year.x), ncol = 2, byrow = TRUE)
yearStack <- reclassify(fire_stack[[1]], rcl = reclass1)

for(i in 2: length(fireNames)){
  
  year.x <- as.numeric(substr(fireNames[i], start = 1, stop = 4))
  reclass <- matrix(data = c(2, year.x,
                             3, year.x,
                             4, year.x), ncol = 2, byrow = TRUE)
  yearLayer <- reclassify(fire_stack[[i]], rcl = reclass)
  yearStack <- stack(yearStack, yearLayer)
}

#THIS YEAR STACK OBJECT IS WHAT I USE FOR EXTRACTING DATA FOR TIME SINCE LAST BURN
names(yearStack) <- fireNames

#creating time since last burn for 2011 as an example
time_since_last_fire_2011 <- (2011 - merge(yearStack[[3:16]]))
time_since_last_fire_2007 <- (2007 - merge(yearStack[[5:16]]))

#playing around with mapping raster math of interaction effects between burn severity and time since last burn.
plot(burn_sev_last_burn_2012/time_since_last_fire_2011)
plot(burn_sev_last_burn_2012)
plot(time_since_last_fire_2011)


#extracting points for the Arc Story Map

rast2points(rasterName = time_since_last_fire_2011, output_name = "Time_since_last_fire_2011", NA_vals = 2011-1987+11, size.denom = 100)

rast2points(rasterName = time_since_last_fire_2007, output_name = "Time_since_last_fire_2007", NA_vals = 34, size.denom = 100)































##Study Area South (Rush Creek Area)

#transforming the burn severity spatial polygons object to WGS1984 with only the best assessment. SA1 = study area north, which burn sev was initially clipped to.
Bsev_best_assess_SA2 <- burn_sev_south[burn_sev_south@data$BEST_ASSES == "YES" ,]
Bsev_WGS1984_SA2 <- spTransform(Bsev_best_assess_SA2, WGS1984)
South_fireNames <- unique(Bsev_WGS1984_SA2@data$VB_ID)
#disaggregating the polygon to make a rasterstack of the burn severity for every fire in study area1. 

South_fireNames <- sort(unique(South_fireNames),decreasing = TRUE)
South_fire_stack <- onefire(data = Bsev_WGS1984_SA2, fireName = South_fireNames[1], field.x = "BURNSEV")
reclass <- matrix(data = c(1,NA), ncol = 2, byrow = TRUE)
South_fire_stack <- reclassify(South_fire_stack, reclass)
for(i in 2:length(South_fireNames)){
  South_frNm <- South_fireNames[i]
  South_fire_rast <- onefire(data = Bsev_WGS1984_SA2, fireName = South_frNm, field.x = "BURNSEV")
  print(paste0("finished ",South_frNm))
  South_fire_rast <- reclassify(x = South_fire_rast, rcl = reclass)
  South_fire_stack <- stack(South_fire_stack, South_fire_rast)
}

#THESE ARE THE OBJECTS TO WORK WITH (SOUTH FIRE STACK) for getting rasters of burn sev of the last burn.
names(South_fire_stack) <- South_fireNames
South_burn_sev_last_burn <- merge(South_fire_stack)

South_burn_sev_1987_thru_2011 <- merge(South_fire_stack[[1:11]])

South_burn_sev_1987_thru_2006 <- merge(South_fire_stack[[4:11]])

South_burn_sev_1987_thru_2005 <- merge(South_fire_stack[[6:11]])


rast2points(rasterName = South_burn_sev_1987_thru_2011, output_name = "Cecilville_burn_sev_1987_thru_2011", NA_vals = 1, size.denom = 100)

rast2points(rasterName = South_burn_sev_1987_thru_2006, output_name = "Cecilville_burn_sev_1987_thru_2006", NA_vals = 1, size.denom = 100)


rast2points(rasterName = South_burn_sev_1987_thru_2005, output_name = "Cecilville_burn_sev_1987_thru_2005", NA_vals = 1, size.denom = 100)



#creating a "time-since-last-burn" stack for the Southern Study Area
year.x.south <- 2011
reclass2 <- matrix(data = c(2, year.x.south,
                            3, year.x.south,
                            4, year.x.south), ncol = 2, byrow = TRUE)
yearStack_south <- reclassify(South_fire_stack[[1]], rcl = reclass2)

for(i in 2:length(South_fireNames)){
  year.x.south <- as.numeric(substr(South_fireNames[i], start = 1, stop = 4))
  reclass <- matrix(data = c(2, year.x.south,
                             3, year.x.south,
                             4, year.x.south,
                             255, year.x.south), ncol = 2, byrow = TRUE)
  yearLayer_South <- reclassify(South_fire_stack[[i]], rcl = reclass)
  yearStack_south <- stack(yearStack_south, yearLayer_South)
}

yearStack_south

names(yearStack_south) <- South_fireNames




























#script to generate fire perimeters of the most important fires
#defining the area I care most about
study_area1_albers <- spTransform(Ps1, CRSobj = CRS(proj4string(fire_perims)))
fire_perims_cropped <- crop(fire_perims, study_area1_albers)

#choosing just one fire to look at at a time
fires_last_50_years <- fire_perims_cropped[fire_perims_cropped@data$YEAR_ >= 2013-50,]
plot(subset(fires_last_50_years, FIRE_NAME == "KING TITUS"))



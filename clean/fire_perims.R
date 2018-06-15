#script to generate fire perimeters of the most important fires
#defining the area I care most about
study_area1_albers <- spTransform(Ps1, CRSobj = CRS(proj4string(fire_perims)))
fire_perims_cropped <- crop(fire_perims, study_area1_albers)

#choosing just one fire to look at at a time
fires_last_50_years <- fire_perims_cropped[fire_perims_cropped@data$YEAR_ >= 2013-50,]
plot(subset(fires_last_50_years, FIRE_NAME == "KING TITUS"))



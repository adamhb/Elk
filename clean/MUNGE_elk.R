#MUNGE

#formatting the date and time
date_time <- paste0(elk_locs$YEAR_,".", elk_locs$MONTH_,".", elk_locs$DAY_,".", elk_locs$HOUR_,".", elk_locs$MINUTE_,".",elk_locs$SECOND_)
date_time <- as.POSIXct(strptime(date_time,format="%Y.%m.%d.%H.%M.%S"), tz = "UTC")
elk_locs$date_time <- date_time
#30108 observations

#deleting the rows where there is not date time information
NAtimes <- is.na(elk_locs$date_time)
elk_locs <- elk_locs[NAtimes == FALSE,]


#deleting a strange point that is way too far south and west
all_elk_locs <- elk_locs[elk_locs$UTM_EAST > 10,]

#creating a subset of just the marble mountain elk herds
elk_locs <- all_elk_locs[elk_locs$UTM_EAST > 440000,]

#deleting rows where there are NAs

elk_locs <- elk_locs[is.na(elk_locs$UTM_EAST) == FALSE,]

#creating new animal ID names
elk_locs$AID <- plyr::mapvalues(elk_locs$ANIMAL_ID, from = unique(elk_locs$ANIMAL_ID), to = paste0("E",seq(1:length(unique(elk_locs$ANIMAL_ID)))))

males <- c("E1", "E5", "E9")

#creating the spatial points dataframe of the coordinates

xy.elk <- elk_locs[c("UTM_EAST", "UTM_NORTH")]
xy.sp.elk <- SpatialPoints(na.omit(xy.elk), proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))
xy.df.elk <- data.frame(xy.sp.elk)

#creating a dataframe with the elk IDs and the dates
move.df.elk <- data.frame(ID = elk_locs$AID, Burst = elk_locs$date_time)
move.df.elk$ID <- factor(move.df.elk$ID, levels = unique(move.df.elk$ID))
#adding coordinates to the move dataframe
coordinates(move.df.elk) <- xy.df.elk

#creating a trajectory object with the above move dataframe
ltraj.elk <- as.ltraj(coordinates(move.df.elk), move.df.elk$Burst, id = move.df.elk$ID)


#converting the trajectory object into dataframe mode
ltraj.elk.df <- ld(ltraj.elk)

#exploring the time lags between points
ggplot(data = ltraj.elk.df) +
  geom_histogram(mapping = aes(x = dt/3600), binwidth = 0.25) +
  coord_cartesian(ylim = c(0, 10000), xlim = c(0,15)) 

#removing relocations where the time lag from the previous point is greater than 50 hours 
ltraj.elk.df <- ltraj.elk.df[ltraj.elk.df$dt/3600 < 50,]

#removing extraneous NAs
ltraj.elk.df <- ltraj.elk.df[!is.na(ltraj.elk.df),]

#converting cleaned traj dataframe back to a trajectory object
ltraj.elk <- dl(ltraj.elk.df)

id_key <- data.frame(num = 1:14, id = summary(ltraj.elk)[,1])






#rogue code

elknums <- c(1,2,4,5,7,9,11,12,13)

Animals_overview$elknums <- elknums






























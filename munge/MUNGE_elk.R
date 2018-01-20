#MUNGE
#library(plyr)

#formatting the date and time
date_time <- paste0(elk_locs$YEAR_,".", elk_locs$MONTH_,".", elk_locs$DAY_,".", elk_locs$HOUR_,".", elk_locs$MINUTE_,".",elk_locs$SECOND_)
date_time <- as.POSIXct(strptime(date_time,format="%Y.%m.%d.%H.%M.%S"))
elk_locs$date_time <- date_time


#deleting the rows where there is not date time information
NAtimes <- is.na(elk_locs$date_time)
elk_locs <- elk_locs[NAtimes == FALSE,]


#choosing just the marble mountain herds
elk_locs <- elk_locs[elk_locs$UTM_EAST > 1E05,]
elk_locs <- elk_locs[elk_locs$UTM_EAST > 440000 & elk_locs$UTM_EAST < 480000,]


#adding the time difference between each relocation
timediff <- diff(elk_locs$date_time)
summary(duplicated(elk_locs$date_time))


elk_timediff.x <- 300
elk_timediff <- diff(elk_locs$date_time)
elk_timediff <- as.numeric(append(elk_timediff.x, elk_timediff))
elk_locs$timediff <- elk_timediff

#plot(elk_locs$UTM_EAST,elk_locs$UTM_NORTH, col = elk_locs$ANIMAL_ID)


#creating new animal ID names
elk_locs$AID <- revalue(elk_locs$ANIMAL_ID, c("E031054F_12"="E1", "E160128F_12" = "E2", "E160888M_10" = "E3", "E22080F_10" = "E4", "E31053F_13" = "E5", "E31055F_13" = "E6"))


#creating the spatial points dataframe and movement trajectories

xy.elk <- elk_locs[c("UTM_EAST", "UTM_NORTH")]
xy.sp.elk <- SpatialPoints(xy.elk)
xy.df.elk <- data.frame(xy.sp.elk)

move.df.elk <- data.frame(ID = factor(elk_locs$AID, levels = unique(elk_locs$AID)), TimeDiff = elk_locs$timediff, Burst = elk_locs$date_time)


coordinates(move.df.elk) <- xy.df.elk

proj4string(move.df.elk) <- CRS("+proj=utm +zone=10 +datum=WGS84")

ltraj.elk <- as.ltraj(coordinates(move.df.elk), move.df.elk$Burst, id=move.df.elk$ID)


plot(ltraj.elk[5])



#looking for time lags between points and distances between points. Goal is to not include ones where time lag and distance is too large... for some reason. 
hist(ltraj.elk[1], "dt", freq = TRUE)
hist(ltraj.elk[1], "dist", freq = TRUE)

hist(ltraj[2], "dt", freq = TRUE)
hist(ltraj[2], "dist", freq = TRUE)



#creating brownian bridge
E4 <- subset(elk_locs, elk_locs$AID == "E4")

library(adehabitatHR)

liker(tr = ltraj.elk[5],sig2 = 25, rangesig1 = c(1,10))

E4_kernel <- kernelbb(ltr = ltraj.elk[5],sig1 = 2.2162,sig2 = 25)

image(E4_kernel)
plot(getverticeshr(E4_kernel, 95), add=TRUE, lwd=2)

MCHu.rast(E4_kernel)

class(E4_kernel)

summary(E4_kernel)

raster(E4_kernel)


#ian wants brownian method
E4.BBMM <- brownian.bridge(x=E4$UTM_EAST, y=E4$UTM_NORTH, time.lag=E4$timediff, location.error=34, cell.size=100)
bbmm.summary(E4.BBMM)

contours <- bbmm.contour(E4.BBMM, levels=c(30,60,99), locations=E4, plot=TRUE)


#mapping that onto a base map
#changing coords of elk points
move.df.elk.latlon <- spTransform(move.df.elk, CRSobj = CRS("+proj=longlat +datum=WGS84"))

ti_creek_patterson <- bbox(move.df.elk)

base_study_area <- ggmap(get_map(location = ti_creek_patterson))

base_study_area + geom_point(data = as.data.frame(coordinates(move.df.elk.latlon)), aes(UTM_EAST,UTM_NORTH))
base



#creating a dataframe of the ltraj data

elk_locs_df <- ld(ltraj.elk)

summary(elk_locs$ANIMAL_ID)


#cleaning time differences that shouldn't be there

E4_c <- E4[E4$timediff >= 100 & E4$timediff <= 200,]

E4_unusual <- E4[E4$timediff > 130,]
E4_unusual$timediff/120

ggplot(data = E4_unusual) +
  geom_histogram(aes(x = timediff)) +
  #coord_cartesian(ylim = c(0,10000), xlim = c(0,200))
  

#Creating histogram of the E4 time intervals

hist(E4$timediff)

class(E4)
str(E4)














#below is scratch

#bringing elk occurance data in from gbif, vertnet, and ecoengine
elk <- occ(query = 'Cervus elaphus', from=c('gbif','vertnet','ecoengine'),has_coords = TRUE)
#converting output to a dataframe
df<-occ2df(elk)
#exporting as a csv
write.csv(df,"elk_occurances_spocc.csv")

#playing around with adehabitatHS

#class of object is list with 1) SpatialPixelsDataFrame (i.e. a bunch of rasters and) 2) the locations


mimage(bauges$map)

bauges$locs

#mapping points on map, not clear how to decide which layer gets mapped
image(bauges$map)
points(bauges$locs) #this maps the points


#bighorn example dataset


#running a compositional analysis of habitat use on the bighorn data

data("bighorn")

class(bighorn$used)
class(bighorn$availTrue)

#cleaning the data to input in the compana function

A1 <- c()
A2 <- c()
A3 <- c()
A4 <- c()
A5 <- c()
A6 <- c()
for(i in 1:length(bighorn$used[1,])){
  A1[i] <- as.numeric(bighorn$used[1,][i])/as.numeric(sum(bighorn$used[1,]))
  A2[i] <- as.numeric(bighorn$used[2,][i])/as.numeric(sum(bighorn$used[2,]))
  A3[i] <- as.numeric(bighorn$used[3,][i])/as.numeric(sum(bighorn$used[3,]))
  A4[i] <- as.numeric(bighorn$used[4,][i])/as.numeric(sum(bighorn$used[4,]))
  A5[i] <- as.numeric(bighorn$used[5,][i])/as.numeric(sum(bighorn$used[5,]))
  A6[i] <- as.numeric(bighorn$used[6,][i])/as.numeric(sum(bighorn$used[6,]))
}

data.frame(A1 = A1, A2 = A2, A3 = A3, A4 = A4, A5 = A5, A6 = A6)


b_used <- rbind(A1, A2, A3, A4, A5, A6)
b_avail <- rbind(bighorn$availTrue, bighorn$availTrue, bighorn$availTrue, bighorn$availTrue, bighorn$availTrue, bighorn$availTrue)

bighorn_compana <- compana(used = bighorn$used[,6:10], avail = b_avail[,6:10], test = "randomisation", rnv = 0.01, nrep = 100, alpha = 0.1)


class(b_avail[,6:10])

#viewing output

bighorn_compana$test #the lambda value and p value of statisical significance
bighorn_compana$profile #the 


e_output <- eisera(used = bighorn$used[,7:10], available = b_avail[,7:10], scannf = TRUE)

e_output$wij


###messing with creating trajectories with the bighorn data

data("puechabonsp")
locs <- puechabonsp$relocs
locs <- as.data.frame(locs)
locs

plot(puechabonsp$map)
head(locs)


da <- as.character(locs$Date)


da <- as.POSIXct(strptime(as.character(locs$Date),"%y%m%d"))
head(da)

puech <- as.ltraj(xy = locs[,c("X","Y")], date = da, id = locs$Name)

str(puech)

head(puech[[1]])
plot(puech)


#ld is a function to convert ltraj to a df





#to assign a coordinate system

#To assign a known CRS to spatial data:
  
  proj4string(x)  <-CRS("+init=epsg:28992")
#or, type in the PROJ.4 attributes:
  proj4string(x) <-CRS("+proj=utm+zone=10 +datum=WGS84")
  
  
  
  
  
  
  
  
  #HOME RANGE ANALYSIS for elk 
  #creating a 95 percent kernel density home range for E13 (memeber of patterson herd monitored in 2012) using a brownian bridge method
  
  
  
  
  
  
  E13 <- subset(elk_locs, elk_locs$AID == "E13")
  #determining the "sig1" parameter for the brownian bridge
  liker(tr = ltraj.elk[5],sig2 = 25, rangesig1 = c(1,10))
  #creating a kernal density home range
  E13_kernel <- kernelbb(ltr = ltraj.elk[5],sig1 = 2.2162,sig2 = 25)
  #plotting the home range
  image(E13_kernel)
  plot(getverticeshr(E13_kernel, 95), add=TRUE, lwd=2)
  E13_hr <- getverticeshr(E13_kernel, 95)
  proj4string(E13_hr) <- CRS("+proj=utm +zone=10 +datum=WGS84")
  E13_hr <- spTransform(E13_hr, CRSobj = CRS("+proj=longlat +datum=WGS84"))
  writeOGR(E13_hr, dsn = "E13_home_range1.kml", driver = "KML", layer = "E13")
  writeOGR(E13_hr, dsn = "E13_home_range", driver = "ESRI Shapefile", layer = "E13")
  
  
  #creating a function that creates a 95% percent home range shapefile using a brownian bridge
  
  
  class(E13_kernel)
  
  
  #pick back up here. Need to do the above with a brownian bridge method.Open the adehabitatHS tutorial for R. Get the brownian 95% kernel density.
  
  
  #reprojecting the 95 % kernel density home range to WGS lat long
  
  E13_hr <- getverticeshr(E13_kernel, 95)
  CRS(E13_hr) <- proj4string(E13_"+proj=utm +zone=10 +datum=WGS84")
  E13_hr <- spTransform(E13_hr, CRSobj = CRS("+proj=longlat +datum=WGS84"))
  
  
  summary(E13_hr)
  
  #writing a kml file of the home range
  writeOGR(getverticeshr(E13_kernel, 95), driver = "KML",dsn = "E13_home_range.kml", layer = "E13_home_range")
  
  
  
  MCHu.rast(E13_kernel)
  
  class(E4_kernel)
  
  summary(E4_kernel)
  
  plot(raster(E13_kernel))
  
  
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
  
  
  #scratch below here
  
  
  
  panther <- onefire(data = Bsev_WGS1984_SA1, fireName = fireNames[12])
  kingtitus <- onefire(data = Bsev_WGS1984_SA1, fireName = fireNames[6])
  elkcomplex <- onefire(data = Bsev_WGS1984_SA1, fireName = fireNames[51])
  
  
  stack(panther, kingtitus)
  
  
  plot(elkcomplex)
  
  plot(Bsev_WGS1984_SA1[Bsev_WGS1984_SA1@data$VB_ID == fireNames[2],])
  
  plot(siskiyoublue)
  
  
  plot(panther)
  
  
  tail(na.omit(getValues(panther)))
  
  test_points_x <- seq(from = 123.3, to = 123.4, by = 0.01) * -1
  test_point_y <-  seq(from = 41.5, to = 41.6, by = 0.01)
  
  
  test_points <- data.frame(append(test_points_x,-123.4), append(test_point_y,41.7))
  names(test_points) <- c("x", "y")
  coordinates(test_points) <- ~x+y
  proj4string(test_points) <- WGS1984
  
  
  
  panther_vals <- getValues(panther, row = 500)
  kingtitus_vals <- getValues(kingtitus, row = 500)
  
  fire_stack_test <- stack(panther_reclass, kingtitus, elkcomplex)
  fire_stack_test_original <- stack(panther, kingtitus, elkcomplex)
  
  
  fire_merge_test <- merge(panther_reclass, kingtitus, elkcomplex)
  fire_merge_test_originl <- merge(panther, kingtitus, elkcomplex)
  
  extract(x = fire_stack, y = test_points)
  extract(x = fire_stack_test, y = test_points)
  extract(x = fire_merge_test_originl, y = test_points)
  extract(x = fire_merge_test, y = test_points)
  
  plot(kingtitus)
  
  
  #reclass matrix
  reclass <- matrix(data = c(4,NA), ncol = 2, byrow = TRUE)
  
  panther_reclass <- reclassify(panther, rcl = reclass)
  
  
  
  sort(unique(fireNames))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #scratch on playing with T-LoCoh
  
  
  
  data(toni)
  class(toni$timestamp.utc)
  whead(toni)
  str(toni)
  plot(toni[,c("long","lat")], pch = 20)
  
  
  
  
  toni.sp.latlong <- SpatialPoints(toni[ , c("long","lat")], proj4string=CRS("+proj=longlat +ellps=WGS84"))
  toni.sp.utm <- spTransform(toni.sp.latlong, CRS("+proj=utm +south +zone=36 +ellps=WGS84"))
  
  toni.mat.utm <- coordinates(toni.sp.utm)
  
  
  colnames(toni.mat.utm) <- c("x","y")
  head(toni.mat.utm)
  
  
  toni.gmt <- as.POSIXct(toni$timestamp.utc, tz="UTC")
  
  local.tz <- "Africa/Johannesburg"
  toni.localtime <- as.POSIXct(format(toni.gmt, tz=local.tz), tz=local.tz)
  
  
  
  
utc <- read.csv("dtc.csv")

utc_pos <- as.POSIXct(strptime(utc$dt,format="%Y-%m-%dT%H:%M:%SZ"), tz = "UTC")
pst_pos <- as.POSIXct(format(utc_pos, tz = "America/Los_Angeles"), tz = "America/Los_Angeles")

utc$dt <- pst_pos

write.csv(utc, "dtc_pst.csv")


date_time <- paste0(elk_locs$YEAR_,".", elk_locs$MONTH_,".", elk_locs$DAY_,".", elk_locs$HOUR_,".", elk_locs$MINUTE_,".",elk_locs$SECOND_)



utc <- read.csv("t2.csv")

utc_pos <- as.POSIXct(strptime(utc$dt,format="%Y-%m-%dT%H:%M:%SZ"), tz = "UTC")
pst_pos <- as.POSIXct(format(utc_pos, tz = "America/Los_Angeles"), tz = "America/Los_Angeles")

utc$dt <- pst_pos

write.csv(utc, "t2_pst.csv")




elk_locs$date_time <- date_time






#creating locs for map of areas where we have collar data now

str(as.data.frame(elk_locs$UTM_EAST, elk_locs$UTM_NORTH), na.omit)


utm_locs <- SpatialPoints(all_elk_locs[,c("UTM_EAST","UTM_NORTH")],proj4string =CRS("+proj=utm +zone=10 +datum=WGS84"))

latlong_locs <- spTransform(utm_locs, CRSobj = WGS1984)


buffer.f <- function(foo, buffer, reps){
  # Make list of suitable vectors
  suitable <- list()
  for(k in 1:reps){
    # Make the output vector
    outvec <- as.numeric(c())
    # Make the vector of dropped (buffered out) points
    dropvec <- c()
    for(i in 1:nrow(foo)){
      # Stop running when all points exhausted
      if(length(dropvec)<nrow(foo)){
        # Set the rows to sample from
        if(i>1){
          rowsleft <- (1:nrow(foo))[-c(dropvec)]
        } else {
          rowsleft <- 1:nrow(foo)
        }
        # Randomly select point
        outpoint <- as.numeric(sample(as.character(rowsleft),1))
        outvec[i] <- outpoint
        # Remove points within buffer
        outcoord <- foo[outpoint,c("x","y")]
        dropvec <- c(dropvec, which(sqrt((foo$x-outcoord$x)^2 + (foo$y-outcoord$y)^2)<buffer))
        # Remove unnecessary duplicates in the buffered points
        dropvec <- dropvec[!duplicated(dropvec)]
      } 
    } 
    # Populate the suitable points list
    suitable[[k]] <- outvec
  }
  # Go through the iterations and pick a list with the most data
  best <- unlist(suitable[which.max(lapply(suitable,length))])
  foo[best,]
}

utm_df <- as.data.frame(utm_locs) 


names(utm_df) <- c("x","y")

testout <- buffer.f(utm_df, 500, 10)

testout_utm <- SpatialPoints(testout, proj4string =CRS("+proj=utm +zone=10 +datum=WGS84"))

testout_latlong <- spTransform(testout_utm, WGS1984)
write.csv(testout_latlong, "elk_herds.csv")



write.csv(testout_latlong, "testoutput.csv")



testing code for ergonomics evaluation. (thismotion) . 

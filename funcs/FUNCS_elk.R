#funcs

#function to make a home range from a trajectory object
HRmaker <- function(trajdata = ltraj.elk, elknum = 1, pos_error = 25, probC = 95, writeShapefile = FALSE, WGS1984 = FALSE){
  AnimalID <- as.character(summary(ltraj.elk[elknum])[1][1,1])
  sig1.x <- liker(tr = ltraj.elk[elknum],sig2 = pos_error, rangesig1 = c(1,10), plotit = F)[[1]][[1]]
  #creating a kernal density home range
  kernel <- kernelbb(ltr = ltraj.elk[elknum],sig1 = sig1.x,sig2 = pos_error)
  hr <- getverticeshr(kernel, probC)
  proj4string(hr) <- CRS("+proj=utm +zone=10 +datum=WGS84")
  if(WGS1984 == TRUE){
    hr <- spTransform(hr, CRSobj = CRS("+proj=longlat +datum=WGS84"))}
  filename <- paste0(AnimalID,"_HR",probC)
  if(writeShapefile == TRUE){writeOGR(hr, dsn = filename, driver = "ESRI Shapefile", layer = AnimalID, overwrite_layer = TRUE)}
  print(AnimalID)
  if(writeShapefile == FALSE){
    return(hr)
    }
}



HR_area <- function(trajdata = ltraj.elk, elknum = 1, pos_error = 25, probC){
  AnimalID <- as.character(summary(ltraj.elk[elknum])[1][1,1])
  sig1.x <- liker(tr = ltraj.elk[elknum],sig2 = pos_error, rangesig1 = c(1,10))[[1]][[1]]
  #creating a kernal density home range
  kernel <- kernelbb(ltr = ltraj.elk[elknum],sig1 = sig1.x,sig2 = pos_error)
  hr <- getverticeshr(kernel, probC)
  proj4string(hr) <- CRS("+proj=utm +zone=10 +datum=WGS84")
  hr <- spTransform(hr, CRSobj = CRS("+proj=longlat +datum=WGS84"))
  area.df <- data.frame(id = AnimalID, hr_size = hr$area)
  return(area.df)
}



#function to create a ggmap of a home range.  This still isn't quite working properly.

HRmap_maker <- function(maptype = "terrain", elkID = "E12"){
  elknumber <- id_key[id_key$id == elkID,]$num
  hrx <- HRmaker(elknum = elknumber, WGS1984 = T)
  extent_map <- extent(hrx)
  
  elk_locs_df %>% filter(id == "E3")
  locs <- SpatialPoints(subset(elk_locs_df, id = "E10")[,c("x","y")], proj4string = UTM10)
  #locs <- 
  
 map <-  ggmap(ggmap = get_map(location = c(extent_map[1], extent_map[3], extent_map[2], extent_map[4]), maptype = "terrain", crop = extent_map)) +
    geom_polygon(data = broom::tidy(hrx),
                 aes(long, lat, group = group),
                 fill = "grey", colour = "black", alpha = 0.7, size = 0.8)
 return(map)
 
}





#visualizing spatial separation of summer and winter range
seasonal_diffs <- function(elkId = "E13", summer = c("07-01", "08-15"), winter = c("02-01", "03-31")){
  
  year <- substr(min(subset(ltraj.elk.df, id == elkId)$date), 0,4)
  
  elknumber = id_key[id_key$id == elkId,]$num
  extent_map <- extent(HRmaker(elknum = elknumber, WGS1984 = T))
  
  srtm_big <- getData('SRTM', lon = extent_map[1], lat = extent_map[3])
  srtm_mask <- crop(srtm_big, extent_map)
  
  plot(srtm_mask)
  plot(HRmaker(elknum = elknumber, WGS1984 = T), add = T)
  #plot(HRmaker(elknum = 5, WGS1984 = T))
  
  summer_points <- ltraj.elk.df %>% filter(id == elkId) %>%
    filter(date >= paste0(year,"-",summer[1]) & date <= paste0(year,"-",summer[2]))
  
  summer_point_utm <- SpatialPoints(summer_points[,c("x", "y")], proj4string = UTM10)
  summer_points_wgs <- spTransform(summer_point_utm, CRSobj = WGS1984)
  
  winter_points <- ltraj.elk.df %>% filter(id == elkId) %>% 
    filter(date >= paste0(year,"-",winter[1]) & date <= paste0(year,"-",winter[2]))
  winter_point_utm <- SpatialPoints(na.omit(winter_points[c("x", "y")]), proj4string = UTM10)
  winter_points_wgs <- spTransform(winter_point_utm, CRSobj = WGS1984)
  
  
  plot(summer_points_wgs, add = T, pch = 1, col = "red")
  plot(winter_points_wgs, add = T, pch = 5, col = "blue")
}


#seasonal differences












#function for creating a polygon of an area that you can use as an extent object. requires matrix of coordiantes, a name of the object, and a projection CRS object.
coords2poly <- function(coords, name, projection){
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = name)), proj4string = projection)
  return(Ps1)
}

#function to create a raster of each fire. Requires a spatial polygons data fram of the fire data, the name of each fire you are interested in plotting, and the field name that you want to extract to a raster.
onefire <- function(data, fireName, field.x = "BURNSEV"){
  
  vector_form <- data[data@data$VB_ID == fireName,]
  
  b_ext <- extent(bbox(obj = data))
  npix <- ((b_ext[4]-b_ext[3]) * 111.32 * 1000) / 30
  
  #creating a raster of burn severity for the Panther fire
  raster_shell <- raster(ncol=npix, nrow=npix, ext = b_ext, crs = CRS(proj4string(vector_form)))
  res(raster_shell) <- c(0.000269409, 0.000269409)
  raster_form  <- rasterize(x = vector_form, y = raster_shell, field = field.x, small = TRUE)
  print(paste0("finished ",fireName))
  return(raster_form)
}



lxy_makr <- function(ID = "E12", burn_data = burn_sev_last_burn_2011){
  E12_tl <- subset(elk_locs, AID == ID) %>%
    select(OBJECTID, AID, UTM_EAST, UTM_NORTH, date_time, TEMP_, ACTIVITY, ONTIME)
  
  E12_coords <- SpatialPoints(E12_tl[ , c("UTM_EAST","UTM_NORTH")], proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))
  
  E12.mat <- coordinates(E12_coords)
  
  E12_coords.latlong <- spTransform(E12_coords, CRSobj = CRS("+init=epsg:4326"))
  E12.mat.latlong <- coordinates(E12_coords.latlong)
  
  E12_burn_sev_vals <- extract(x = burn_sev_last_burn_2011, E12_coords.latlong) 
  
  anc_vars <- as.data.frame(cbind(E12_burn_sev_vals, as.data.frame(E12_tl[, c("TEMP_", "ACTIVITY", "ONTIME")])))
  
  names(anc_vars) <- c("brn_sev", "temp", "act", "ontime")
  
  colnames(E12.mat.latlong) <- c("x","y")
  
  tzfile <- file.path(R.home("share"), "zoneinfo", "zone.tab")
  tzones <- read.delim(tzfile, row.names = NULL, header = FALSE, col.names = c("country", "coords", "name", "comments"), as.is = TRUE, fill = TRUE, comment.char = "#")
  sort(tzones$name)
  
  tzone <- "America/Los_Angeles"
  
  date_time.PST <- as.POSIXct(format(E12_tl$date_time, tz= tzone))
  
  E12.lxy.f <- xyt.lxy(xy = E12.mat, dt = date_time.PST, id = ID, proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"), ptid = E12_tl$OBJECTID, anv = anc_vars)
  
  return(E12.lxy.f)
}




#basic habitat selection function
library(tidyverse)
library(magrittr)


habitat_selectR <- function(elkId = "E2", habitat_data = canopy_edge, n.x = 500){
  
  elknumber = id_key[id_key$id == elkId,]$num
  
  hr <- HRmaker(elknum = elknumber, writeShapefile = FALSE, WGS1984 = FALSE)
  r_avail <- spsample(x = hr, n = 500, type = "random")
  
  all_points <- ltraj.elk.df %>% filter(id == elkId) %>% select(x,y)
  all_points <- SpatialPoints(all_points, proj4string = UTM10)
  r_selected <- as.data.frame(all_points)[sample(x = 1:nrow(as.data.frame(all_points)), size = n.x, replace = F),]
  
  avail.x <- na.omit(raster::extract(habitat_data, r_avail))
  selected <- na.omit(raster::extract(habitat_data, r_selected))
  return(prop_test <- prop.test(c(sum(selected),sum(avail.x)), c(length(selected), length(avail.x))))
}



habitat_selectR_2 <- function(elkId = "E12", habitat_data = cover_habitats, n.x = 500){
  
  elknumber = id_key[id_key$id == elkId,]$num
  
  hr <- HRmaker(elknum = elknumber, writeShapefile = FALSE, WGS1984 = FALSE)
  r_avail <- spsample(x = hr, n = 500, type = "random")
  
  all_points <- ltraj.elk.df %>% filter(id == elkId) %>% select(x,y)
  all_points <- SpatialPoints(all_points, proj4string = UTM10)
  r_selected <- as.data.frame(all_points)[sample(x = 1:nrow(as.data.frame(all_points)), size = n.x, replace = F),]
  
  avail.x <- as.data.frame(na.omit(raster::extract(habitat_data, r_avail)))
  selected <- as.data.frame(na.omit(raster::extract(habitat_data, r_selected)))
  
  use_avail_data <- rbind(avail.x, selected)
  use_avail_data$use_vs_avail <- c(rep(0, length(avail.x$inside_edge)), rep(1, length(selected$inside_edge)))
  
  return(prop_test <- prop.test(c(sum(selected),sum(avail.x)), c(length(selected), length(avail.x))))
}





















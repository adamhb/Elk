habitat_selection <- function(elkId = "E13", summer = c("07-01", "08-15"), winter = c("02-01", "03-31"), habitat_data = canopy_edge, WGS1984ON = F, summeronly = F){
  
  elknumber = id_key[id_key$id == elkId,]$num
  year <- substr(min(subset(ltraj.elk.df, id == elkId)$date), 0,4)
  
  summer_points <- ltraj.elk.df %>% filter(id == elkId) %>%
    filter(date >= paste0(year,"-",summer[1]) & date <= paste0(year,"-",summer[2]))
  
  summer_point_utm <- SpatialPoints(summer_points[,c("x", "y")], proj4string = UTM10)
  summer_points_wgs <- spTransform(summer_point_utm, CRSobj = WGS1984)
  
  winter_points <- ltraj.elk.df %>% filter(id == elkId) %>% 
    filter(date >= paste0(year,"-",winter[1]) & date <= paste0(year,"-",winter[2]))
  winter_point_utm <- SpatialPoints(na.omit(winter_points[c("x", "y")]), proj4string = UTM10)
  winter_points_wgs <- spTransform(winter_point_utm, CRSobj = WGS1984)
  
  extent_map <- extent(HRmaker(elknum = elknumber, WGS1984 = WGS1984ON))
  
  extent.x <- crop(habitat_data, extent_map)
  
  hr <- HRmaker(elknum = elknumber, WGS1984 = WGS1984ON)
  
  #random sample of available points
  
  if(WGS1984ON == T){
    summer_bounds <- summer_points_wgs
  }else{summer_bounds <- summer_point_utm}
  
  
  
  if(summeronly == T){
    r_avail <- spsample(x = hr, n = n.x, type = "random", bb = summer_bounds)
  }else{r_avail <- spsample(x = hr, n = n.x, type = "random")}
  
  
  all_points <- ltraj.elk.df %>% filter(id == elkId) %>% select(x,y)
  all_points <- SpatialPoints(all_points, proj4string = UTM10)
  
  if(WGS1984ON == T){all_points <- spTransform(all_points, CRSobj = WGS1984)}
  
  r_selected <- as.data.frame(all_points)[sample(x = 1:nrow(as.data.frame(all_points)), size = n.x, replace = F),]
  
  
  par(mfrow = c(1,2))
  plot(extent.x, box = FALSE, axes = FALSE)
  plot(hr, add = T)
  points(r_avail)
  
  plot(extent.x, box = FALSE, axes = FALSE)
  plot(hr, add = T)
  points(r_selected)
  
  
  edge_avail <- raster::extract(habitat_data, r_avail)
  edge_selected <- raster::extract(habitat_data, r_selected)
  edge_used_v_avail <- data.frame(used = edge_selected, avail = edge_avail)
  
  return(prop.test(c(sum(edge_avail),sum(edge_selected)), c(length(edge_avail), length(edge_selected))))
  
}


habitat_selection()


#choosing a random sample of selected points

if(summeronly = T){
  r_selected <- E12_table[sample(1:length(E12_table$x), size = n, replace=F),c("x", "y")]
}


#plot(HRmaker(elknum = 5, WGS1984 = T))




plot(summer_points_wgs, add = T, pch = 1, col = "red")
plot(winter_points_wgs, add = T, pch = 5, col = "blue")
}

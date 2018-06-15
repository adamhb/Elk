#seasonal diffs

seasonal_diff2 <- function(elkId = "E6", summer = c("07-01", "08-15"), winter = c("02-01", "03-31")){

elknumber <- id_key[id_key$id == elkId,]$num  
  
year <- substr(min(subset(ltraj.elk.df, id == elkId)$date), 0,4)


HRmap <- HRmaker(elknum = elknumber, WGS1984 = T)

multi <- c(1.0001,.9999, .9999, 1.0001)

extent_map <- extent(HRmap) * multi

srtm_big <- raster::getData('SRTM', lon = extent_map[1], lat = extent_map[3])

srtm_mask <- crop(srtm_big, extent_map)



#getting summer and winter points
summer_points <- ltraj.elk.df %>% filter(id == elkId) %>%
  filter(date >= paste0(year,"-",summer[1]) & date <= paste0(year,"-",summer[2]))

summer_point_utm <- SpatialPoints(summer_points[,c("x", "y")], proj4string = UTM10)
summer_points_wgs <- spTransform(summer_point_utm, CRSobj = WGS1984)

winter_points <- ltraj.elk.df %>% filter(id == elkId) %>% 
  filter(date >= paste0(year,"-",winter[1]) & date <= paste0(year,"-",winter[2]))
winter_point_utm <- SpatialPoints(na.omit(winter_points[c("x", "y")]), proj4string = UTM10)
winter_points_wgs <- spTransform(winter_point_utm, CRSobj = WGS1984)


N.x <- min(append(length(winter_points_wgs), length(summer_points_wgs)))



#finding the distance between the center of summer and winter points
smx <- mean(as.data.frame(summer_point_utm)$x)
smy <- mean(as.data.frame(summer_point_utm)$y)

wmx <- mean(as.data.frame(winter_point_utm)$x)
wmy <- mean(as.data.frame(winter_point_utm)$y)

summer_centroid <- SpatialPoints(matrix(data = c(smx,smy), nrow = 1), proj4string = UTM10)
summer_centroid <- spTransform(x = summer_centroid, CRSobj = WGS1984)


winter_centroid <- SpatialPoints(matrix(data = c(wmx,wmy), nrow = 1), proj4string = UTM10)
winter_centroid <- spTransform(x = winter_centroid, CRSobj = WGS1984)



migration_distance <- sqrt((wmx - smx)^2 + (wmy - smy)^2)


#finding the mean elevation of summer and winter points

elevations <- melt(data.frame(summer = sample(x = raster::extract(srtm_mask, y = summer_points_wgs), size = N.x, replace = FALSE), winter = sample(x = raster::extract(srtm_mask, y = winter_points_wgs), size = N.x, replace = FALSE)))

elevations <- na.omit(elevations)

elevations_output <- c(mean(subset(elevations, variable == "summer")$value), mean(subset(elevations, variable == "winter")$value), mean(subset(elevations, variable == "summer")$value) - mean(subset(elevations, variable == "winter")$value))


barplot_elevation <- ggplot(data = elevations) +
  geom_boxplot(mapping = aes(x = variable, y = value))


#the home range map
seasonal_level_plot <- levelplot(srtm_mask, 
                                 main = list(elkId, cex = 2), 
                                 colorkey = list(title = "Elevation (m)", vjust = 1,
                                                 title.gpar = list(cex = 0.9)), 
                                 labels = list(cex = 1, height = 1)) +
  layer(sp.points(HRmap, lwd = 2, col = "grey"))+
  layer(sp.points(summer_points_wgs, pch = 1, col = "chartreuse4"))+
  layer(sp.points(winter_points_wgs, col = "cadetblue1"))+
  #layer(sp.points(winter_centroid, col = "yellow", pch = 10, size = 12))+
  layer(sp.points(summer_centroid, lwd = 2, cex = 2, pch = 8, col = "yellow"))+
  layer(sp.points(winter_centroid, lwd = 2, cex = 2, pch = 8, col = "blue3"))



output <- list(seasonal_level_plot, elevations_output, migration_distance, barplot_elevation, N.x)

names(output) <- c("plot", "elevation", "migration", "elevation bar plot", "N")

return(output)
}


outputtest <- seasonal_diff2(elkId = "E13")

outputtest[[1]]


Seasonal_Diffs <- list()
for(i in Animals_overview$id){
  Seasonal_Diffs[i] <- list(seasonal_diff2(elkId = i))
}




undebug(seasonal_diff2)
seasonal_diff2(elkId = "E8")

Seasonal_Diffs[[1]]

Seasonal_Diffs_E8 <- seasonal_diff2(elkId = "E8")

Seasonal_Diffs_E8[[1]]

undebug(seasonal_diff2)


plot(winter_centroid, lwd = 2, cex = 2, pch = 8, col = "blue3")

plot(summer_centroid, lwd = 2, cex = 2, pch = 8, col = "gold1")


HRmap <- HRmaker(elknum = elknumber, WGS1984 = T)

levelplot(srtm_mask, 
          main = list("E13", cex = 2), 
          colorkey = list(title = "Elevation (m)", vjust = 1,
            title.gpar = list(cex = 0.9)), 
          labels = list(cex = 1, height = 1)) +
layer(sp.points(HRmap))+
layer(sp.points(summer_points_wgs, pch = 1, col = "chartreuse4"))+
layer(sp.points(winter_points_wgs, col = "cadetblue1"))



levelplot(r, margin = FALSE,
          colorkey = list(title = "[m]",
                          title.gpar = list(cex = 1,
                                            font = 2,
                                            col = 'red')
          ))





+
  legend(title = "Elevation")








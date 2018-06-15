WKRP_focal_areas <- readOGR(dsn = "WKRP_focal_areas.shp")

WKRP_focal_areas <- spTransform(WKRP_focal_area, CRS("+proj=longlat"))

writeOGR(WKRP_focal_areas, "WKRP_focal_areas.kml", layer="WKRP_focal_areas", driver="KML", overwrite_layer = TRUE) 


WKRP_treatments <- readOGR(dsn = "treatments.shp")

WKRP_treatments <- spTransform(WKRP_treatments, CRS("+proj=longlat"))

writeOGR(WKRP_treatments, "WKRP_treatments.kml", layer="WKRP_treatments", driver="KML", overwrite_layer = TRUE) 



#creating a map of an elk

hrx <- HRmaker(elknum = 5, WGS1984 = T)
extent_map <- extent(HRmaker(elknum = 5, WGS1984 = T))
ggmap(ggmap = get_map(location = c(extent_map[1], extent_map[3], extent_map[2], extent_map[4]), maptype = "terrain", crop = extent_map)) +
  geom_polygon(data = fortify(hrx),
               aes(long, lat, group = group),
               fill = NA, colour = "black", alpha = 0.7, size = 0.8) 




#original code for visualizing the seasonal differences in home ranges

srtm_big <- getData('SRTM', lon = -123, lat = 41)
srtm_mask <- crop(srtm_big, extent_map)

plot(srtm_mask)
plot(HRmaker(elknum = 5, WGS1984 = T), add = T)
#plot(HRmaker(elknum = 5, WGS1984 = T))

summer_points <- ltraj.elk.df %>% filter(id == "E13") %>% 
  filter(date >= "2011-07-01" & date <= "2011-8-01")

summer_point_utm <- SpatialPoints(summer_points[c("x", "y")], proj4string = UTM10)
summer_points_wgs <- spTransform(summer_point_utm, CRSobj = WGS1984)

winter_points <- ltraj.elk.df %>% filter(id == "E13") %>% 
  filter(date >= "2011-12-01" & date <= "2011-12-31")
winter_point_utm <- SpatialPoints(winter_points[c("x", "y")], proj4string = UTM10)
winter_points_wgs <- spTransform(winter_point_utm, CRSobj = WGS1984)


plot(summer_points_wgs, add = T, pch = 1, col = "red")
plot(winter_points_wgs, add = T, pch = 5, col = "blue")











#statistical analyses of the E12 hull data
E12_table <- read.csv("E12.s0.025.k24.01.csv")
edge_vals <- raster::extract(canopy_edge, E12_table[,c("x","y")])
E12_table$canopy_edge <- edge_vals



#changing burn severity values 
ggplot(data = E12_table) +
  geom_bar(mapping = aes(x = brn_sev))


ggplot(data = na.omit(E12_table), mapping = aes(x = as.factor(canopy_edge), y = mnlv.43200)) +
  geom_boxplot()


HRmap_maker(maptype = "terrain", elknumber = 4)







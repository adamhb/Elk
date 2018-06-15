#analyses

#creating brownian bridge 95 % kernel density HR
for(i in 2:14){
  HRmaker(elknum = i)
}

HRmaker(elknum = 5, trajdata = ltraj.elk, pos_error = 25, probC = 50)


#getting the list of areas for all homeranges of elk in Marbles
hr_areas <- data.frame()
include <- c(1,2,4,5,7,9,11,12,13)
for(i in include){
  temp <- HR_area(elknum = i, probC = 95, trajdata = ltraj.elk, pos_error = 25)
  hr_areas <- rbind(hr_areas, temp)
}

summary(hr_areas$hr_size)


#visualizing the spatial and elevational partitioning of seasonal ranges
par(mfrow = c(3,3))
for(elkIds in Animals_overview$id){
  seasonal_diffs(elkId = elkIds)
}


seasonal_diffs(elkId = "E12")




####TESTING IF E12 SELECTS FOR EDGE HABITAT (within 150 m of forest edge)
#development data
elknum <- 4
AnimalID <- as.character(summary(ltraj.elk[elknum])[1][1,1])
sig1.x <- liker(tr = ltraj.elk[elknum],sig2 = pos_error, rangesig1 = c(1,10), plotit = F)[[1]][[1]]
#creating a kernal density home range
kernel <- kernelbb(ltr = ltraj.elk[elknum],sig1 = sig1.x,sig2 = pos_error)
hr <- getverticeshr(kernel, probC)
proj4string(hr) <- CRS("+proj=utm +zone=10 +datum=WGS84")

plot(burn_sev_last_burn_2011)

#choosing random points within the 95 percent home range
r_avail <- spsample(x = hr, n = 500, type = "random")

#choosing a random sample of selected points
r_selected <- E12_table[sample(1:length(E12_table$x), 500, replace=F),c("x", "y")]

#edge values of available points
edge_avail <- raster::extract(canopy_edge, r_avail)
edge_selected <- raster::extract(canopy_edge, r_selected)
edge_used_v_avail <- data.frame(used = edge_selected, avail = edge_avail)

plot(hr)
#canopy edge
plot(canopy_edge)
plot(hr, add = T)
points(r_avail)
points(r_selected)

#refined function
habitat_selectR(elkId = "E10", habitat_data = canopy_edge_north_60_m_2010, n.x = 500)
habitat_selectR(elkId = "E1", habitat_data = canopy_edge_south_60m, n.x = 500)


plot(canopy_edge_north_60_m_2010)
plot(HRmaker(elknum = 5), add = T, lwd = 2)
plot(HRmaker(elknum = 2), add = T, lwd = 2)





#testing if elk select for open areas
plot(open_areas_north)

id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc != "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = open_areas_north)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}

open_areas_results_north <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)

#plot(open_areas_south)



id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc == "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = open_areas_south)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}




#undebug(habitat_selectR)
#habitat_selectR("E1", habitat_data = open_areas_south)



open_areas_results_south <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)

open_areas_results <- rbind(open_areas_results_north, open_areas_results_south)

write.csv(open_areas_results, "results_open_area_results.csv")







#NORTH VARIABLES

#testing if they select for just the inside edge
inside_edge <- (canopy_edge_north_60_m_2010 - open_areas_north) == 1
plot(inside_edge)
plot(open_areas_north)
deepforest_or_open_next_to_forest <-  (canopy_edge_north_60_m_2010 - open_areas_north) == 0


#open edge
open_edge <-  (open_areas_north + canopy_edge_north_60_m_2010) == 2
plot(open_edge)

open_not_edge <- (open_areas_north - open_edge) == 1
plot(open_not_edge)





#SOUTH VARIABLES

#testing if they select for just the inside edge
inside_edge_south <- (canopy_edge_south_60m_2005 - open_areas_south) == 1
plot(inside_edge_south)


#plot(canopy_edge_south_60m_2005)
#plot(HRmaker(elknum = 7), add = T)

#open edge
open_edge_south <-  (open_areas_south + canopy_edge_south_60m_2005) == 2
plot(open_edge_south)


#open not edge
open_not_edge_south <- (open_areas_south - open_edge_south) == 1









#creating open areas far from edge,north
#NORTH
id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc != "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = open_not_edge)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}


open_not_edge_results_north <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)



id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc == "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = open_not_edge_south)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}

open_not_edge_results_south <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)

open_note_edge_results <- rbind(open_not_edge_results_north, open_not_edge_results_south)
write.csv(open_note_edge_results, "results_open_not_edge.csv")




#testing open edges
id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc == "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = open_edge_south)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}

open_edge_results_south <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)



id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc != "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = open_edge)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}

open_edge_results_north <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)




open_edge_results <- rbind(open_edge_results_north, open_edge_results_south)
write.csv(open_edge_results, "results_open_edge.csv")
















#inside edges



id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc == "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = inside_edge_south)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}

inside_edge_results_south <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)



id <- c()
prop_used <- c()
prop_avail <- c()
p <- c()
rownum <- 1
for(i in subset(Animals_overview, loc != "Summerville")$id){
  tmp <- habitat_selectR(elkId = i, habitat_data = inside_edge)
  id[rownum] <- i
  prop_used[rownum] <- tmp$estimate[1]
  prop_avail[rownum] <- tmp$estimate[2]
  p[rownum] <- tmp$p.value
  rownum <- rownum + 1
}

inside_edge_results_north <- data.frame(idx = id, propused = prop_used, propavail = prop_avail, pval = p)




inside_edge_results <- rbind(inside_edge_results_south, inside_edge_results_north)
write.csv(inside_edge_results, "results_inside_edge.csv")

#5 of 9 selected for inside edge, least strong selection

#6 of 9 selected open edge (all southern ones selected open edges), med strong selection













#cover_habitats <- stack(inside_edge, open_edge, open_not_edge)
#names(cover_habitats) <- c("inside_edge", "open_edge", "open")



#reclass.xx <- matrix(data = c(-1.01,0.99,0,
 #                             0.999, 1.01, 1), ncol = 3, byrow = T)
#inside_edge <- reclassify(x = inside_edge, rcl = reclass.xx)



#testing if they select for 


















prop_out <- habitat_selectR(elkId = "E12", habitat_data = open_areas_north)

prop_used <- prop_out$estimate)




#creating a raster of just the canopy edge on the inside forest side, so that its separated from open ground.



undebug(habitat_selectR)




plot(HRmaker(elknum = 7), add = T)

habitat_selectR(elkNum = 8, habitat_data = canopy_edge_south)


debug(habitat_selectR)



















#graphing this


canopy_edge_north_60_m_2010_cropped <- crop(canopy_edge_north_60_m_2010, extent(HRmaker(elknum = 5)))

plot(canopy_edge_north_60_m_2010_cropped, asp = 1)


plot(canopy_edge_north_60_m_2010)
plot(HRmaker(elknum = 5), add = T, lwd = 2)
plot(HRmaker(elknum = 2), add = T, lwd = 2)








#TESTING FOR HABITAT SELECTION OF HIGH SEVERITY BURN AREAS
#northern area
#creating burn severity raster in utm

burn_sev_last_burn_2011_north_extent <- projectExtent(burn_sev_last_burn_2011, crs = UTM10)
burn_sev_last_burn_2011_north <- projectRaster(from = burn_sev_last_burn_2011, to = burn_sev_last_burn_2011_north_extent, method = "ngb")

#creating a reclassify matrix
reclass.x <- matrix(data = c(0,3.9,0,
                3.99, 4.1, 1), ncol = 3, byrow = T)

#creating a raster that shows 1s for all high burn severity patches
high_burn_sev_up2_2011_north <- reclassify(x = burn_sev_last_burn_2011_north, rcl = reclass.x)

heatScheme <- colorRampPalette(c("green", "red"), bias=1, space="rgb", interpolate="linear")

plot(high_burn_sev_up2_2011_north, col = heatScheme(100))
plot(HRmaker(elknum = 4), add = T, lwd = 2, lty = 5)
plot(HRmaker(elknum = 5), add = T, lwd = 2)
plot(HRmaker(elknum = 2), add = T, lwd = 2)


habitat_selectR(elkId = "E10", habitat_data = high_burn_sev_up2_2011_north, n.x = 2000)



#southern area
#creating burn severity raster in utm

#figuring out when the Rush and Pigeon Fires went through
as.data.frame(fire_perims) %>% select(FIRE_NAME, ALARM_DATE, CONT_DATE, Shape_Area) %>%
  filter(ALARM_DATE > "2006/01/01" & ALARM_DATE < "2007/01/01") %>%
  filter(FIRE_NAME == "PIGEON" | FIRE_NAME == "RUSH" | FIRE_NAME == "BAKE-OVEN")


#plotting the change between 2005 and end of 2006


plot(South_burn_sev_1987_thru_2005)
plot(South_burn_sev_1987_thru_2006)


#reprojecting south burn sev to UTM
burn_sev_last_burn_2006_south_extent <- projectExtent(South_burn_sev_1987_thru_2006, crs = UTM10)
burn_sev_last_burn_2006_south <- projectRaster(from = South_burn_sev_1987_thru_2006, to = burn_sev_last_burn_2011_south_extent, method = "ngb")

#creating a reclassify matrix
reclass.x <- matrix(data = c(0,3.9,0,
                             3.99, 4.1, 1), ncol = 3, byrow = T)

#creating a raster that shows 1s for all high burn severity patches
high_burn_sev_up2_2006_south <- reclassify(x = burn_sev_last_burn_2006_south, rcl = reclass.x)

plot(high_burn_sev_up2_2006_south, col = heatScheme(100))
plot(HRmaker(elknum = 1), add = T, lwd = 2, lty = 5)
plot(HRmaker(elknum = 11), add = T, lwd = 2)
plot(HRmaker(elknum = 12), add = T, lwd = 2)
plot(HRmaker(elknum = 13), add = T, lwd = 2)


#creating a 2005 layer for E2

burn_sev_last_burn_2005_south_extent <- projectExtent(South_burn_sev_1987_thru_2005, crs = UTM10)
burn_sev_last_burn_2005_south <- projectRaster(from = South_burn_sev_1987_thru_2005, to = burn_sev_last_burn_2011_south_extent, method = "ngb")

high_burn_sev_up2_2005_south <- reclassify(x = burn_sev_last_burn_2006_south, rcl = reclass.x)



#doing the habitat selection analyses

habitat_selectR(elkId = "E2", habitat_data = high_burn_sev_up2_2005_south, n.x = 1000)

plot(burn_sev_last_burn_2005_south)
plot(HRmaker(elknum = 7), add = T)

plot(high_burn_sev_up2_2005_south)




#looking at home range size by study area

Cecilville <- Animals_overview %>% filter(loc == "Summerville") %>%  summarise(hr_mean = mean(hr_size), sd = sd(hr_size), n = length(hr_size))

MMtn <- Animals_overview %>% filter(loc != "Summerville") %>%  summarise(hr_mean = mean(hr_size), sd = sd(hr_size), n = length(hr_size))

Animals_overview$study_area <- c("Cecilville", rep("MarbleMtn", 3), rep("Cecilville", 5))

t.test(formula = hr_size ~ study_area, data = Animals_overview)




undebug(habitat_selectR)



























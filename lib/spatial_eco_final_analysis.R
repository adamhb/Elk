library(ggmap)
library(maps)
library(foreign)
library(adehabitatHS)
library(magrittr)
library(tidyverse)
library(sp)
library(raster)
library(ggplot2)


#working with E000 for the full analysis

plot(E000)
base_E000 <- base + geom_point(data = as.data.frame(coordinates(E000)), aes(x,y))

#creating a separate sp df of the summer range
summer_extent <- drawExtent()
E000_summer <- crop(E000, summer_extent)

#creating a polygon around the summer range

E000_summer_chull <-  chull(as.data.frame(E000_summer)[,c("x","y")])

E000_summer_poly <- as.data.frame(E000_summer)[c(E000_summer_chull, E000_summer_chull[1]), c("x", "y")]
coordinates(E000_summer_poly) = ~x+y
crs(E000_summer_poly) <- "+proj=longlat +datum=WGS84"

summer_range <- bbox(E000_summer_poly)

base_summer_range <- ggmap(get_map(location = summer_range))

poly <- ggplot(as.data.frame(E000_summer_poly), aes(x = x, y = y)) + geom_polygon(fill = NA, colour = "black")

summer_range_poly_graph <- base_summer_range + geom_polygon(data = as.data.frame(E000_summer_poly), aes(x = x, y = y), fill = NA, colour = "black")

png("sum_range.png", height=8.5, width=11, units="in", res=300)
summer_range_poly_graph
dev.off()



#importing the raster with veg types
veg <- brick("veg_c", layers = "WHRTYPE", RAT = TRUE)

#importing the raster attribute table
rat <- read.csv("rat.csv")

#coming up with values for the WHRTYPES
WHR <- unique(rat$WHRTYPE)
new_value <- seq(1:length(WHR))
WHR_code <- data.frame(value = new_value, WHRTYPE = WHR)

#creating a reclassification matrix
reclass_df <- rat %>%
  select(COUNT, VALUE, WHRTYPE)%>%
  arrange(WHRTYPE) %>%
  mutate(min = VALUE-0.1) %>%
  mutate(max = VALUE+0.1) %>%
  select(min, max, WHRTYPE)

#adding the new codes to the reclass dataframe
reclass_df <- merge(reclass_df, WHR_code, by = "WHRTYPE")
reclass_df <- reclass_df[,-1]

#reclassifing the raster
veg_reclass <- reclassify(veg, as.matrix(reclass_df), include.lowest = TRUE)
veg <- veg_reclass

#reprojecting the raster to a geographic coordinate system
veg_ext <- projectExtent(veg, crs = CRS("+proj=longlat +datum=WGS84"))
veg <- projectRaster(from = veg, to = veg_ext, method = "ngb")

#cropping it for just the summer range
summer_veg <- crop(veg, summer_range)
plot(summer_veg)
points(E000_summer)

#determining land cover percentages from pixel counts on summer range
summer_avail <- as.data.frame(table(values(summer_veg))/sum(table(values(summer_veg))))
t(summer_avail)
names(summer_avail) <- c("value", "cover")
summer_avail <- merge(WHR_code,summer_avail)[,-1]
summer_avail <- summer_avail %>%
  arrange(cover)


#determining the number of units of each habitat type used using extract
summer_use <- raster::extract(summer_veg, E000_summer)
summer_use <- as.data.frame(table(summer_use))
names(summer_use) <- c("value", "use")
summer_use <- merge(WHR_code,summer_use)[-1]

#merging use and availability into one dataframe
use_avail <- merge(summer_avail,summer_use)

use_vals <- t(use_avail$use)
avail_vals <- t(use_avail$cover)

colnames(use_vals) <- use_avail$WHRTYPE

use_vals <- rbind(use_vals, use_vals)
use_vals <- rbind(use_vals, use_vals)
avail_vals <- rbind(avail_vals, avail_vals)
avail_vals <- rbind(avail_vals, avail_vals)

colnames(avail_vals) <- use_avail$WHRTYPE


use_vals.x <- use_vals[,c(1,3,5,7)]
avail_vals.x <- avail_vals[,c(1,3,5,7)]


use_vals.x[2,] <- c(21, 79, 176, 245)
use_vals.x[3,] <- c(23, 83, 177, 240)
use_vals.x[4,] <- c(20, 77, 173, 239)

comp_E000_summer <- compana(used = use_vals.x, avail = avail_vals.x, test = "randomisation", rnv = 0.01, nrep = 500, alpha = 0.1)

comp_E000_summer$test










#cleaning up the NA values
trim_test <- trim(veg)
sum(is.na(values(veg)))
hist(veg)




#getting a pixel count for the whole raster, by veg type. This doesn't yet take into account the NA values though.
pixel_count_1 <- rat %>%
  select(VALUE, COUNT, WHRTYPE)%>%
  arrange(COUNT) %>%
  group_by(WHRTYPE) %>%
  summarise(count = sum(COUNT)) %>%
  arrange(count)



#cropping and changing the projection
veg_ext <- projectExtent(veg, crs = CRS("+proj=longlat +datum=WGS84"))
veg_1 <- projectRaster(from = veg, to = veg_ext)


#creating an attribute table to see the different vegetation types
veg_rat <- read.dbf("f_veg_clip1.tif.vat.dbf")

#cropping the raster to to the summer polygon
#creating a box around the polygon (for now because I can't figure out how to do any operations on the polygon)

E00_summer_extent <- bbox(E000co)
veg_E00_summer <- crop(veg_1, E00_summer_extent)
veg_E00_summer <- as.factor(veg_E00_summer)

#quantifying the available habitat

values(veg_E00_summer)

#extracting the veg values for the collar points
library(raster)
E000_summer_used <- raster::extract(x = veg_E00_summer, y = E000_summer, method = 'simple')

hist(E000_summer_used)


class(veg_E00_summer)

#formatting for input into compana function
s_use <- rbind(as.character(use_avail$WHRTYPE), use_avail$use)
names(s_use) <- use_avail$WHRTYPE
s_use <- s_use[-1,]
s_use <- rbind(s_use, s_use)


s_avail <- as.data.frame(t(use_avail[,c(1,2)]))
names(s_avail) <- use_avail$WHRTYPE
s_avail <- s_avail[-1,]
s_avail <- rbind(s_avail, s_avail)

s_avail_odd <- s_avail[,c(3,5,7,9)]
s_use_odd <- s_use[,c(3,5,7,9)]
s_use_odd$KMC <- as.integer(s_use_odd$KMC)







#scratch below here
#for NAD 
CRS("+init=epsg:4269")
EPSG:4269
crs(veg) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 




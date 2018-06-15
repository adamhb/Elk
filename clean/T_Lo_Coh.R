#T-LoCoh Analysis

install.packages("tlocoh", dependencies=T, repos=c("http://R-Forge.R-project.org" , "http://cran.cnr.berkeley.edu"))


library(tlocoh)
library(sp)
library(rgdal)
library(tidyverse)
library(magrittr)


#creating a .lxy object. This allows you to plot the relocation data, see time intervals, distances traveled, and other temporal statistics about each elk. This code chunk has been converted to a function (lxy_makr).

E12_tl <- subset(elk_locs, AID == "E12") %>%
  select(OBJECTID, AID, UTM_EAST, UTM_NORTH, date_time, TEMP_, ACTIVITY, ONTIME)

E12_coords <- SpatialPoints(E12_tl[ , c("UTM_EAST","UTM_NORTH")], proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))

E12_coords.latlong <- spTransform(E12_coords, CRSobj = CRS("+init=epsg:4326"))
E12.mat.latlong <- coordinates(E12_coords.latlong)

E12_burn_sev_vals <- extract(x = burn_sev_last_burn_2011, E12_coords.latlong) 

anc_vars <- as.data.frame(cbind(E12_burn_sev_vals, as.data.frame(E12_tl[, c("TEMP_", "ACTIVITY", "ONTIME")])))

names(anc_vars) <- c("brn_sev", "temp", "act", "ontime")


colnames(E12.mat) <- c("x","y")

tzfile <- file.path(R.home("share"), "zoneinfo", "zone.tab")
tzones <- read.delim(tzfile, row.names = NULL, header = FALSE, col.names = c("country", "coords", "name", "comments"), as.is = TRUE, fill = TRUE, comment.char = "#")
sort(tzones$name)

tzone <- "America/Los_Angeles"

date_time.PST <- as.POSIXct(format(E12_tl$date_time, tz= tzone))

E12.lxy <- xyt.lxy(xy = E12.mat, dt = date_time.PST, id = "E12", proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"), ptid = E12_tl$OBJECTID, anv = anc_vars)

#saving the lxy object as a dataframe

E12.df <- cbind(E12_tl, E12_burn_sev_vals)


E12.df <- E12.df %>% arrange(date_time)


class(E12.df$date_time)

ggplot(data = E12.df[270:290,], (aes(x = date_time, y = TEMP_))) + geom_point() + 
  scale_x_datetime(date_breaks = "4 hour", labels = date_format("%H"))




plot(E12.df$date_time[1:100], E12.df$TEMP_[1:100])


adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20),
                     strip.text.x = element_text(size = 18),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = 15), # change the axis title
                     axis.title.y = element_text (size = 15),
                     axis.text.x = element_text (size = 14, colour = "black"),
                     axis.text.y = element_text (size = 14, colour = "black"),
                     legend.text = element_text (size = 15))lib
library(lubridate)
library(scales)

time_axis <-  scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%H%M%S"))
smooth_line <- geom_smooth(size = 1.8, method = "loess", span = .01, se = F)
smoother_line <- geom_smooth(size = 1.8, method = "loess", span = .1, se = F)


ggplot(data = full_output, aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('mortality rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  labs(title = 'light-dependent seedling mortality') +
  theme_classic() +
  adams_theme


#A
plot(E12.lxy)

hist(E12.lxy)

lxy.plot.freq(E12.lxy, cp=T)


#DETERMINING SPACE TIME BALANCE
#choosing s param

E12.lxy <- lxy.ptsh.add(E12.lxy)

s <- 0.025

lxy.plot.sfinder(E12.lxy)


#CREATING THE ISOPLETHS
#ID the nearest neighbors with the k method

E12.lxy <- lxy.nn.add(E12.lxy, s=0.025, k=25)




#ID the nearest neigbors with the a method
E12.lxy.amethod <- lxy.nn.add(E12.lxy, a = 10000, s = 0.025)

E12.lhs.amethod <- lxy.lhs(E12.lxy, a = 10000, s=0.025)

summary(E12.lhs.amethod)

E12.lhs.amethod <- lhs.iso.add(E12.lhs.amethod)

plot(E12.lhs.amethod, iso = T)


E12.lhs.amethod <- lhs.ellipses.add(E12.lhs.amethod)
  
E12.lhs.amethod <- lhs.iso.add(E12.lhs.amethod, sort.metric="ecc")

plot(E12.lhs.amethod, iso=T, iso.sort.metric="ecc")


#exporting new tlocoh movement map to shapefile
flhs.exp.shp(lhs = E12.lhs.amethod, id = "E12", a = 10000, s = 0.025, iso = T, dir = ".", file.base = "E12_TLocoh_Use_a_method",iso.metric = "ecc")



#E12.lhs.k24 <- lhs.iso.add(E12.lhs.k24, sort.metric="ecc")


#plotting this
plot(E12.lhs.k24, iso=T, iso.sort.metric="ecc")



#don't include this in the function

lxy.save(E12.lxy, dir=".")


#load(file.choose())


#CREATING Hull sets
E12.lhs <- lxy.lhs(E12.lxy, k=3*2:8, s=0.025)
summary(E12.lhs)
summary(E12.lhs, compact=T)


#creating isolpleths with default sorting setting as density- a proxy for intensity of use
E12.lhs <- lhs.iso.add(E12.lhs)
plot(E12.lhs, iso=T, record=T, ufipt=F)


plot(E12.lhs, iso=T, k=24, allpts=F, cex.allpts=0.1, col.allpts="gray30", ufipt=F)

#exporting the intensity of use map as a shapefile
lhs.exp.shp(lhs = E12.lhs.k24, id = "E12", k = 24, s = 0.025, iso = T, dir = ".", file.base = "E12_TLocoh_Use" ,iso.metric = "area")


lhs.exp.shp(lhs = E12.lhs.k24, id = "E12", k = 24, s = 0.025, iso = T, dir = ".", file.base = "E12_TLocoh_Use" ,iso.metric = "ecc")


#shapefile("E12_TLocoh_Use.iso.srt-ecc.iso-q.h6431.i5.00.iso.shp")

#bringing in the shapefile 


E12_useinten_map <- shapefile("E12_TLocoh_Use.iso.srt-area.iso-q.h6431.i5.00.iso.shp")

E12_movement_map <- shapefile("E12_TLocoh_Use.iso.srt-ecc.iso-q.h6431.i5.02.iso.shp")



plot(E12_useinten_map, col = "iso")


str(E12_useinten_map)


summary(E12.lhs.k24)


#checking the k value where the isopleths start to fill in enough. We need k = 24
lhs.plot.isoear(E12.lhs)

summary(E12.lhs)

#selecting k = 24 nearest neighbors
E12.lhs.k24 <- lhs.select(E12.lhs, k=24)
summary(E12.lhs.k24)

#exploring other hull metrics
#calculating the eccentricity of a bounding ellipsoid for each hull.
E12.lhs.k24 <- lhs.ellipses.add(E12.lhs.k24)
#viewing and plotting the eccentricity of a sample hull
summary(E12.lhs.k24)
plot(E12.lhs.k24, hulls=T, ellipses=T, allpts=T, nn=T, ptid="auto")


#adding revisitation metrics with an inter-visit gap of 12 hours
E12.lhs.k24 <- lhs.visit.add(E12.lhs.k24, ivg=3600*12)

lhs.save(E12.lhs.k24)


#adding hulls based on eccentricity data
E12.lhs.k24 <- lhs.iso.add(E12.lhs.k24, sort.metric="ecc")


#plotting this
plot(E12.lhs.k24, iso=T, iso.sort.metric="ecc")


#looking at revisitation rates to certain hulls
hist(E12.lhs.k24, metric="nsv")
#you can see that they are highly habitualized


#mapping where those highly revisited hulls are

plot(E12.lhs.k24, hpp=T, hpp.classify="nsv", ivg=3600*12, col.ramp="rainbow")

#aoi for E12
E12.aoi <- aoi()

#plotting within that boudning box
plot(E12.lhs.k24, hpp=T, hpp.classify="nsv", col.ramp="rainbow", aoi=E12.aoi)



#looking at the duration of each visit
hist(E12.lhs.k24, metric="mnlv", ivg=3600*12)
plot(E12.lhs.k24, hpp=T, hpp.classify="mnlv", col.ramp="rainbow")



#visitation rate versus duration of visit
hsp <- lhs.plot.scatter(E12.lhs.k24, x="nsv", y="mnlv", col="spiral", bg="black")


plot(E12.lhs.k24, hpp=T, hsp=hsp, hpp.classify="hsp")

summary(E12.lhs.k24)


#coverting hull metrics to a csv file for statistical analysis
lhs.exp.csv(lhs = E12.lhs.k24, anv = 'all')









#this creates a bunch of plots of revisitation rates and duration of visit for each hull.
lhs.plot.scatter.auto(E12.lhs.k24)



















#creating at lxy for E13
E1.lxy <- lxy_makr(ID = "E1")

hist(E1.lxy, )


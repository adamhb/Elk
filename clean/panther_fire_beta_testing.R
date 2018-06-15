###making raster data of burn severity for the 2008 panther fire  

#getting only the best assessment of burn severity for Panther Fire
Panther_2008 <- burn_sev[burn_sev@data$VB_ID == "2008PANTHER_MERRILL_HAYPRESS" & burn_sev@data$BEST_ASSES == "YES",]

#writing that vector data to shapefile
writeOGR(Panther_2008, driver='ESRI Shapefile', dsn = "Panther_2008_poly", layer = "Panther_2008_poly", overwrite_layer = TRUE)

#creating extent object to determine the extent of a raster grid
burn_ext <- extent(bbox(obj = Panther_2008))
n_pixels <- (bbox(Panther_2008)[2,2] - bbox(Panther_2008)[2,1])/30

#creating a raster of burn severity for the Panther fire
burn_rast <- raster(ncol=n_pixels, nrow=n_pixels, ext = burn_ext, crs = proj4string(Panther_2008))
res(burn_rast) <- c(30,30)
burn_rast_panther_2008 <- rasterize(x = Panther_2008, y = burn_rast, field = "BURNSEV", small = TRUE)
plot(burn_rast_panther_2008)
writeRaster(burn_rast_panther_2008, "Panther_Fire_Burn_Severity", format = "GTiff", overwrite = TRUE)

#converting the raster object so that it can be saved as a KML.
#first changing the coordinate system
kml_rast_panther <- projectExtent(object = burn_rast_panther_2008, crs = CRS("+init=epsg:4326"))
kml_rast_panther_vals <- projectRaster(from = burn_rast_panther_2008, to = kml_rast_panther, method = "ngb")
KML(kml_rast_panther_vals, "Panther_fire")
plot(kml_rast_panther_vals)

#creating a filled contour plot of the panther vals
filledContour(kml_rast_panther_vals)

#making points for extraction if you don't want the whole grid there (just points where the initial polygon had values)
keep <- !is.na(values(kml_rast_panther_vals))
all_points <- values(kml_rast_panther_vals)[keep]
point_vals <- extract(x = kml_rast_panther_vals, y = seq(1:length(values(kml_rast_panther_vals)))[keep])

#converting the whole raster to point values. This method is too big of a file.
panther_fire_points_vals <- rasterToPoints(kml_rast_panther_vals, spatial = TRUE)
writeOGR(panther_fire_points_vals,driver='ESRI Shapefile', dsn = "Panther_2008_points", layer = "Panther_2008_points", overwrite_layer = TRUE)


panther_fire_extent <- extent(panther_fire_points_vals)


reg_Samp_panther <- sampleRegular(x = kml_rast_panther_vals, size = 5000, extent = panther_fire_extent, xy = TRUE)

keep1 <- !is.na(reg_Samp_panther[,"layer"])
reg_Samp_panther_Clean <- as.data.frame(reg_Samp_panther[keep1,])
coordinates(reg_Samp_panther_Clean) <- ~x+y
proj4string(reg_Samp_panther_Clean) <- CRS("+init=epsg:4326")
writeOGR(reg_Samp_panther_Clean,driver='ESRI Shapefile', dsn = "Panther_2008_points", layer = "Panther_2008_points", overwrite_layer = TRUE)










#function to convert a raster layer to a series of regularly sampled point values
















coords = reg_Samp_panther_Clean[,c("x","y")]





length(reg_Samp_panther_Clean)





reg_Samp_panther[layer = 1]
str(reg_Samp_panther)



head(point_vals)


point_vals <- y = seq(1:length(values(kml_rast_panther_vals)))[keep]



values(kml_rast_panther_vals)[c(1939,2612,2613,3284,3285)]
#creatin
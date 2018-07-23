#creating a simple polygon and exporting as a shapefile


var study_area_south = /* color: #d63000 */ee.Geometry.Polygon(
  [[[-123.19450378417969, 41.161080048697045],
    [-123.19313049316406, 40.97004819103926],
    [-122.93701171875, 40.97108508961964],
    [-122.95005798339844, 41.161080048697045]]]);



library(sp)
library(rgdal)
coords = matrix(c(-123.19450378417969, 41.161080048697045,
                  -123.19313049316406, 40.97004819103926,
                  -122.93701171875, 40.97108508961964,
                  -122.95005798339844, 41.161080048697045,
                  -123.19450378417969, 41.161080048697045), 
                ncol = 2, byrow = TRUE)


P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(Ps1, axes = TRUE)

shapefile(Ps1, filename = "Cecilville_area")


writeOGR(obj = Ps1, dsn = "Cecilville_study_area", layer = "study_area", driver = "ESRI Shapefile")


class(Ps1)

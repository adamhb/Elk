habitat_selectR_graph <- function(elkId = "E12", habitat_data = cover_habitats, n.x = 500){
  
  elknumber = id_key[id_key$id == elkId,]$num
  
  hr <- HRmaker(elknum = elknumber, writeShapefile = FALSE, WGS1984 = TRUE)
  r_avail <- spsample(x = hr, n = 500, type = "random")
  
  all_points <- ltraj.elk.df %>% filter(id == elkId) %>% select(x,y)
  all_points <- SpatialPoints(all_points, proj4string = UTM10)
  all_points <- spTransform(all_points, CRSobj = WGS1984)
  r_selected <- as.data.frame(all_points)[sample(x = 1:nrow(as.data.frame(all_points)), size = n.x, replace = F),]
  
  #avail.x <- as.data.frame(na.omit(raster::extract(habitat_data, r_avail)))
  #selected <- as.data.frame(na.omit(raster::extract(habitat_data, r_selected)))
  
  #use_avail_data <- rbind(avail.x, selected)
  #use_avail_data$use_vs_avail <- c(rep(0, length(avail.x$inside_edge)), rep(1, length(selected$inside_edge)))
  
  return(as.data.frame(r_selected))
}



tester <- habitat_selectR_graph()


write.csv(tester, file = "E12_selected.csv")


migrate <- read.csv("results_migrations.csv")


migrate

boxplot(migrate$X..Elevation..m.)

library(ggplot2)

ggplot(data = elk_locs, aes(x = UTM_EAST, y = UTM_NORTH, color = AID)) + geom_point()



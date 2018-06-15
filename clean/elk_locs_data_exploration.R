#animals in my study
Animals_overview <- cbind(hr_areas,data.frame(loc = c("Summerville", "Independence", "Patterson", "Patterson","Summerville", "Summerville", "Summerville", "Summerville", "Summerville")))


#determining the date ranges and temporal coverage of each elk
summary(ltraj.elk)

#temporal coverage
summary(ltraj.elk)[c(1,2,4,5,7,9,11,12,13),6]-summary(ltraj.elk)[c(1,2,4,5,7,9,11,12,13),5]

o
summary(ltraj.elk)[6]-summary(ltraj.elk)[5]

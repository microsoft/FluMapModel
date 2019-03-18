# build neighbor graph for shapefile
# http://www.r-inla.org/faq#TOC-I-have-a-shapefile.-How-can-I-fit-a-spatial-model-with-R-INLA-
# https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
# https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html

setwd('C:/Users/mfamulare/git/seattleIncidenceMapper/censusData')

#Load libraries
library(sf)
library(spdep)
library(INLA)

# load shape
waShapes <- sf::st_read("cb_2017_53_tract_500k.shp")

#Create adjacency matrix
waNeighborGraph <- poly2nb(as(waShapes,"Spatial"))

# TODO: add edges for islands

#Convert the adjacency matrix into a file in the INLA format
nb2INLA("cb_2017_53_tract_500k.adj", waNeighborGraph)



# save images
png(filename="waCensusTracts.png",width = 10, height = 6, units = "in", res=300)
plot(st_geometry(waShapes), border="grey", lwd=0.1)
dev.off()

png(filename="waNeighborGraph.png",width = 10, height = 6, units = "in", res=300)
plot(st_geometry(waShapes), border="grey", lwd=0.1)
plot(waNeighborGraph, coordinates(as(waShapes,"Spatial")), points=FALSE, add=TRUE, lwd=0.5)
dev.off()


# King County only
# load shape
kcShapes <- waShapes[waShapes$COUNTYFP=='033',]

#Create adjacency matrix
kcNeighborGraph <- poly2nb(as(kcShapes,"Spatial"))

# TODO: add edges for islands

#Convert the adjacency matrix into a file in the INLA format
nb2INLA("kc.adj", kcNeighborGraph)



# save images
png(filename="kcCensusTracts.png",width = 10, height = 6, units = "in", res=300)
plot(st_geometry(kcShapes), border="grey", lwd=0.1)
dev.off()

png(filename="kcNeighborGraph.png",width = 10, height = 6, units = "in", res=300)
plot(st_geometry(kcShapes), border="grey", lwd=0.1)
plot(kcNeighborGraph, coordinates(as(kcShapes,"Spatial")), points=FALSE, add=TRUE, lwd=0.5)
dev.off()


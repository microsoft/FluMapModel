#' constructAdjacencyNetwork:  function for constructing adjacency graph for INLA GMRF from shapefile
#'
#' @param shp sf object with polygon geometry
#' @return neighborGraph in INLA format
#'
#' @importFrom INLA inla.read.graph
#' @import dbViewR
#' @importFrom spdep poly2nb nb2INLA
#' @export
#'
#' @examples
#' return neighbor census tract adjacency network for King County, WA
#'    neighborGraph <- constructAdjacencyNetwork( shp = dbViewR::masterSpatialDB() )
#'
constructAdjacencyNetwork <- function( shp ){
  
  #Create adjacency matrix
  adj <- spdep::poly2nb(as(shp,"Spatial"))
  
  # TODO: add edges for islands
  # TODO: allow alternate defintions of neighbor
  
  #Convert the adjacency matrix into a file in the INLA format
  spdep::nb2INLA("shp.adj", adj)
  
  #read in INLA graph
  neighborGraph <- INLA::inla.read.graph("shp.adj")
  
  # delete file
  unlink("shp.adj")
  
  return(neighborGraph)
  
}


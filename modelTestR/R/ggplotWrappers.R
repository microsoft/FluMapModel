#' ggplotSmoothMap: function for plotting data and smoothed model next to each other on map
#'
#' @param model model object from incidenceMapR::modelTrainR
#' @param shp sf object with GEOID shapes (GEOID only for now)
#' @return ggplot object
#'
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import viridis
#'
#' @export
#' @examples
#' plotDat <- right_join(model$modeledData,shp, by=c('GEOID'))
#' plotDat$positive[plotDat$positive==0]<-NaN
#' ggplotSmoothMap(plotDat,shp)
#'
ggplotSmoothMap <- function(model, shp, title=''){


plotDat <- right_join(model$modeledData,shp, by=c('GEOID'))
plotDat$positive[plotDat$n==0]<-NaN


mapSettings <- ggplot() + xlim(c(122.5,121.7)) + ylim(c(47.17,47.76)) +  theme_bw() +
  theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.grid.major=element_line(colour="transparent"), panel.border = element_blank())
p<-mapSettings + geom_sf(data=shp,size=0.1,aes(fill=NaN))

# mapSettingsTight <- ggplot() + xlim(c(122.5,122.15)) + ylim(c(47.42,47.76)) +  theme_bw() +
#   theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.grid.major=element_line(colour="transparent"), panel.border = element_blank())
# p<-mapSettingsTight + geom_sf(data=shp,size=0.1,aes(fill=NaN))

colorLimits<-c(min(c(plotDat$positive,plotDat$fitted.values.mode),na.rm=TRUE),max(c(plotDat$positive,plotDat$fitted.values.mode),na.rm=TRUE))
colorBreaks<-round(seq(min(colorLimits),sqrt(max(colorLimits)), length.out = 6)^2)

p1 <- p + geom_sf(data=plotDat,size=0, aes(fill=positive))  +
  guides(fill=guide_legend(title="observed")) +
  scale_fill_viridis(na.value="transparent",trans = "sqrt",breaks=colorBreaks,limits=colorLimits)

p2 <- p + geom_sf(data=plotDat,size=0, aes(fill=fitted.values.mode))  +
  guides(fill=guide_legend(title="expected")) +
  scale_fill_viridis(na.value="transparent",trans = "sqrt",breaks=colorBreaks,limits=colorLimits)

grid.arrange(p1,p2,nrow=1, top=textGrob(title))
}

#' ggplotSmoothSequential: function for plotting data and smoothed model next to each other as sequential variable (timeseries, age)
#'
#' NOT YET IMPLEMENTED!
#'
#' @param plotDat data.frame that joins dbViewR::modeledData
#' @return ggplot object
#'
#' @import ggplot2
#' @import gridExtra
#' @import viridis
#'
#' @export
#' @examples
#' plotDat <- right_join(model$modeledData,shp, by=c('GEOID'))
#' plotDat$positive[plotDat$positive==0]<-NaN
#' ggplotSmoothSequential(plotDat)
#'
ggplotSmoothSequential <- function(plotDat){
  return(NULL)
}

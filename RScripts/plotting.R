# ------------------------------------------------------------------------------
#
# Author: Magdalena Fischer
# Email: magdalena.fischer@stud-mail.uni-wuerzburg.de
# date: 10.04.2022
# GitHub: https://github.com/ma9dalen8/IceSat2-glacierAnalysis
#
# This script creates different plots to visualize the results
#
# R-version: 4.1.1
# operating system: windows
# Libraries used:
#
# library(ggplot2)
# library(raster)
# library(ggpubr)
#
#-------------------------------------------------------------------------------


#' plots two DEMs next to each other
#'
#' @param spDEM historic DEM
#' @param newDEMdf more recent DEM
#'
#' @return plot
#'

rasterplot <- function(spDEM, newDEMdf){

  spDEMdf <- as.data.frame(spDEM, xy=TRUE)
  spDEMdf <- na.omit(spDEMdf)
  colnames(spDEMdf) <- c('x', 'y', 'elevation')

  histPlt <- ggplot()+
    geom_tile(data = spDEMdf, aes(x = x, y = y, fill = elevation))+
    scale_fill_gradientn(colors = terrain.colors(10))+
    theme_bw()+
    labs(title = "DEM 1949", x = "latitude", y = "longitude", fill = "elevation (m)")



  newPlt <- ggplot()+
    geom_tile(data = newDEMdf, aes(x = x, y = y, fill = var1.pred))+
    scale_fill_gradientn(colors = terrain.colors(10))+
    theme_bw()+
    labs(title = "DEM 2021", x = "latitude",
          y = "longitude", fill = "elevation (m)")


    figure <- ggarrange(histPlt, newPlt, ncol =2, nrow=1,
                        common.legend = TRUE, legend = 'bottom')+
      labs(title = "Digital elevation Models Jan Mayen")

  return(figure)

}


#' plots difference between two rasters
#'
#' @param eleDiff data.frame with historic and recent elevation information
#'
#' @return plot
#'

diff2raster <- function(eleDiff){

  plt <- ggplot()+
    geom_tile(data=eleDiff, aes(x=lat, y=lon, fill = changeYear), colour ='white')+
    scale_fill_gradient2('elevation difference (m)', low='darkred', mid='azure2', high='darkblue')+
    theme_bw()+
    labs(title = "Elevation changes between 1949 and 2021 on Jan Mayen",
         x = "latitude", y = "longitude")+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    guides(fill = guide_colourbar(title.position = "top", barwidth = 20, barheight = 0.5))

  return(plt)


}


#' plots historic elevation points next to the IceSat-2 points
#'
#' @param combinedpointData data.frame with historic and recent point elevation information
#'
#' @return plot
#'

elevPoints <- function(combinedpointData){

  colors <- c('elevation 1949' ='chocolate1', 'elevation 2021' = 'lightskyblue4')

  plt <- ggplot()+
    geom_point(data = combinedpointData, aes(x=lat, y= rasValue, color = 'elevation 1949'),
               size = 0.5, shape = 1)+
    geom_point(data = combinedpointData, aes(x=lat, y= dem_h, color = 'elevation 2021'),
               size = 0.5, shape = 1)+
    labs(title = "Elevation 1949 and 2021 on Jan Mayen",
         x = "latitude", y = "elevation", color = '')+
    scale_color_manual(values = colors)+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    guides(color = guide_legend(override.aes = list(size=3)))


  return(plt)

}


#' plots elevation Difference as well as trend lines
#'
#' @param combinedpointData data.frame with elevation difference information
#'
#' @return plot
#'

elevPointsDiff <- function(combinedpointData) {

  plt <- ggplot(data = combinedpointData, aes(x = lat, y = eleChange))+
    geom_point(size = 0.5, color = 'lightskyblue3')+
    geom_smooth(method = 'gam', size = 0.7, col = 'navyblue')+
    geom_smooth(method = 'lm', size =0.7, col = 'firebrick4')+
    labs(title = "Elevation Change between 1949 and 2021 per year on Jan Mayen",
          x = "latitude", y = "elevation change / year")

  return(plt)
}


#' plots elevation Difference points on the hillshade of the glacier
#'
#' @param combinedpointData data.frame with elevation Difference and lat, lon information
#' @param spDEM historic DEM for creating the hillshade
#'
#' @return plot
#'

pointsDiffGlacier <- function(histDEM, combinedpointData){

  spDEM <- projectRaster(histDEM, crs = "+init=epsg:4326")
  slope = terrain(spDEM, opt='slope')
  aspect = terrain(spDEM, opt='aspect')
  hill = hillShade(slope, aspect)

  hillshadeDF <- as.data.frame(hill, xy=TRUE)
  demDF <- as.data.frame(spDEM, xy=TRUE)
  demDF <-  cbind(demDF, hillshadeDF$layer)
  colnames(demDF) <- c('x', 'y', 'dem', 'hill')
  demDF <- na.omit(demDF)


  plt <- ggplot()+
    geom_tile(data=demDF, aes(x=x, y=y, fill= dem, alpha = hill))+
    geom_point(data = combinedpointData, aes(x=lat, y= lon, color=eleChange), size = 0.5)+
    scale_fill_gradient( low = 'lightskyblue4', high = 'lightskyblue1')+
    scale_color_gradient2(low = 'firebrick4', high = 'navyblue')+
    guides(alpha='none')+
    labs(title = "Yearly elevation difference between 1949 and 2021 on Jan Mayen ",
          x = "latitude", y = "longitude", colour = "elevation DIfference (m)",
          fill = "elevation 1949 (m)")

  return(plt)


}

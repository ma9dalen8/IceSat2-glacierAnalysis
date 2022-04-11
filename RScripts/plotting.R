# ------------------------------------------------------------------------------
#
# Author: Magdalena Fischer
# Email: magdalena.fischer@stud-mail.uni-wuerzburg.de
# date: 10.04.2022
# GitHub: https://github.com/ma9dalen8/IceSat2-glacierAnalysis
#
# This script creates different plots to visualize the results
#
# Libraries used:
#
# library(ggplot2)
# library(raster)
#
#-------------------------------------------------------------------------------


#' plots two DEMs next to each other
#'
#' @param spDEM historic DEM
#' @param newDEMdf more recent DEM
#'
#' @return plot
#'

library(ggpubr)

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

  plt
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

  plt
  return(plt)


}






  # scale_fill_gradient( low = 'lightskyblue4', high = 'lightskyblue1')+
  # scale_color_gradient2(low = 'firebrick4', high = 'navyblue')+
  # guides(alpha='none')




















# library(plot3D)
# library(rgdal)
# library(sf)
# library(sp)
# library(raster)
# library(rgeos)
# library(ggplot2)
# library(viridisLite)
# library(gstat)
# library(tidyverse)
# library(maptools)
# library(rayshader)
# library(leaflet)
# library(insol)
# library(ggThemeAssist)
#
# histDEM <- raster('../GlacierDEMJanMayen.tif')
# spRasterDEM <- projectRaster(histDEM, crs = "+init=epsg:4326")
#
# slope = terrain(spRasterDEM, opt='slope')
# aspect = terrain(spRasterDEM, opt='aspect')
# hill = hillShade(slope, aspect)
#
# # hillshade <- raster('../hillshadeJanMayen.tif')
# # spRasterHill <- projectRaster(hillshade, crs = "+init=epsg:4326")
#
#
# #janmayen= raster_to_matrix(histDEM)
#
#
#
# aoiData <- st_read('../aoipointsJanMayen2021.gpkg')
# aoiData <- st_transform(aoiData, crs = "+init=epsg:4326" )
#
# aoiDataLatLon <- aoiData %>%
#   mutate(lat= unlist(map(aoiData[]$geom, 1)),
#          lon= unlist(map(aoiData[]$geom, 2)))
#
#
#
# rasValue <- raster::extract(spRasterDEM, aoiDataLatLon)
# eleChange <- (aoiDataLatLon$dem_h-rasValue)/72 # change years
# aoiDataLatLon <- cbind(aoiDataLatLon, rasValue, eleChange)
#
#







#
#cropHillshade <-crop(spRasterHill, extent(spRasterDEM))
#
# ggplot()+
#   geom_raster(data=hillshadeDF, aes(x=x, y=y, fill=hillshadeJanMayen))+
#   #geom_raster(data=demDF, aes(x=x, y=y, fill=GlacierDEMJanMayen, alpha=0.5))+
#   scale_fill_continuous(na.value = "transparent")+
#   geom_point(data = aoiDataLatLon, aes(x=lat, y= lon, fill=dem_h))+
#     theme(panel.grid.major = element_line(colour = "grey",
#     linetype = "solid"), panel.grid.minor = element_line(colour = "grey",
#     linetype = "solid"), plot.background = element_rect(fill = "transparent",
#     linetype = "solid"), legend.key = element_rect(fill = NA),
#     legend.background = element_rect(fill = NA)) +labs(title = "elevation change", x = "latitude",
#     y = "longitude", fill = "elevation in m")
#
#
#
# # scale_fill_gradientn(name = "Elevation", colors = rainbow(100)) +
#   # guides(fill = guide_colorbar()) +
#   # scale_alpha(range = c(0.15, 0.65), guide = "none") +
#   # theme(axis.title.x = element_blank(),
#   #       axis.title.y = element_blank()) +
#   # ggtitle("DSM with Hillshade - NEON Harvard Forest Field Site") +
#   # coord_equal()
#   #
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# hillshadeDF <- as.data.frame(spRasterHill, xy=TRUE)
# cropHillshade <-crop(spRasterDEM, extent(spRasterHill))
# rast <- stack(spRasterDEM, cropHillshade)
# rastdf <- rasterToPoints(rast)
# rastdf <- data.frame(rastdf)
# colnames(rastdf) <- c('x','y','dem','hillshade')
#
# rescale_hill <- function(x, minx, maxx){  # input is the vector to reescale, the minimum and maximum desired
#   d <- numeric(length(x)) #make a new vector
#   max_old<- max(x, na.rm = T) #find the min and max values
#   min_old <- min(x, na.rm = T)
#   for(i in 1:length(x)){
#     if(is.na(x[i])== T){ d[i]<- minx} #for the NAs assign the min value
#     else{
#       d[i] <- ((maxx-minx)/(max_old- min_old ))*(x[i]-max_old) +maxx
#     }
#   }
#   d
# }
# rastdf <- as.data.frame(rastdf)
# rastdf$hill <- rescale_hill(rastdf$hillshade, 1,0.5)
#
# ggplot()+
#   geom_raster(data=rastdf, aes(x, y, fill = dem), alpha=rastdf$hill)+
#   scale_fill_gradientn(colours=grey(1:100/100))
#
#
# ggplot()+
#   geom_tile(data = hillshadeDF, aes(x=x, y=y, fill=hillshadeJanMayen))+
#
#   geom_point(data = aoiDataLatLon, aes(x=lat, y=lon), color ='red')
#
#
#
#
#
# janmayen %>%
#   sphere_shade()%>%
#   plot_3d(janmayen, zscale = 50, theta = 210, phi = 22, fov = 55)
#
# render_points(extent = attr(histDEM, 'extent'),
#               lat = unlist(lat), long = unlist(lon),
#               altitude = 0, zscale = 50, color = 'red')


#
#
# elmat %>%
#   sphere_shade(texture = "desert") %>%
#   plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
# Sys.sleep(0.2)
# render_snapshot()


# janmayendf <- as.data.frame(spRaster, xy= TRUE)
#
# bbox <- list(
#   p1= list(long=spRaster@extent@ymin, lat = spRaster@extent@xmin),
#   p2= list(long=spRaster@extent@ymax, lat = spRaster@extent@xmax)
# )
#
# define_image_size <- function(bbox, major_dim = 400) {
#   # calculate aspect ration (width/height) from lat/long bounding box
#   aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
#   # define dimensions
#   img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
#   img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
#   size_str <- paste(img_width, img_height, sep = ",")
#   list(height = img_height, width = img_width, size = size_str)
# }
#
#
# image_size <- define_image_size(bbox, major_dim=600)

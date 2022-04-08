# ------------------------------------------------------------------------------
#
# Author: Magdalena Fischer
# Email: magdalena.fischer@stud-mail.uni-wuerzburg.de
# date: 01.04.2022
#
# this script can be used to convert iceSat2 hdf5 data, downloaded from the
# ndsci.org/data/atl06 website, to gpgk. All datapoints will be stored in a
# single file.
#
# Libraries used:
#
# library(rhdf5)
# library(rgdal)
# library(sf)
# library(sp)
# library(raster)
#
#-------------------------------------------------------------------------------
library(rhdf5)
library(rgdal)
library(sf)
library(sp)
library(raster)
library(rgeos)
library(ggplot2)
library(gstat)
library(tidyverse)



copyFiles <- function(inpath){

  folders <- list.files(path=dir, full.names = TRUE)
  print(folders)
  for (folder in folders){
    fileH5 <- list.files(path=folder, pattern='*.h5')
    fullpath <- str_replace_all(paste(folder,'/',fileH5), fixed(" "), "")
    print(fullpath)
    file.copy(fullpath, dir)
  }
}


#' convert iceSat2 hdf5 to gpgk
#'
#' @param paths to currently stored data
#' @param outpath where to save converted file
#'
#' @return data.frame including all iceSat2points
#' @export spatialdata holding all icesat2points as gpkg to outpath
#'

hdf5togpkg <- function(paths, outpath){

  # creating empty variables to store temp data
  beams<- c()
  allData <- data.frame('latitude', 'longitude', 'h_li', 'atl06_quality_summary', 'dem_h', 'geoid_h', 'x_atc', 'y_atc', 'dh_fit_dx', 'dh_fit_dy', 'beamnr')
  allData <- allData[FALSE]
  beamnr <- 1

  # looping through list with all hdf5 files in provided directory

  for (file in paths){
    hdf <- H5Fopen(file)

    # check which beams are included in the file

    res1 <- try(hdf&'gt1l')
    if(class(res1) == 'try-error'){
    }
    else{
      beams <- append(beams, c('gt1r','gt1l'))
    }
    res2 <- try(hdf&'gt2l')
    if(class(res2) == 'try-error'){
    }
    else{
      beams <- append(beams, c('gt2r','gt2l'))
    }
    res3 <- try(hdf&'gt3l')
    if(class(res3) == 'try-error'){
    }
    else{
      beams <- append(beams, c('gt3r','gt3l'))
    }

    # extract date from filename
    filebasename <- basename(file)
    date <- substr(filebasename, 7,15)

    # loop through all beams in current file, extrecting relevant information and storing it in data.frame
    for (beam in beams){

      latitude <- (hdf&beam&'land_ice_segments')$'latitude'
      longitude <- (hdf&beam&'land_ice_segments')$'longitude'
      h_li <- (hdf&beam&'land_ice_segments')$'h_li'
      atl06_quality_summary <- (hdf&beam&'land_ice_segments')$'atl06_quality_summary'

      dem_h <- (hdf&beam&'land_ice_segments'&'dem')$'dem_h'
      geoid_h <- (hdf&beam&'land_ice_segments'&'dem')$'geoid_h'

      x_atc <- (hdf&beam&'land_ice_segments'&'ground_track')$'x_atc'
      y_atc <- (hdf&beam&'land_ice_segments'&'ground_track')$'y_atc'

      dh_fit_dx <- (hdf&beam&'land_ice_segments'&'fit_statistics')$'dh_fit_dx'
      dh_fit_dy <- (hdf&beam&'land_ice_segments'&'fit_statistics')$'dh_fit_dy'

      beamnr <- beamnr

      # concatenate previously exreacted data with allData

      tempdata <- data.frame(latitude, longitude, date, h_li, atl06_quality_summary, dem_h, geoid_h, x_atc, y_atc, dh_fit_dx, dh_fit_dy, beamnr)
      print(tempdata)
      allData <- rbind(allData, tempdata)

      # to be improved: indexing depending on beam
      beamnr <- beamnr+1
    }
    beamnr <- 1
    beams <- c()

  }
  # convert data.frame with all datapoints to SpatialDataFrame
  spatialdata <- allData
  coordinates(spatialdata) <- c( 'longitude', 'latitude')


  # saving all Datapoints as gpkg file to provided output path
  writeOGR(spatialdata, outpath , driver='GPKG', layer='icesat2')

  return (allData)
}




intersectPoints <- function(aoi, allDatapoints, outpath){

  # assign crs
  st_crs(allDatapoints) = 4326
  aoi <- st_transform(aoi, crs = 4326)

  # intersect of polygon and iceSat2 points
  aoiDatapoints <- st_intersection(aoi, allDatapoints)


  st_write(aoiDatapoints, outpath, driver='GPKG', layer='icesat2')

  return(aoiDatapoints)

}


#' createDEM
#'
#' @param seasonaldata gpkg of iceSat2 points, including height, lat, lon
#'
#' @return list with lat, lon, var1.pred values as result of the interpolation


createDEM <- function(seasonaldata, spRaster){

  # converting DF to large spatial PointsDataframe and assining a projection (UTM28N)
  spatialdf <- data.frame(seasonaldata)
  coordinates(spatialdf) <- ~lat+lon
  proj4string(spatialdf) <-  CRS("+init=epsg:32633 ")



  grid = as(spRaster, "SpatialPixels")
  proj4string(grid) <-  CRS("+init=epsg:32633 ")

  # creating a raster based on the point dataset
  #df <- SpatialPoints(griddf, proj4string = spatialdf@proj4string)

  # interpolate height of spatial points from spatialdf and writing predicted values to the grid
  interpolate <-  idw(formula = dem_h~1, locations=spatialdf, newdata = grid)

  # creating grid from interpolated values
  dem <- data.frame(interpolate)


  return (dem)
}





#' diffDEMs
#'
#' @param dem1 list with lat, lon, var1.pred values, result of createDEM, first timestep
#' @param dem2 list with lat, lon, var1.pred values, result of createDEM, later timestep
#'
#' @return list with with the difference between the two DEMs

diffDEMs <- function(dem1, dem2){

  #calculating difference of two timestemps
  elevationDiff <- data.frame(dem1$x, dem1$y, (dem1$GlacierDEMJanMayen-dem2$var1.pred)/72)
  colnames(elevationDiff) <- c('lat', 'lon', 'change/year')

  return (elevationDiff)
}

#-------------------------------------------------------------------------------
#
# EXECUTE FUNCTIONS
#
#-------------------------------------------------------------------------------


dir <- "C:/Users/Magdalena/Eagle/MB2/FinalProject/JanMayen2021/more"
copyFiles(dir)



paths <-  list.files(path='C:/Users/Magdalena/Eagle/MB2/FinalProject/JanMayen2021/more', pattern='*.h5', full.names = TRUE)
outpath_hdf5togpkg <- 'C:/Users/Magdalena/Eagle/MB2/FinalProject/allPointsJanMayen.gpkg'

hdf5togpkg(paths, outpath_hdf5togpkg)



aoi <- st_read('../outlineJanMayen.gpkg')
allDatapoints <- st_read(outpath_hdf5togpkg)
outpath_intersectPoints <- '../aoipointsJanMayen2021.gpkg'

intersectPoints(aoi, allDatapoints, outpath_intersectPoints)


histDEM <- raster('../GlacierDEMJanMayen.tif')
histDEM <- aggregate(histDEM, fact=10)
spRaster <- projectRaster(histDEM, crs = "+init=epsg:4326")


aoiData <- st_read(outpath_intersectPoints)
aoiDataLatLon <- aoiData %>%
  mutate(lat= unlist(map(aoiData$geom, 1)),
         lon= unlist(map(aoiData$geom, 2)))


dem1 <- createDEM(aoiDataLatLon, spRaster) #2019
dem2 <- createDEM(aoiDataLatLon) #2021


ggplot()+
  geom_tile(data = dem1, aes(x = x, y = y, fill = var1.pred))+
  scale_fill_gradientn(colors = terrain.colors(10))+
  geom_sf(data=aoiData, colour='black', fill=NA, alpha=50)+
  theme_bw()

# Step 4: calculating the difference between two elevation models to evaluate the change
# needs two DEMs from different times
# plot difference

rasterDF <-  as.data.frame(spRaster, xy=TRUE)
plot(rasterDF$GlacierDEMJanMayen)
eleDiff <- diffDEMs(rasterDF, dem1)

ggplot()+
  geom_tile(data=eleDiff, aes(x=lat, y=lon, fill = diff), colour ='white')+
  scale_fill_gradient2('diff', low='darkred', mid='azure2', high='darkblue')+
  geom_sf(data=aoi, colour='black', fill=NA)+
  theme_bw()


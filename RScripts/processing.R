# ------------------------------------------------------------------------------
#
# Author: Magdalena Fischer
# Email: magdalena.fischer@stud-mail.uni-wuerzburg.de
# GitHub: https://github.com/ma9dalen8/IceSat2-glacierAnalysis
# Date: 10.04.2022
#
#
# In this script all functions for processing the IceSat-2 data are included.
#
# R-version: 4.1.1
# operating system: windows
# Libraries used:
#
# library(gstat)
# library(raster)
# library(rgdal)
# library(rhdf5)
# library(sf)
# library(stringr)
# library(tidyverse)
#
#-------------------------------------------------------------------------------


#' restructure files and folders
#'
#' @param inpath directory where subfolders holding IceSat-2 hdf5 files are stored
#'

copyFiles <- function(inpath){

  folders <- list.files(path=dir, full.names = TRUE)
  print(folders)
  # loop through all folders copying all .h5 files to directory above
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


#' intersection area of interest with IceSat-2 points
#'
#' @param aoi gpkg with polygon of area of interes
#' @param allDatapoints gpkg all IceSat-2 points
#' @param outpath path where to save result
#'
#' @return data.frame with points within the area of interest
#'

intersectPoints <- function(aoi, allDatapoints, outpath){

  # assign crs
  st_crs(allDatapoints) = 4326
  aoi <- st_transform(aoi, crs = 4326)

  print('intersecting points may take a while, example result data set provided on GitHub')
  # intersect of polygon and iceSat2 points
  aoiDatapoints <- st_intersection(aoi, allDatapoints)


  st_write(aoiDatapoints, outpath, driver='GPKG', layer='icesat2')

  return(aoiDatapoints)

}


#' aggregating and projecting historic DEM
#'
#' @param histDEM Formal class RasterLayer
#'
#' @return RasterLayer with projection (WGS84)
#'

preprocesshistDEM <- function(histDEM){


  histDEMagg <- aggregate(histDEM, fact=10)
  spDEM <- projectRaster(histDEMagg, crs = "+init=epsg:4326")

  return(spDEM)

}


#' createDEM
#'
#' @param seasonaldata gpkg of iceSat2 points, including height, lat, lon
#' @param  spDEM historic DEM with projection
#'
#' @source https://www.youtube.com/watch?v=93_JSqQ3aG4
#'
#' @return list with lat, lon, var1.pred values as result of the interpolation
#'

createDEM <- function(seasonaldata, spDEM){


  # create grid same extent as historic DEM
  grid = as(spDEM, "SpatialPixels")

  # converting DF to large spatial PointsDataframe and assining the same projection as grid (WGS84)
  seasonaldata <- seasonaldata %>%
    mutate(lat= unlist(map(seasonaldata[]$geom, 1)),
           lon= unlist(map(seasonaldata[]$geom, 2)))


  spatialdf <- data.frame(seasonaldata)
  coordinates(spatialdf) <- ~lat+lon
  proj4string(spatialdf) <- crs(spDEM)


  # interpolate height of spatial points from spatialdf and writing predicted values to the grid
  interpolate <-  idw(formula = dem_h~1, locations=spatialdf, newdata = grid)

  # creating grid from interpolated values
  dem <- data.frame(interpolate)



  return (dem)
}


#' diffDEMs
#'
#' @param dem1 Formal class RasterLayer, historic DEM with projection
#' @param dem2 list with lat, lon, var1.pred values, more recent DEM, result of createDEM
#' @param years integer years between the two DEMs
#'
#' @return list with with  lat, lon and the difference between the two DEMs per year
#'

diffDEMs <- function(dem1, dem2, years){

  dem1 <-  as.data.frame(spDEM, xy=TRUE)
  dem1 <- na.omit(dem1)
  colnames(dem1) <- c('lat', 'lon', 'elevation' )

  #calculating difference of two timestemps
  elevationDiff <- data.frame(dem1$lat, dem1$lon, (dem1$elevation-dem2$var1.pred)/years)
  colnames(elevationDiff) <- c('lat', 'lon', 'changeYear')



  return (elevationDiff)
}



#' extracting elevation points from hist DEM accordingly to the IceSat-2 points
#'
#' @param aoiData data.frame with all IceSat-2 points over the glacier
#' @param spDEM Raster layer of the historic DEM
#' @param years amount of years between the two elevation datasets
#'
#' @return data.frame with historic and recent elevation information as well as elevation difference
#'

extractPointsDEM <- function(aoiData, spDEM, years){#

  aoiDataLatLon <- aoiData %>%
    mutate(lat= unlist(map(aoiData[]$geom, 1)),
           lon= unlist(map(aoiData[]$geom, 2)))


  rasValue <- raster::extract(spDEM, aoiData)
  eleChange <- (aoiData$dem_h-rasValue)/years
  combinedData <- cbind(aoiDataLatLon, rasValue, eleChange)

  return(combinedData)

}

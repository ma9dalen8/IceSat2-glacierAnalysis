# ------------------------------------------------------------------------------
#
# Author: Magdalena Fischer
# Email: magdalena.fischer@stud-mail.uni-wuerzburg.de
# date: 10.04.2022
# GitHub: https://github.com/ma9dalen8/IceSat2-glacierAnalysis
#
# this script is the main script to execute the workflow to analyse glaciers
# with IceSat-2 data. Considering the following steps:
#
# (1) restructure downloaded IceSat-2 files into one folder
# (2) converting single .hdf5 files to one .gpkg file
# (3) intersecting all points with area of interest
# (4) interpolating aoiPoints to raster DEM
# (5) calculating the elevation change per year between historic DEM and newly calculated DEM
# (6) plotting some results
#
#
#-------------------------------------------------------------------------------

library(rgdal) #
library(sf) #
library(raster) #
library(ggplot2) #
library(gstat) #
library(tidyverse) #
library(rhdf5) #
library(stringr) #

library(rgeos)
library(sp)
library(viridisLite)
library(maptools)
library(rayshader)

source('RScripts/processing.R')
source('RScripts/plotting.R')

#-------------------------------------------------------------------------------
#
# DEFINE PARAMETERS
#
#-------------------------------------------------------------------------------

# define Glacier name (default Jan Mayen)

galcier <- 'JanMayen'

# define path to IceSat-2 Data --> if stored as on GitHub no change needed

dir <- str_replace_all(paste(getwd(),'/Data'), fixed(' '), '')

# define path of gpkg with area of interest --> if stored as on GitHub no change needed

aoipath <- 'Data/outlineJanMayen.gpkg'

# define path to historic DEM --> if stored as on GitHub no change needed

DEMpath <- 'Data/GlacierDEMJanMayen.tif'

# define timespan which is investigated e.g. on GitHub 72 years

years <- 72

#-------------------------------------------------------------------------------
#
# EXECUTE FUNCTIONS
#
#-------------------------------------------------------------------------------

copyFiles(dir)


paths <-  list.files(path=dir, pattern='*.h5', full.names = TRUE)
outpath_hdf5togpkg <- str_replace_all(paste('allPoints', galcier, '.gpkg'), fixed(' '), '')

hdf5togpkg(paths, outpath_hdf5togpkg)



aoi <- st_read(aoipath)
allDatapoints <- st_read(outpath_hdf5togpkg)
outpath_intersectPoints <- str_replace_all(paste('aoiPoints', galcier, '.gpkg'), fixed(' '), '')

intersectPoints(aoi, allDatapoints, outpath_intersectPoints)



histDEM <- raster('Data/GlacierDEMJanMayen.tif')
aoiData <- st_read('aoiPointsJanMayen.gpkg')

spDEM <- preprocesshistDEM(histDEM)
newDEMdf <- createDEM(aoiData, spDEM)
eleDiff <- diffDEMs(spDEM, newDEMdf, years)


combinedpointData <- extractPointsDEM(aoiData, spDEM, years)



#-------------------------------------------------------------------------------
#
# CREATE PLOTS
#
#-------------------------------------------------------------------------------

# the two DEMs next to each other
demPLT <- rasterplot (spDEM, newDEMdf)
demPLT

# raster showing elevation difference per year per pixel
elevDiffPLT <- diff2raster (eleDiff)
elevDiffPLT

# scatter plot with elevation points from 1949 and 2021
pointsPLT <- elevPoints (combinedpointData)
pointsPLT

# scatter plot with elevation difference points and trend lines
pointsDiffPLT <- elevPointsDiff (combinedpointData)
pointsDiffPLT

# elevation difference points on hillshade of glacier
pntsOnGlacierPLT <- pointsDiffGlacier (histDEM, combinedpointData)
pntsOnGlacierPLT





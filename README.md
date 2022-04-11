 # Analysing Glacier Elevation Change with IceSat - 2 Data
 #### by Magdalena Fischer 
 magdalena.fischer@stud-mail.uni-wuerzburg.de <br/><br/>

## General information:<br/>
R-version: 4.1.1<br/>
operating system: windows<br/>
scripts needed: executing.R, processing.R, plotting.R<br/>
libraries: <br/>
* library(ggplot2)<br/>
* library(ggpubr)<br/>
* library(gstat)<br/>
* library(raster)<br/>
* library(rgdal)<br/>
* library(rhdf5)<br/>
* library(sf)<br/>
* library(stringr)<br/>
* library(tidyverse)<br/><br/>

## Data used:<br/>
https://data.npolar.no/home/<br/>
https://nsidc.org/data/atl06<br/>
https://www.glims.org/RGI/<br/><br/>



 ## Research
 This Repository contains three scripts, one main script to execute the functions, 
 one processing script with all the functions and a plotting script where the results are visulized.
 Generally the changes in the glacier elevation are researched and therefore gathering information
 about ice thickening or thinning are the focus. The analysis is done considering IceSat-2
 data as well as a historic DEM from NPI. As area of interest the
 Randolpgh Glacier Inventory was considered.<br/><br/>



 The process considers the following steps:<br/>

(1) restructure downloaded IceSat-2 files into one folder<br/>
(2) converting single .hdf5 files to one .gpkg file<br/>
(3) intersecting all points with area of interest<br/>
(4) interpolating aoiPoints to raster DEM<br/>
(5) calculating the elevation change per year between historic DEM and newly calculated DEM<br/>
(6) extractig teh saem points as IceSat-2 from historic DEM <br/>
(7) plotting some results<br/><br/>

## Execution
This Repository includes an example Dataset for Jan Mayen, with all IceSat-2 data from the summer season 2021 (hdf5-format), a historic DEM (tif-format) and a gpkg file with the extends 
of the glacier. After cloning the repository, some variables need to be set. If the example data is used the paths don't need to be changed. After executing all 
the functions step by step from the executing.R script, the elevation changes can be investigated in different plots. <br/>*NOTICE*: the intersectPoints function 
needs a long time for extracting the relevant points. To skip this process an already cropped dataset is provided in the Data folder and can be used instead.

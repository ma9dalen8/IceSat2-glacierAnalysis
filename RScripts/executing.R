library(plot3D)
library(rgdal)
library(sf)
library(sp)
library(raster)
library(rgeos)
library(ggplot2)
library(viridisLite)
library(gstat)
library(tidyverse)
library(maptools)
library(rayshader)


histDEM <- raster('../GlacierDEMJanMayen.tif')
spRaster <- projectRaster(histDEM, crs = "+init=epsg:4326")
aoiData <- st_read('../aoipointsJanMayen2021.gpkg')
coordinates(aoiData) <- ~lat+lon

rasValue <- raster::extract(spRaster, aoiData)

aoiData <- cbind(aoiData, rasValue)
eleChange <- aoiData$rasValue-aoiData$dem_h
aoiData <- cbind(aoiData, eleChange)

aoiDataLatLon <- aoiData %>%
  mutate(lat= unlist(map(aoiData[]$geom, 1)),
         lon= unlist(map(aoiData[]$geom, 2)))

plot(aoiData$dem_h)

scatter3D (x=aoiDataLatLon$lat, y=aoiDataLatLon$lon, z=aoiData$eleChange, col='black', pch = 20,  theta = 25, phi = 5)

myplot <- ggplot(aoiData)+
  geom_sf(aes(geometry=geom, fill=dem_h), color='red', shape=4)+
  geom_sf(aes(geometry=geom, fill=rasValue), color='blue')

plot_gg(myplot, multicore=TRUE, width= 3.5,sunangle = 225, phi= 30, theta= 45)


ggplot(aoiData, aes(x=lat, y=dem_h, z=lon))+theme_void()+axes_3D()+stat_3D()

ggplot()+
    geom_point(data = aoiData, aes(x=lat, y=dem_h), shape =3)+
    geom_point(data = aoiData, aes(x=lat, y=dem_h), color ='red')


ggplot(aoiData, aes(x=lat, y=lon, fill=rasValue))+geom_point()
  # geom_point(aoiData$dem_h)+
  # geom_point(aoiData$rasValue)


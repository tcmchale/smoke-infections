library(sf)
#import shapefile
smoke.shp <- st_read("shapefiles/us-annual-smoke_PM2.5/us_annual-smokePM_10km.shp")
smoke.shp <- st_set_crs(smoke.shp, 4326)

# Project using Mercator CRS (EPSG:3857)
#smoke.merc <- st_transform(smoke.shp, crs = 3857)

#load the readr package
library(readr)


tm_shape(smoke.shp) +
  tm_fill("smoke.2006", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2006 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2007", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2007 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2008", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2008 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2009", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2009 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2010", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2010 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2011", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2011 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2012", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2012 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2013", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2013 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2014", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2014 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2015", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2015 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2016", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2016 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2017", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2017 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2018", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2018 Cumulative PM2.5 Density",
    main.title.position = 't'
  )


tm_shape(smoke.shp) +
  tm_fill("smoke.2019", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2019 Cumulative PM2.5 Density",
    main.title.position = 't'
  )

tm_shape(smoke.shp) +
  tm_fill("smoke.2020", 
          style ="fixed", 
          title = "µg/m^3",
          breaks=c(0,1,2,3,4,Inf),
          title.position = 't') +
  tm_layout(
    main.title = "2020 Cumulative PM2.5 Density",
    main.title.position = 't'
  )


##Test drive
#convert to raster

library(raster)
library(maptools) #FYI maptools will be retired soon, will have to use 'sp'
library(sf)
library(rgdal)

smoke.proj <- spTransform(smoke.shp, CRS = "+proj=longlat +datum=WGS84")

raster_2006 <- rasterize(smoke.shp, field="smoke.2006", fun=mean, res=0.5)

##rasterized in QGIS (Much easier)
#load in rasters
# Load the raster package
library(raster)

wgs84 <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs")

# Load the raster file and project to wgs84 for each year

##2006
smoke.raster06 <- raster("raster/smoke-cumulative-rasters_2006-2020/cumulative-smoke-2006.tif")
projectRaster(smoke.raster06, crs=wgs84)
plot(smoke.raster06,
     main="Cumulative Smoke PM2.5 2006, µg/m^3",
     xlab = "Longitude",
     ylab = "Latitude")

#2007
smoke.raster07 <- raster("raster/smoke-cumulative-rasters_2006-2020/cumulative-smoke-2007.tif")
projectRaster(smoke.raster07, crs=wgs84)
plot(smoke.raster07)

#2008
smoke.raster08 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2008.tif")
projectRaster(smoke.raster08, crs=wgs84)
plot(smoke.raster08)

#2009
smoke.raster09 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2009.tif")
projectRaster(smoke.raster09, crs=wgs84)
plot(smoke.raster09)

#2010
smoke.raster10 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2010.tif")
projectRaster(smoke.raster10, crs=wgs84)
plot(smoke.raster10)

#2011
smoke.raster11 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2011.tif")
projectRaster(smoke.raster11, crs=wgs84)
plot(smoke.raster11)

#2012
smoke.raster12 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2012.tif")
projectRaster(smoke.raster12, crs=wgs84)
plot(smoke.raster12)

#2013
smoke.raster13 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2013.tif")
projectRaster(smoke.raster13, crs=wgs84)
plot(smoke.raster13)

#2014
smoke.raster14 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2014.tif")
projectRaster(smoke.raster14, crs=wgs84)
plot(smoke.raster14)

#2015
smoke.raster15 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2015.tif")
projectRaster(smoke.raster15, crs=wgs84)
plot(smoke.raster15)

#2016
smoke.raster16 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2016.tif")
projectRaster(smoke.raster16, crs=wgs84)
plot(smoke.raster16)

#2017
smoke.raster17 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2017.tif")
projectRaster(smoke.raster17, crs=wgs84)
plot(smoke.raster17)

#2018
smoke.raster18 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2018.tif")
projectRaster(smoke.raster18, crs=wgs84)
plot(smoke.raster18)

#2019
smoke.raster19 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2019.tif")
projectRaster(smoke.raster19, crs=wgs84)
plot(smoke.raster19)

#2020
smoke.raster20 <- raster("smoke-cumulative-rasters_2006-2020/cumulative-smoke-2020.tif")
projectRaster(smoke.raster20, crs=wgs84)
plot(smoke.raster20)



#annual avg temp usa

avgtemp <- raster("rasters/usa-continental-temp/usa-continental-avg-temp.tif")
projectRaster(avgtemp, crs=wgs84)
plot(avgtemp)




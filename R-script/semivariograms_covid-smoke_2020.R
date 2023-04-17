#### Semivariograms

#pkgs
library(gstat)
library(sp)
library(readr)
library(dplyr)
library(sf)
library(geoR)
library(automap)

annual_smoke <- st_read("shapefiles/smoke/us-annual-smoke_county/us-annual-smoke_county.shp",crs=4326)
st_transform(annual_smoke, crs=4326)


covid_county <- st_read("shapefiles/covid/us_total-covid2020_with-pop")


#read in shp with centroids
cov_centroids <- st_read("shapefiles/covid/us_total-covid-2020_county_with-pop_centroids/us_total-covid-2020_county_with-pop_centroids.shp")
sm_centroids <- st_read("shapefiles/smoke/centroids/us-annual-smoke_county_centroids.shp")

# Extract x and y coordinates of centroids
cov_centroids$X <- st_coordinates(cov_centroids)[,1]
cov_centroids$Y <- st_coordinates(cov_centroids)[,2]

sm_centroids$X <- st_coordinates(sm_centroids)[,1]
sm_centroids$Y <- st_coordinates(sm_centroids)[,2]




######################


#log transform smoke
sm_centroids$log_smoke <- log(sm_centroids$smoke.2020)

#histograms
hist(sm_centroids$smoke.2020, xlab="PM2.5", main="observed",
     border="white", col="royalblue2")

hist(sm_centroids$log_smoke, xlab = "PM2.5", main = "empirical logit",
     border="white", col= "seagreen3")


#convert to a sf object
smoke_sp <- sm_centroids %>%
  st_as_sf(coords=c("LONGITUDE","LATITUDE"), crs=4326)

#plot the points
plot(smoke_sp$geometry)

#### aside, need to remove non-continental parts of US
#lets move over to qgis
#clipped and then made counties into centroids again
smokeclip <- st_read("shapefiles/smoke/centroids/us-annual-smoke_county_clipped_centroids.shp")

#logit again
smokeclip$logit2020 <- log(smokeclip$smoke.2020)

#convert to sf
smokeclip_sp <- smokeclip %>%
  st_as_sf(coords=c("LONGITUDE","LATITUDE"),crs=4326)

#plot points again
plot(smokeclip_sp$geometry)
## much better

#plot PM2.5 points
smokeclip_sp$quantile <- cut(smokeclip_sp$smoke.2020, breaks=4, labels=FALSE)
plot(smokeclip_sp[,"smoke.2020"])


#ggvario(coords=st_coordinates(smokeclip_sp), data=smokeclip$logit2020)
#function requires too much memory

#smoke variogram
smokeclip_sp <- na.omit(smokeclip_sp)
smoke_v <- variogram(smoke.2020~1, data=smokeclip_sp)

plot(smoke_v, pch=19, col="black", main="Semivariogram of PM2.5 by county (2020)")

vgm <- vgm(psill=NA, model = "Sph", range=NA, nugget = NA)
smoke.fit <- fit.variogram(smoke_v, model=vgm)
smoke_fit <- fit.variogram(smoke_v, model = vgm("Sph", range = 47, nugget = 10))


smoke.fit$psill
smoke.fit$range
smoke.fit$nugget

smoke_fit$psill
smoke_fit$range
smoke_fit$nugget

lines(smoke_fit, col="red")


smoke_vlog <- variogram(logsmoke~1, data=smokeclip_sp)


#logit smoke var
smokelog_v <- variogram(logit2020~1, data=smokeclip_sp)

plot(smokelog_v, pch=19, col="black", main = "Semivariogram of log(PM2.5) by county (2020)")

smokelog.fit <- fit.variogram(smokelog_v, model=vgm)

smoke.fit$psill
smoke.fit$range
smoke.fit$nugget


#krigging

#reproject in mercator
smoke_sp <- st_transform(smoke_sp, crs = "+init=epsg:3857")


orkriging <- autoKrige(formula = logsmoke ~ 1, input_data = as(smoke_sp, "Spatial"))
plot(orkriging)

ukriging <- autoKrige(formula = logsmoke ~ ALAND,
                      input_data = as(smoke_sp, "Spatial"),
                      new_data = as(smoke_sp, "Spatial"))


###
smoke_vario <- variogram(logsmoke ~ 1, data = smoke_sp)
smoke_fit <- fit.variogram(smoke_vario, model = c("Sph", "Exp", "Gau", "Lin", "Ste"))

################################

#log transform covid
cov_centroids$logitcss <- log(cov_centroids$ttl_css)
cov_centroids$logitdth <- log(cov_centroids$ttl_dth)

#histograms
hist(cov_centroids$ttl_css, xlab="", main="observed",
     border="white", col="royalblue2", xlim=c(0,20000000), breaks=100)

hist(cov_centroids$logitcss, xlab = "", main = "empirical logit",
     border="white", col= "seagreen3")

## variogram covid
cov_centroids <- na.omit(cov_centroids)
v <- variogram(log_total_cases~1, data=cov_centroids)
plot(v)

vgm <- vgm(psill = 150000, model = "Sph", range = 1000000, nugget = 500000)


v.fit <- fit.variogram(v, model = vgm)
plot(v, pch=19, col = "black", main = "Semivariogram with Fitted Model")
lines(v.fit, col="red")

v.fit$psill
v.fit$range
v.fit$nugget


#adjust covid by population

cov_centroids$csspcap <- (cov_centroids$ttl_css/cov_centroids$pop2020) *100000
cov_centroids$dthpcap <- (cov_centroids$ttl_dth/cov_centroids$pop2020) *100000




ggplot() + 
  geom_sf(data=cov_centroids, aes(geometry=geometry, fill=dthpcap)) + 
  scale_fill_gradient(low = "white", high = "red") + 
  labs(title = "COVID cases by County (2020)", fill = "COVID Data")


#################################
# Fit a variogram model
vgm <- fit.variogram(v, model = vgm("Gau", nugget = 0, psill = 10000, range = 150000))
vgm <- gstat::vgm(psill = 100, model = "Exp", range = 50000, nugget = 0)

# Fit a spatial trend model
trend <- lm(total_cases ~ X + Y, data = centroids)

# Combine the variogram model and the spatial trend model
krig <- gstat::gstat(formula = total_cases ~ X + Y, locations = centroids, 
                     model = vgm
                     )

# Perform the kriging
kriging_results <- predict(krig, newdata = centroids, nsim = 100)


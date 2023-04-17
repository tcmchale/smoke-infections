library(automap)

#### clipping to just california

cali_centr <- st_read("shapefiles/smoke/centroids/california_smoke_county_centroids.shp")

cali_centr$X <- st_coordinates(cali_centr)[,1]
cali_centr$Y <- st_coordinates(cali_centr)[,2]

cali_centr$logsmoke <- log(cali_centr$smoke.2020)


hist(cali_centr$smoke.2020, xlab = "PM2.5", main = "observed", 
     border = "white", col = "royalblue2")
hist(cali_centr$logsmoke, ylab = "PM2.5", main = "empirical logit",
     border= "white", col = "seagreen3")

#conver to sp
cali_centr_sp <- cali_centr %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs =4326)


plot(cali_centr_sp$geometry)

coords <- st_coordinates(cali_centr_sp)
ggvario(coords=st_coordinates(cali_centr_sp), data= cali_centr$logsmoke)


coords <- st_coordinates(cali_centr_sp)
vgm_model <- vgm(psill = 1, model = "Sph", range = max(dist(coords))/3)
emp_vario <- variogram(logsmoke ~ 1, data = cali_centr, locations = coords, cutoff = max(dist(coords))/3, model = vgm_model, messages = FALSE)

envmc <- variog.mc.env(geodata = as.geodata(cbind(coords, cali_centr)), obj.variog = empvario, nsim = 99, messages = F)


cali_v <- variogram(smoke.2020~1, data=cali_centr_sp)

vgm <- vgm(psill=NA, model = "Sph", range=NA, nugget = NA)
vgm <- vgm(psill = 15000, model = "Exp", range = 100000, nugget = 50000)
vgm <- vgm(psill=150, model = "Sph", range = 500, nugget = 50000)


smoke.fit <- fit.variogram(cali_v, model=vgm)

smoke.fit$psill
smoke.fit$nugget
smoke.fit$range

orkriging <- autoKrige(formula = logsmoke ~ 1, input_data = as(cali_centr_sp, "Spatial"))





krig <- gstat::gstat(formula = logsmoke ~ X + Y, locations = coords, 
                     model = vgm
)

# Perform the kriging
kriging_results <- predict(krig, newdata = coords, nsim = 100)


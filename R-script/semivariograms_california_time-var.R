
library(sf)
library(gstat)
library(sp)
library(spacetime)
library(lubridate)
library(dplyr)
library(tidycensus)
library(stringr)
library(ggplot2)
library(tidyr)

#load and manipulate covid data
covid <- read.csv("final-project/covid-19/data/covid_counties_date.csv") %>%
  rename(NAME=county)
covid$date <- lubridate::mdy(covid$date)
county <- st_read("final-project/shapefiles/grids/us_county_continental.shp") %>%
  filter(STATEFP=="06")

covid_data <- full_join(covid, county, by = "NAME")
covid_data <- st_as_sf(covid_data)
covid_data <- covid_data %>%
  filter(state=="California")

cal_pop <- get_acs(geography = "county", 
                   variables = "B01003_001",
                   state = "CA",
                   survey = "acs1",
                   year = 2019)
cal_pop$NAME <- str_remove(cal_pop$NAME, " County, California")


covid.shp <- merge(covid_data, cal_pop, by = "NAME")
covid.shp <- covid.shp %>%
  rename(population = estimate) %>%
  select(c(-variable, -moe))
covid.shp$cases_per10k <- (covid.shp$cases/covid.shp$population)*10000
covid.shp$deaths_per10k <- (covid.shp$deaths/covid.shp$population)*10000

#create centroids out of counties
covid.centr <- st_centroid(covid.shp)


#log transform cases and deaths
covid.centr$logcase <- log(covid.centr$cases_per100k)
covid.centr$logdeath <- log(covid.centr$deaths_per100k)


#look at histograms
hist(covid.centr$cases_per100k, xlab = "Cases per 100k Persons", main = "observed", 
     border = "white", col = "royalblue2")
hist(covid.centr$logcase, ylab = "Cases per 100k Persons", main = "empirical logit",
     border= "white", col = "seagreen3")

#convert to sp
covid.centr_sp <- covid.centr %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

plot(covid.centr_sp$geometry)

coords <- st_coordinates(covid.centr_sp)
#ggvario(coords=st_coordinates(covid.centr_sp), data= covid.centr$logcase)

vgm_model <- vgm(psill = 1, model = "Sph", range = max(dist(coords))/3)
emp_vario <- variogram(logcase ~ 1, data = covid.centr, locations = coords, cutoff = max(dist(coords))/3, model = vgm_model, messages = FALSE)

cali_v <- variogram(logcase~1, data=covid.centr_sp)
vgm <- vgm(psill=150, model = "Exp", range = 500, nugget = 50000)
covid.fit <- fit.variogram(cali_v, model=vgm, fit.method=1)
plot(cali_v, covid.fit)
psill <- covid.fit$psill
range <- covid.fit$range
nugget <- covid.fit$nugget


###### account for time
#conver tto spacetime dataframe

covid.centr_sp_pts <- as(covid.centr_sp, "Spatial")
covid.centr_sp_pts <- SpatialPointsDataFrame(coords = coordinates(covid.centr_sp_pts),
                                             data = as.data.frame(covid.centr_sp),
                                             proj4string = CRS(proj4string(covid.centr_sp_pts)))
covid.st <- STFDF(covid.centr_sp_pts, time_grid, data.frame(logcase = covid.complete$logcase))

#### the above not working.. trying to clean the data by making it wide first
covid.wide <- covid.centr %>%
  select(NAME, date, logcase) %>%
  spread(key = date, value = logcase, fill = NA)
library(xts)

# Create a time series object
time.series <- xts(covid.wide[, -1], order.by = as.Date(colnames(covid.wide[, -1])))

# Create the spatio-temporal object
covid.sts <- sts(sp = covid.centr_sp_pts, time.series = time.series)


# Create a data frame with all combinations of spatial locations and time steps
covid.expanded <- merge(covid.centr_sp_pts@data, data.frame(date = time_grid), all = TRUE)

# Merge the expanded dataset with the original dataset to include the logcase values
covid.complete <- merge(covid.expanded, covid.centr, by = c("date", "NAME"), all.x = TRUE)

# Replace NAs in the logcase column with an appropriate value (e.g., the mean of the logcase values)
covid.complete$logcase[is.na(covid.complete$logcase)] <- mean(covid.complete$logcase, na.rm = TRUE)


time_grid <- seq(min(covid.centr$date), max(covid.centr$date), by = "day")
covid.st <- STFDF(covid.centr_sp_pts, time_grid, data.frame(logcase = covid.centr_sp_pts$logcase))

# Extract longitude and latitude from geometry column
coords <- st_coordinates(covid.centr_sp.sf)
covid.centr_sp$longitude <- coords[, 1]
covid.centr_sp$latitude <- coords[, 2]
# define the trend model
trend.model <- lm(logcase ~ longitude + latitude, data = covid.centr_sp)

# Set the extent of the prediction grid
xrange <- c(min(covid.centr_sp$longitude), max(covid.centr_sp$longitude)) # longitude range of California
yrange <- c(min(covid.centr_sp$latitude), max(covid.centr_sp$latitude)) # latitude range of California
n <- 50 # number of grid cells in each direction

# create a prediction grid
grid <- expand.grid(x = seq(from = xrange[1], to = xrange[2], length.out = 50),
                    y = seq(from = yrange[1], to = yrange[2], length.out = 50))

grid <- st_as_sf(grid, coords = c("x", "y"), crs = 4326)

uk <- krige(logcase ~ longitude + latitude, locations = covid.centr_sp, newdata = grid, model = covid.fit, trend.model = trend.model)

# create the gstat object
covid_gstat <- gstat(formula = logcase ~ 1, data = covid.centr_sp,
                     model = covid.fit, beta = coef(trend.model), 
                     dummy = TRUE)

# perform Universal kriging prediction
uk <- predict(covid_gstat, newdata = grid)


# extract the predicted values and coordinates from the UK object
uk_pred <- as.data.frame(cbind(grid, var1.pred = uk$var1.pred))

# plot the predicted values
ggplot() + 
  geom_raster(data = uk_pred, aes(x = x, y = y, fill = var1.pred)) + 
  scale_fill_gradient(low = "white", high = "red") + 
  theme_bw()



#########################################################

# set the extent of the prediction grid
xrange <- c(-125, -114) # longitude range of California
yrange <- c(32, 43) # latitude range of California
n <- 50 # number of grid cells in each direction

# create grid of prediction locations
grid <- st_as_sf(expand.grid(x = seq(from = xrange[1], to = xrange[2], length.out = n),
                             y = seq(from = yrange[1], to = yrange[2], length.out = n)),
                 coords = c("x", "y"), crs = 4326)


# perform ordinary kriging
ok <- krige(logcase ~ 1, locations = covid.centr_sp.sf, newdata = grid, model = vgm)

# plot results
spplot(ok["var1.pred"], main = "Ordinary Kriging of Log(Cases) in California")



#convert to spatialtemporal object
covid.centr_sp$date <- as.POSIXct(covid.centr_sp$date)

# order rows by date variable
covid.centr_sp <- covid.centr_sp[order(covid.centr_sp$date), ]

# create spatial object
covid.centr_sp.sf <- st_as_sf(covid.centr_sp, coords = c("longitude", "latitude"))
covid.centr_sp.sp <- as(covid.centr_sp, "Spatial")


# create spatio-temporal object
covid.centr_sp.st <- STFDF(covid.centr_sp.sf, time = covid.centr_sp.sf$date, endTime = tail(covid.centr_sp$date, 1))

covid.centr_sp.st <- STFDF(covid.centr_sp.sp, time = "date", endTime = tail(covid.centr_sp$date, 1))
covid.centr_sp.st <- STFDF(covid.centr_sp.sp, time = covid.centr_sp$date, endTime = tail(covid.centr_sp$date, 1))

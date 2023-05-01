
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

####rying to clean the data by making it wide first
covid.wide <- covid.centr %>%
  select(NAME, date, logcase) %>%
  spread(key = date, value = logcase, fill = NA)
library(xts)

# Create a time series object
time.series <- xts(covid.wide[, -1], order.by = as.Date(colnames(covid.wide[, -1])))

# Create the spatio-temporal object
covid.sts <- sts(sp = covid.centr_sp_pts, time.series = time.series)


####
covid.centr <- covid.centr %>%
  arrange(NAME, date)

library(dplyr)
library(purrr)
library(xts)


#create selet columns of interest
covid.stdata <- covid.centr %>%
  select(NAME, date, logcase)

# Split the data frame by unique location identifiers
location_data <- split(covid.centr, covid.centr$NAME)

# Create a time series object for each location
time.series <- map(location_data, function(df) {
  xts(df$logcase, order.by = df$date)
})

covid.sts <- STSDF(SpatialObj = covid.centr_sp.sp, time = time_grid, data = do.call(cbind, time.series))
covid.sts <- STSDF(s = covid.centr_sp_pts, t = time_grid, data = do.call(cbind, time.series))
covid.stidf <- STIDF(s = covid.centr_sp_pts, t = covid.stdata$date, data = covid.stdata$logcase)



###### using gstat to create separate variograms for each date

#breaking into weeks since daily is too intensive

# Aggregate data by week
covid.weekly <- covid.centr %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(NAME, week) %>%
  summarize(logcase = mean(logcase), date = min(date)) %>%
  ungroup()

# Replace 'covid.centr' with 'covid.weekly' in the subsequent analysis


# Create an empty list to store variograms
variograms <- list()

# Loop over each unique date
for (date in unique(covid.weekly$date)) {
  # Filter the data for the current date
  covid.date <- filter(covid.weekly, date == date)%>%
  print()
  # Compute the variogram for the current date
  variograms[[as.character(date)]] <- variogram(logcase ~ 1, data = covid.date)
}


# Convert the numeric names to proper date format
variogram_dates <- as.Date(as.numeric(names(variograms)), origin = "1970-01-01")

# Assign the correct date names to the variograms list
names(variograms) <- variogram_dates

# Check the updated names
head(names(variograms))


# Initialize lists to store variogram parameters and fitted models
nuggets <- numeric()
psills <- numeric()
ranges <- numeric()
dates <- character()
fitted_variograms <- list()

# Loop over each variogram
for (date in names(variograms)) {
  # Fit a variogram model using fitted parameters
  vgm_fit <- fit.variogram(variograms[[date]], model = vgm(psill = 1.129079, model = "Sph", range = 292.0991, nugget = 5.051748), fit.method = 2)
  
  # Store the variogram parameters
  nuggets <- c(nuggets, vgm_fit$nugget)
  psills <- c(psills, vgm_fit$psill)
  ranges <- c(ranges, vgm_fit$range)
  dates <- c(dates, date)
  
  # Save the fitted variogram model for each time step
  fitted_variograms[[date]] <- vgm_fit
}

# Create a data frame with the variogram parameters
variogram.params <- data.frame(
  date = as.Date(dates),
  nugget = nuggets,
  psill = psills,
  range = ranges
)

#plot the nugget, sill and range

# Plot the nugget over time
ggplot(variogram.params, aes(x = date, y = nugget)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Nugget over time",
       x = "Date",
       y = "Nugget")

# Plot the partial sill over time
ggplot(variogram.params, aes(x = date, y = psill)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Partial sill over time",
       x = "Date",
       y = "Partial sill")

# Plot the range over time
ggplot(variogram.params, aes(x = date, y = range)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Range over time",
       x = "Date",
       y = "Range")



library(gstat)

# Create spatio-temporal data frame
data.st <- st(as.data.frame(covid.weekly), coords=c("lon", "lat", "date"))

# Define external drift model
ed_model <- lm(logcase ~ week, data = covid.weekly)

# Create KED model
ked_model <- gstat(formula = logcase ~ 1, locations = ~lon+lat+date, data = data.st, model = ed_model)

# Predict using KED model
predictions <- predict(ked_model, newdata = data.st)


###################################
#animations
#ca_county as sf
covid.weekly_sf <- st_as_sf(covid.weekly)


# create base map plot
covid.weekly_sf <- st_set_crs(covid.weekly_sf, 4326)
base_plot <- ggplot() + 
  geom_sf(data = covid.weekly_sf, aes(geometry = geometry), fill = "white", color = "gray", size = 0.1) +
  theme_void()
base_plot <- st_set_crs(base_plot, 4326)

covid.weekly_sf <- st_transform(covid.weekly_sf, st_crs(shapefile))

# create animation
animation <- base_plot + 
  geom_sf(data = covid.weekly_sf, aes(fill = logcase), size = 0.1) +
  scale_fill_gradient(low = "white", high = "red") +
  transition_time(as.POSIXct(as.character(date))) +
  labs(title = "Date: {frame_time}") +
  ease_aes('linear') +
  view_follow(fixed_y = TRUE) +
  coord_sf(xlim = st_bbox(covid.weekly_sf)$xmin,
           ylim = st_bbox(covid.weekly_sf)$ymin) +
  theme(legend.position = "bottom") + 
  coord_sf(xlim = c(st_bbox(covid.weekly_sf)[1], st_bbox(covid.weekly_sf)[3]), 
           ylim = c(st_bbox(covid.weekly_sf)[2], st_bbox(covid.weekly_sf)[4]))
animate(animation, nframes = 200, width = 800, height = 600, res = 150)


# save animation as GIF
bbox <- st_bbox(covid.weekly_sf)
animate(animation, nframes = 200, width = 800, height = 600, res = 150)

library(plotly)
animation_gg <- ggplotly(ggplotly(animation))
animation_gg


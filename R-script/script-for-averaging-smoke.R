###creating daily average annual smoke

smokedata <- read.csv('data/smoke_daily_10km.csv')
head(smokedata)
summary(smokedata) ##median smokePM 3.48, mean 5.69, 3rd Qu 6.135

smokedata$date <- lubridate::ymd(smokedata$date) #convert date column to appropriate format

# group the data by date and calculate the mean value for each date
daily_averages <- aggregate(smokedata$smokePM_pred, by = list(smokedata$date), mean)

# rename the columns
colnames(daily_averages) <- c("date", "smoke_daily_average")
head(daily_averages)

#append the daily average back to smokedata
#######################################################
##all that data manipulation did not average each grid!########

library(dplyr)
library(tidyr)

##grouped by grid

# group the data by identifier and date columns
grouped_df <- smokedata %>% 
  group_by(grid_id_10km,date)
head(grouped_df)

# calculate the mean of the value column for each group
daily_average <- grouped_df %>% 
  summarize(mean = mean(smokePM_pred))
summarize(daily_average)


write.csv(daily_average, 'daily_average_smokePM-pred.csv')




dly.avg.smoke <- read.csv('daily_average_smokePM-pred.csv')


###############e
#exploring

#add daily average back to smokedata

smokedata_merge <- merge(smokedata, daily_average, by="grid_id_10km")

smokedata <- left_join(smokedata, daily_average, by = "grid_id_10km")

smokedata <- smokedata %>% group_by(year %/% 5) %>% mutate(year_group=min(year)+5*(year %/% 5))

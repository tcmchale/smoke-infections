

avgsmoke.2006 <- daily_average %>% 
  group_by(grid_id_10km) %>%
  filter(date<20061231)
write.csv(avgsmoke.2006, 'avgsmoke-2006.csv')

avgsmoke.2007 <- daily_average %>% 
  group_by(grid_id_10km) %>%
  filter(date<20071231, date>20070101)
write.csv(avgsmoke.2007, 'avgsmoke-2007.csv')

avgsmoke.2008 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20081231, date>20080101)
write.csv(avgsmoke.2008, 'avgsmoke-2008.csv')

avgsmoke.2009 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20091231, date>20090101)
write.csv(avgsmoke.2009, 'avgsmoke-2009.csv')

avgsmoke.2010 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20101231, date>20100101)
write.csv(avgsmoke.2010, 'avgsmoke-2010.csv')

avgsmoke.2011 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20111231, date>20110101)
write.csv(avgsmoke.2011, 'avgsmoke-2011.csv')

avgsmoke.2012 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20121231, date>20120101)
write.csv(avgsmoke.2012, 'avgsmoke-2012.csv')

avgsmoke.2013 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20131231, date>20130101)
write.csv(avgsmoke.2013, 'avgsmoke-2013.csv')

avgsmoke.2014 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20141231, date>20140101) 
write.csv(avgsmoke.2014, 'avgsmoke-2014.csv')

avgsmoke.2015 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20151231, date>20150101)
write.csv(avgsmoke.2015, 'avgsmoke-2015.csv')

avgsmoke.2016 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20161231, date>20160101)
write.csv(avgsmoke.2016, 'avgsmoke-2016.csv')

avgsmoke.2017 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20171231, date>20170101)
write.csv(avgsmoke.2017, 'avgsmoke-2017.csv')

avgsmoke.2018 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20181231, date>20180101)
write.csv(avgsmoke.2018, 'avgsmoke-2018.csv')

avgsmoke.2019 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20191231, date>20190101)
write.csv(avgsmoke.2019, 'avgsmoke-2019.csv')

avgsmoke.2020 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20201231, date>20200101)
write.csv(avgsmoke.2020, 'avgsmoke-2020.csv')

avgsmoke.2021 <- daily_average %>% group_by(grid_id_10km) %>% filter(date<20211231, date>20210101)
write.csv(avgsmoke.2021, 'avgsmoke-2021.csv')


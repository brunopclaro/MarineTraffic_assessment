      
#Reading data

mtraffic <- read.csv2("/mtraffic.csv")

#Converting time data

mtraffic$TIMESTAMP<- as.POSIXct(mtraffic$TIMESTAMP)

mtraffic$Time<-format(as.POSIXct(mtraffic$TIMESTAMP, format= "%m/%d/%Y %H:%M:%S"),
                            format="%H:%M:%S")

mtraffic$TIMESTAMP<- ymd_hms(mtraffic$TIMESTAMP)

mtraffic$day <- factor(day(mtraffic$TIMESTAMP))
mtraffic$month <- factor(month(mtraffic$TIMESTAMP, label=TRUE))
mtraffic$year <- factor(year(mtraffic$TIMESTAMP))
mtraffic$dayofweek <- factor(wday(mtraffic$TIMESTAMP,label = TRUE))

mtraffic$hour <- factor(hour(hms(mtraffic$Time)))
mtraffic$minute <- factor(minute(hms(mtraffic$Time)))
mtraffic$second <- factor(second(hms(mtraffic$Time)))


#Plotting the trips by hour

hour_traffic <- mtraffic %>%
  group_by(hour) %>%
  dplyr::summarize(Total=n())

datatable(hour_traffic)


ggplot(hour_traffic, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- mtraffic %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

#Plotting trips by day of month

day_group <- mtraffic %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


day_month_group <- mtraffic %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

datatable(day_month_group)


ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma)

#Plotting trips each montrh

month_group <- mtraffic %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 

datatable(month_group)

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


month_weekday <- mtraffic %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma)

#HeatMap by day, week and month

day_and_hour <- mtraffic %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")


ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")





#Create Map visualization of marine traffic speed in area

min_lat <- 42.60
max_lat <- 43.75
min_long<-5.750
max_long<-7.750


traffic_speed <- mtraffic %>%
  group_by(MMSI) %>%
  filter (SPEED > 0 & SPEED <=15)
  
SPEED = traffic_speed$SPEED
  

ggplot(traffic_speed, aes(x=LON, y=LAT,color=SPEED))+
         geom_point(size=0.3)+ scale_x_continuous(limits=c(min_long, max_long)) +
         scale_y_continuous(limits=c(min_lat, max_lat)) +
          theme_map() + ggtitle("MARINE TRAFFIC SPEED MAP BASED ON CARGO DATA DURING 2011")










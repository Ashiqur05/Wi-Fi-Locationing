
library(caret)       #R modeling workhorse & ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
wifi_data_vis <- read.csv(file="data/prepreocessed/FilteredWifiData.csv", header = TRUE )

## Total Number of Observations in each Floor
wifi_data_vis %>% 
  group_by((BUILDINGID)) %>% 
  ggplot(aes(FLOOR, y =X)) +
  geom_col(fill = "brown") +
  labs(title = "Total Number of Observations in Each Building & Floor") + 
  ylab("Number of Observations") + 
  xlab("FLOOR") + 
  facet_wrap(~BUILDINGID)
## Building 0 Analysis
wifi_data_vis %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

wifi_data_vis %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

## Building 1 Analysis
wifi_data_vis %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

wifi_data_vis %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

## Building 2 Analysis
wifi_data_vis %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

wifi_data_vis %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)
# What happens if we plot longitude to latitude?
plot(wifi_data_vis$LONGITUDE, wifi_data_vis$LATITUDE)

wifi_data_vis %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "brown") +
  labs(title = "Training Data Longitude vs Latitude")

# Building 0 Preview
building1<-wifi_data_vis %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~FLOOR, 
          colors = c("green", "orange", "yellow", "pink", "purple"))
add_markers(building1) %>%
  layout(title = "Building 0 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))
# Building 1 Preview
wifi_data_vis %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~FLOOR, 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 1 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 2 Preview
wifi_data_vis %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~FLOOR, 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 2 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))
## Users' Behaviors Visualization

# Building 0 
building1<-wifi_data_vis %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~USERID, 
          colors = c("green", "orange", "yellow", "pink", "purple"))
add_markers(building1) %>%
  layout(title = "User Behavior Building 0",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 1
wifi_data_vis %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~USERID) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 1",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 2
wifi_data_vis %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~USERID) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 2",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))
## Phone ID,spaceID Patterns
# Building 0 
building1<-wifi_data_vis %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~FLOOR, 
          color = ~PHONEID, #PHONEID
          colors = c("green", "orange", "yellow", "pink", "purple"))
add_markers(building1) %>%
  layout(title = "User Behavior Building 0",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Check distribution of signal strength

# traning data
x <- wifi_data_zero_V[,1:465]
x <- stack(x)
x <- x[-grep(-105, x$values),]
hist(x$values, xlab = "WAP strength", 
     main = "Distribution of WAPs signal stength (Training set)", 
     col = "red", breaks = 150)

# test data
y <- wifi_validation_zero_V[,1:367]
y <- stack(y)
y <- y[-grep(-105, y$values),]
hist(y$values, xlab = "WAP strength", 
     main = "Distribution of WAPs signal stength (Test set)", 
     col = "blue",
     breaks = 150)

ggplot() +
  geom_histogram(data = x, aes(values), fill = "red", alpha = 1, binwidth = 5) +
  geom_histogram(data = y, aes(values), fill = "blue", alpha = 1, binwidth = 5) +
  ggtitle("Distribution of WAPs signal strength (Training and Test sets)") +
  xlab("WAP strength")

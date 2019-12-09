
library(caret)       #R modeling workhorse & ggplot2)
library(tidyr)
library(dplyr)
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


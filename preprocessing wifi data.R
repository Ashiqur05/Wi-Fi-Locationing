
# load and preprocessing the data,converting data type,check missing value,erase duplicate rows,change floor level name, change the range of RSSI signal.



# Load packages
library(dplyr)


#  Load Data Set
# Import training dataset
wifi_data <- read.csv(file="data/UJIndoorLoc/trainingData.csv", header = TRUE )

# Import validation dataset
wifi_data_validation <- read.csv(file="data/UJIndoorLoc/validationData.csv", header = TRUE )

# 




head(wifi_data[,520:529],2)

summary(wifi_data[,510:529])

str(wifi_data[,510:529])
str(wifi_data[,500:529])
summary(wifi_data)

#########################################Data cleaning##############################################
sum(is.na(wifi_data))
# To check the duplicate value 
head(which(duplicated(wifi_data)),2)
# remove duplicate rows
wifi_data<-wifi_data[!duplicated(wifi_data),]

############# converting data type###############

wifi_data$FLOOR <- as.factor(wifi_data$FLOOR)
wifi_data$BUILDINGID <- as.factor(wifi_data$BUILDINGID)
wifi_data$RELATIVEPOSITION <- as.factor(wifi_data$RELATIVEPOSITION)
wifi_data$USERID <- as.factor(wifi_data$USERID)
wifi_data$PHONEID <- as.factor(wifi_data$PHONEID)

str(wifi_data[,521:529])


# Remove columns (WAP) where all the values = 100 (WAP was not detected)

uniquelength <- sapply(wifi_data,function(x) length(unique(x)))
wifi_data_u <- subset(wifi_data, select=uniquelength>1)

keep <- apply(wifi_data_u[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
wifi_data_u[keep, ]
dim(wifi_data_u)

#-Recode floor factor level names
wifi_data_u$FLOOR <- recode(wifi_data$FLOOR, '0'=1, '1'=2, '2'=3, '3'=4, '4'=5)
#Replaced the rest of the 100 ( Waps without signal) by -105 to not confuse the model, and added 105 to all Waps signal. Hence, no signal will be = 0 , and max signal = +104
wifi_data_u[wifi_data_u == 100] <- -105

wifi_data_u[,c(1:465)]<-wifi_data_u[,c(1:465)] +105
summary(wifi_data_u)

write.csv(wifi_data_u, file = "FilteredWifiData.csv")


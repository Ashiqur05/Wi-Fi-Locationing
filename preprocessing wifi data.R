
#
# load and preprocessing the data,converting data type,check missing value,erase duplicate rows,change floor level name, change the range of RSSI signal.



# Load packages
library(dplyr)


#  Load Data Set
# Import training dataset
wifi_data <- read.csv(file="data/UJIndoorLoc/trainingData.csv", header = TRUE )

# Import validation dataset
wifi_data_validation <- read.csv(file="data/UJIndoorLoc/validationData.csv", header = TRUE )

#Combain two dataset
wifi_data$origin <- "train"
wifi_data_validation$origin <- "validation"
wifi_complete <- rbind(wifi_data, wifi_data_validation)

# Initial exploration
dim(wifi_data)  # 19937 530
dim(wifi_data_validation)  # 1111  530    we have same number of variables in training and validation data set

head(wifi_data[,520:529],2)
summary(wifi_data[,510:529])
str(wifi_data[,510:529])


head(wifi_data_validation[,520:529],2)
summary(wifi_data_validation[,510:529])
str(wifi_data_validation[,510:529])

names(wifi_data)#all attribute name
head(wifi_data, 5)[1:8]
str(wifi_data, list.len=3)

#########################################Data cleaning##############################################

# Check for missing values
sum(is.na(wifi_data_validation)) # no missing values
sum(is.na(wifi_data))# no missing values

# To check the duplicate value 
head(which(duplicated(wifi_data)),2)
# remove duplicate rows
wifi_data<-wifi_data[!duplicated(wifi_data),]

head(which(duplicated(wifi_data_validation)),2)## no duplicated rows in validation data set

#---------------------------------------------------------------------------------------------------------
#Another way to check duplicate value
#count(distinct(wifi_data))   # from 19937 rows 19300 are distict, we have 637 duplicated rows 
#count(distinct(wifi_data_validation))   # no duplicated rows in validation data set
#wifi_data <- distinct(wifi_data)
#---------------------------------------------------------------------------------------------------------

#-Recode floor factor level names(wifi-data)
wifi_data$FLOOR <- recode(wifi_data$FLOOR, '0'=1, '1'=2, '2'=3, '3'=4, '4'=5)
wifi_data$BUILDINGID <- recode(wifi_data$BUILDINGID, '0'=1, '1'=2, '2'=3)

#-Recode floor factor level names(wifi-data Validation)
wifi_data_validation$FLOOR <- recode(wifi_data_validation$FLOOR, '0'=1, '1'=2, '2'=3, '3'=4, '4'=5)
wifi_data_validation$BUILDINGID <- recode(wifi_data_validation$BUILDINGID, '0'=1, '1'=2, '2'=3)

# Add ID column
#wifi_data$ID <- paste(wifi_data$BUILDINGID,wifi_data$FLOOR,sep = "B_f")

#-Convert ID variable to categorical
#wifi_data$ID <- as.factor(wifi_data$ID)

############# converting data type###############

#Wifi validation data
wifi_data_validation$FLOOR <- as.factor(wifi_data_validation$FLOOR)
wifi_data_validation$BUILDINGID <- as.factor(wifi_data_validation$BUILDINGID)
wifi_data_validation$RELATIVEPOSITION <- wifi_data_validation(wifi_data$RELATIVEPOSITION)
wifi_data_validation$USERID <- as.factor(wifi_data_validation$USERID)
wifi_data_validation$PHONEID <- as.factor(wifi_data_validation$PHONEID)

#Wifi data
wifi_data$FLOOR <- as.factor(wifi_data$FLOOR)
wifi_data$BUILDINGID <- as.factor(wifi_data$BUILDINGID)
wifi_data$RELATIVEPOSITION <- as.factor(wifi_data$RELATIVEPOSITION)
wifi_data$USERID <- as.factor(wifi_data$USERID)
wifi_data$PHONEID <- as.factor(wifi_data$PHONEID)


## Convert Unix Time
wifi_data$TIMESTAMP <- as.POSIXct((wifi_data$TIMESTAMP),
                                     origin="1970-01-01", 
                                     tz="GMT")

wifi_data_validation$TIMESTAMP <- as.POSIXct((wifi_data_validation$TIMESTAMP),
                                  origin="1970-01-01", 
                                  tz="GMT")



#wifi_data_30<-wifi_data
#wifi_replace<-wifi_data_30[,1:520]# we do replacing only in WAPS columns
#wifi_replace[wifi_replace> -30&wifi_replace!=100]<- -30
#wifi_data_30<-cbind(wifi_replace,wifi_data_30[,521:530])

# Remove columns (WAP) where all the values = 100 (WAP was not detected)
#----------------------------------------------------------------------------------------------------------
uniquelength <- sapply(wifi_data,function(x) length(unique(x)))
wifi_data_u <- subset(wifi_data, select=uniquelength>1)
keep <- apply(wifi_data_u[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
wifi_data_u[keep, ]
dim(wifi_data_u)
#----------------------------------------------------------------------------------------------------------


# check for zero variance and remove columns with zero variance from wifi_data
wifi_data_zero_V <- wifi_data[ -which(apply(wifi_data, 2, var) == 0 )] 
dim(wifi_data_zero_V)
which(apply(wifi_data_zero_V, 2, var) == 0)
dim(wifi_data_zero_V)# after removing zero variance rows and columns we have 19300 rows and 474 columns

wifi_validation_zero_V <- wifi_data_validation[ -which(apply(wifi_data_validation, 2, var) == 0 )] 
dim(wifi_validation_zero_V)
which(apply(wifi_validation_zero_V, 2, var) == 0)
dim(wifi_validation_zero_V)#no variance

#Replaced the rest of the 100 ( Waps without signal cz 100 can be calculated good as signal) 
wifi_data_zero_V[wifi_data_zero_V == 100] <- -105
wifi_validation_zero_V[wifi_validation_zero_V == 100] <- -105

#Move the info to the front and gather the data
wifi_Data_Gathered <- wifi_data_zero_V[ , c((ncol(wifi_data_zero_V)-8):(ncol(wifi_data_zero_V)), 1:(ncol(wifi_data_zero_V)-9))]
wifi_Data_Gathered <- gather(wifi_Data_Gathered, WAP, RSSI, 10:ncol(wifi_Data_Gathered))
summary(wifi_Data_Gathered$RSSI)

wifi_Data_Gathered %>% filter(RSSI>=-30 & RSSI!=0)

# Filter good signal streght

wifi_Data_Gathered <- filter(wifi_Data_Gathered, RSSI <= -30 & RSSI >= -70)

wifi_Data_Gathered %>% 
  group_by(BUILDINGID, FLOOR) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID)) +
  geom_point() +
  labs(title = "Training Data Longitude vs Latitude")



write.csv(wifi_data_u, file = "FilteredWifiData.csv")



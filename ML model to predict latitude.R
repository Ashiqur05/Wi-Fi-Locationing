library(caret)
library(kernlab)

dim(wifi_data_zero_V)
dim(wifi_validation_zero_V)

wifi_data_zero_V_for_lat <- subset(wifi_data_zero_V, 
                                     select = -c(USERID, 
                                                 PHONEID, 
                                                 TIMESTAMP,
                                                 LONGITUDE,
                                                 BUILDINGID,
                                                 FLOOR))

wifi_validation_zero_V_for_lat <- subset(wifi_validation_zero_V, 
                                           select = -c(PHONEID, 
                                                       TIMESTAMP,
                                                       LONGITUDE,
                                                       BUILDINGID,
                                                       FLOOR))

wifi_data_traning_pre_latitude<-wifi_data_zero_V_for_lat
wifi_data_validation_pre_latitude<-wifi_validation_zero_V_for_lat

# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning_pre_latitude),colnames(wifi_data_validation_pre_latitude))
wifi_data_traning_pre_latitude<-wifi_data_traning_pre_latitude[,cols_to_keep,drop=FALSE]

wifi_data_validation_pre_latitude<-wifi_data_validation_pre_latitude[,cols_to_keep,drop=FALSE]

#split the data into training and testing set

set.seed(123)
trainIndex <- createDataPartition(y = wifi_data_traning_pre_latitude$LATITUDE, p = 0.75,
                                  list = FALSE)

# Training and Test sets 

trainSet <- wifi_data_traning_pre_latitude [trainIndex,]
testSet <- wifi_data_traning_pre_latitude [-trainIndex,]

set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

############################################## KNN #########################################################
knnFit_lat <- train(( LATITUDE~ .), data = trainSet, 
                    method = "knn", 
                    trControl = ctrl, 
                    tuneLength = 2)
knnFit_lat
# test the k-NN model
knnPredict_lat <- predict(knnFit_lat,newdata = testSet)
postResample(knnPredict_lat, testSet$LATITUDE)
knnPredict_val_lat <- predict(knnFit_lat,newdata = wifi_data_validation_pre_latitude)
postResample(knnPredict_val_lat, wifi_data_validation_pre_latitude$LATITUDE)
############################################## KNN #########################################################

################################################# SVM ######################################################
set.seed(123)
SampleTrain_latitude <- trainSet[sample(1:nrow(trainSet), 1000, replace = F),]

SvmFit_lat <- train(( LATITUDE~ .), data = SampleTrain_latitude, 
                    method = "svmLinear", 
                    trControl = ctrl, 
                    tuneLength = 2)
SvmFit_lat
# test the k-NN model
svmPredict_lat <- predict(SvmFit_lat,newdata = testSet)
postResample(svmPredict_lat, testSet$LATITUDE)
svmPredict_val_lat <- predict(SvmFit_lat,newdata = wifi_data_validation_pre_latitude)
postResample(svmPredict_val_lat, wifi_data_validation_pre_latitude$LATITUDE)
################################################# SVM ###########################################################


################################################# RENDOM FOREST ############################################################
set.seed(123)
SampleTrain_latitude <- trainSet[sample(1:nrow(trainSet), 1000, replace = F),]

RfFit_lat <- train(( LATITUDE~ .), data = SampleTrain_latitude, 
                    method = "ranger", 
                    trControl = ctrl, 
                    tuneLength = 2)
RfFit_lat
# test the k-NN model
rfPredict_lat <- predict(RfFit_lat,newdata = testSet)
postResample(rfPredict_lat, testSet$LATITUDE)
rfPredict_val_lat <- predict(RfFit_lat,newdata = wifi_data_validation_pre_latitude)
postResample(rfPredict_val_lat, wifi_data_validation_pre_latitude$LATITUDE)

################################################# RENDOM FOREST ############################################################

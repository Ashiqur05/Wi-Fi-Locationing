library(caret)
library(kernlab)

dim(wifi_data_zero_V)
dim(wifi_validation_zero_V)

wifi_data_zero_V_for_long <- subset(wifi_data_zero_V, 
                                   select = -c(USERID, 
                                               PHONEID, 
                                               TIMESTAMP,
                                               LATITUDE,
                                               BUILDINGID,
                                               FLOOR))

wifi_validation_zero_V_for_long <- subset(wifi_validation_zero_V, 
                                         select = -c(PHONEID, 
                                                     TIMESTAMP,
                                                     LATITUDE,
                                                     BUILDINGID,
                                                     FLOOR))

wifi_data_traning_pre_longitude<-wifi_data_zero_V_for_long
wifi_data_validation_pre_longitude<-wifi_validation_zero_V_for_long

# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning_pre_longitude),colnames(wifi_data_validation_pre_longitude))
wifi_data_traning_pre_longitude<-wifi_data_traning_pre_longitude[,cols_to_keep,drop=FALSE]

wifi_data_validation_pre_longitude<-wifi_data_validation_pre_longitude[,cols_to_keep,drop=FALSE]

#split the data into training and testing set

set.seed(123)
trainIndex <- createDataPartition(y = wifi_data_traning_pre_longitude$LONGITUDE, p = 0.75,
                                  list = FALSE)

# Training and Test sets 

trainSet <- wifi_data_traning_pre_longitude [trainIndex,]
testSet <- wifi_data_traning_pre_longitude [-trainIndex,]

set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

############################################## KNN #########################################################
set.seed(123)
SampleTrain_longitude <- trainSet[sample(1:nrow(trainSet), 10000, replace = F),]
knnFit_long <- train(( LONGITUDE~ .), data = SampleTrain_longitude, 
                    method = "knn", 
                    trControl = ctrl, 
                    tuneLength = 2)
knnFit_long
# test the k-NN model
knnPredict_long <- predict(knnFit_long,newdata = testSet)
postResample(knnPredict_long, testSet$LONGITUDE)
knnPredict_val_long <- predict(knnFit_long,newdata = wifi_data_validation_pre_longitude)
postResample(knnPredict_val_long, wifi_data_validation_pre_longitude$LONGITUDE)
############################################## KNN #########################################################

################################################# SVM ######################################################
set.seed(123)
SampleTrain_longitude <- trainSet[sample(1:nrow(trainSet), 1000, replace = F),]

SvmFit_long <- train(( LONGITUDE~ .), data = SampleTrain_longitude, 
                    method = "svmLinear", 
                    trControl = ctrl, 
                    tuneLength = 2)
SvmFit_long
# test the k-NN model
svmPredict_long <- predict(SvmFit_long,newdata = testSet)
postResample(svmPredict_long, testSet$LONGITUDE)
svmPredict_val_long <- predict(SvmFit_long,newdata = wifi_data_validation_pre_longitude)
postResample(svmPredict_val_long, wifi_data_validation_pre_longitude$LONGITUDE)
################################################# SVM ###########################################################


################################################# RENDOM FOREST ############################################################
set.seed(123)
SampleTrain_longitude <- trainSet[sample(1:nrow(trainSet), 1000, replace = F),]

RfFit_long <- train(( LONGITUDE~ .), data = SampleTrain_longitude, 
                   method = "ranger", 
                   trControl = ctrl, 
                   tuneLength = 2)
RfFit_long
# test the k-NN model
rfPredict_long <- predict(RfFit_long,newdata = testSet)
postResample(rfPredict_long, testSet$LONGITUDE)
rfPredict_val_long <- predict(RfFit_long,newdata = wifi_data_validation_pre_longitude)
postResample(rfPredict_val_long, wifi_data_validation_pre_longitude$LONGITUDE)

################################################# RENDOM FOREST ############################################################

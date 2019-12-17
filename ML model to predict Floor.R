library(caret)
library(kernlab)


dim(wifi_data_zero_V)
dim(wifi_validation_zero_V)

#wifi_data_traning<-wifi_data_zero_V[,1:469]
#wifi_data_validation<-wifi_validation_zero_V

wifi_data_zero_V_for_Floor <- subset(wifi_data_zero_V, 
                                     select = -c(USERID, 
                                                 PHONEID, 
                                                 TIMESTAMP,
                                                 LONGITUDE,
                                                 LATITUDE,
                                                 BUILDINGID))

wifi_validation_zero_V_for_Floor <- subset(wifi_validation_zero_V, 
                                           select = -c(PHONEID, 
                                                       TIMESTAMP,
                                                       LONGITUDE,
                                                       LATITUDE,
                                                       BUILDINGID))

wifi_data_traning_pre_Floor<-wifi_data_zero_V_for_Floor
wifi_data_validation_pre_Floor<-wifi_validation_zero_V_for_Floor



# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning_pre_Floor),colnames(wifi_data_validation_pre_Floor))
wifi_data_traning_pre_Floor<-wifi_data_traning_pre_Floor[,cols_to_keep,drop=FALSE]

wifi_data_validation_pre_Floor<-wifi_data_validation_pre_Floor[,cols_to_keep,drop=FALSE]

#split the data into training and testing set

set.seed(123)
trainIndex <- createDataPartition(y = wifi_data_traning_pre_Floor$FLOOR, p = 0.75,
                                  list = FALSE)

# Training and Test sets 

trainSet <- wifi_data_traning_pre_Floor [trainIndex,]
testSet <- wifi_data_traning_pre_Floor [-trainIndex,]

###############################################  KNN  #####################################################
#trainSet_sample <- trainSet[sample(1:nrow(trainSet), 3000, replace=FALSE),]

set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

knnFit_F <- train((FLOOR ~ .), data = trainSet, method = "knn", trControl = ctrl, tuneLength = 2)
#Output of kNN fit
knnFit_F
#This model seems to be really good as  accuracy and kappa  are almost 1

# test the k-NN model
knnPredict_floor <- predict(knnFit_F,newdata = testSet)
# confusion matrix to see accuracy value and other parameter values
knnCM_F <-confusionMatrix(knnPredict_floor, testSet$FLOOR)
knnCM_F
# Apply k-NN model to the validation data
knnPredict_validation_F <- predict(knnFit_F,newdata = wifi_data_validation_pre_Floor)
validation_knnCM_F<-confusionMatrix(knnPredict_validation_F,wifi_data_validation_pre_Floor$FLOOR)
postResample(knnPredict_validation_F, wifi_data_validation_pre_Floor$FLOOR)
# prediction Accuracy : 0.9107
############################################## KNN #########################################################

############################################### SVM #######################################################################

set.seed(123)
ctrl <- trainControl(method="cv",number = 5) 
SVMFit_F <- train((FLOOR ~ .), data = trainSet, 
                  method = "svmLinear", 
                  trControl = ctrl, 
                  tuneLength = 2,
                  preProcess="zv")
SVMFit_F
SVMPredict_floor <- predict(SVMFit_F,newdata = testSet)
SVMCM <-confusionMatrix(SVMPredict_floor, testSet$FLOOR)
postResample(SVMPredict_floor, testSet$FLOOR)
SVMPredict_validation_F <- predict(SVMFit_F,newdata = wifi_data_validation_pre_Floor)
validation_SVMCM_F<-confusionMatrix(SVMPredict_validation_F,wifi_data_validation_pre_Floor$FLOOR)
postResample(SVMPredict_validation_F, wifi_data_validation_pre_Floor$FLOOR)

############################################### SVM #######################################################################

################################################# RENDOM FOREST ############################################################
set.seed(123)
SampleTrainFloor <- trainSet[sample(1:nrow(trainSet), 1000, replace = F),]

RFGrid <- expand.grid(mtry=c(1:5))
ctrl <- trainControl(method="cv",number = 5,repeats = 2) 
RFFit_F <- train((FLOOR ~ .), data = trainSet, 
                 method = "ranger", 
                 trControl = ctrl, 
                 tuneLength = 2,
                 #tuneGrid=RFGrid,
                 preProcess="zv")
RFFit_F
RFPredict_Floor <- predict(RFFit_F,newdata = testSet)#test set is more then the training set
postResample(RFPredict_Floor, testSet$FLOOR)
RFCM_F <-confusionMatrix(RFPredict_Floor, testSet$FLOOR)
RFPredict_validation_F <- predict(RFFit_F,newdata = wifi_data_validation_pre_Floor)
validation_RFCM_F<-confusionMatrix(RFPredict_validation_F,wifi_data_validation_pre_Floor$FLOOR)
postResample(RFPredict_validation_F, wifi_data_validation_pre_Floor$FLOOR)
#################################################  RENDOM FOREST ############################################################
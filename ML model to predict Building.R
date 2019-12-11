library(caret)
library(kernlab)


dim(wifi_data_zero_V)
dim(wifi_validation_zero_V)

wifi_data_traning<-wifi_data_zero_V[,1:469]
wifi_data_validation<-wifi_validation_zero_V

# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning),colnames(wifi_data_validation))
wifi_data_traning<-wifi_data_traning[,cols_to_keep,drop=FALSE]

wifi_data_validation<-wifi_data_validation[,cols_to_keep,drop=FALSE]

#split the data into training and testing set

set.seed(123)
trainIndex <- createDataPartition(y = wifi_data_traning$BUILDINGID, p = 0.75,
                                  list = FALSE)

# Training and Test sets 

trainSet <- wifi_data_traning [trainIndex,]
testSet <- wifi_data_traning [-trainIndex,]

###############################################  KNN  #####################################################
#trainSet_sample <- trainSet[sample(1:nrow(trainSet), 3000, replace=FALSE),]

set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

knnFit <- train((BUILDINGID ~ .), data = trainSet, method = "knn", trControl = ctrl, tuneLength = 2)
#Output of kNN fit
knnFit

# test the k-NN model
knnPredict_building <- predict(knnFit,newdata = testSet)
# confusion matrix to see accuracy value and other parameter values
knnCM <-confusionMatrix(knnPredict_building, testSet$BUILDINGID)

# Apply k-NN model to the validation data
knnPredict_validation <- predict(knnFit,newdata = wifi_data_validation)
validation_knnCM<-confusionMatrix(knnPredict_validation,wifi_data_validation$BUILDINGID)
#This model seems to be really good as we get accuracy and kappa is equal to one 
############################################## KNN #########################################################


############################################### SVM #######################################################################

set.seed(123)
ctrl <- trainControl(method="cv",number = 5) 
SVMFit <- train((BUILDINGID ~ .), data = trainSet, method = "svmLinear", trControl = ctrl, tuneLength = 2,preProcess="zv")
SVMFit
SVMPredict_building <- predict(SVMFit,newdata = testSet)
SVMCM <-confusionMatrix(SVMPredict_building, testSet$BUILDINGID)
SVMPredict_validation <- predict(SVMFit,newdata = wifi_data_validation)
validation_SVMCM<-confusionMatrix(SVMPredict_validation,wifi_data_validation$BUILDINGID)

################################################# SVM ######################################################################

################################################# RENDOM FOREST ############################################################
set.seed(123)
SampleTrainBuilding <- trainSet[sample(1:nrow(trainSet), 1000, replace = F),]

RFGrid <- expand.grid(mtry=c(1:5))
ctrl <- trainControl(method="cv",number = 5,repeats = 2) 
RFFit <- train((BUILDINGID ~ .), data = SampleTrainBuilding, method = "rf", trControl = ctrl, tuneLength = 2,tuneGrid=RFGrid,preProcess="zv")
RFFit
RFPredict_building <- predict(RFFit,newdata = testSet)#test set is more then the training set
RFCM <-confusionMatrix(RFPredict_building, testSet$BUILDINGID)
RFPredict_validation <- predict(RFFit,newdata = wifi_data_validation)
validation_RFCM<-confusionMatrix(RFPredict_validation,wifi_data_validation$BUILDINGID)
#################################################  RENDOM FOREST ############################################################
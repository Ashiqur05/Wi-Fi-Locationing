 

############################################################## BUILDING 1 #####################################################

library(caret)
library(kernlab)

dim(wifi_data_zero_V)
dim(wifi_validation_zero_V)

wifi_data_traning<-wifi_data_zero_V[,1:469]
wifi_data_validation<-wifi_validation_zero_V

wifi_data_traning_B1<-wifi_data_traning %>%
  filter(BUILDINGID==1)
wifi_data_traning_B1$FLOOR <- as.character(wifi_data_traning_B1$FLOOR)
wifi_data_traning_B1$FLOOR <- as.factor(wifi_data_traning_B1$FLOOR)

wifi_data_validation_B1<-wifi_data_validation %>%
  filter(BUILDINGID==1)
wifi_data_validation_B1$FLOOR <- as.character(wifi_data_validation_B1$FLOOR)
wifi_data_validation_B1$FLOOR <- as.factor(wifi_data_validation_B1$FLOOR)

# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning_B1),colnames(wifi_data_validation_B1))
wifi_data_traning_B1<-wifi_data_traning_B1[,cols_to_keep,drop=FALSE]

wifi_data_validation_B1<-wifi_data_validation_B1[,cols_to_keep,drop=FALSE]
wifi_data_validation_B1
#split the data into training and testing set

set.seed(123)
trainIndex_B1 <- createDataPartition(y = wifi_data_traning_B1$FLOOR, p = 0.75,
                                  list = FALSE)  

# Training and Test sets 

trainSet_B1 <- wifi_data_traning_B1 [trainIndex_B1,]
testSet_B1 <- wifi_data_traning_B1 [-trainIndex_B1,]






###################  KNN_BUILDING_1  #############################################
#trainSet_sample <- trainSet[sample(1:nrow(trainSet), 3000, replace=FALSE),]

set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

knnFit_b1 <- train(FLOOR ~ ., data = trainSet_B1, 
                   method = "knn", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
#Output of kNN fit
knnFit_b1
#This model seems to be really good as  accuracy and kappa  are almost 1

# test the k-NN model
knnPredict_b1_floor <- predict(knnFit_b1,newdata = testSet_B1)
# confusion matrix to see accuracy value and other parameter values
knnCM_b1_F <-confusionMatrix(knnPredict_b1_floor, testSet_B1$FLOOR)
knnCM_b1_F
# Apply k-NN model to the validation data
knnPredict_validation_b1_F <- predict(knnFit_b1,newdata = wifi_data_validation_B1)
validation_knnCM_b1_F<-confusionMatrix(knnPredict_validation_b1_F,wifi_data_validation_B1$FLOOR)
postResample(knnPredict_validation_F, wifi_data_validation$FLOOR)

#################### KNN_BUILDING_1  #####################


###################  SVM_BUILDING_1  #############################################
#trainSet_sample <- trainSet[sample(1:nrow(trainSet), 3000, replace=FALSE),]

set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

svmFit_b1 <- train(FLOOR ~ ., data = trainSet_B1, 
                   method = "svmLinear", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
#Output of kNN fit
svmFit_b1
#This model seems to be really good as  accuracy and kappa  are almost 1

# test the k-NN model
svmPredict_b1_floor <- predict(svmFit_b1,newdata = testSet_B1)
# confusion matrix to see accuracy value and other parameter values
svmCM_b1_F <-confusionMatrix(svmPredict_b1_floor, testSet_B1$FLOOR)
svmCM_b1_F
# Apply k-NN model to the validation data
svmPredict_validation_b1_F <- predict(svmFit_b1,newdata = wifi_data_validation_B1)
validation_svmCM_b1_F<-confusionMatrix(svmPredict_validation_b1_F,wifi_data_validation_B1$FLOOR)
postResample(svmPredict_validation_b1_F, wifi_data_validation_B1$FLOOR)

############################################## SVM_BUILDING_1  #####################

#################### RENDOM_FOREST_BUILDING_1 ###############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

RFFit_b1 <- train(FLOOR ~ ., data = trainSet_B1, 
                  method = "ranger", 
                  trControl = ctrl, 
                  #preProc = c("center", "scale"),
                  tuneLength = 2)
RFFit_b1
# test the RF model
RFPredict_b1_floor <- predict(RFFit_b1,newdata = testSet_B1)
RFCM_b1_F <-confusionMatrix(RFPredict_b1_floor, testSet_B1$FLOOR)
RFCM_b1_F
# Apply RF model to the validation data
RFPredict_validation_b1_F <- predict(RFFit_b1,newdata = wifi_data_validation_B1)
validation_RFCM_b1_F<-confusionMatrix(RFPredict_validation_b1_F,wifi_data_validation_B1$FLOOR)
postResample(RFPredict_validation_b1_F, wifi_data_validation_B1$FLOOR)

#################### RENDOM_FOREST_BUILDING_1 #########################################


############################################################## BUILDING 2 #####################################################
wifi_data_traning<-wifi_data_zero_V[,1:469]
wifi_data_validation<-wifi_validation_zero_V

wifi_data_traning_B2<-wifi_data_traning %>%
  filter(BUILDINGID==2)
#NO data are available in floor 5 so it gives an error  so to remove this error bellow code has been  used
wifi_data_traning_B2$FLOOR <- as.character(wifi_data_traning_B2$FLOOR)
wifi_data_traning_B2$FLOOR <- as.factor(wifi_data_traning_B2$FLOOR)

wifi_data_validation_B2<-wifi_data_validation %>%
  filter(BUILDINGID==2)
wifi_data_validation_B2$FLOOR <- as.character(wifi_data_validation_B2$FLOOR)
wifi_data_validation_B2$FLOOR <- as.factor(wifi_data_validation_B2$FLOOR)

# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning_B2),colnames(wifi_data_validation_B2))
wifi_data_traning_B2<-wifi_data_traning_B2[,cols_to_keep,drop=FALSE]

wifi_data_validation_B2<-wifi_data_validation_B2[,cols_to_keep,drop=FALSE]
wifi_data_validation_B2
#split the data into training and testing set

set.seed(123)
trainIndex_B2 <- createDataPartition(y = wifi_data_traning_B2$FLOOR, p = 0.75,
                                     list = FALSE)  

# Training and Test sets 

trainSet_B2 <- wifi_data_traning_B2 [trainIndex_B2,]
testSet_B2 <- wifi_data_traning_B2 [-trainIndex_B2,]


###################  KNN_BUILDING_2  #############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

knnFit_b2 <- train(FLOOR ~ ., data = trainSet_B2, 
                   method = "knn", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
#Output of kNN fit
knnFit_b2
#This model seems to be really good as  accuracy and kappa  are almost 1

# test the k-NN model
knnPredict_b2_floor <- predict(knnFit_b2,newdata = testSet_B2)
# confusion matrix to see accuracy value and other parameter values
knnCM_b2_F <-confusionMatrix(knnPredict_b2_floor, testSet_B2$FLOOR)
knnCM_b2_F
# Apply k-NN model to the validation data
knnPredict_validation_b2_F <- predict(knnFit_b2,newdata = wifi_data_validation_B2)
validation_knnCM_b2_F<-confusionMatrix(knnPredict_validation_b2_F,wifi_data_validation_B2$FLOOR)
postResample(knnPredict_validation_b2_F, wifi_data_validation_B2$FLOOR)
####################### KNN_BUILDING_2  #########################################


###################  SVM_BUILDING_2  #############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

svmFit_b2 <- train(FLOOR ~ ., data = trainSet_B2, 
                   method = "svmLinear", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
svmFit_b2
# test the SVM model
svmPredict_b2_floor <- predict(svmFit_b2,newdata = testSet_B2)
svmCM_b2_F <-confusionMatrix(svmPredict_b2_floor, testSet_B2$FLOOR)
svmCM_b2_F
# Apply k-NN model to the validation data
svmPredict_validation_b2_F <- predict(svmFit_b2,newdata = wifi_data_validation_B2)
validation_svmCM_b2_F<-confusionMatrix(svmPredict_validation_b2_F,wifi_data_validation_B2$FLOOR)
postResample(svmPredict_validation_b2_F, wifi_data_validation_B2$FLOOR)
###################  SVM_BUILDING_2  #############################################

#################### RENDOM FOREST_BUILDING_2 ###############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

RFFit_b2 <- train(FLOOR ~ ., data = trainSet_B2, 
                  method = "ranger", 
                  trControl = ctrl, 
                  #preProc = c("center", "scale"),
                  tuneLength = 2)
RFFit_b2
# test the SVM model
RFPredict_b2_floor <- predict(RFFit_b2,newdata = testSet_B2)
RFCM_b2_F <-confusionMatrix(RFPredict_b2_floor, testSet_B2$FLOOR)
RFCM_b2_F
# Apply k-NN model to the validation data
RFPredict_validation_b2_F <- predict(RFFit_b2,newdata = wifi_data_validation_B2)
validation_RFCM_b2_F<-confusionMatrix(RFPredict_validation_b2_F,wifi_data_validation_B2$FLOOR)
postResample(RFPredict_validation_b2_F, wifi_data_validation_B2$FLOOR)

#################### RENDOM FOREST_BUILDING_2 ###############################################



############################################################## BUILDING 3 #####################################################
wifi_data_traning<-wifi_data_zero_V[,1:469]
wifi_data_validation<-wifi_validation_zero_V

wifi_data_traning_B3<-wifi_data_traning %>%
  filter(BUILDINGID==3)

wifi_data_validation_B3<-wifi_data_validation %>%
  filter(BUILDINGID==3)

# Drop columns from validation that do not match with traning set
cols_to_keep <- intersect(colnames(wifi_data_traning_B3),colnames(wifi_data_validation_B3))
wifi_data_traning_B3<-wifi_data_traning_B3[,cols_to_keep,drop=FALSE]

wifi_data_validation_B3<-wifi_data_validation_B3[,cols_to_keep,drop=FALSE]
wifi_data_validation_B3
#split the data into training and testing set

set.seed(123)
trainIndex_B3 <- createDataPartition(y = wifi_data_traning_B3$FLOOR, p = 0.75,
                                     list = FALSE)  

# Training and Test sets 

trainSet_B3 <- wifi_data_traning_B3 [trainIndex_B3,]
testSet_B3 <- wifi_data_traning_B3 [-trainIndex_B3,]
###################  KNN_BUILDING_3  #############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

knnFit_b3 <- train(FLOOR ~ ., data = trainSet_B3, 
                   method = "knn", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
knnFit_b3
# test the k-NN model
knnPredict_b3_floor <- predict(knnFit_b3,newdata = testSet_B3)
knnCM_b3_F <-confusionMatrix(knnPredict_b3_floor, testSet_B3$FLOOR)
knnCM_b3_F
# Apply k-NN model to the validation data
knnPredict_validation_b3_F <- predict(knnFit_b3,newdata = wifi_data_validation_B3)
validation_knnCM_b3_F<-confusionMatrix(knnPredict_validation_b3_F,wifi_data_validation_B3$FLOOR)
postResample(knnPredict_validation_b3_F, wifi_data_validation_B3$FLOOR)
####################### KNN_BUILDING_3  #########################################

###################  SVM_BUILDING_3  #############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

svmFit_b3 <- train(FLOOR ~ ., data = trainSet_B3, 
                   method = "svmLinear", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
svmFit_b3
# test the SVM model
svmPredict_b3_floor <- predict(svmFit_b3,newdata = testSet_B3)
svmCM_b3_F <-confusionMatrix(svmPredict_b3_floor, testSet_B3$FLOOR)
svmCM_b3_F
# Apply k-NN model to the validation data
svmPredict_validation_b3_F <- predict(svmFit_b3,newdata = wifi_data_validation_B3)
validation_svmCM_b3_F<-confusionMatrix(svmPredict_validation_b3_F,wifi_data_validation_B3$FLOOR)
postResample(svmPredict_validation_b3_F, wifi_data_validation_B3$FLOOR)
###################  SVM_BUILDING_3  #############################################

#################### RENDOM FOREST_BUILDING_3  ###############################################
set.seed(123)
#Cross validation to avoid overlapping 
ctrl <- trainControl(method="cv",number = 5) 

RFFit_b3 <- train(FLOOR ~ ., data = trainSet_B3, 
                   method = "ranger", 
                   trControl = ctrl, 
                   #preProc = c("center", "scale"),
                   tuneLength = 2)
RFFit_b3
# test the SVM model
RFPredict_b3_floor <- predict(RFFit_b3,newdata = testSet_B3)
RFCM_b3_F <-confusionMatrix(RFPredict_b3_floor, testSet_B3$FLOOR)
RFCM_b3_F
# Apply k-NN model to the validation data
RFPredict_validation_b3_F <- predict(RFFit_b3,newdata = wifi_data_validation_B3)
validation_RFCM_b3_F<-confusionMatrix(RFPredict_validation_b3_F,wifi_data_validation_B3$FLOOR)
postResample(RFPredict_validation_b3_F, wifi_data_validation_B3$FLOOR)

#################### RENDOM FOREST_BUILDING_3  ###############################################


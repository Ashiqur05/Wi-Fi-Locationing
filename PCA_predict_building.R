
library(psych)
library(devtools)
library(ggbiplot)
library(caret)
library(kernlab)

dim(wifi_data_zero_V)
dim(wifi_validation_zero_V)



#split the data into training and testing set

set.seed(123)
trainIndex_PCA <- sample(2,nrow(wifi_data_zero_V),
                         replace = TRUE,
                         prob = c(0.8,0.2))

# Training and Test sets 

trainSet_PCA <- wifi_data_zero_V [trainIndex_PCA==1,]
testSet_PCA <- wifi_data_zero_V [trainIndex_PCA==2,]

#pairs.panels(trainSet_PCA[,1:10],gap=0,bg=c("red","yellow","blue")[trainSet_PCA$BUILDINGID],pch = 21)

PCA_wifi<-prcomp(trainSet_PCA[,-c(468:475)],center = TRUE)

PCA_wifi_test<-prcomp(testSet_PCA[,-c(468:475)],center = TRUE)

attributes(PCA_wifi)
PCA_wifi$center
mean(trainSet_PCA$LATITUDE)
PCA_wifi$sdev
summary(PCA_wifi)



#compute standard deviation of each principal component
std_dev <- PCA_wifi$sdev

std_dev_test<-PCA_wifi_test$sdev

#compute variance
pr_var <- std_dev^2

pr_var_test <- std_dev_test^2
#check variance of first 10 components
pr_var[1:10]
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

prop_varex_test <- pr_var_test/sum(pr_var_test)
prop_varex_test[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
 plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
 
 #scree plot
 plot(prop_varex_test, xlab = "Principal Component",
      ylab = "Proportion of Variance Explained",
      type = "b")
 
 #cumulative scree plot
 plot(cumsum(prop_varex_test), xlab = "Principal Component",
      ylab = "Cumulative Proportion of Variance Explained",
      type = "b")
 

 #add a training set with principal components
 train_data_pca <- data.frame(BUILDINGID = trainSet_PCA$BUILDINGID, PCA_wifi$x) 
 
 test_data_pca <- data.frame(BUILDINGID = testSet_PCA$BUILDINGID, PCA_wifi_test$x) 
 
 #we are interested in first 200 PCAs
 train_data_pca <- train_data_pca[,1:201]
 
 test_data_pca<-test_data_pca[,1:201]

 set.seed(123)
 #Cross validation to avoid overlapping 
 ctrl <- trainControl(method="cv",number = 5) 
 
 knnFit_build_PCA <- train(BUILDINGID ~ ., data = train_data_pca, 
                    method = "knn", 
                    trControl = ctrl, 
                    #preProc = c("center", "scale"),
                    tuneLength = 2)
 knnFit_build_PCA
 
 knnPredict_building_PCA <- predict(knnFit_build_PCA,newdata = test_data_pca)#test set is more then the training set
 knn_CM_build_PCA <-confusionMatrix(knnPredict_building_PCA, test_data_pca$BUILDINGID)
 
 #RFPredict_validation <- predict(RFFit,newdata = wifi_data_validation_pre_building)
 #validation_RFCM<-confusionMatrix(RFPredict_validation,wifi_data_validation_pre_building$BUILDINGID)
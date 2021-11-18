install.packages('caret',dependencies=c("Depends", "Suggests"))
install.packages("lattice")
install.packages("ggplot2")
install.packages("randomForest")


library(caret)
library(lattice)
library(ggplot2)

library(randomForest)
install.packages('mlbench',dependencies=c("Depends", "Suggests"))
library(mlbench)

#Installed for Correlation 
install.packages("corrplot")
library(corrplot)

#INSTALED FOR SVM
install.packages('e1071',dependencies=c("Depends", "Suggests"))
library(e1071)

install.packages("rpart", dependencies=c("Depends", "Suggests"))
library(rpart)


data<-read.csv('C:/Users/ypbow/Documents/C3T3 files/existingproductattributes2017.csv')
test<-read.csv('C:/Users/ypbow/Documents/C3T3 files/newproductattributes2017.csv')
test$Volume<-NULL
attributes(data)
summary(data)
str(data)
names(data)
hist(data$Price,main="Price Distribution",xlab="Price",ylab="Frequency")
hist(data$Recommendproduct,main="Product Recommendation",xlab="Recommendation Scale 0-1",ylab="count")
hist(data$ProductType,main="Product Recommendation",xlab="Recommendation Scale 0-1",ylab="count")
data$ProductType<-as.numeric(data$ProductType)

summary

#Check which column has missing values
names(which(colSums(is.na(data))>0))

#delete the attribute with missing values
data$BestSellersRank<-NULL

str(data)

#Dummify product types in data
dmy<-dummyVars("~ ProductType ",data=data)
trsf<-data.frame(predict(dmy,newdata=data))
str(trsf)

data$PC<-trsf$ProductTypePC
data$Laptop<-trsf$ProductTypeLaptop
data$Netbook<-trsf$ProductTypeNetbook
data$Smartphone<-trsf$ProductTypeSmartphone

head(data)

str(data)

#delete productType
data$ProductType<-NULL

corrData <- cor(data)

corrplot(corrData)
corrData
print(corrData)
print(corrData[15,2:6])







# Deleting Variables with high Correlation
data$x5StarReviews<-NULL
data$x4StarReviews<-NULL
data$x3StarReviews<-NULL
data$x2StarReviews<-NULL
data$x1StarReviews<-NULL
#data$Laptop<-NULL


#Final data
str(data)



#Dummify product types in test data
dmy1<-dummyVars("~ ProductType",data=test)
trsf1<-data.frame(predict(dmy,newdata=test))
str(trsf1)

test$PC<-trsf1$ProductTypePC
test$Laptop<-trsf1$ProductTypeLaptop
test$Netbook<-trsf1$ProductTypeNetbook
test$Smartphone<-trsf1$ProductTypeSmartphone

str(data)


# Deleting Variables with high Correlation in Test
test$x5StarReviews<-NULL
test$x4StarReviews<-NULL
test$x3StarReviews<-NULL
test$x2StarReviews<-NULL
test$x1StarReviews<-NULL

str(test)




1  #EXAMPLE 1 Linear 

set.seed(123)
names(data)
str(data)

inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)

#LMFit1 <- train(Volume~Price+Recommendproduct+ShippingWeight+ProductDepth+ProductWidth+ProductHeight+ProfitMargin+PC+Netbook+Smartphone, data = training, method = "lm", trControl=fitControl)
LMFit1 <- train(Volume~., data = training, method = "lm", trControl=fitControl)


#check the results
LMFit1
summary(LMFit1)

predlm<-predict(LMFit1,test)
test$modelPred<-predlm
plot(test$modelPred, main="Model Prediction DT TL5",xlab="index",ylab="count")


summary(test$modelPred)
str(test)

write.csv(test, file="C2.T3output.csv", row.names = TRUE)

head(test)






data<-read.csv('C:/Users/ypbow/Documents/C3T3 files/existingproductattributes2017.csv')
test<-read.csv('C:/Users/ypbow/Documents/C3T3 files/newproductattributes2017.csv')
test$Volume<-NULL
attributes(data)
summary(data)
str(data)
names(data)
hist(data$Price,main="Price Distribution",xlab="Price",ylab="Frequency")
hist(data$Recommendproduct,main="Product Recommendation",xlab="Recommendation Scale 0-1",ylab="count")
summary

#Check which column has missing values
names(which(colSums(is.na(data))>0))

#delete the attribute with missing values
data$BestSellersRank<-NULL
data$ProductNum<-NULL
data$xProfitMargin<-NULL

str(data)

#Dummify product types in data
dmy<-dummyVars("~ ProductType ",data=data)
trsf<-data.frame(predict(dmy,newdata=data))
str(trsf)


data$PC<-trsf$ProductTypePC
data$Laptop<-trsf$ProductTypeLaptop
data$Netbook<-trsf$ProductTypeNetbook

data$Smartphone<-trsf$ProductTypeSmartphone

head(data)

str(data)

#delete productType
data$ProductType<-NULL



#Dummify product types in test data
dmy1<-dummyVars("~ ProductType",data=test)
trsf1<-data.frame(predict(dmy,newdata=test))
str(trsf1)

test$PC<-trsf1$ProductTypePC
test$Laptop<-trsf1$ProductTypeLaptop
test$Netbook<-trsf1$ProductTypeNetbook
test$Smartphone<-trsf1$ProductTypeSmartphone

str(data)


test$BestSellersRank<-NULL
test$ProductNum<-NULL
test$xProfitMargin<-NULL






#Multicollinearity in DT and boosted trees algorithms

#https://towardsdatascience.com/why-feature-correlation-matters-a-lot-847e8ba439c4



#NON PARAMETRIC MACHINE LEARNING 

2  #EXAMPLE 2 Support Vector Machine!!!!!
set.seed(123)


inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)

SVMFit1 <- train(Volume~., data = training, method = "svmLinear", trControl=fitControl)


#check the results
SVMFit1
summary(SVMFit1)

predsvm<-predict(SVMFit1,test)
test$modelPred<-predsvm
plot(test$modelPred, main="Model Prediction SVM",xlab="index",ylab="count")


summary(test$modelPred)
str(test)

write.csv(test, file="prediction.csv", row.names = TRUE)

head(test)


importanceSVM1<-varImp(SVMFit1, scale=FALSE)
plot(importanceSVM1)







2  #EXAMPLE 2 Support Vector Machine 2 EXPEREMENT!!!!!
set.seed(123)


inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)


SVM1Fit1 <- train(Volume~x5StarReviews+x4StarReviews+x3StarReviews+x2StarReviews+x1StarReviews+PositiveServiceReview+NegativeServiceReview+PC+Netbook+Smartphone, data = training, method = "lm", trControl=fitControl)

#SVM1Fit1 <- train(Volume~., data = training, method = "svmliner", trControl=fitControl)


#check the results
SVM1Fit1
summary(SVM1Fit1)

predsvm<-predict(SVMFit1,test)
test$modelPred<-predsvm
plot(test$modelPred, main="Model Prediction SVM",xlab="index",ylab="count")


summary(test$modelPred)
str(test)

write.csv(test, file="predictionsvm1.csv", row.names = TRUE)

head(test)






3.  #THIRD EXAMPLE RANDOM FOREST Rsq .92

#Following the tutorial in resources
# define an 75%/25% train/test split of the dataset
#test$Volume=as.numeric(data$Volume)
names(data)
str(data)


inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

rfFit1 <- train(Volume~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results

rfFit1
#plot(rfFit1)

importance1<-varImp(rfFit1, scale=FALSE)
plot(importance1)

predrf1<-predict(rfFit1,test)

test$modelPred<-predrf1
head(test)
plot(test$modelPred, main="Model Prediction DT 1",xlab="Sales",ylab="count")
plot(test$x5StarReviews,test$modelPred, main="Model Prediction DT 1",xlab="5 Star Reviews",ylab="volume")

write.csv(test, file="C3T3DTpred.csv", row.names = TRUE)

head(test)


3a #THIRD EXAMPLE RANDOM FOREST TL5

#Following the tutorial in resources
# define an 75%/25% train/test split of the dataset
#test$Volume=as.numeric(data$Volume)
names(data)
str(data)


inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)

#train Random Forest with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

rfFit5 <- train(Volume~., data = training, method = "rf", trControl=fitControl, tuneLength = 5)
#training results

rfFit5
#plot(rfFit5)

importance5<-varImp(rfFit5, scale=FALSE)
plot(importance5)

predrf5<-predict(rfFit5,test)

test$modelPred<-predrf5
head(test)
plot(test$modelPred, main="Model Prediction FR",xlab="Sales",ylab="count")

write.csv(test, file="C3T3DTpredL5.csv", row.names = TRUE)


3b  #THIRD EXAMPLE RANDOM FOREST  Manual Tuning Grid


#Following the tutorial in resources
# define an 75%/25% train/test split of the dataset
#test$Volume=as.numeric(data$Volume)
names(data)
str(data)


inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)
rfGrid <- expand.grid(mtry=c(3,2,1))

#train Random Forest with a manual grid 

system.time(rfFit2 <- train(Volume~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))
#training results


rfFit2
#plot(rfFit2)

importance2<-varImp(rfFit2, scale=FALSE)
plot(importance2)

predrf2<-predict(rfFit2,test)

test$modelPred<-predrf2
head(test)
plot(test$modelPred, main="Model Prediction DT 1",xlab="Brand: 0-Acer    1-Sony",ylab="count")


write.csv(test, file="DTgr.csv", row.names = TRUE)

head(test)



#EXAMPLE WITH GBM

names(data)
str(data)

inTrain <- createDataPartition(data$Volume, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)
gbmFit1 <- train(Volume~., data = training, method = "gbm", trControl=fitControl, verbose=FALSE)



gbmFit1
plot(gbmFit1)

summary(data$Volume)




predgbm<-predict(gbmFit1,test)
test$modelPred<-predgbm
plot(test$modelPred)

write.csv(test, file="C3T3GBM.csv", row.names = TRUE)

head(test)





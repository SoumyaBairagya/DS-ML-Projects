#Problem Statement ----
#ABHA MARATHE2:27 PM
#Classifier Performance
#Apply KNN to the Smarket data set of ISLR package. Split the data into training and testing samples. Scale the data and find the following
#1.	Accuracy
#2.	Sensitivity
#3.	Specificity
#4.	Precision

#Finding whether market is UP i.e Positive case is UP
#Using Library caTools for sample.split function ----
library(caTools)
library(ISLR)
print(head(Smarket,3))
f <- Smarket[,2:9]
print(head(f,3))

#Scaling the data and partition the data ----
set.seed(123)
split <- sample.split(f,SplitRatio=0.8)
tr_data <- subset(f,split==T)
ts_data <- subset(f,split==F)
print(tr_data)
print(ts_data)
#print(length(ts_data))

#Normalize Function ----
#norm <- function(x){return(x-min(x))}
#tr_scaled <- norm(tr_data[,-5])
#ts_scaled <- norm(ts_data[,-5])

#Scaling ----
tr_scaled <- scale(tr_data[,-8])
ts_scaled <- scale(ts_data[,-8])

#train<-cbind(tr_scaled,tr_data$Species)
#test<-cbind(ts_scaled,ts_data$Species)
#print(train)
#print(test)

#Finding KNN ----
library(class)
pred <- knn(tr_scaled,ts_scaled,tr_data$Direction,k=13)
t <- table(actual=ts_data$Direction, predicted=pred)
print(t)

#Finding Classifier's performance metrics ----
accuracy1 <- sum(diag(t))/sum(t)
sensitivity1 <- t[2,2]/sum(t[2,])
specificity1 <- t[1,1]/sum(t[1,])
precision1 <- t[2,2]/sum(t[,2])

#AUC ----
library(cvAUC)
ac <- AUC(as.numeric(pred),as.numeric(ts_data$Direction))

#Printing Performance Metrics ----
cat("1. The accuracy of the model is: ",accuracy1)
cat("\n2. The sensitivity of the model is: ",sensitivity1)
cat("\n3. The specificity of the model is: ",specificity1)
cat("\n4. The precision of the model is: ",precision1)
cat("\n5. The area under the curve is: ",ac)

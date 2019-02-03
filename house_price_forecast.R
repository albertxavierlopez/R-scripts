#Directorio de trabajo
setwd("C:/Users/Albert/Desktop/R Studio")

###------- Starting a machine learning project ---------###
###------- ( Regression Tree )---------###

#Packages
# utility functions

library("tidyverse") 
# for regression trees

library(rpart) 
# for random forests

library(randomForest) 


#Read the data from a table and visualize
data<-read.csv("../train.csv")
summary(data)
names(data)
nrow(data)
ncol(data)


#Defining model (regression tree)
fit<- rpart(SalePrice~LotArea+YearBuilt, data=data)
plot(fit, uniform=TRUE)
text(fit, cex=.6)

#Predictions based on model
print("Predictions for the following 5 houses")
print(head(data))

print("The predictions are")

print(predict(fit, head(data)))

print("Actual price")
print(head(data$SalePrice))

#Measuring model accuracy

library("modelr")
#Getting the mean average error (MAE). On average, our predictions are off by about X.
mae(model=fit, data=data)

#We HAVE TO separate data into testing an training data. 
#To split data we can use this function from modelr library

splitdata<-resample_partition(data, c(test=0.3, train=0.7))
#how many cases are in test and training set?
lapply(splitdata, dim)

#Fitting a new model to our trainig data
fit2<- rpart(SalePrice~LotArea+YearBuilt, data=splitdata$train)
mae(model=fit2, data=splitdata$test)

###------- ( Random forest )---------###

fitRandomForest <- randomForest(SalePrice~LotArea+YearBuilt, data=splitdata$train)
mae(model=fitRandomForest, data=splitdata$train)

#Competition between models adding more variables (añadiendo más variables los modelos se vuelven peores)

fit2<- rpart(SalePrice~LotArea+YearBuilt+HouseStyle+BedroomAbvGr+GarageCars, data=splitdata$train)
mae(model=fit2, data=splitdata$train)

fitRandomForest <- randomForest(SalePrice~LotArea+YearBuilt+HouseStyle+BedroomAbvGr+GarageCars, data=splitdata$train)
mae(model=fitRandomForest, data=splitdata$train)

# Predict values in test set
pred <- predict(fitRandomForest, newdata=splitdata$test)

head(pred)

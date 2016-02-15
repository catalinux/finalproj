library(caret)

#'read data
train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

#'peek our data
dim(train)
dim(test)

#'what type of results we have
table(train$classe)


#'having same results each time
set.seed(131)

#'parition data for training and testing sets

inTrain <- createDataPartition(train$classe, p = 0.7, list = FALSE)
data.train <- train[inTrain, ]
data.valid <- train[-inTrain, ]

#'preprocess data

#'remove zerovariance data
nearZero <- nearZeroVar(data.train)
data.train <- subset(data.train,select = -c(nearZero))
test <- subset(test,select = -c(nearZero))


#'remove uncomplete observations
#data.train<-na.omit(data.train)
#'using random forest for training data
library(randomForest)
randForestModel <- randomForest(classe ~ ., data = data.train, importance = TRUE, ntrees = 10)

data.predict <- predict(randForestModel,data.train)
print(confusionMatrix(data.predict, data.train$classe))
#'The result of the model are great. We shall validate the result by using cross-validation
data.predictT <- predict(randForestModel,data.valid)
print(confusionMatrix(data.predictT, data.valid$classe))

#'accuracy is 94%  so our model went great als  with validation set havint 5% sample-out error

#' Prediction of algorithm
pred <- predict(randForestModel,test)
pred
#'writing predicted data to a file
write(pred,"pred.txt")


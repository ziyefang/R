install.packages("e1071")
install.packages("randomForest")
library(pROC)
library(e1071)
library(randomForest)
#1. import data
raw.training <- read.table("simulatedConcentrations.recoded.training.txt", sep = "\t", header = T)
raw.test <- read.table("simulatedConcentrations.recoded.test.txt", sep = "\t", header = T)
raw.training$cc <- as.factor(raw.training$cc)
raw.test$cc <- as.factor(raw.test$cc)
#How many rows and columns are in the training set?
training_row <- nrow(raw.training)
training_row
#700
traing_col <- ncol(raw.training)
traing_col
#251
#How many rows and columns are in the test?
test_row <- nrow(raw.test)
test_row
#300
test_col <- ncol(raw.test)
test_col
#251
#Is it a balanced problem (same number of elements in each class)?
#The number of columns of training and test set are the same, the number of rows of training set is 700, and test set has 300 rows, which correspond to 70% and 30% of the whole dataset.

#2. Support vector machines
#Fit a linear SVM on the training set data with C classification
lsvm.fit <- svm(cc ~ ., data = raw.training, type = "C-classification", kernel = "linear", probability = T)
#inspect the resulting support vectors (number and type). What is the percentage of training data used for classification?
percentage_of_training <- nrow(lsvm.fit$SV) / training_row
percentage_of_training
#0.3085714
#Predict the class of the test set and estimate the discriminative performances by computing the AUC. 
lsvm.pred <- predict(lsvm.fit, newdata = raw.test[,-1], decision.values = F, probability = T)
auc(raw.test$cc, attr(lsvm.pred, "probabilities")[,2])
#auc 0.893
#Change cost value
#Change kernel functions
lsvm.fit <- svm(cc ~ ., data = raw.training, type = "C-classification", kernel = "linear", probability = T, cost = 0.1)
lsvm.pred <- predict(lsvm.fit, newdata = raw.test[,-1], decision.values = F, probability = T)
auc(raw.test$cc, attr(lsvm.pred, "probabilities")[,2])
#auc 0.8877
lsvm.fit <- svm(cc ~ ., data = raw.training, type = "C-classification", kernel = "linear", probability = T, cost = 1)
lsvm.pred <- predict(lsvm.fit, newdata = raw.test[,-1], decision.values = F, probability = T)
auc(raw.test$cc, attr(lsvm.pred, "probabilities")[,2])
#auc 0.893
lsvm.fit <- svm(cc ~ ., data = raw.training, type = "C-classification", kernel = "linear", probability = T, cost = 10)
lsvm.pred <- predict(lsvm.fit, newdata = raw.test[,-1], decision.values = F, probability = T)
auc(raw.test$cc, attr(lsvm.pred, "probabilities")[,2])
#auc 0.893
#Different methods
lsvm.fit <- svm(cc ~ ., data = raw.training, type = "C-classification", kernel = "polynomial", probability = T)
lsvm.pred <- predict(lsvm.fit, newdata = raw.test[,-1], decision.values = F, probability = T)
auc(raw.test$cc, attr(lsvm.pred, "probabilities")[,2])
#auc 0.8998
lsvm.fit <- svm(cc ~ ., data = raw.training, type = "C-classification", kernel = "radial", probability = T)
lsvm.pred <- predict(lsvm.fit, newdata = raw.test[,-1], decision.values = F, probability = T)
auc(raw.test$cc, attr(lsvm.pred, "probabilities")[,2])
#auc 0.9126


#3.RandomForest
rf.fit <- randomForest(cc ~ ., data = raw.training, importance = T, type = "classification")
varImpPlot(rf.fit)
rf.pred <- predict(rf.fit, raw.test, type = "prob")
auc(raw.test$cc, rf.pred[,2])
#auc 0.9126
rf.fit <- randomForest(cc ~ ., data = raw.training, ntree = 500, importance = T, type = "classification")
varImpPlot(rf.fit)
rf.pred <- predict(rf.fit, raw.test, type = "prob")
auc(raw.test$cc, rf.pred[,2])
#0.913
rf.fit <- randomForest(cc ~ ., data = raw.training, ntree = 1000, importance = T, type = "classification")
varImpPlot(rf.fit)
rf.pred <- predict(rf.fit, raw.test, type = "prob")
auc(raw.test$cc, rf.pred[,2])
#0.9214
rf.fit <- randomForest(cc ~ ., data = raw.training, ntree = 1500, importance = T, type = "classification")
varImpPlot(rf.fit)
rf.pred <- predict(rf.fit, raw.test, type = "prob")
auc(raw.test$cc, rf.pred[,2])
#0.9232
rf.fit <- randomForest(cc ~ ., data = raw.training, ntree = 2000, importance = T, type = "classification")
varImpPlot(rf.fit)
rf.pred <- predict(rf.fit, raw.test, type = "prob")
auc(raw.test$cc, rf.pred[,2])
#0.9158
rf.fit <- randomForest(cc ~ ., data = raw.training, ntree = 2500, importance = T, type = "classification")
varImpPlot(rf.fit)
rf.pred <- predict(rf.fit, raw.test, type = "prob")
auc(raw.test$cc, rf.pred[,2])
#0.9202
plot(lsvm.fit, raw.test, p_61 ~ p_141)
